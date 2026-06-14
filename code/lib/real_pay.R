# =============================================================================
# File: code/lib/real_pay.R
#
# Purpose:
#   Helpers for converting UKHLS monetary variables from nominal pounds to
#   constant December 2019 pounds using a monthly UK price index.
# =============================================================================

REAL_PAY_VARIABLES <- c(
  "basrate",
  "paygu_dv",
  "fimnlabgrs_dv",
  "fimngrs_dv",
  "fihhmngrs_dv"
)

clean_monetary_value <- function(x) {
  if (requireNamespace("haven", quietly = TRUE)) {
    x <- haven::zap_labels(x)
  }
  x <- suppressWarnings(as.numeric(x))
  x[!is.na(x) & x < 0] <- NA_real_
  x
}

load_real_pay_deflator <- function(
    price_index_path = UK_PRICE_INDEX_MONTHLY_CSV,
    base_ym = REAL_PAY_BASE_YM
) {
  if (!file.exists(price_index_path)) {
    stop("Price-index file not found: ", price_index_path)
  }

  idx <- utils::read.csv(price_index_path, stringsAsFactors = FALSE)

  required_cols <- c("ym", "index")
  missing_cols <- setdiff(required_cols, names(idx))
  if (length(missing_cols) > 0) {
    stop(
      "Price-index file must contain columns: ",
      paste(required_cols, collapse = ", "),
      ". Missing: ",
      paste(missing_cols, collapse = ", ")
    )
  }

  idx$ym <- as.Date(idx$ym)
  idx$index <- suppressWarnings(as.numeric(idx$index))

  idx <- idx[!is.na(idx$ym) & !is.na(idx$index), required_cols]
  idx <- idx[!duplicated(idx$ym), ]

  base_ym <- as.Date(base_ym)
  base_index <- idx$index[idx$ym == base_ym]

  if (length(base_index) != 1 || is.na(base_index)) {
    stop("Base month missing from price-index file: ", format(base_ym, "%Y-%m"))
  }

  idx$deflator <- base_index / idx$index
  idx
}

# -----------------------------------------------------------------------------
# impute_ym_median_month
#
# Builds a Date (first of month) from raw UKHLS interview-date components,
# imputing a missing interview month as the median month among observations
# that share the same interview year within the same wave load.
#
# Args:
#   intdaty_dv : integer vector of interview years  (negative = UKHLS missing)
#   intdatm_dv : integer vector of interview months (negative = UKHLS missing)
#
# Returns a Date vector (NA where year is also missing or unresolvable).
# -----------------------------------------------------------------------------
impute_ym_median_month <- function(intdaty_dv, intdatm_dv) {
  year  <- suppressWarnings(as.integer(intdaty_dv))
  month <- suppressWarnings(as.integer(intdatm_dv))

  year[!is.na(year)   & year  < 0] <- NA_integer_
  month[!is.na(month) & month < 0] <- NA_integer_

  needs_imputation <- !is.na(year) & is.na(month)

  if (any(needs_imputation)) {
    fully_observed <- !is.na(year) & !is.na(month)
    if (any(fully_observed)) {
      median_by_year <- tapply(
        month[fully_observed],
        year[fully_observed],
        function(m) as.integer(round(stats::median(m)))
      )
      imputed <- median_by_year[as.character(year[needs_imputation])]
      month[needs_imputation] <- as.integer(imputed)
      n_ok  <- sum(!is.na(as.integer(imputed)))
      n_gap <- sum( is.na(as.integer(imputed)))
    } else {
      n_ok  <- 0L
      n_gap <- sum(needs_imputation)
    }
    if (n_ok  > 0) message("Imputed interview month for ", n_ok,
                           " obs using median month within interview year.")
    if (n_gap > 0) warning(n_gap,
                           " obs have missing interview month with no year-mates",
                           " to impute from — ym set to NA.")
  }

  dplyr::if_else(
    !is.na(year) & !is.na(month),
    as.Date(sprintf("%04d-%02d-01", year, month)),
    as.Date(NA_character_)
  )
}

# -----------------------------------------------------------------------------
add_real_pay_vars <- function(df,
                              ym_col = "ym",
                              vars = REAL_PAY_VARIABLES,
                              suffix = "_real",
                              deflator = NULL,
                              price_index_path = UK_PRICE_INDEX_MONTHLY_CSV,
                              base_ym = REAL_PAY_BASE_YM) {
  if (!ym_col %in% names(df)) {
    stop("Month column not found for real-pay conversion: ", ym_col)
  }

  vars <- intersect(vars, names(df))
  if (length(vars) == 0) {
    return(df)
  }

  if (is.null(deflator)) {
    deflator <- load_real_pay_deflator(
      price_index_path = price_index_path,
      base_ym = base_ym
    )
  }

  ym <- as.Date(df[[ym_col]])
  deflator_lookup <- stats::setNames(deflator$deflator, as.character(deflator$ym))

  cleaned_values <- lapply(vars, function(v) clean_monetary_value(df[[v]]))
  names(cleaned_values) <- vars

  has_monetary_value <- Reduce(
    `|`,
    lapply(cleaned_values, function(x) !is.na(x))
  )

  n_missing_ym <- sum(has_monetary_value & is.na(ym))
  if (n_missing_ym > 0) {
    warning(
      n_missing_ym, " observation(s) have monetary values but missing '",
      ym_col, "' — real-pay values set to NA for those rows."
    )
  }

  # Exclude NA months from the coverage check; deflator_lookup[as.character(NA)]
  # returns NA naturally, so those rows receive NA real-pay values.
  needed_ym <- unique(na.omit(as.character(ym[has_monetary_value])))
  missing_ym <- setdiff(needed_ym, names(deflator_lookup))

  if (length(missing_ym) > 0) {
    stop(
      "Price-index file is missing required month(s): ",
      paste(sort(missing_ym), collapse = ", ")
    )
  }

  row_deflator <- unname(deflator_lookup[as.character(ym)])

  for (v in vars) {
    df[[paste0(v, suffix)]] <- cleaned_values[[v]] * row_deflator
  }

  df
}
