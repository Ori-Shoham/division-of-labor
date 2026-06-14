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

  if (any(has_monetary_value & is.na(ym))) {
    stop(
      "Cannot deflate monetary values with missing month in column: ",
      ym_col
    )
  }

  needed_ym <- unique(as.character(ym[has_monetary_value]))
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
