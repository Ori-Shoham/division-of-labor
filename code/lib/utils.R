# =============================================================================
# File: code/lib/utils.R
#
# Purpose:
#   Small helper functions reused across modules/scripts.
# =============================================================================

# first_valid()
# Used when the same "baseline" variable is repeated across waves:
#   - take the first non-missing and not equal to -8 (NIU)
first_valid <- function(x) {
  x2 <- x[!is.na(x) & x != -8]
  if (length(x2) == 0) NA else x2[1]
}

# wave_label_map()
# Consistent ordering + human-readable labels for plotting.
wave_label_map <- function() {
  wave_label_lookup() %>%
    dplyr::transmute(
      wave,
      wave_lab = wave_label_short
    )
}

# =============================================================================
# make_person_year_panel()
#
# Convert person-month (pidp x ym) data into person-year (pidp x year).
#
# Rules:
#   - "last": keep latest observed month in the year (often best for outcomes)
#   - "first": keep earliest observed month in the year
#   - "mean": average numeric vars within year; take modal for categorical
#
# Notes:
#   - Requires columns: pidp, ym (Date), year (int)
#   - Set exclude to avoid aggregating ids/labels you want unchanged or dropped
# =============================================================================

make_person_year_panel <- function(df,
                                   rule = c("last", "first", "mean"),
                                   exclude = c("wave", "intdaty_dv", "intdatm_dv", "ym_str")) {
  rule <- match.arg(rule)
  
  stopifnot(all(c("pidp", "ym", "year") %in% names(df)))
  
  # columns we’ll aggregate
  cols <- setdiff(names(df), c("pidp", "ym", "year", exclude))
  
  if (rule %in% c("last", "first")) {
    df2 <- df %>%
      dplyr::arrange(pidp, year, ym) %>%
      dplyr::group_by(pidp, year)
    
    df2 <- if (rule == "last") df2 %>% dplyr::slice_tail(n = 1) else df2 %>% dplyr::slice_head(n = 1)
    
    return(df2 %>% dplyr::ungroup())
  }
  
  # ---- rule == "mean" --------------------------------------------------------
  # numeric -> mean; non-numeric -> mode (most frequent non-missing)
  mode1 <- function(x) {
    x <- x[!is.na(x)]
    if (length(x) == 0) return(NA)
    tab <- sort(table(x), decreasing = TRUE)
    names(tab)[1]
  }
  
  summarise_one <- function(x) {
    if (is.numeric(x)) mean(x, na.rm = TRUE) else mode1(x)
  }
  
  df %>%
    dplyr::group_by(pidp, year) %>%
    dplyr::summarise(
      dplyr::across(dplyr::all_of(cols), summarise_one),
      # keep a representative ym for reference: last month observed in year
      ym = max(ym, na.rm = TRUE),
      .groups = "drop"
    )
}

# =============================================================================
# make_yearly_summary()
#
# Create year-level summaries from person-year data.
# Numeric vars -> mean, with N.
# =============================================================================

make_yearly_summary <- function(df_person_year, vars) {
  stopifnot("year" %in% names(df_person_year))
  stopifnot(all(vars %in% names(df_person_year)))
  
  df_person_year %>%
    dplyr::group_by(year) %>%
    dplyr::summarise(
      n = dplyr::n(),
      dplyr::across(dplyr::all_of(vars), ~ mean(.x, na.rm = TRUE), .names = "mean_{.col}"),
      .groups = "drop"
    )
}