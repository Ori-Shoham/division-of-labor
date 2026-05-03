# =============================================================================
# File: code/lib/husits_harmonization.R
#
# Purpose:
#   Harmonize Understanding Society childcare-responsibility variable husits.
#
# Main-stage coding:
#   -9 missing
#   -8 inapplicable
#   -7 proxy
#   -2 refusal
#   -1 don't know
#    1 Mainly self
#    2 Mainly partner
#    3 Shared
#    4 Or someone else?
#
# COVID coding:
#   -8 inapplicable
#   -2 refusal
#   -1 don't know
#    1 Always me
#    2 Usually me
#    3 Me and my partner about equally
#    4 Usually partner
#    5 Always partner
#    6 Always or usually other person in the household
#    7 Children look after themselves
#    8 Not applicable
#    9 Always or usually other person in support bubble
#
# Harmonized coding:
#    1 Mainly self
#    2 Mainly partner
#    3 Shared
#    4 Someone else / other
#
# Default:
#   COVID code 8 is treated as missing, not as "other".
# =============================================================================

suppressPackageStartupMessages({
  library(tidyverse)
})

clean_husits_main <- function(x) {
  x <- suppressWarnings(as.numeric(x))
  
  dplyr::case_when(
    is.na(x) ~ NA_real_,
    x < 0 ~ NA_real_,
    x %in% c(1, 2, 3, 4) ~ x,
    TRUE ~ NA_real_
  )
}

clean_husits_covid <- function(x, covid_code8_to_missing = TRUE) {
  x <- suppressWarnings(as.numeric(x))
  
  dplyr::case_when(
    is.na(x) ~ NA_real_,
    x < 0 ~ NA_real_,
    x %in% c(1, 2) ~ 1,
    x %in% c(4, 5) ~ 2,
    x == 3 ~ 3,
    x %in% c(6, 7, 9) ~ 4,
    x == 8 & covid_code8_to_missing ~ NA_real_,
    x == 8 & !covid_code8_to_missing ~ 4,
    TRUE ~ NA_real_
  )
}

label_husits <- function(x) {
  x <- suppressWarnings(as.numeric(x))
  
  factor(
    dplyr::case_when(
      x == 1 ~ "Mainly self",
      x == 2 ~ "Mainly partner",
      x == 3 ~ "Shared",
      x == 4 ~ "Someone else / other",
      TRUE ~ NA_character_
    ),
    levels = c(
      "Mainly self",
      "Mainly partner",
      "Shared",
      "Someone else / other"
    )
  )
}

add_husits_main_vars <- function(df,
                                 raw_var = "husits",
                                 out_var = "husits") {
  if (!(raw_var %in% names(df))) {
    return(df)
  }
  
  raw_name <- paste0(out_var, "_raw")
  cat_name <- paste0(out_var, "_cat")
  
  raw <- suppressWarnings(as.numeric(df[[raw_var]]))
  clean <- clean_husits_main(raw)
  
  df %>%
    dplyr::mutate(
      "{raw_name}" := raw,
      "{out_var}" := clean,
      "{cat_name}" := label_husits(clean)
    )
}

add_husits_covid_vars <- function(df,
                                  raw_var = "husits",
                                  out_var = "husits",
                                  covid_code8_to_missing = TRUE) {
  if (!(raw_var %in% names(df))) {
    return(df)
  }
  
  raw_name <- paste0(out_var, "_raw")
  cat_name <- paste0(out_var, "_cat")
  
  raw <- suppressWarnings(as.numeric(df[[raw_var]]))
  clean <- clean_husits_covid(
    raw,
    covid_code8_to_missing = covid_code8_to_missing
  )
  
  df %>%
    dplyr::mutate(
      "{raw_name}" := raw,
      "{out_var}" := clean,
      "{cat_name}" := label_husits(clean)
    )
}

# -----------------------------------------------------------------------------
# Couple-level husband/wife directional variables
#
# Input:
#   husits_w = wife's harmonized report
#   husits_h = husband's harmonized report
#
# Main harmonized code:
#   1 Mainly self
#   2 Mainly partner
#   3 Shared
#   4 Someone else / other
#
# Wife report:
#   self    = wife
#   partner = husband
#
# Husband report:
#   self    = husband
#   partner = wife
# -----------------------------------------------------------------------------
add_couple_husits_direction_vars <- function(df) {
  
  if (!("husits_w" %in% names(df))) {
    df$husits_w <- NA_real_
  }
  if (!("husits_h" %in% names(df))) {
    df$husits_h <- NA_real_
  }
  
  df %>%
    dplyr::mutate(
      # Wife report
      husits_wife_main_wreport = dplyr::case_when(
        is.na(husits_w) ~ NA_real_,
        husits_w == 1 ~ 1,
        husits_w %in% c(2, 3, 4) ~ 0,
        TRUE ~ NA_real_
      ),
      husits_husband_main_wreport = dplyr::case_when(
        is.na(husits_w) ~ NA_real_,
        husits_w == 2 ~ 1,
        husits_w %in% c(1, 3, 4) ~ 0,
        TRUE ~ NA_real_
      ),
      husits_shared_wreport = dplyr::case_when(
        is.na(husits_w) ~ NA_real_,
        husits_w == 3 ~ 1,
        husits_w %in% c(1, 2, 4) ~ 0,
        TRUE ~ NA_real_
      ),
      husits_other_wreport = dplyr::case_when(
        is.na(husits_w) ~ NA_real_,
        husits_w == 4 ~ 1,
        husits_w %in% c(1, 2, 3) ~ 0,
        TRUE ~ NA_real_
      ),
      
      # Husband report
      husits_wife_main_hreport = dplyr::case_when(
        is.na(husits_h) ~ NA_real_,
        husits_h == 2 ~ 1,
        husits_h %in% c(1, 3, 4) ~ 0,
        TRUE ~ NA_real_
      ),
      husits_husband_main_hreport = dplyr::case_when(
        is.na(husits_h) ~ NA_real_,
        husits_h == 1 ~ 1,
        husits_h %in% c(2, 3, 4) ~ 0,
        TRUE ~ NA_real_
      ),
      husits_shared_hreport = dplyr::case_when(
        is.na(husits_h) ~ NA_real_,
        husits_h == 3 ~ 1,
        husits_h %in% c(1, 2, 4) ~ 0,
        TRUE ~ NA_real_
      ),
      husits_other_hreport = dplyr::case_when(
        is.na(husits_h) ~ NA_real_,
        husits_h == 4 ~ 1,
        husits_h %in% c(1, 2, 3) ~ 0,
        TRUE ~ NA_real_
      )
    ) %>%
    dplyr::mutate(
      husits_wife_main_avg = rowMeans(
        dplyr::across(dplyr::any_of(c(
          "husits_wife_main_wreport",
          "husits_wife_main_hreport"
        ))),
        na.rm = TRUE
      ),
      husits_husband_main_avg = rowMeans(
        dplyr::across(dplyr::any_of(c(
          "husits_husband_main_wreport",
          "husits_husband_main_hreport"
        ))),
        na.rm = TRUE
      ),
      husits_shared_avg = rowMeans(
        dplyr::across(dplyr::any_of(c(
          "husits_shared_wreport",
          "husits_shared_hreport"
        ))),
        na.rm = TRUE
      ),
      husits_other_avg = rowMeans(
        dplyr::across(dplyr::any_of(c(
          "husits_other_wreport",
          "husits_other_hreport"
        ))),
        na.rm = TRUE
      ),
      dplyr::across(
        dplyr::any_of(c(
          "husits_wife_main_avg",
          "husits_husband_main_avg",
          "husits_shared_avg",
          "husits_other_avg"
        )),
        ~ dplyr::if_else(is.nan(.x), NA_real_, .x)
      )
    )
}