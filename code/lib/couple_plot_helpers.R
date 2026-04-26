# =============================================================================
# File: code/lib/couple_plot_helpers.R
#
# Purpose:
#   Helpers for couple-level plotting workflows:
#     - reshape couple-long wide-by-spouse panels to spouse-long
#     - child-subset filtering
#     - treatment labeling
#     - variable labeling for plot titles / axes / file stems
# =============================================================================

suppressPackageStartupMessages({
  library(tidyverse)
})

# -----------------------------------------------------------------------------
# Internal helper: identify spouse-specific columns ending in _h / _w
# -----------------------------------------------------------------------------
.get_spouse_specific_cols <- function(df) {
  nms <- names(df)
  
  husband_cols <- nms[stringr::str_detect(nms, "_h$")]
  wife_cols    <- nms[stringr::str_detect(nms, "_w$")]
  
  bases_h <- stringr::str_remove(husband_cols, "_h$")
  bases_w <- stringr::str_remove(wife_cols, "_w$")
  
  common_bases <- intersect(bases_h, bases_w)
  
  list(
    husband_cols = husband_cols,
    wife_cols = wife_cols,
    common_bases = common_bases
  )
}

# -----------------------------------------------------------------------------
# Convert couple-long wide-by-spouse panel to spouse-long panel
# -----------------------------------------------------------------------------
reshape_couple_long_to_spouse_long <- function(
    df,
    time_vars = c("couple_id", "wave", "ym", "year", "intdaty_dv", "intdatm_dv"),
    keep_only_common_spouse_vars = TRUE
) {
  
  spouse_info <- .get_spouse_specific_cols(df)
  
  if (keep_only_common_spouse_vars) {
    husband_keep <- paste0(spouse_info$common_bases, "_h")
    wife_keep    <- paste0(spouse_info$common_bases, "_w")
  } else {
    husband_keep <- spouse_info$husband_cols
    wife_keep    <- spouse_info$wife_cols
  }
  
  couple_level_cols <- setdiff(
    names(df),
    c(spouse_info$husband_cols, spouse_info$wife_cols)
  )
  
  couple_level_cols <- union(time_vars[time_vars %in% names(df)], couple_level_cols)
  
  df_h <- df %>%
    dplyr::select(dplyr::all_of(c(couple_level_cols, husband_keep))) %>%
    dplyr::rename_with(
      ~ stringr::str_remove(.x, "_h$"),
      .cols = dplyr::all_of(husband_keep)
    ) %>%
    dplyr::mutate(
      spouse = "Husband",
      spouse_pidp = husband_pidp
    )
  
  df_w <- df %>%
    dplyr::select(dplyr::all_of(c(couple_level_cols, wife_keep))) %>%
    dplyr::rename_with(
      ~ stringr::str_remove(.x, "_w$"),
      .cols = dplyr::all_of(wife_keep)
    ) %>%
    dplyr::mutate(
      spouse = "Wife",
      spouse_pidp = wife_pidp
    )
  
  dplyr::bind_rows(df_h, df_w) %>%
    dplyr::mutate(
      spouse = factor(spouse, levels = c("Wife", "Husband"))
    ) %>%
    dplyr::arrange(couple_id, spouse, dplyr::across(dplyr::any_of(c("wave", "ym", "year"))))
}

# -----------------------------------------------------------------------------
# Child subgroup filter helper
#
# child_subset options:
#   "all"
#   "u10"
#   "11_17"
# -----------------------------------------------------------------------------
filter_couples_by_child_subset <- function(df, child_subset = c("all", "u10", "11_17")) {
  child_subset <- match.arg(child_subset)
  
  if (child_subset == "all") {
    return(df)
  }
  
  if (child_subset == "u10") {
    return(df %>% dplyr::filter(has_child_u10_2019))
  }
  
  if (child_subset == "11_17") {
    return(df %>% dplyr::filter(has_child_11_17_2019))
  }
  
  df
}

# -----------------------------------------------------------------------------
# Keep only the two comparison child groups for facet-grid versions
# -----------------------------------------------------------------------------
filter_couples_for_child_grid <- function(df) {
  df %>%
    dplyr::filter(has_child_u10_2019 | has_child_11_17_2019) %>%
    dplyr::mutate(
      child_group_plot = dplyr::case_when(
        has_child_u10_2019   ~ "Young kids: 0-10",
        has_child_11_17_2019 ~ "Older kids: 11-17",
        TRUE                 ~ NA_character_
      ),
      child_group_plot = factor(
        child_group_plot,
        levels = c("Young kids: 0-10", "Older kids: 11-17")
      )
    )
}

# -----------------------------------------------------------------------------
# Default treated/control labels by treatment variable
# -----------------------------------------------------------------------------
default_treated_label <- function(treatment_var) {
  dplyr::case_when(
    treatment_var == "treat_wife_key_notedu_husb_not_or_edu" ~
      "Wife key (non-edu), husband not / edu",
    treatment_var == "treat_wife_key_notedu_any" ~
      "Wife key (non-edu)",
    treatment_var == "treat_husb_shutdown_wife_not" ~
      "Husband shutdown, wife not",
    TRUE ~ "Treated"
  )
}

default_untreated_label <- function(treatment_var) {
  "All other couples"
}

# -----------------------------------------------------------------------------
# Treatment variable label helper
#
# Order is always:
#   1) treated
#   2) untreated
# -----------------------------------------------------------------------------
label_treatment_values <- function(x,
                                   treatment_var,
                                   treated_label = NULL,
                                   untreated_label = NULL) {
  if (is.numeric(x)) x <- as.integer(x)
  
  if (is.null(treated_label)) {
    treated_label <- default_treated_label(treatment_var)
  }
  
  if (is.null(untreated_label)) {
    untreated_label <- default_untreated_label(treatment_var)
  }
  
  dplyr::case_when(
    is.na(x) ~ NA_character_,
    x == 1   ~ treated_label,
    x == 0   ~ untreated_label,
    TRUE     ~ as.character(x)
  )
}

# -----------------------------------------------------------------------------
# Treatment-group levels used in all treatment/control figures
#
# Order is always:
#   1) treated
#   2) untreated
# -----------------------------------------------------------------------------
treatment_group_levels <- function(treatment_var,
                                   treated_label = NULL,
                                   untreated_label = NULL) {
  if (is.null(treated_label)) {
    treated_label <- default_treated_label(treatment_var)
  }
  
  if (is.null(untreated_label)) {
    untreated_label <- default_untreated_label(treatment_var)
  }
  
  c(treated_label, untreated_label)
}

# -----------------------------------------------------------------------------
# Add labeled treatment group for plotting
# -----------------------------------------------------------------------------
add_treatment_group_label <- function(df,
                                      treatment_var,
                                      treated_label = NULL,
                                      untreated_label = NULL) {
  stopifnot(treatment_var %in% names(df))
  
  group_levels <- treatment_group_levels(
    treatment_var = treatment_var,
    treated_label = treated_label,
    untreated_label = untreated_label
  )
  
  df %>%
    dplyr::mutate(
      treatment_group = label_treatment_values(
        .data[[treatment_var]],
        treatment_var = treatment_var,
        treated_label = group_levels[[1]],
        untreated_label = group_levels[[2]]
      ),
      treatment_group = factor(
        treatment_group,
        levels = group_levels
      )
    )
}

# -----------------------------------------------------------------------------
# Variable labels / stems / units for couple-treatment figures
# -----------------------------------------------------------------------------
couple_plot_var_label <- function(var) {
  dplyr::case_when(
    var == "any_work"      ~ "Any work",
    var == "workoutside"   ~ "Working outside the home",
    var == "wfh_some"      ~ "Working from home at least sometimes",
    var == "howlng"        ~ "Housework hours",
    var == "howlng_cv"     ~ "Housework hours",
    var == "timechcare"    ~ "Childcare / home-schooling hours",
    var == "jbhrs"         ~ "Weekly hours worked",
    var == "paygu_dv"      ~ "Gross monthly pay",
    var == "fimnlabgrs_dv" ~ "Gross monthly labour income",
    var == "fimngrs_dv"    ~ "Gross monthly personal income",
    TRUE                   ~ var
  )
}

couple_plot_var_stem <- function(var) {
  dplyr::case_when(
    var == "any_work"      ~ "any_work",
    var == "workoutside"   ~ "workoutside",
    var == "wfh_some"      ~ "wfh_any",
    var == "howlng"        ~ "housework_hours",
    var == "howlng_cv"     ~ "housework_hours",
    var == "timechcare"    ~ "childcare_hours",
    var == "jbhrs"         ~ "weekly_hours",
    var == "paygu_dv"      ~ "gross_monthly_pay",
    var == "fimnlabgrs_dv" ~ "gross_monthly_labour_income",
    var == "fimngrs_dv"    ~ "gross_monthly_personal_income",
    TRUE                   ~ var
  )
}

couple_plot_var_units <- function(var, is_binary = FALSE) {
  if (is_binary) {
    return("Share")
  }
  
  dplyr::case_when(
    var == "howlng"        ~ "Weekly Hours",
    var == "howlng_cv"     ~ "Weekly Hours",
    var == "timechcare"    ~ "Weekly Hours",
    var == "jbhrs"         ~ "Weekly Hours",
    var == "paygu_dv"      ~ "Amount",
    var == "fimnlabgrs_dv" ~ "Amount",
    var == "fimngrs_dv"    ~ "Amount",
    TRUE                   ~ "Mean"
  )
}

couple_plot_is_binary <- function(var) {
  var %in% c("any_work", "workoutside", "wfh_some")
}

# -----------------------------------------------------------------------------
# Expand couple-level data into sample facets for count plots
#
# Output:
#   original rows repeated across:
#     - All couples
#     - Young kids: 0-10
#     - Older kids: 11-17
# -----------------------------------------------------------------------------
expand_couple_samples_for_counts <- function(df) {
  
  dplyr::bind_rows(
    df %>%
      dplyr::mutate(sample_group = "All couples"),
    
    df %>%
      dplyr::filter(has_child_u10_2019) %>%
      dplyr::mutate(sample_group = "Young kids: 0-10"),
    
    df %>%
      dplyr::filter(has_child_11_17_2019) %>%
      dplyr::mutate(sample_group = "Older kids: 11-17")
  ) %>%
    dplyr::mutate(
      sample_group = factor(
        sample_group,
        levels = c("All couples", "Young kids: 0-10", "Older kids: 11-17")
      )
    )
}

# -----------------------------------------------------------------------------
# Rowwise indicator: at least one outcome is observed for both spouses
#
# Inputs:
#   df   : couple-level long data with spouse-suffixed columns (_h / _w)
#   vars : vector of unsuffixed outcome names, e.g. c("workoutside", "wfh_some")
#
# Rule:
#   Returns TRUE if there exists at least one variable v in vars such that
#   both v_h and v_w are non-missing in that row.
# -----------------------------------------------------------------------------
has_any_joint_outcome_observed <- function(df, vars) {
  
  if (length(vars) == 0) {
    return(rep(FALSE, nrow(df)))
  }
  
  valid_vars <- vars[
    paste0(vars, "_h") %in% names(df) &
      paste0(vars, "_w") %in% names(df)
  ]
  
  if (length(valid_vars) == 0) {
    return(rep(FALSE, nrow(df)))
  }
  
  out <- rep(FALSE, nrow(df))
  
  for (v in valid_vars) {
    both_obs <- !is.na(df[[paste0(v, "_h")]]) & !is.na(df[[paste0(v, "_w")]])
    out <- out | both_obs
  }
  
  out
}

# -----------------------------------------------------------------------------
# Keep only couple-time rows with at least one jointly observed couple outcome
# -----------------------------------------------------------------------------
filter_jointly_observed_couple_rows <- function(df, vars) {
  keep <- has_any_joint_outcome_observed(df, vars)
  df[keep, , drop = FALSE]
}

# -----------------------------------------------------------------------------
# Default outcome families for count plots
#
# These are the variables currently used in the couple-treatment descriptives.
# Counts are based on whether at least one of these outcomes is observed
# for both spouses at a given time point.
# -----------------------------------------------------------------------------
covid_count_outcome_vars <- function() {
  c(
    "any_work",
    "workoutside",
    "wfh_some",
    "howlng",
    "timechcare"
  )
}

future_count_outcome_vars <- function() {
  c(
    "any_work",
    "workoutside",
    "wfh_some",
    "jbhrs",
    "paygu_dv",
    "fimnlabgrs_dv",
    "fimngrs_dv"
  )
}

# -----------------------------------------------------------------------------
# Optional baseline-sample restriction for couple-treatment plots
#
# restriction options:
#   NULL
#   "husb_notkey_or_edu"
# -----------------------------------------------------------------------------
filter_couple_plot_restriction <- function(df, restriction = NULL) {
  
  if (is.null(restriction)) {
    return(df)
  }
  
  if (restriction == "husb_notkey_or_edu") {
    return(
      df %>%
        dplyr::filter(sample_husb_notkey_or_edu == 1)
    )
  }
  
  stop("Unknown restriction: ", restriction)
}