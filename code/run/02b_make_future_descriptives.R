# =============================================================================
# Script: code/run/02b_make_future_descriptives.R
#
# Purpose:
#   Descriptive figures for future outcomes.
#
# Notes:
#   - Uses 2019 baseline where appropriate.
#   - Does NOT use 2019 baseline for WFH-related outcomes:
#       * workoutside
#       * wfh_some
#       * wfh_cat
# =============================================================================

suppressPackageStartupMessages({
  library(tidyverse)
})

rm(list = ls())

source("code/lib/config.R")
source("code/lib/wave_labels.R")
source("code/lib/future_descriptives_plots.R")

dir.create(fig_path, showWarnings = FALSE, recursive = TRUE)

# =============================================================================
# Load data
# =============================================================================

df_future <- readRDS(file.path(der_path, "future_outcomes_long_lmo.rds"))

s_all     <- readRDS(file.path(samples_path, "s2019_all.rds"))
s_couples <- readRDS(file.path(samples_path, "s2019_couples.rds"))

pidp_all     <- s_all$pidp
pidp_couples <- s_couples$pidp

df_future_all <- df_future %>%
  dplyr::filter(pidp %in% pidp_all)

df_future_couples <- df_future %>%
  dplyr::filter(pidp %in% pidp_couples)

.dataset_specs <- list(
  list(name = "all",     df = df_future_all,     prefix = "future_all"),
  list(name = "couples", df = df_future_couples, prefix = "future_couples")
)

# =============================================================================
# Global plotting options
# =============================================================================

RUN_AUTO_ALL <- FALSE

NUMERIC_ZERO_IF_NOT_WORKING <- c(
  "jbhrs",
  "jbot",
  "basrate",
  "paygu_dv",
  "fimnlabgrs_dv",
  "fimngrs_dv"
)

HEADLINE_NUMERIC_OUTCOMES <- c(
  "workoutside",
  "wfh_some",
  "jbhrs",
  "jbot",
  "paygu_dv",
  "fimnlabgrs_dv",
  "fimngrs_dv",
  "scghq1_dv",
  "scghq2_dv",
  "sclfsato",
  "nchild_dv",
  "ndepchl_dv"
)

HEADLINE_CATEGORICAL_OUTCOMES <- c(
  "wfh_cat",
  "jbstat",
  "mastat_dv",
  "jbft_dv",
  "health_sf"
)

FACET_GROUPS <- c(
  "sex",
  "group_industry_based",
  "group_industry_based_detailed"
)

LINE_GROUPS <- c(
  "sex",
  "group_industry_based",
  "group_industry_based_detailed"
)

TIME_AGGS_MAIN <- c("wave", "year")

YM_NUMERIC_OUTCOMES <- c(
  "workoutside",
  "wfh_some",
  "jbhrs",
  "paygu_dv",
  "fimnlabgrs_dv",
  "health_sf",
  "scghq1_dv",
  "sclfsato"
)

YM_CATEGORICAL_OUTCOMES <- c(
  "wfh_cat",
  "jbstat"
)

NO_BASELINE_VARS <- c(
  "workoutside",
  "wfh_some",
  "wfh_cat"
)

# =============================================================================
# Helpers
# =============================================================================

.has_data <- function(df, var) {
  var %in% names(df) && !all(is.na(df[[var]]))
}

.use_baseline <- function(var) {
  !(var %in% NO_BASELINE_VARS)
}

# =============================================================================
# SECTION A: Main ungrouped mean/share figures
# =============================================================================
for (spec in .dataset_specs) {
  df <- spec$df
  prefix <- spec$prefix
  
  for (v in HEADLINE_NUMERIC_OUTCOMES) {
    if (!.has_data(df, v)) next
    
    zero_flag <- v %in% NUMERIC_ZERO_IF_NOT_WORKING
    baseline_flag <- .use_baseline(v)
    
    for (agg in TIME_AGGS_MAIN) {
      plot_future_numeric_mean(
        df = df,
        var = v,
        agg = agg,
        include_baseline_2019 = baseline_flag,
        zero_if_not_working = zero_flag,
        out_file = paste0(prefix, "_", v, "_", agg, ".png"),
        fig_path = fig_path
      )
    }
  }
}

# =============================================================================
# SECTION B: Grouped mean/share figures
# =============================================================================
for (spec in .dataset_specs) {
  df <- spec$df
  prefix <- spec$prefix
  
  for (v in HEADLINE_NUMERIC_OUTCOMES) {
    if (!.has_data(df, v)) next
    
    zero_flag <- v %in% NUMERIC_ZERO_IF_NOT_WORKING
    baseline_flag <- .use_baseline(v)
    
    for (g in LINE_GROUPS) {
      if (!.has_data(df, g)) next
      
      for (agg in TIME_AGGS_MAIN) {
        plot_future_numeric_mean(
          df = df,
          var = v,
          by = g,
          agg = agg,
          include_baseline_2019 = baseline_flag,
          zero_if_not_working = zero_flag,
          out_file = paste0(prefix, "_", v, "_by_", g, "_", agg, ".png"),
          fig_path = fig_path
        )
      }
    }
  }
}

# =============================================================================
# SECTION C: Main categorical distributions over time
# =============================================================================
for (spec in .dataset_specs) {
  df <- spec$df
  prefix <- spec$prefix
  
  for (v in HEADLINE_CATEGORICAL_OUTCOMES) {
    if (!.has_data(df, v)) next
    
    baseline_flag <- .use_baseline(v)
    
    for (agg in TIME_AGGS_MAIN) {
      plot_future_categorical_dist(
        df = df,
        var = v,
        agg = agg,
        include_baseline_2019 = baseline_flag,
        include_missing = FALSE,
        out_file = paste0(prefix, "_", v, "_dist_", agg, ".png"),
        fig_path = fig_path
      )
    }
  }
}

# =============================================================================
# SECTION D: Faceted categorical distributions over time
# =============================================================================
for (spec in .dataset_specs) {
  df <- spec$df
  prefix <- spec$prefix
  
  for (v in HEADLINE_CATEGORICAL_OUTCOMES) {
    if (!.has_data(df, v)) next
    
    baseline_flag <- .use_baseline(v)
    
    for (g in FACET_GROUPS) {
      if (!.has_data(df, g)) next
      
      plot_future_categorical_dist_facet(
        df = df,
        var = v,
        by = g,
        agg = "wave",
        include_baseline_2019 = baseline_flag,
        include_missing = FALSE,
        scales = "fixed",
        ncol = NULL,
        out_file = paste0(prefix, "_", v, "_facet_", g, "_wave.png"),
        fig_path = fig_path
      )
    }
  }
}

# =============================================================================
# SECTION E: Year-month figures for a smaller set of outcomes
# =============================================================================
for (spec in .dataset_specs) {
  df <- spec$df
  prefix <- spec$prefix
  
  for (v in YM_NUMERIC_OUTCOMES) {
    if (!.has_data(df, v)) next
    
    zero_flag <- v %in% NUMERIC_ZERO_IF_NOT_WORKING
    baseline_flag <- .use_baseline(v)
    
    plot_future_numeric_mean(
      df = df,
      var = v,
      agg = "ym",
      include_baseline_2019 = baseline_flag,
      zero_if_not_working = zero_flag,
      out_file = paste0(prefix, "_", v, "_ym.png"),
      fig_path = fig_path
    )
  }
  
  for (v in YM_CATEGORICAL_OUTCOMES) {
    if (!.has_data(df, v)) next
    
    baseline_flag <- .use_baseline(v)
    
    plot_future_categorical_dist(
      df = df,
      var = v,
      agg = "ym",
      include_baseline_2019 = baseline_flag,
      include_missing = FALSE,
      out_file = paste0(prefix, "_", v, "_dist_ym.png"),
      fig_path = fig_path
    )
  }
}

# =============================================================================
# SECTION F: Hand-picked main figures
# =============================================================================
for (spec in .dataset_specs) {
  df <- spec$df
  prefix <- spec$prefix
  
  if (.has_data(df, "wfh_cat") && .has_data(df, "group_industry_based")) {
    plot_future_categorical_dist_facet(
      df = df,
      var = "wfh_cat",
      by = "group_industry_based",
      agg = "wave",
      include_baseline_2019 = FALSE,
      include_missing = FALSE,
      scales = "fixed",
      out_file = paste0(prefix, "_wfh_cat_facet_group_industry_based_wave_main.png"),
      fig_path = fig_path
    )
  }
  
  if (.has_data(df, "jbstat") && .has_data(df, "sex")) {
    plot_future_categorical_dist_facet(
      df = df,
      var = "jbstat",
      by = "sex",
      agg = "wave",
      include_baseline_2019 = TRUE,
      include_missing = FALSE,
      scales = "fixed",
      out_file = paste0(prefix, "_jbstat_facet_sex_wave_main.png"),
      fig_path = fig_path
    )
  }
  
  if (.has_data(df, "workoutside") && .has_data(df, "group_industry_based")) {
    plot_future_numeric_mean(
      df = df,
      var = "workoutside",
      by = "group_industry_based",
      agg = "wave",
      include_baseline_2019 = FALSE,
      zero_if_not_working = FALSE,
      out_file = paste0(prefix, "_workoutside_by_group_industry_based_wave_main.png"),
      fig_path = fig_path
    )
  }
  
  if (.has_data(df, "jbhrs") && .has_data(df, "group_industry_based")) {
    plot_future_numeric_mean(
      df = df,
      var = "jbhrs",
      by = "group_industry_based",
      agg = "wave",
      include_baseline_2019 = TRUE,
      zero_if_not_working = TRUE,
      out_file = paste0(prefix, "_jbhrs_by_group_industry_based_wave_main.png"),
      fig_path = fig_path
    )
  }
  
  if (.has_data(df, "scghq1_dv") && .has_data(df, "sex")) {
    plot_future_numeric_mean(
      df = df,
      var = "scghq1_dv",
      by = "sex",
      agg = "year",
      include_baseline_2019 = TRUE,
      zero_if_not_working = FALSE,
      out_file = paste0(prefix, "_scghq1_dv_by_sex_year_main.png"),
      fig_path = fig_path
    )
  }
}

# =============================================================================
# SECTION G: Optional automatic "plot everything"
# =============================================================================
if (RUN_AUTO_ALL) {
  plot_all_future_outcomes(
    df = df_future_all,
    prefix = "future_all_auto",
    fig_path = fig_path
  )
  
  plot_all_future_outcomes(
    df = df_future_couples,
    prefix = "future_couples_auto",
    fig_path = fig_path
  )
}

cat("\nFuture outcome descriptives complete.\n")
cat("Figures saved to: ", fig_path, "\n", sep = "")