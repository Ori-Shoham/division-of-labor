# =============================================================================
# Script: code/run/02d_make_couple_treatment_descriptives.R
#
# Purpose:
#   Couple-level descriptive figures by baseline treatment definitions.
#
# Design:
#   - helpers live in code/lib/
#   - treatment shown in color / shape
#   - standard version: facet rows = spouse
#   - additional child-grid version:
#       rows = young kids vs older kids
#       cols = spouse
#
# Outputs:
#   COVID outcomes:
#     - workoutside
#     - wfh_some   (your "wfh_any")
#     - howlng
#     - timechcare
#
#   Future outcomes:
#     - workoutside
#     - wfh_some   (your "wfh_any")
#     - jbhrs
#     - paygu_dv
#     - fimnlabgrs_dv
#     - fimngrs_dv
#
# Notes:
#   - Future outcomes saved at both wave and year aggregation
#   - Child-grid versions compare only 0-10 vs 11-17 child groups
# =============================================================================

suppressPackageStartupMessages({
  library(tidyverse)
})

rm(list = ls())

source("code/lib/config.R")
source("code/lib/wave_labels.R")
source("code/lib/descriptives_plots.R")
source("code/lib/future_descriptives_plots.R")
source("code/lib/couple_plot_helpers.R")
source("code/lib/couple_treatment_plots.R")

dir.create(fig_path, showWarnings = FALSE, recursive = TRUE)

# =============================================================================
# Load data
# =============================================================================

df_covid_couple  <- readRDS(file.path(der_path, "df_sample_long_covid_couplelevel.rds"))
df_future_couple <- readRDS(file.path(der_path, "future_outcomes_couple_long_lmo.rds"))

# Convert to spouse-long
df_covid_spouse  <- reshape_couple_long_to_spouse_long(df_covid_couple)
df_future_spouse <- reshape_couple_long_to_spouse_long(df_future_couple)

# =============================================================================
# Settings
# =============================================================================

TREATMENT_VARS <- c(
  "treat_wife_key_notedu_husb_not_or_edu",
  "treat_wife_key_notedu_any",
  "treat_husb_shutdown_wife_not"
)

CHILD_SUBSETS <- c("all", "u10", "11_17")

COVID_OUTCOMES <- c(
  "workoutside",
  "wfh_some",
  "howlng",
  "timechcare"
)

FUTURE_OUTCOMES <- c(
  "workoutside",
  "wfh_some",
  "jbhrs",
  "paygu_dv",
  "fimnlabgrs_dv",
  "fimngrs_dv"
)

FUTURE_AGGS <- c("wave", "year")

.has_data <- function(df, var) {
  var %in% names(df) && !all(is.na(df[[var]]))
}

# =============================================================================
# Run COVID figures
# =============================================================================

for (tr in TREATMENT_VARS) {
  for (v in COVID_OUTCOMES) {
    
    if (!.has_data(df_covid_spouse, v)) next
    
    # Standard spouse-facet versions
    for (child_subset in CHILD_SUBSETS) {
      plot_covid_spouse_treatment_overtime(
        df = df_covid_spouse,
        var = v,
        treatment_var = tr,
        child_subset = child_subset,
        out_file = paste0(
          "covid_",
          couple_plot_var_stem(v), "_wave_",
          tr, "_",
          child_subset,
          "_spousefacet.png"
        ),
        fig_path = fig_path
      )
    }
    
    # Child-group facet-grid comparison
    plot_covid_spouse_treatment_childgrid(
      df = df_covid_spouse,
      var = v,
      treatment_var = tr,
      out_file = paste0(
        "covid_",
        couple_plot_var_stem(v), "_wave_",
        tr,
        "_childgrid_spousecols.png"
      ),
      fig_path = fig_path
    )
  }
}

# =============================================================================
# Run future figures
# =============================================================================

for (tr in TREATMENT_VARS) {
  for (v in FUTURE_OUTCOMES) {
    
    if (!.has_data(df_future_spouse, v)) next
    
    for (agg in FUTURE_AGGS) {
      
      # Standard spouse-facet versions
      for (child_subset in CHILD_SUBSETS) {
        plot_future_spouse_treatment_numeric(
          df = df_future_spouse,
          var = v,
          treatment_var = tr,
          child_subset = child_subset,
          agg = agg,
          out_file = paste0(
            "future_",
            couple_plot_var_stem(v), "_",
            agg, "_",
            tr, "_",
            child_subset,
            "_spousefacet.png"
          ),
          fig_path = fig_path
        )
      }
      
      # Child-group facet-grid comparison
      plot_future_spouse_treatment_childgrid(
        df = df_future_spouse,
        var = v,
        treatment_var = tr,
        agg = agg,
        out_file = paste0(
          "future_",
          couple_plot_var_stem(v), "_",
          agg, "_",
          tr,
          "_childgrid_spousecols.png"
        ),
        fig_path = fig_path
      )
    }
  }
}

# =============================================================================
# Couple-count figures by treatment group
# =============================================================================

for (tr in TREATMENT_VARS) {
  
  # COVID wave counts
  plot_covid_treatment_group_counts(
    df = df_covid_couple,
    treatment_var = tr,
    out_file = paste0(
      "covid_counts_wave_",
      tr,
      "_samplefacets.png"
    ),
    fig_path = fig_path
  )
  
  # Future counts by wave and year
  for (agg in FUTURE_AGGS) {
    plot_future_treatment_group_counts(
      df = df_future_couple,
      treatment_var = tr,
      agg = agg,
      out_file = paste0(
        "future_counts_",
        agg, "_",
        tr,
        "_samplefacets.png"
      ),
      fig_path = fig_path
    )
  }
}

cat("\nCouple-treatment descriptives complete.\n")
cat("Figures saved to: ", fig_path, "\n", sep = "")