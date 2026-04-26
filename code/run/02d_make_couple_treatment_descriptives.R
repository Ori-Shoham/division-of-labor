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
#     - any_work
#     - workoutside
#     - wfh_some   (your "wfh_any")
#     - howlng
#     - timechcare
#
#   Future outcomes:
#     - any_work
#     - workoutside
#     - wfh_some   (your "wfh_any")
#     - jbhrs
#     - paygu_dv
#     - fimnlabgrs_dv
#     - fimngrs_dv
#     - howlng
#
# Notes:
#   - Future outcomes saved at both wave and year aggregation
#   - Child-grid versions compare only 0-10 vs 11-17 child groups
#   - Restricted wife-treatment variants limit the sample to couples where
#     the husband is not a key worker or is in education
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

TREATMENT_LABS <- list(
  "Wife essential (not education),\nhusband not / education",
  NULL,
  NULL
)
names(TREATMENT_LABS) <- TREATMENT_VARS

WIFE_TREATMENT_VARS <- c(
  "treat_wife_key_notedu_husb_not_or_edu",
  "treat_wife_key_notedu_any"
)

CHILD_SUBSETS <- c("all", "u10", "11_17")

COVID_OUTCOMES <- c(
  "any_work",
  "workoutside",
  "wfh_some",
  "howlng",
  "timechcare"
)

FUTURE_OUTCOMES <- c(
  "any_work",
  "workoutside",
  "wfh_some",
  "jbhrs",
  "paygu_dv",
  "fimnlabgrs_dv",
  "fimngrs_dv",
  "howlng"
)

FUTURE_AGGS <- c("wave", "year")

# -----------------------------------------------------------------------------
# Plot readability controls
# -----------------------------------------------------------------------------
AXIS_TEXT_SIZE   <- 14
AXIS_TITLE_SIZE  <- 16
STRIP_TEXT_SIZE  <- 14
LEGEND_TEXT_SIZE <- 14
LEGEND_TITLE_SIZE <- 14
TITLE_SIZE       <- 14

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
        fig_path = fig_path,
        treated_label = TREATMENT_LABS[[tr]],
        axis_text_size = AXIS_TEXT_SIZE,
        axis_title_size = AXIS_TITLE_SIZE,
        strip_text_size = STRIP_TEXT_SIZE,
        legend_text_size = LEGEND_TEXT_SIZE,
        legend_title_size = LEGEND_TITLE_SIZE,
        title_size = TITLE_SIZE
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
      fig_path = fig_path,
      treated_label = TREATMENT_LABS[[tr]],
      axis_text_size = AXIS_TEXT_SIZE,
      axis_title_size = AXIS_TITLE_SIZE,
      strip_text_size = STRIP_TEXT_SIZE,
      legend_text_size = LEGEND_TEXT_SIZE,
      legend_title_size = LEGEND_TITLE_SIZE,
      title_size = TITLE_SIZE
    )
    
    # Restricted comparison sample for wife-based treatments only
    if (tr %in% WIFE_TREATMENT_VARS) {
      
      for (child_subset in CHILD_SUBSETS) {
        plot_covid_spouse_treatment_overtime(
          df = df_covid_spouse,
          var = v,
          treatment_var = tr,
          child_subset = child_subset,
          restriction = "husb_notkey_or_edu",
          out_file = paste0(
            "covid_",
            couple_plot_var_stem(v), "_wave_",
            tr, "_",
            child_subset,
            "_spousefacet_husb_notkey_or_edu.png"
          ),
          fig_path = fig_path,
          treated_label = TREATMENT_LABS[[tr]],
          axis_text_size = AXIS_TEXT_SIZE,
          axis_title_size = AXIS_TITLE_SIZE,
          strip_text_size = STRIP_TEXT_SIZE,
          legend_text_size = LEGEND_TEXT_SIZE,
          legend_title_size = LEGEND_TITLE_SIZE,
          title_size = TITLE_SIZE
        )
      }
      
      plot_covid_spouse_treatment_childgrid(
        df = df_covid_spouse,
        var = v,
        treatment_var = tr,
        restriction = "husb_notkey_or_edu",
        out_file = paste0(
          "covid_",
          couple_plot_var_stem(v), "_wave_",
          tr,
          "_childgrid_spousecols_husb_notkey_or_edu.png"
        ),
        fig_path = fig_path,
        treated_label = TREATMENT_LABS[[tr]],
        axis_text_size = AXIS_TEXT_SIZE,
        axis_title_size = AXIS_TITLE_SIZE,
        strip_text_size = STRIP_TEXT_SIZE,
        legend_text_size = LEGEND_TEXT_SIZE,
        legend_title_size = LEGEND_TITLE_SIZE,
        title_size = TITLE_SIZE
      )
    }
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
          fig_path = fig_path,
          treated_label = TREATMENT_LABS[[tr]],
          axis_text_size = AXIS_TEXT_SIZE,
          axis_title_size = AXIS_TITLE_SIZE,
          strip_text_size = STRIP_TEXT_SIZE,
          legend_text_size = LEGEND_TEXT_SIZE,
          legend_title_size = LEGEND_TITLE_SIZE,
          title_size = TITLE_SIZE
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
        fig_path = fig_path,
        treated_label = TREATMENT_LABS[[tr]],
        axis_text_size = AXIS_TEXT_SIZE,
        axis_title_size = AXIS_TITLE_SIZE,
        strip_text_size = STRIP_TEXT_SIZE,
        legend_text_size = LEGEND_TEXT_SIZE,
        legend_title_size = LEGEND_TITLE_SIZE,
        title_size = TITLE_SIZE
      )
      
      # Restricted comparison sample for wife-based treatments only
      if (tr %in% WIFE_TREATMENT_VARS) {
        
        for (child_subset in CHILD_SUBSETS) {
          plot_future_spouse_treatment_numeric(
            df = df_future_spouse,
            var = v,
            treatment_var = tr,
            child_subset = child_subset,
            agg = agg,
            restriction = "husb_notkey_or_edu",
            out_file = paste0(
              "future_",
              couple_plot_var_stem(v), "_",
              agg, "_",
              tr, "_",
              child_subset,
              "_spousefacet_husb_notkey_or_edu.png"
            ),
            fig_path = fig_path,
            treated_label = TREATMENT_LABS[[tr]],
            axis_text_size = AXIS_TEXT_SIZE,
            axis_title_size = AXIS_TITLE_SIZE,
            strip_text_size = STRIP_TEXT_SIZE,
            legend_text_size = LEGEND_TEXT_SIZE,
            legend_title_size = LEGEND_TITLE_SIZE,
            title_size = TITLE_SIZE
          )
        }
        
        plot_future_spouse_treatment_childgrid(
          df = df_future_spouse,
          var = v,
          treatment_var = tr,
          agg = agg,
          restriction = "husb_notkey_or_edu",
          out_file = paste0(
            "future_",
            couple_plot_var_stem(v), "_",
            agg, "_",
            tr,
            "_childgrid_spousecols_husb_notkey_or_edu.png"
          ),
          fig_path = fig_path,
          treated_label = TREATMENT_LABS[[tr]],
          axis_text_size = AXIS_TEXT_SIZE,
          axis_title_size = AXIS_TITLE_SIZE,
          strip_text_size = STRIP_TEXT_SIZE,
          legend_text_size = LEGEND_TEXT_SIZE,
          legend_title_size = LEGEND_TITLE_SIZE,
          title_size = TITLE_SIZE
        )
      }
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
    fig_path = fig_path,
    treated_label = TREATMENT_LABS[[tr]],
    axis_text_size = AXIS_TEXT_SIZE,
    axis_title_size = AXIS_TITLE_SIZE,
    strip_text_size = STRIP_TEXT_SIZE,
    legend_text_size = LEGEND_TEXT_SIZE,
    legend_title_size = LEGEND_TITLE_SIZE,
    title_size = TITLE_SIZE
  )
  
  # Restricted count plots for wife-based treatments
  if (tr %in% WIFE_TREATMENT_VARS) {
    plot_covid_treatment_group_counts(
      df = df_covid_couple,
      treatment_var = tr,
      restriction = "husb_notkey_or_edu",
      out_file = paste0(
        "covid_counts_wave_",
        tr,
        "_samplefacets_husb_notkey_or_edu.png"
      ),
      fig_path = fig_path,
      treated_label = TREATMENT_LABS[[tr]],
      axis_text_size = AXIS_TEXT_SIZE,
      axis_title_size = AXIS_TITLE_SIZE,
      strip_text_size = STRIP_TEXT_SIZE,
      legend_text_size = LEGEND_TEXT_SIZE,
      legend_title_size = LEGEND_TITLE_SIZE,
      title_size = TITLE_SIZE
    )
  }
  
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
      fig_path = fig_path,
      treated_label = TREATMENT_LABS[[tr]],
      axis_text_size = AXIS_TEXT_SIZE,
      axis_title_size = AXIS_TITLE_SIZE,
      strip_text_size = STRIP_TEXT_SIZE,
      legend_text_size = LEGEND_TEXT_SIZE,
      legend_title_size = LEGEND_TITLE_SIZE,
      title_size = TITLE_SIZE
    )
    
    if (tr %in% WIFE_TREATMENT_VARS) {
      plot_future_treatment_group_counts(
        df = df_future_couple,
        treatment_var = tr,
        agg = agg,
        restriction = "husb_notkey_or_edu",
        out_file = paste0(
          "future_counts_",
          agg, "_",
          tr,
          "_samplefacets_husb_notkey_or_edu.png"
        ),
        fig_path = fig_path,
        treated_label = TREATMENT_LABS[[tr]],
        axis_text_size = AXIS_TEXT_SIZE,
        axis_title_size = AXIS_TITLE_SIZE,
        strip_text_size = STRIP_TEXT_SIZE,
        legend_text_size = LEGEND_TEXT_SIZE,
        legend_title_size = LEGEND_TITLE_SIZE,
        title_size = TITLE_SIZE
      )
    }
  }
}

cat("\nCouple-treatment descriptives complete.\n")
cat("Figures saved to: ", fig_path, "\n", sep = "")