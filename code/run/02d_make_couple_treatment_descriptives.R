# =============================================================================
# Script: code/run/02d_make_couple_treatment_descriptives.R
#
# Purpose:
#   Couple-level descriptive figures by baseline treatment definitions.
#
# Design:
#   - helpers live in code/lib/
#   - treatment shown in color / shape
#   - treatment is always the first group in the legend and color mapping
#   - standard version: facet rows = spouse
#   - additional child-grid version:
#       rows = young kids vs older kids
#       cols = spouse
#
# Outputs:
#   COVID outcomes:
#     - any_work
#     - hours      (hours worked last week)
#     - work_last_week_status  (3-category: worked / has job but didn't work / not employed)
#     - workoutside
#     - wfh_some   (your "wfh_any")
#     - howlng
#     - timechcare
#     - husits_wife_main_both
#
#   Future outcomes:
#     - any_work
#     - workoutside
#     - wfh_some   (your "wfh_any")
#     - jbhrs
#     - paygu_dv_real
#     - fimnlabgrs_dv_real
#     - fimngrs_dv_real
#     - howlng
#
#   Main-survey history + future outcomes:
#     - any_work
#     - jbhrs
#     - paygu_dv_real
#     - fimnlabgrs_dv_real
#     - fimngrs_dv_real
#     - howlng
#
# Notes:
#   - Future-only WFH/work-outside outcomes are saved at month and year
#     aggregation even when the broader future-only switch is off.
#   - History + future figures use the main-survey-only stacked couple panel:
#       couple_history_future_mainonly_long.rds
#     This deliberately excludes COVID-study rows because COVID has a different
#     questionnaire and a different sample.
#   - History + future figures are plotted by calendar month and year.
#   - Child-grid versions compare only 0-10 vs 11-17 child groups.
#   - Restricted wife-treatment variants limit the sample to couples where
#     the husband is not a key worker or is in education.
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
source("code/lib/husits_covid_plots.R")
source("code/lib/couple_treatment_plots.R")
source("code/lib/couple_baseline_dist_plots.R")

for (d in c(
  fig_path_couple_treatment_covid_childgrids,
  fig_path_couple_treatment_future_childgrids,
  fig_path_couple_treatment_history_future_childgrids,
  fig_path_couple_treatment_spousefacets,
  fig_path_couple_treatment_counts
)) {
  dir.create(d, showWarnings = FALSE, recursive = TRUE)
}

# =============================================================================
# Load data
# =============================================================================

df_covid_couple  <- readRDS(file.path(der_path, "df_sample_long_covid_couplelevel.rds"))
df_future_couple <- readRDS(file.path(der_path, "future_outcomes_couple_long_lmo.rds"))

future_monthly_file <- file.path(der_path, "future_outcomes_couple_long_lmo_monthly.rds")
df_future_couple_monthly <- if (file.exists(future_monthly_file)) {
  readRDS(future_monthly_file)
} else {
  stop(
    "Monthly future couple file not found: ",
    future_monthly_file,
    ". Rerun code/run/01_build_data.R before making monthly descriptive plots."
  )
}

history_future_file <- file.path(der_path, "couple_history_future_mainonly_long.rds")
history_future_monthly_file <- file.path(
  der_path,
  "couple_history_future_mainonly_monthly_long.rds"
)
history_future_both_in_covid_file <- file.path(
  der_path,
  "couple_history_future_mainonly_long_both_in_covid.rds"
)
history_future_both_in_covid_monthly_file <- file.path(
  der_path,
  "couple_history_future_mainonly_monthly_long_both_in_covid.rds"
)

if (file.exists(history_future_file)) {
  df_history_future_couple <- readRDS(history_future_file)
} else {
  warning(
    "History + future stacked couple file not found: ",
    history_future_file,
    ". History + future treatment plots will be skipped."
  )
  df_history_future_couple <- NULL
}

if (is.null(df_history_future_couple)) {
  df_history_future_couple_monthly <- NULL
} else if (file.exists(history_future_monthly_file)) {
  df_history_future_couple_monthly <- readRDS(history_future_monthly_file)
} else {
  stop(
    "Monthly history + future stacked couple file not found: ",
    history_future_monthly_file,
    ". Rerun code/run/01_build_data.R before making monthly descriptive plots."
  )
}

if (file.exists(history_future_both_in_covid_file)) {
  df_history_future_couple_both_in_covid <- readRDS(history_future_both_in_covid_file)
} else {
  warning(
    "COVID-observed history + future stacked couple file not found: ",
    history_future_both_in_covid_file,
    ". COVID-observed history + future treatment plots will be skipped."
  )
  df_history_future_couple_both_in_covid <- NULL
}

if (is.null(df_history_future_couple_both_in_covid)) {
  df_history_future_couple_monthly_both_in_covid <- NULL
} else if (file.exists(history_future_both_in_covid_monthly_file)) {
  df_history_future_couple_monthly_both_in_covid <- readRDS(
    history_future_both_in_covid_monthly_file
  )
} else {
  stop(
    "Monthly COVID-observed history + future stacked couple file not found: ",
    history_future_both_in_covid_monthly_file,
    ". Rerun code/run/01_build_data.R before making monthly descriptive plots."
  )
}

df_covid_couple <- add_husits_wife_main_both(df_covid_couple)

# Convert to spouse-long
df_covid_spouse  <- reshape_couple_long_to_spouse_long(df_covid_couple)
df_future_spouse <- reshape_couple_long_to_spouse_long(df_future_couple)
df_future_spouse_monthly <- reshape_couple_long_to_spouse_long(df_future_couple_monthly)

if (!is.null(df_history_future_couple)) {
  df_history_future_spouse <- reshape_couple_long_to_spouse_long(df_history_future_couple)
} else {
  df_history_future_spouse <- NULL
}

if (!is.null(df_history_future_couple_monthly)) {
  df_history_future_spouse_monthly <- reshape_couple_long_to_spouse_long(
    df_history_future_couple_monthly
  )
} else {
  df_history_future_spouse_monthly <- NULL
}

if (!is.null(df_history_future_couple_both_in_covid)) {
  df_history_future_spouse_both_in_covid <- reshape_couple_long_to_spouse_long(
    df_history_future_couple_both_in_covid
  )
} else {
  df_history_future_spouse_both_in_covid <- NULL
}

if (!is.null(df_history_future_couple_monthly_both_in_covid)) {
  df_history_future_spouse_monthly_both_in_covid <- reshape_couple_long_to_spouse_long(
    df_history_future_couple_monthly_both_in_covid
  )
} else {
  df_history_future_spouse_monthly_both_in_covid <- NULL
}

history_future_sample_specs <- purrr::compact(list(
  if (!is.null(df_history_future_spouse)) {
    list(
      suffix = "",
      couple = df_history_future_couple,
      couple_monthly = df_history_future_couple_monthly,
      spouse = df_history_future_spouse,
      spouse_monthly = df_history_future_spouse_monthly
    )
  },
  if (!is.null(df_history_future_spouse_both_in_covid)) {
    list(
      suffix = "_both_in_covid",
      couple = df_history_future_couple_both_in_covid,
      couple_monthly = df_history_future_couple_monthly_both_in_covid,
      spouse = df_history_future_spouse_both_in_covid,
      spouse_monthly = df_history_future_spouse_monthly_both_in_covid
    )
  }
))

# =============================================================================
# Settings
# =============================================================================

TREATMENT_VARS <- c(
  "treat_wife_key_notedu_husb_not_or_edu",
  "treat_husb_shutdown_wife_not"
)

if (isTRUE(MAKE_WIFE_KEY_ANY_TREATMENT)) {
  TREATMENT_VARS <- c(
    "treat_wife_key_notedu_husb_not_or_edu",
    "treat_wife_key_notedu_any",
    "treat_husb_shutdown_wife_not"
  )
}

TREATMENT_LABS <- list(
  "Wife essential (not education),\nhusband not / education",
  NULL,
  NULL
)
names(TREATMENT_LABS) <- c(
  "treat_wife_key_notedu_husb_not_or_edu",
  "treat_wife_key_notedu_any",
  "treat_husb_shutdown_wife_not"
)

WIFE_TREATMENT_VARS <- c(
  "treat_wife_key_notedu_husb_not_or_edu",
  "treat_wife_key_notedu_any"
)
WIFE_TREATMENT_VARS <- intersect(WIFE_TREATMENT_VARS, TREATMENT_VARS)

CHILD_SUBSETS <- if (isTRUE(MAKE_COUPLE_TREATMENT_SPOUSEFACETS)) {
  c("all", "u10", "11_17")
} else {
  character(0)
}

COVID_OUTCOMES <- c(
  "any_work",
  "hours",
  "workoutside",
  "wfh_some",
  "howlng",
  "timechcare",
  "husits_wife_main_both"
)

FUTURE_OUTCOMES <- c(
  "any_work",
  "workoutside",
  "wfh_some",
  "jbhrs",
  "paygu_dv_real",
  "fimnlabgrs_dv_real",
  "fimngrs_dv_real",
  "howlng"
)

REQUIRED_FUTURE_ONLY_OUTCOMES <- c(
  "workoutside",
  "wfh_some"
)

FUTURE_ONLY_OUTCOMES_TO_PLOT <- if (isTRUE(MAKE_FUTURE_ONLY_TREATMENT)) {
  FUTURE_OUTCOMES
} else {
  intersect(FUTURE_OUTCOMES, REQUIRED_FUTURE_ONLY_OUTCOMES)
}

# Main-survey variables with comparable history and future values.
# WFH/workoutside are intentionally excluded here because the regular pre-2020
# main survey does not ask the relevant WFH questions.
HISTORY_FUTURE_OUTCOMES <- c(
  "any_work",
  "jbhrs",
  "paygu_dv_real",
  "fimnlabgrs_dv_real",
  "fimngrs_dv_real",
  "howlng"
)

FUTURE_AGGS <- c("wave", "ym", "year")
HISTORY_FUTURE_AGGS <- c("ym", "year")
FUTURE_ONLY_AGGS_TO_PLOT <- if (isTRUE(MAKE_FUTURE_ONLY_TREATMENT)) {
  FUTURE_AGGS
} else {
  c("ym", "year")
}

DESCRIPTIVE_MONTHLY_START_YM <- as.Date("2019-01-01")
DESCRIPTIVE_MONTHLY_END_YM <- as.Date("2021-12-01")

# -----------------------------------------------------------------------------
# Plot readability controls
# -----------------------------------------------------------------------------
AXIS_TEXT_SIZE    <- 14
AXIS_TITLE_SIZE   <- 16
STRIP_TEXT_SIZE   <- 14
LEGEND_TEXT_SIZE  <- 14
LEGEND_TITLE_SIZE <- 14
TITLE_SIZE        <- 14

.has_data <- function(df, var) {
  !is.null(df) && var %in% names(df) && !all(is.na(df[[var]]))
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
        fig_path = fig_path_couple_treatment_spousefacets,
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
      fig_path = fig_path_couple_treatment_covid_childgrids,
      treated_label = TREATMENT_LABS[[tr]],
      axis_text_size = AXIS_TEXT_SIZE,
      axis_title_size = AXIS_TITLE_SIZE,
      strip_text_size = STRIP_TEXT_SIZE,
      legend_text_size = LEGEND_TEXT_SIZE,
      legend_title_size = LEGEND_TITLE_SIZE,
      title_size = TITLE_SIZE
    )
    
    # Restricted comparison sample for wife-based treatments only
    if (tr %in% WIFE_TREATMENT_VARS && isTRUE(MAKE_RESTRICTED_HUSB_NOTKEY_VARIANTS)) {
      
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
          fig_path = fig_path_couple_treatment_spousefacets,
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
        fig_path = fig_path_couple_treatment_covid_childgrids,
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
# Run future-only figures
# =============================================================================

if (length(FUTURE_ONLY_OUTCOMES_TO_PLOT) > 0) {
for (tr in TREATMENT_VARS) {
  for (v in FUTURE_ONLY_OUTCOMES_TO_PLOT) {
    
    if (!.has_data(df_future_spouse, v)) next
    
    for (agg in FUTURE_ONLY_AGGS_TO_PLOT) {
      df_future_spouse_agg <- if (agg == "ym") {
        df_future_spouse_monthly
      } else {
        df_future_spouse
      }
      
      # Standard spouse-facet versions
      for (child_subset in CHILD_SUBSETS) {
        plot_future_spouse_treatment_numeric(
          df = df_future_spouse_agg,
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
          fig_path = fig_path_couple_treatment_spousefacets,
          treated_label = TREATMENT_LABS[[tr]],
          axis_text_size = AXIS_TEXT_SIZE,
          axis_title_size = AXIS_TITLE_SIZE,
          strip_text_size = STRIP_TEXT_SIZE,
          legend_text_size = LEGEND_TEXT_SIZE,
          legend_title_size = LEGEND_TITLE_SIZE,
          title_size = TITLE_SIZE,
          monthly_start_ym = DESCRIPTIVE_MONTHLY_START_YM,
          monthly_end_ym = DESCRIPTIVE_MONTHLY_END_YM
        )
      }
      
      # Child-group facet-grid comparison
      plot_future_spouse_treatment_childgrid(
        df = df_future_spouse_agg,
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
        fig_path = fig_path_couple_treatment_future_childgrids,
        treated_label = TREATMENT_LABS[[tr]],
        axis_text_size = AXIS_TEXT_SIZE,
        axis_title_size = AXIS_TITLE_SIZE,
        strip_text_size = STRIP_TEXT_SIZE,
        legend_text_size = LEGEND_TEXT_SIZE,
        legend_title_size = LEGEND_TITLE_SIZE,
        title_size = TITLE_SIZE,
        monthly_start_ym = DESCRIPTIVE_MONTHLY_START_YM,
        monthly_end_ym = DESCRIPTIVE_MONTHLY_END_YM
      )
      
      # Restricted comparison sample for wife-based treatments only
      if (tr %in% WIFE_TREATMENT_VARS && isTRUE(MAKE_RESTRICTED_HUSB_NOTKEY_VARIANTS)) {
        
        for (child_subset in CHILD_SUBSETS) {
          plot_future_spouse_treatment_numeric(
            df = df_future_spouse_agg,
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
            fig_path = fig_path_couple_treatment_spousefacets,
            treated_label = TREATMENT_LABS[[tr]],
            axis_text_size = AXIS_TEXT_SIZE,
            axis_title_size = AXIS_TITLE_SIZE,
            strip_text_size = STRIP_TEXT_SIZE,
            legend_text_size = LEGEND_TEXT_SIZE,
            legend_title_size = LEGEND_TITLE_SIZE,
            title_size = TITLE_SIZE,
            monthly_start_ym = DESCRIPTIVE_MONTHLY_START_YM,
            monthly_end_ym = DESCRIPTIVE_MONTHLY_END_YM
          )
        }
        
        plot_future_spouse_treatment_childgrid(
          df = df_future_spouse_agg,
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
          fig_path = fig_path_couple_treatment_future_childgrids,
          treated_label = TREATMENT_LABS[[tr]],
          axis_text_size = AXIS_TEXT_SIZE,
          axis_title_size = AXIS_TITLE_SIZE,
          strip_text_size = STRIP_TEXT_SIZE,
          legend_text_size = LEGEND_TEXT_SIZE,
          legend_title_size = LEGEND_TITLE_SIZE,
          title_size = TITLE_SIZE,
          monthly_start_ym = DESCRIPTIVE_MONTHLY_START_YM,
          monthly_end_ym = DESCRIPTIVE_MONTHLY_END_YM
        )
      }
    }
  }
}
}
# =============================================================================
# COVID couple-level husits distribution figures
# =============================================================================

if (.has_husits_distribution(df_covid_couple)) {
  
  for (tr in TREATMENT_VARS) {
    
    if (isTRUE(MAKE_COUPLE_TREATMENT_SPOUSEFACETS)) {
      plot_covid_husits_distribution(
        df = df_covid_couple,
        treatment_var = tr,
        out_file = paste0(
          "covid_husits_distribution_wave_",
          tr,
          "_treatmentfacets.png"
        ),
        fig_path = fig_path_couple_treatment_spousefacets,
        treated_label = TREATMENT_LABS[[tr]],
        axis_text_size = AXIS_TEXT_SIZE,
        axis_title_size = AXIS_TITLE_SIZE,
        strip_text_size = STRIP_TEXT_SIZE,
        legend_text_size = LEGEND_TEXT_SIZE,
        legend_title_size = LEGEND_TITLE_SIZE,
        title_size = TITLE_SIZE
      )
    }
    
    plot_covid_husits_distribution_childgrid(
      df = df_covid_couple,
      treatment_var = tr,
      out_file = paste0(
        "covid_husits_distribution_wave_",
        tr,
        "_childgrid_treatmentcols.png"
      ),
      fig_path = fig_path_couple_treatment_covid_childgrids,
      treated_label = TREATMENT_LABS[[tr]],
      axis_text_size = AXIS_TEXT_SIZE,
      axis_title_size = AXIS_TITLE_SIZE,
      strip_text_size = STRIP_TEXT_SIZE,
      legend_text_size = LEGEND_TEXT_SIZE,
      legend_title_size = LEGEND_TITLE_SIZE,
      title_size = TITLE_SIZE
    )
    
    if (tr %in% WIFE_TREATMENT_VARS && isTRUE(MAKE_RESTRICTED_HUSB_NOTKEY_VARIANTS)) {
      
      plot_covid_husits_distribution(
        df = df_covid_couple,
        treatment_var = tr,
        restriction = "husb_notkey_or_edu",
        out_file = paste0(
          "covid_husits_distribution_wave_",
          tr,
          "_treatmentfacets_husb_notkey_or_edu.png"
        ),
        fig_path = fig_path_couple_treatment_spousefacets,
        treated_label = TREATMENT_LABS[[tr]],
        axis_text_size = AXIS_TEXT_SIZE,
        axis_title_size = AXIS_TITLE_SIZE,
        strip_text_size = STRIP_TEXT_SIZE,
        legend_text_size = LEGEND_TEXT_SIZE,
        legend_title_size = LEGEND_TITLE_SIZE,
        title_size = TITLE_SIZE
      )
      
      plot_covid_husits_distribution_childgrid(
        df = df_covid_couple,
        treatment_var = tr,
        restriction = "husb_notkey_or_edu",
        out_file = paste0(
          "covid_husits_distribution_wave_",
          tr,
          "_childgrid_treatmentcols_husb_notkey_or_edu.png"
        ),
        fig_path = fig_path_couple_treatment_covid_childgrids,
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
# COVID work_last_week_status distribution figures (3-category stacked bar)
# =============================================================================

for (tr in TREATMENT_VARS) {
  for (sp in c("wife", "husband")) {
    suffix <- if (sp == "wife") "_w" else "_h"
    status_col <- paste0("work_last_week_status", suffix)
    if (!.has_data(df_covid_couple, status_col)) next

    plot_covid_work_status_childgrid(
      df_couple     = df_covid_couple,
      spouse        = sp,
      treatment_var = tr,
      out_file      = paste0(
        "covid_work_status_last_week_wave_",
        tr,
        "_childgrid_treatmentcols_", sp, ".png"
      ),
      fig_path        = fig_path_couple_treatment_covid_childgrids,
      treated_label   = TREATMENT_LABS[[tr]],
      axis_text_size  = AXIS_TEXT_SIZE,
      axis_title_size = AXIS_TITLE_SIZE,
      strip_text_size = STRIP_TEXT_SIZE,
      legend_text_size = LEGEND_TEXT_SIZE,
      title_size      = TITLE_SIZE
    )
  }
}

# =============================================================================
# Main-survey work_last_week_status distribution figures (3-category stacked bar)
# =============================================================================

if (length(history_future_sample_specs) > 0) {
  for (sample_spec in history_future_sample_specs) {
    history_future_suffix <- sample_spec$suffix
    df_hf_couple <- sample_spec$couple

    for (tr in TREATMENT_VARS) {
      for (sp in c("wife", "husband")) {
        suffix <- if (sp == "wife") "_w" else "_h"
        status_col <- paste0("work_last_week_status", suffix)
        if (!.has_data(df_hf_couple, status_col)) next

        plot_covid_work_status_childgrid(
          df_couple     = df_hf_couple,
          spouse        = sp,
          treatment_var = tr,
          out_file      = paste0(
            "main_work_status_last_week_wave_",
            tr,
            "_childgrid_treatmentcols_", sp, history_future_suffix, ".png"
          ),
          fig_path         = fig_path_couple_treatment_history_future_childgrids,
          treated_label    = TREATMENT_LABS[[tr]],
          wave_scale       = "main_history_future",
          axis_text_size   = AXIS_TEXT_SIZE,
          axis_title_size  = AXIS_TITLE_SIZE,
          strip_text_size  = STRIP_TEXT_SIZE,
          legend_text_size = LEGEND_TEXT_SIZE,
          title_size       = TITLE_SIZE
        )
      }
    }
  }
}

# =============================================================================
# Run main-survey history + future figures
# =============================================================================

if (length(history_future_sample_specs) > 0) {
  for (sample_spec in history_future_sample_specs) {
    history_future_suffix <- sample_spec$suffix
    df_history_future_spouse_variant <- sample_spec$spouse
    df_history_future_spouse_monthly_variant <- sample_spec$spouse_monthly

  for (tr in TREATMENT_VARS) {
    for (v in HISTORY_FUTURE_OUTCOMES) {
      
      if (!.has_data(df_history_future_spouse_variant, v)) next
      
      for (agg in HISTORY_FUTURE_AGGS) {
        df_history_future_spouse_agg <- if (agg == "ym") {
          df_history_future_spouse_monthly_variant
        } else {
          df_history_future_spouse_variant
        }
        
        # Standard spouse-facet versions
        for (child_subset in CHILD_SUBSETS) {
          plot_main_history_future_spouse_treatment_numeric(
            df = df_history_future_spouse_agg,
            var = v,
            treatment_var = tr,
            child_subset = child_subset,
            agg = agg,
            out_file = paste0(
              "main_history_future_",
              couple_plot_var_stem(v), "_",
              agg, "_",
              tr, "_",
              child_subset,
              history_future_suffix,
              "_spousefacet.png"
            ),
            fig_path = fig_path_couple_treatment_spousefacets,
            treated_label = TREATMENT_LABS[[tr]],
            axis_text_size = AXIS_TEXT_SIZE,
            axis_title_size = AXIS_TITLE_SIZE,
            strip_text_size = STRIP_TEXT_SIZE,
            legend_text_size = LEGEND_TEXT_SIZE,
            legend_title_size = LEGEND_TITLE_SIZE,
            title_size = TITLE_SIZE,
            monthly_start_ym = DESCRIPTIVE_MONTHLY_START_YM,
            monthly_end_ym = DESCRIPTIVE_MONTHLY_END_YM
          )
        }
        
        # Child-group facet-grid comparison
        plot_main_history_future_spouse_treatment_childgrid(
          df = df_history_future_spouse_agg,
          var = v,
          treatment_var = tr,
          agg = agg,
          out_file = paste0(
            "main_history_future_",
            couple_plot_var_stem(v), "_",
            agg, "_",
            tr,
            history_future_suffix,
            "_childgrid_spousecols.png"
          ),
          fig_path = fig_path_couple_treatment_history_future_childgrids,
          treated_label = TREATMENT_LABS[[tr]],
          axis_text_size = AXIS_TEXT_SIZE,
          axis_title_size = AXIS_TITLE_SIZE,
          strip_text_size = STRIP_TEXT_SIZE,
          legend_text_size = LEGEND_TEXT_SIZE,
          legend_title_size = LEGEND_TITLE_SIZE,
          title_size = TITLE_SIZE,
          monthly_start_ym = DESCRIPTIVE_MONTHLY_START_YM,
          monthly_end_ym = DESCRIPTIVE_MONTHLY_END_YM
        )
        
        # Restricted comparison sample for wife-based treatments only
        if (tr %in% WIFE_TREATMENT_VARS && isTRUE(MAKE_RESTRICTED_HUSB_NOTKEY_VARIANTS)) {
          
          for (child_subset in CHILD_SUBSETS) {
            plot_main_history_future_spouse_treatment_numeric(
              df = df_history_future_spouse_agg,
              var = v,
              treatment_var = tr,
              child_subset = child_subset,
              agg = agg,
              restriction = "husb_notkey_or_edu",
              out_file = paste0(
                "main_history_future_",
                couple_plot_var_stem(v), "_",
                agg, "_",
                tr, "_",
                child_subset,
                history_future_suffix,
                "_spousefacet_husb_notkey_or_edu.png"
              ),
              fig_path = fig_path_couple_treatment_spousefacets,
              treated_label = TREATMENT_LABS[[tr]],
              axis_text_size = AXIS_TEXT_SIZE,
              axis_title_size = AXIS_TITLE_SIZE,
              strip_text_size = STRIP_TEXT_SIZE,
              legend_text_size = LEGEND_TEXT_SIZE,
              legend_title_size = LEGEND_TITLE_SIZE,
              title_size = TITLE_SIZE,
              monthly_start_ym = DESCRIPTIVE_MONTHLY_START_YM,
              monthly_end_ym = DESCRIPTIVE_MONTHLY_END_YM
            )
          }
          
          plot_main_history_future_spouse_treatment_childgrid(
            df = df_history_future_spouse_agg,
            var = v,
            treatment_var = tr,
            agg = agg,
            restriction = "husb_notkey_or_edu",
            out_file = paste0(
              "main_history_future_",
              couple_plot_var_stem(v), "_",
              agg, "_",
              tr,
              history_future_suffix,
              "_childgrid_spousecols_husb_notkey_or_edu.png"
            ),
            fig_path = fig_path_couple_treatment_history_future_childgrids,
            treated_label = TREATMENT_LABS[[tr]],
            axis_text_size = AXIS_TEXT_SIZE,
            axis_title_size = AXIS_TITLE_SIZE,
            strip_text_size = STRIP_TEXT_SIZE,
            legend_text_size = LEGEND_TEXT_SIZE,
            legend_title_size = LEGEND_TITLE_SIZE,
            title_size = TITLE_SIZE,
            monthly_start_ym = DESCRIPTIVE_MONTHLY_START_YM,
            monthly_end_ym = DESCRIPTIVE_MONTHLY_END_YM
          )
        }
      }
    }
  }
  }
}

# =============================================================================
# Couple-count figures by treatment group
# =============================================================================

COUNT_REQUIRE_BOTH <- if (isTRUE(MAKE_EXPLORATORY_EXTRA)) c(FALSE, TRUE) else FALSE

.count_suffix <- function(require_both_spouses) {
  if (isTRUE(require_both_spouses)) {
    "_both_spouses_observed"
  } else {
    ""
  }
}

for (tr in TREATMENT_VARS) {
  for (require_both in COUNT_REQUIRE_BOTH) {

    suffix <- .count_suffix(require_both)

    # COVID wave counts
    plot_covid_treatment_group_counts(
      df = df_covid_couple,
      treatment_var = tr,
      require_both_spouses = require_both,
      out_file = paste0(
        "covid_counts_wave_",
        tr,
        "_samplefacets",
        suffix,
        ".png"
      ),
      fig_path = fig_path_couple_treatment_counts,
      treated_label = TREATMENT_LABS[[tr]],
      axis_text_size = AXIS_TEXT_SIZE,
      axis_title_size = AXIS_TITLE_SIZE,
      strip_text_size = STRIP_TEXT_SIZE,
      legend_text_size = LEGEND_TEXT_SIZE,
      legend_title_size = LEGEND_TITLE_SIZE,
      title_size = TITLE_SIZE
    )

    # Restricted count plots for wife-based treatments
    if (tr %in% WIFE_TREATMENT_VARS && isTRUE(MAKE_RESTRICTED_HUSB_NOTKEY_VARIANTS)) {
      plot_covid_treatment_group_counts(
        df = df_covid_couple,
        treatment_var = tr,
        restriction = "husb_notkey_or_edu",
        require_both_spouses = require_both,
        out_file = paste0(
          "covid_counts_wave_",
          tr,
          "_samplefacets_husb_notkey_or_edu",
          suffix,
          ".png"
        ),
        fig_path = fig_path_couple_treatment_counts,
        treated_label = TREATMENT_LABS[[tr]],
        axis_text_size = AXIS_TEXT_SIZE,
        axis_title_size = AXIS_TITLE_SIZE,
        strip_text_size = STRIP_TEXT_SIZE,
        legend_text_size = LEGEND_TEXT_SIZE,
        legend_title_size = LEGEND_TITLE_SIZE,
        title_size = TITLE_SIZE
      )
    }

    # Future counts by wave, month, and year
    if (isTRUE(MAKE_FUTURE_ONLY_TREATMENT)) {
      for (agg in FUTURE_AGGS) {
        df_future_couple_agg <- if (agg == "ym") {
          df_future_couple_monthly
        } else {
          df_future_couple
        }

        plot_future_treatment_group_counts(
          df = df_future_couple_agg,
          treatment_var = tr,
          agg = agg,
          require_both_spouses = require_both,
          out_file = paste0(
            "future_counts_",
            agg, "_",
            tr,
            "_samplefacets",
            suffix,
            ".png"
          ),
          fig_path = fig_path_couple_treatment_counts,
          treated_label = TREATMENT_LABS[[tr]],
          axis_text_size = AXIS_TEXT_SIZE,
          axis_title_size = AXIS_TITLE_SIZE,
          strip_text_size = STRIP_TEXT_SIZE,
          legend_text_size = LEGEND_TEXT_SIZE,
          legend_title_size = LEGEND_TITLE_SIZE,
          title_size = TITLE_SIZE,
          monthly_start_ym = DESCRIPTIVE_MONTHLY_START_YM,
          monthly_end_ym = DESCRIPTIVE_MONTHLY_END_YM
        )

        if (tr %in% WIFE_TREATMENT_VARS && isTRUE(MAKE_RESTRICTED_HUSB_NOTKEY_VARIANTS)) {
          plot_future_treatment_group_counts(
            df = df_future_couple_agg,
            treatment_var = tr,
            agg = agg,
            restriction = "husb_notkey_or_edu",
            require_both_spouses = require_both,
            out_file = paste0(
              "future_counts_",
              agg, "_",
              tr,
              "_samplefacets_husb_notkey_or_edu",
              suffix,
              ".png"
            ),
            fig_path = fig_path_couple_treatment_counts,
            treated_label = TREATMENT_LABS[[tr]],
            axis_text_size = AXIS_TEXT_SIZE,
            axis_title_size = AXIS_TITLE_SIZE,
            strip_text_size = STRIP_TEXT_SIZE,
            legend_text_size = LEGEND_TEXT_SIZE,
            legend_title_size = LEGEND_TITLE_SIZE,
            title_size = TITLE_SIZE,
            monthly_start_ym = DESCRIPTIVE_MONTHLY_START_YM,
            monthly_end_ym = DESCRIPTIVE_MONTHLY_END_YM
          )
        }
      }
    }

    # Main-survey history + future counts by month and year
    if (length(history_future_sample_specs) > 0) {
      for (sample_spec in history_future_sample_specs) {
        history_future_suffix <- sample_spec$suffix
        df_history_future_couple_variant <- sample_spec$couple
        df_history_future_couple_monthly_variant <- sample_spec$couple_monthly

      for (agg in HISTORY_FUTURE_AGGS) {
        df_history_future_couple_agg <- if (agg == "ym") {
          df_history_future_couple_monthly_variant
        } else {
          df_history_future_couple_variant
        }

        plot_main_history_future_treatment_group_counts(
          df = df_history_future_couple_agg,
          treatment_var = tr,
          agg = agg,
          require_both_spouses = require_both,
          out_file = paste0(
            "main_history_future_counts_",
            agg, "_",
            tr,
            "_samplefacets",
            history_future_suffix,
            suffix,
            ".png"
          ),
          fig_path = fig_path_couple_treatment_counts,
          treated_label = TREATMENT_LABS[[tr]],
          axis_text_size = AXIS_TEXT_SIZE,
          axis_title_size = AXIS_TITLE_SIZE,
          strip_text_size = STRIP_TEXT_SIZE,
          legend_text_size = LEGEND_TEXT_SIZE,
          legend_title_size = LEGEND_TITLE_SIZE,
          title_size = TITLE_SIZE,
          monthly_start_ym = DESCRIPTIVE_MONTHLY_START_YM,
          monthly_end_ym = DESCRIPTIVE_MONTHLY_END_YM
        )

        if (tr %in% WIFE_TREATMENT_VARS && isTRUE(MAKE_RESTRICTED_HUSB_NOTKEY_VARIANTS)) {
          plot_main_history_future_treatment_group_counts(
            df = df_history_future_couple_agg,
            treatment_var = tr,
            agg = agg,
            restriction = "husb_notkey_or_edu",
            require_both_spouses = require_both,
            out_file = paste0(
              "main_history_future_counts_",
              agg, "_",
              tr,
              "_samplefacets_husb_notkey_or_edu",
              history_future_suffix,
              suffix,
              ".png"
            ),
            fig_path = fig_path_couple_treatment_counts,
            treated_label = TREATMENT_LABS[[tr]],
            axis_text_size = AXIS_TEXT_SIZE,
            axis_title_size = AXIS_TITLE_SIZE,
            strip_text_size = STRIP_TEXT_SIZE,
            legend_text_size = LEGEND_TEXT_SIZE,
            legend_title_size = LEGEND_TITLE_SIZE,
            title_size = TITLE_SIZE,
            monthly_start_ym = DESCRIPTIVE_MONTHLY_START_YM,
            monthly_end_ym = DESCRIPTIVE_MONTHLY_END_YM
          )
        }
      }
      }
    }
  }
}

cat("\nCouple-treatment descriptives complete.\n")
cat("Figures saved under: ", fig_path_couple_treatment, "\n", sep = "")
