# =============================================================================
# Script: code/run/02e_make_couple_baseline_descriptives.R
#
# Purpose:
#   Distribution figures for couples_graphs_short.tex.
#   2x2 panels (rows = child age group, cols = spouse).
#
#   Distributions pool ALL available waves from each data source:
#     - COVID source (waves ca-ci): for COVID-measured outcomes
#     - Main-survey source (all main-survey history + future waves): for
#       main-survey outcomes, including real-pay variables
#
#   File stems:
#     dist_covid_[var_stem]_childgrid_spousecols.png  <- COVID-source outcomes
#     dist_main_[var_stem]_childgrid_spousecols.png   <- main-survey outcomes
#
# Outputs:
#   figures/couple_treatment/baseline_distributions/
#
# Prerequisites:
#   Run code/run/01_build_data.R first.
#   Required derived files under der_path:
#     df_sample_long_covid_couplelevel.rds
#     couple_history_future_mainonly_long.rds
# =============================================================================

suppressPackageStartupMessages({
  library(tidyverse)
})

rm(list = ls())

source("code/lib/config.R")
source("code/lib/wave_labels.R")
source("code/lib/couple_plot_helpers.R")
source("code/lib/couple_baseline_dist_plots.R")

# =============================================================================
# Output directory
# =============================================================================

fig_path_baseline_dist <- file.path(fig_path_couple_treatment, "baseline_distributions")
dir.create(fig_path_baseline_dist, showWarnings = FALSE, recursive = TRUE)

# =============================================================================
# Load data
# =============================================================================

covid_file <- file.path(der_path, "df_sample_long_covid_couplelevel.rds")
if (!file.exists(covid_file)) {
  stop("COVID couple file not found: ", covid_file,
       "\nRun code/run/01_build_data.R first.")
}
df_covid_couple <- readRDS(covid_file)
df_covid_couple <- add_husits_wife_main_both(df_covid_couple)

# Exclude synthetic pre-period rows; keep only actual COVID study waves
df_covid_spouse <- df_covid_couple %>%
  dplyr::filter(!wave %in% c("2019", "baseline")) %>%
  reshape_couple_long_to_spouse_long()

main_file <- file.path(der_path, "couple_history_future_mainonly_long.rds")
if (file.exists(main_file)) {
  df_main_couple <- readRDS(main_file)
  # Exclude synthetic baseline rows (baseline_i / baseline_j / baseline_k)
  df_main_spouse <- df_main_couple %>%
    dplyr::filter(!grepl("^baseline_", wave)) %>%
    reshape_couple_long_to_spouse_long()
} else {
  warning("Main-survey couple file not found: ", main_file,
          ". Main-survey distributions will be skipped.")
  df_main_spouse <- NULL
}

# =============================================================================
# Plot settings
# =============================================================================

AXIS_TEXT_SIZE  <- 13
AXIS_TITLE_SIZE <- 14
STRIP_TEXT_SIZE <- 13
TITLE_SIZE      <- 13

# =============================================================================
# COVID-source outcomes  (all COVID waves ca-ci pooled)
# =============================================================================

COVID_OUTCOMES <- c(
  "any_work",             # binary
  "hours",                # continuous: hours worked last week (COVID measure)
  "workoutside",          # binary
  "wfh_some",             # binary
  "howlng",               # continuous: housework hours
  "timechcare",           # continuous: childcare hours
  "husits_wife_main_both" # binary: childcare responsibility
)

for (v in COVID_OUTCOMES) {
  if (!v %in% names(df_covid_spouse)) {
    message("Skipping '", v, "': not found in COVID data.")
    next
  }
  stem <- couple_plot_var_stem(v)
  plot_baseline_dist_for_var(
    df_spouse_long  = df_covid_spouse,
    var             = v,
    out_file        = paste0("dist_covid_", stem, "_childgrid_spousecols.png"),
    fig_path        = fig_path_baseline_dist,
    axis_text_size  = AXIS_TEXT_SIZE,
    axis_title_size = AXIS_TITLE_SIZE,
    strip_text_size = STRIP_TEXT_SIZE,
    title_size      = TITLE_SIZE
  )
}

if ("work_last_week_status" %in% names(df_covid_spouse)) {
  plot_baseline_work_status(
    df_spouse_long  = df_covid_spouse,
    out_file        = "dist_covid_work_status_last_week_childgrid_spousecols.png",
    fig_path        = fig_path_baseline_dist,
    axis_text_size  = AXIS_TEXT_SIZE,
    axis_title_size = AXIS_TITLE_SIZE,
    strip_text_size = STRIP_TEXT_SIZE,
    title_size      = TITLE_SIZE
  )
} else {
  message("Skipping COVID work_last_week_status: not found in COVID data.")
}

# =============================================================================
# Main-survey outcomes  (all main-survey history + future waves pooled)
# =============================================================================

if (!is.null(df_main_spouse)) {

  MAIN_OUTCOMES <- c(
    "any_work",              # binary
    "jbhrs",                 # continuous: usual weekly hours
    "workoutside",           # binary
    "wfh_some",              # binary
    "paygu_dv_real",         # continuous: gross monthly pay (real)
    "fimnlabgrs_dv_real",    # continuous: gross monthly labour income (real)
    "howlng"                 # continuous: housework hours
  )

  for (v in MAIN_OUTCOMES) {
    if (!v %in% names(df_main_spouse)) {
      message("Skipping '", v, "': not found in main-survey data.")
      next
    }
    stem <- couple_plot_var_stem(v)
    plot_baseline_dist_for_var(
      df_spouse_long  = df_main_spouse,
      var             = v,
      out_file        = paste0("dist_main_", stem, "_childgrid_spousecols.png"),
      fig_path        = fig_path_baseline_dist,
      axis_text_size  = AXIS_TEXT_SIZE,
      axis_title_size = AXIS_TITLE_SIZE,
      strip_text_size = STRIP_TEXT_SIZE,
      title_size      = TITLE_SIZE
    )
  }

  if ("work_last_week_status" %in% names(df_main_spouse)) {
    plot_baseline_work_status(
      df_spouse_long  = df_main_spouse,
      out_file        = "dist_main_work_status_last_week_childgrid_spousecols.png",
      fig_path        = fig_path_baseline_dist,
      axis_text_size  = AXIS_TEXT_SIZE,
      axis_title_size = AXIS_TITLE_SIZE,
      strip_text_size = STRIP_TEXT_SIZE,
      title_size      = TITLE_SIZE
    )
  } else {
    message("Skipping main work_last_week_status: not found in main-survey data.")
  }
}

cat("\nDistribution figures complete.\n")
cat("Figures saved under: ", fig_path_baseline_dist, "\n", sep = "")
