# =============================================================================
# Script: code/run/02e_make_couple_baseline_descriptives.R
#
# Purpose:
#   Baseline distribution figures for couples_graphs_short.tex.
#   2x2 panels (rows = child age group, cols = spouse) showing:
#     - continuous outcomes: histogram + density at 2019 baseline
#     - binary outcomes: bar chart of share at 2019 baseline
#     - timechcare / husits: COVID wave 1 (no 2019 equivalent)
#     - work_last_week_status: categorical bar chart at COVID wave 1
#
#   Over-time work_last_week_status figures (stacked bar by COVID wave) are
#   generated in code/run/02d_make_couple_treatment_descriptives.R.
#
# Outputs:
#   figures/couple_treatment/baseline_distributions/
#     baseline_dist_[var_stem]_childgrid_spousecols.png
#
# Prerequisites:
#   Run code/run/01_build_data.R first.
#   The following derived files must exist under der_path:
#     s2019_baseline_couplelevel.rds      (main 2019 baseline outcomes)
#     df_sample_long_covid_couplelevel.rds (COVID couple panel, for wave-1 figures)
# =============================================================================

suppressPackageStartupMessages({
  library(tidyverse)
})

rm(list = ls())

source("code/lib/config.R")
source("code/lib/couple_plot_helpers.R")
source("code/lib/couple_baseline_dist_plots.R")

# =============================================================================
# Output directory
# =============================================================================

fig_path_baseline_dist <- file.path(fig_path_couple_treatment, "baseline_distributions")
dir.create(fig_path_baseline_dist, showWarnings = FALSE, recursive = TRUE)

# =============================================================================
# Load 2019 baseline couple-level data
# =============================================================================

baseline_file <- file.path(der_path, "s2019_baseline_couplelevel.rds")
if (!file.exists(baseline_file)) {
  stop(
    "Baseline couple-level file not found: ", baseline_file,
    "\nRun code/run/01_build_data.R first."
  )
}
df_baseline_couple <- readRDS(baseline_file)

# Load COVID couple panel for COVID-only variables (timechcare, husits)
covid_file <- file.path(der_path, "df_sample_long_covid_couplelevel.rds")
if (!file.exists(covid_file)) {
  stop(
    "COVID couple-level file not found: ", covid_file,
    "\nRun code/run/01_build_data.R first."
  )
}
df_covid_couple <- readRDS(covid_file)
df_covid_couple <- add_husits_wife_main_both(df_covid_couple)

# =============================================================================
# Reshape to spouse-long
# =============================================================================

df_baseline_spouse <- reshape_couple_long_to_spouse_long(df_baseline_couple)

# COVID-only variables: use first wave (ca) to show a baseline-like distribution
df_covid_wave1_spouse <- df_covid_couple %>%
  dplyr::filter(wave == "ca") %>%
  reshape_couple_long_to_spouse_long()

# =============================================================================
# Plot settings
# =============================================================================

AXIS_TEXT_SIZE  <- 13
AXIS_TITLE_SIZE <- 14
STRIP_TEXT_SIZE <- 13
TITLE_SIZE      <- 13

# =============================================================================
# Baseline (2019) outcomes
# =============================================================================

BASELINE_OUTCOMES <- c(
  "any_work",           # binary
  "jbhrs",              # continuous: usual weekly hours (main study)
  "workoutside",        # binary (may be missing in 2019 main survey)
  "wfh_some",           # binary (may be missing in 2019 main survey)
  "paygu_dv_real",      # continuous
  "fimnlabgrs_dv_real", # continuous
  "howlng"              # continuous: housework hours
  # husits_wife_main_both handled separately via COVID wave 1
)

for (v in BASELINE_OUTCOMES) {
  if (!v %in% names(df_baseline_spouse)) {
    message("Skipping '", v, "': not found in baseline data.")
    next
  }

  stem <- couple_plot_var_stem(v)

  plot_baseline_dist_for_var(
    df_spouse_long = df_baseline_spouse,
    var            = v,
    out_file       = paste0("baseline_dist_", stem, "_childgrid_spousecols.png"),
    fig_path       = fig_path_baseline_dist,
    axis_text_size  = AXIS_TEXT_SIZE,
    axis_title_size = AXIS_TITLE_SIZE,
    strip_text_size = STRIP_TEXT_SIZE,
    title_size      = TITLE_SIZE
  )
}

# =============================================================================
# COVID-only variables: first COVID wave (no 2019 equivalent)
# =============================================================================

COVID_ONLY_OUTCOMES <- list(
  list(var = "timechcare",           out_stem = "childcare_hours"),
  list(var = "husits_wife_main_both", out_stem = "childcare_responsibility")
)

for (spec in COVID_ONLY_OUTCOMES) {
  v    <- spec$var
  stem <- spec$out_stem
  if (v %in% names(df_covid_wave1_spouse)) {
    plot_baseline_dist_for_var(
      df_spouse_long = df_covid_wave1_spouse,
      var            = v,
      out_file       = paste0("baseline_dist_", stem, "_childgrid_spousecols.png"),
      fig_path       = fig_path_baseline_dist,
      axis_text_size  = AXIS_TEXT_SIZE,
      axis_title_size = AXIS_TITLE_SIZE,
      strip_text_size = STRIP_TEXT_SIZE,
      title_size      = TITLE_SIZE
    )
  } else {
    message("Skipping '", v, "': not found in wave-1 COVID data.")
  }
}

# =============================================================================
# work_last_week_status: baseline distribution (COVID wave 1)
# =============================================================================

if ("work_last_week_status" %in% names(df_covid_wave1_spouse)) {
  plot_baseline_work_status(
    df_spouse_long = df_covid_wave1_spouse,
    out_file       = "baseline_dist_work_status_last_week_childgrid_spousecols.png",
    fig_path       = fig_path_baseline_dist,
    axis_text_size  = AXIS_TEXT_SIZE,
    axis_title_size = AXIS_TITLE_SIZE,
    strip_text_size = STRIP_TEXT_SIZE,
    title_size      = TITLE_SIZE
  )
} else {
  message("Skipping work_last_week_status baseline: not found in wave-1 COVID data.")
}

cat("\nBaseline distribution figures complete.\n")
cat("Figures saved under: ", fig_path_baseline_dist, "\n", sep = "")
