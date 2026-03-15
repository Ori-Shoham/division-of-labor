# =============================================================================
# Script: code/run/02_make_descriptives.R
#
# Purpose:
#   Generate descriptive figures for:
#     (1) Full COVID sample
#     (2) Couples-only COVID sample
#
# Notes:
#   - Plotting primitives live in code/lib/descriptives_plots.R
#   - IMPORTANT: All plotting calls use explicit argument names to avoid
#     positional argument misalignment bugs (e.g., missing min_n).
# =============================================================================

suppressPackageStartupMessages({
  library(tidyverse)
})

source("code/lib/config.R")
source("code/lib/wave_labels.R")
source("code/lib/descriptives_plots.R")

dir.create(fig_path, showWarnings = FALSE, recursive = TRUE)

# ---- Load datasets -----------------------------------------------------------
df_full    <- readRDS(file.path(der_path, "df_sample_long_covid.rds"))
df_couples <- readRDS(file.path(der_path, "df_sample_long_covid_couples.rds"))

# Global plotting options (so you change them once)
MIN_N_DEFAULT <- 25

# Helper to loop over the two datasets while keeping file names clean
.dataset_specs <- list(
  list(name = "full",    df = df_full,    suffix = ""),
  list(name = "couples", df = df_couples, suffix = "_couples")
)

# =============================================================================
# SECTION A: Industry plots (April / May)
# =============================================================================
for (spec in .dataset_specs) {
  
  df <- spec$df
  suffix <- spec$suffix
  
  # ---- Worked at all (counts + perc) ----------------------------------------
  plot_worked_at_all_bar(
    df = df,
    wave_code = "ca",
    by = "industry",
    min_n = MIN_N_DEFAULT,
    perc = FALSE,
    out_file = paste0("worked_at_all_april20_ind", suffix, ".png"),
    fig_path = fig_path
  )
  plot_worked_at_all_bar(
    df = df,
    wave_code = "ca",
    by = "industry",
    min_n = MIN_N_DEFAULT,
    perc = TRUE,
    out_file = paste0("worked_at_all_april20_ind_perc", suffix, ".png"),
    fig_path = fig_path
  )
  
  plot_worked_at_all_bar(
    df = df,
    wave_code = "cb",
    by = "industry",
    min_n = MIN_N_DEFAULT,
    perc = FALSE,
    out_file = paste0("worked_at_all_may20_ind", suffix, ".png"),
    fig_path = fig_path
  )
  plot_worked_at_all_bar(
    df = df,
    wave_code = "cb",
    by = "industry",
    min_n = MIN_N_DEFAULT,
    perc = TRUE,
    out_file = paste0("worked_at_all_may20_ind_perc", suffix, ".png"),
    fig_path = fig_path
  )
  
  # ---- Work status (counts + perc) ------------------------------------------
  plot_work_status_bar(
    df = df,
    wave_code = "ca",
    by = "industry",
    min_n = MIN_N_DEFAULT,
    perc = FALSE,
    out_file = paste0("work_status_april20_ind", suffix, ".png"),
    fig_path = fig_path
  )
  plot_work_status_bar(
    df = df,
    wave_code = "ca",
    by = "industry",
    min_n = MIN_N_DEFAULT,
    perc = TRUE,
    out_file = paste0("work_status_april20_ind_perc", suffix, ".png"),
    fig_path = fig_path
  )
  
  plot_work_status_bar(
    df = df,
    wave_code = "cb",
    by = "industry",
    min_n = MIN_N_DEFAULT,
    perc = FALSE,
    out_file = paste0("work_status_may20_ind", suffix, ".png"),
    fig_path = fig_path
  )
  plot_work_status_bar(
    df = df,
    wave_code = "cb",
    by = "industry",
    min_n = MIN_N_DEFAULT,
    perc = TRUE,
    out_file = paste0("work_status_may20_ind_perc", suffix, ".png"),
    fig_path = fig_path
  )
  
  # ---- Furlough (April; percent) --------------------------------------------
  plot_furlough_bar(
    df = df,
    wave_code = "ca",
    by = "industry",
    min_n = MIN_N_DEFAULT,
    out_file = paste0("furlough_april20_ind_perc", suffix, ".png"),
    fig_path = fig_path
  )
  
  # ---- WFH (April + May; percent) -------------------------------------------
  plot_wfh_bar(
    df = df,
    wave_code = "ca",
    by = "industry",
    min_n = MIN_N_DEFAULT,
    out_file = paste0("wfh_april20_ind_perc", suffix, ".png"),
    fig_path = fig_path
  )
  plot_wfh_bar(
    df = df,
    wave_code = "cb",
    by = "industry",
    min_n = MIN_N_DEFAULT,
    out_file = paste0("wfh_may20_ind_perc", suffix, ".png"),
    fig_path = fig_path
  )
}

# =============================================================================
# SECTION B: Group plots (April/May) + Over-time plots
# =============================================================================
for (spec in .dataset_specs) {
  
  df <- spec$df
  suffix <- spec$suffix
  
  # ---- April groups: worked-at-all (counts + perc) ---------------------------
  plot_worked_at_all_bar(
    df = df,
    wave_code = "ca",
    by = "group_industry_based",
    min_n = MIN_N_DEFAULT,
    perc = FALSE,
    out_file = paste0("worked_at_all_april20_groups", suffix, ".png"),
    fig_path = fig_path
  )
  plot_worked_at_all_bar(
    df = df,
    wave_code = "ca",
    by = "group_industry_based",
    min_n = MIN_N_DEFAULT,
    perc = TRUE,
    out_file = paste0("worked_at_all_april20_groups_perc", suffix, ".png"),
    fig_path = fig_path
  )
  
  plot_worked_at_all_bar(
    df = df,
    wave_code = "ca",
    by = "group_industry_based_detailed",
    min_n = MIN_N_DEFAULT,
    perc = FALSE,
    out_file = paste0("worked_at_all_april20_groups_detailed", suffix, ".png"),
    fig_path = fig_path
  )
  plot_worked_at_all_bar(
    df = df,
    wave_code = "ca",
    by = "group_industry_based_detailed",
    min_n = MIN_N_DEFAULT,
    perc = TRUE,
    out_file = paste0("worked_at_all_april20_groups_detailed_perc", suffix, ".png"),
    fig_path = fig_path
  )
  
  # ---- April groups: work status (counts + perc) -----------------------------
  plot_work_status_bar(
    df = df,
    wave_code = "ca",
    by = "group_industry_based",
    min_n = MIN_N_DEFAULT,
    perc = FALSE,
    out_file = paste0("work_status_april20_groups", suffix, ".png"),
    fig_path = fig_path
  )
  plot_work_status_bar(
    df = df,
    wave_code = "ca",
    by = "group_industry_based",
    min_n = MIN_N_DEFAULT,
    perc = TRUE,
    out_file = paste0("work_status_april20_groups_perc", suffix, ".png"),
    fig_path = fig_path
  )
  
  # ---- April groups: furlough ------------------------------------------------
  plot_furlough_bar(
    df = df,
    wave_code = "ca",
    by = "group_industry_based",
    min_n = MIN_N_DEFAULT,
    out_file = paste0("furlough_april20_groups_perc", suffix, ".png"),
    fig_path = fig_path
  )
  plot_furlough_bar(
    df = df,
    wave_code = "ca",
    by = "group_industry_based_detailed",
    min_n = MIN_N_DEFAULT,
    out_file = paste0("furlough_april20_groups_detailed_perc", suffix, ".png"),
    fig_path = fig_path
  )
  
  # ---- April groups: WFH -----------------------------------------------------
  plot_wfh_bar(
    df = df,
    wave_code = "ca",
    by = "group_industry_based",
    min_n = MIN_N_DEFAULT,
    out_file = paste0("wfh_april20_groups_perc", suffix, ".png"),
    fig_path = fig_path
  )
  plot_wfh_bar(
    df = df,
    wave_code = "ca",
    by = "group_industry_based_detailed",
    min_n = MIN_N_DEFAULT,
    out_file = paste0("wfh_april20_detailed_groups_perc", suffix, ".png"),
    fig_path = fig_path
  )
  
  # ---- May groups: worked-at-all + work status + WFH -------------------------
  plot_worked_at_all_bar(
    df = df,
    wave_code = "cb",
    by = "group_industry_based",
    min_n = MIN_N_DEFAULT,
    perc = FALSE,
    out_file = paste0("worked_at_all_may20_groups", suffix, ".png"),
    fig_path = fig_path
  )
  plot_worked_at_all_bar(
    df = df,
    wave_code = "cb",
    by = "group_industry_based",
    min_n = MIN_N_DEFAULT,
    perc = TRUE,
    out_file = paste0("worked_at_all_may20_groups_perc", suffix, ".png"),
    fig_path = fig_path
  )
  plot_worked_at_all_bar(
    df = df,
    wave_code = "cb",
    by = "group_industry_based_detailed",
    min_n = MIN_N_DEFAULT,
    perc = TRUE,
    out_file = paste0("worked_at_all_may20_detailed_groups_perc", suffix, ".png"),
    fig_path = fig_path
  )
  
  plot_work_status_bar(
    df = df,
    wave_code = "cb",
    by = "group_industry_based",
    min_n = MIN_N_DEFAULT,
    perc = FALSE,
    out_file = paste0("work_status_may20_groups", suffix, ".png"),
    fig_path = fig_path
  )
  plot_work_status_bar(
    df = df,
    wave_code = "cb",
    by = "group_industry_based",
    min_n = MIN_N_DEFAULT,
    perc = TRUE,
    out_file = paste0("work_status_may20_groups_perc", suffix, ".png"),
    fig_path = fig_path
  )
  
  plot_wfh_bar(
    df = df,
    wave_code = "cb",
    by = "group_industry_based",
    min_n = MIN_N_DEFAULT,
    out_file = paste0("wfh_may20_groups_perc", suffix, ".png"),
    fig_path = fig_path
  )
  plot_wfh_bar(
    df = df,
    wave_code = "cb",
    by = "group_industry_based_detailed",
    min_n = MIN_N_DEFAULT,
    out_file = paste0("wfh_may20_detailed_groups_perc", suffix, ".png"),
    fig_path = fig_path
  )
  
  # =============================================================================
  # Over-time plots (the ones you asked to add)
  # =============================================================================
  
  # Worked-at-all over time (groups + detailed)
  plot_overtime_worked(
    df = df,
    by = "group_industry_based",
    out_file = paste0("worked_at_all_groups_overtime", suffix, ".png"),
    fig_path = fig_path
  )
  plot_overtime_worked(
    df = df,
    by = "group_industry_based_detailed",
    out_file = paste0("worked_at_all_detailed_groups_overtime", suffix, ".png"),
    fig_path = fig_path
  )
  
  # Hours over time (groups + detailed)
  plot_overtime_hours(
    df = df,
    by = "group_industry_based",
    out_file = paste0("hours_groups_overtime", suffix, ".png"),
    fig_path = fig_path
  )
  plot_overtime_hours(
    df = df,
    by = "group_industry_based_detailed",
    out_file = paste0("hours_detailed_groups_overtime", suffix, ".png"),
    fig_path = fig_path
  )
  
  # Work outside over time (overall + groups + detailed)
  plot_workoutside_overtime(
    df = df,
    by = NULL,
    out_file = paste0("workoutside_overtime", suffix, ".png"),
    fig_path = fig_path
  )
  plot_workoutside_overtime(
    df = df,
    by = "group_industry_based",
    out_file = paste0("workoutside_overtime_groups", suffix, ".png"),
    fig_path = fig_path
  )
  plot_workoutside_overtime(
    df = df,
    by = "group_industry_based_detailed",
    out_file = paste0("workoutside_overtime_detailed_groups", suffix, ".png"),
    fig_path = fig_path
  )
  
  # WFH over time (faceted bars; groups + detailed)
  plot_wfh_overtime_facets(
    df = df,
    by = "group_industry_based",
    out_file = paste0("wfh_groups_overtime", suffix, ".png"),
    fig_path = fig_path
  )
  plot_wfh_overtime_facets(
    df = df,
    by = "group_industry_based_detailed",
    out_file = paste0("wfh_detailed_groups_overtime", suffix, ".png"),
    fig_path = fig_path
  )
}

# =============================================================================
# Keyworker definition exploration
#
# Purpose:
#   Diagnostic figures comparing:
#     - self-reported COVID keyworker status / sector
#   against:
#     - baseline industry-based treatment groups
#     - baseline detailed industry-based treatment groups
#     - industries
#     - occupations
#
# Notes:
#   - April 2020 (ca): self-report comes from `keyworker`
#   - May 2020   (cb): self-report comes from `keyworksector`
#   - Industry / occupation versions:
#       * lump small categories into "other"
#       * sort by descending share self-reporting as key workers
#       * use horizontal bars for readability
# =============================================================================

# ---------------------------------------------------------------------------
# Group-level comparison figures
# ---------------------------------------------------------------------------

# April 2020: binary self-reported keyworker status vs broad groups
plot_keyworker_definition_compare(
  df = df,
  wave_code = "ca",
  by = "group_industry_based",
  detailed_fill = FALSE,
  out_file = paste0("keyworker_def_explore_groups_april20", suffix, ".png"),
  fig_path = fig_path
)

# April 2020: binary self-reported keyworker status vs detailed groups
plot_keyworker_definition_compare(
  df = df,
  wave_code = "ca",
  by = "group_industry_based_detailed",
  detailed_fill = FALSE,
  out_file = paste0("keyworker_def_explore_detailed_groups_april20", suffix, ".png"),
  fig_path = fig_path
)

# May 2020: binary self-reported keyworker status vs broad groups
plot_keyworker_definition_compare(
  df = df,
  wave_code = "cb",
  by = "group_industry_based",
  detailed_fill = FALSE,
  out_file = paste0("keyworker_def_explore_groups_may20", suffix, ".png"),
  fig_path = fig_path
)

# May 2020: binary self-reported keyworker status vs detailed groups
plot_keyworker_definition_compare(
  df = df,
  wave_code = "cb",
  by = "group_industry_based_detailed",
  detailed_fill = FALSE,
  out_file = paste0("keyworker_def_explore_detailed_groups_may20", suffix, ".png"),
  fig_path = fig_path
)

# May 2020: detailed self-reported keywork sector vs detailed groups
plot_keyworker_definition_compare(
  df = df,
  wave_code = "cb",
  by = "group_industry_based_detailed",
  detailed_fill = TRUE,
  out_file = paste0("keywork_sector_def_explore_detailed_groups_may20", suffix, ".png"),
  fig_path = fig_path
)

# ---------------------------------------------------------------------------
# Industry-level comparison figures
# ---------------------------------------------------------------------------

# April 2020: binary self-reported keyworker status vs industries
plot_keyworker_definition_compare(
  df = df,
  wave_code = "ca",
  by = "industry",
  detailed_fill = FALSE,
  min_n = MIN_N_DEFAULT,
  order_desc = TRUE,
  out_file = paste0("keyworker_def_explore_industry_april20", suffix, ".png"),
  fig_path = fig_path
)

# May 2020: binary self-reported keyworker status vs industries
plot_keyworker_definition_compare(
  df = df,
  wave_code = "cb",
  by = "industry",
  detailed_fill = FALSE,
  min_n = MIN_N_DEFAULT,
  order_desc = TRUE,
  out_file = paste0("keyworker_def_explore_industry_may20", suffix, ".png"),
  fig_path = fig_path
)

# May 2020: detailed self-reported keywork sector vs industries
plot_keyworker_definition_compare(
  df = df,
  wave_code = "cb",
  by = "industry",
  detailed_fill = TRUE,
  min_n = MIN_N_DEFAULT,
  order_desc = TRUE,
  out_file = paste0("keywork_sector_def_explore_industry_may20", suffix, ".png"),
  fig_path = fig_path
)

# ---------------------------------------------------------------------------
# Occupation-level comparison figures
# ---------------------------------------------------------------------------

# April 2020: binary self-reported keyworker status vs occupations
plot_keyworker_definition_compare(
  df = df,
  wave_code = "ca",
  by = "occupation",
  detailed_fill = FALSE,
  min_n = MIN_N_DEFAULT,
  order_desc = TRUE,
  out_file = paste0("keyworker_def_explore_occupation_april20", suffix, ".png"),
  fig_path = fig_path
)

# May 2020: binary self-reported keyworker status vs occupations
plot_keyworker_definition_compare(
  df = df,
  wave_code = "cb",
  by = "occupation",
  detailed_fill = FALSE,
  min_n = MIN_N_DEFAULT,
  order_desc = TRUE,
  out_file = paste0("keyworker_def_explore_occupation_may20", suffix, ".png"),
  fig_path = fig_path
)

# May 2020: detailed self-reported keywork sector vs occupations
plot_keyworker_definition_compare(
  df = df,
  wave_code = "cb",
  by = "occupation",
  detailed_fill = TRUE,
  min_n = MIN_N_DEFAULT,
  order_desc = TRUE,
  out_file = paste0("keywork_sector_def_explore_occupation_may20", suffix, ".png"),
  fig_path = fig_path
)

cat("\nDescriptives complete (full + couples).\n")
cat("Figures saved to: ", fig_path, "\n")