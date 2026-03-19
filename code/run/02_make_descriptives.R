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

# Grouping variables used repeatedly throughout the script
.group_specs <- list(
  list(
    by = "group_industry_based",
    stem = "groups"
  ),
  list(
    by = "group_industry_based_detailed",
    stem = "detailed_groups"
  )
)

.numeric_overtime_specs <- list(
  list(var = "howlng",
       stem = "howlng",
       y_lab = "Housework hours",
       title = "Housework hours over time"),
  list(var = "timechcare",
       stem = "timechcare",
       y_lab = "Childcare / home-schooling hours",
       title = "Childcare / home-schooling hours over time")
)

.binary_childcare_specs <- list(
  list(var = "workchsch",
       stem = "workchsch"),
  list(var = "workchsch2",
       stem = "workchsch2")
)

# =============================================================================
# SECTION A: Industry plots (April / May)
# =============================================================================
for (spec in .dataset_specs) {

  df <- spec$df
  suffix <- spec$suffix

  # ---- Worked at all (counts + perc) ----------------------------------------
  for (wave_info in list(
    list(wave = "ca", tag = "april20"),
    list(wave = "cb", tag = "may20")
  )) {
    for (perc_flag in c(FALSE, TRUE)) {
      plot_worked_at_all_bar(
        df = df,
        wave_code = wave_info$wave,
        by = "industry",
        min_n = MIN_N_DEFAULT,
        perc = perc_flag,
        out_file = paste0(
          "worked_at_all_", wave_info$tag, "_ind",
          if (perc_flag) "_perc" else "",
          suffix,
          ".png"
        ),
        fig_path = fig_path
      )
    }
  }

  # ---- Work status (counts + perc) ------------------------------------------
  for (wave_info in list(
    list(wave = "ca", tag = "april20"),
    list(wave = "cb", tag = "may20")
  )) {
    for (perc_flag in c(FALSE, TRUE)) {
      plot_work_status_bar(
        df = df,
        wave_code = wave_info$wave,
        by = "industry",
        min_n = MIN_N_DEFAULT,
        perc = perc_flag,
        out_file = paste0(
          "work_status_", wave_info$tag, "_ind",
          if (perc_flag) "_perc" else "",
          suffix,
          ".png"
        ),
        fig_path = fig_path
      )
    }
  }

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
  for (wave_info in list(
    list(wave = "ca", tag = "april20"),
    list(wave = "cb", tag = "may20")
  )) {
    plot_wfh_bar(
      df = df,
      wave_code = wave_info$wave,
      by = "industry",
      min_n = MIN_N_DEFAULT,
      out_file = paste0("wfh_", wave_info$tag, "_ind_perc", suffix, ".png"),
      fig_path = fig_path
    )
  }
}

# =============================================================================
# SECTION B: Group plots (April/May) + Over-time plots
# =============================================================================
for (spec in .dataset_specs) {

  df <- spec$df
  suffix <- spec$suffix

  # ---- April/May grouped bar plots ------------------------------------------
  for (grp in .group_specs) {

    # Worked at all
    for (wave_info in list(
      list(wave = "ca", tag = "april20"),
      list(wave = "cb", tag = "may20")
    )) {
      for (perc_flag in c(FALSE, TRUE)) {
        plot_worked_at_all_bar(
          df = df,
          wave_code = wave_info$wave,
          by = grp$by,
          min_n = MIN_N_DEFAULT,
          perc = perc_flag,
          out_file = paste0(
            "worked_at_all_", wave_info$tag, "_", grp$stem,
            if (perc_flag) "_perc" else "",
            suffix,
            ".png"
          ),
          fig_path = fig_path
        )
      }
    }

    # Work status
    for (wave_info in list(
      list(wave = "ca", tag = "april20"),
      list(wave = "cb", tag = "may20")
    )) {
      for (perc_flag in c(FALSE, TRUE)) {
        plot_work_status_bar(
          df = df,
          wave_code = wave_info$wave,
          by = grp$by,
          min_n = MIN_N_DEFAULT,
          perc = perc_flag,
          out_file = paste0(
            "work_status_", wave_info$tag, "_", grp$stem,
            if (perc_flag) "_perc" else "",
            suffix,
            ".png"
          ),
          fig_path = fig_path
        )
      }
    }

    # Furlough (April only)
    plot_furlough_bar(
      df = df,
      wave_code = "ca",
      by = grp$by,
      min_n = MIN_N_DEFAULT,
      out_file = paste0("furlough_april20_", grp$stem, "_perc", suffix, ".png"),
      fig_path = fig_path
    )

    # WFH (April + May)
    for (wave_info in list(
      list(wave = "ca", tag = "april20"),
      list(wave = "cb", tag = "may20")
    )) {
      plot_wfh_bar(
        df = df,
        wave_code = wave_info$wave,
        by = grp$by,
        min_n = MIN_N_DEFAULT,
        out_file = paste0("wfh_", wave_info$tag, "_", grp$stem, "_perc", suffix, ".png"),
        fig_path = fig_path
      )
    }
  }

  # ---- Over-time plots -------------------------------------------------------

  # Worked-at-all over time
  for (grp in .group_specs) {
    plot_overtime_worked(
      df = df,
      by = grp$by,
      out_file = paste0("worked_at_all_", grp$stem, "_overtime", suffix, ".png"),
      fig_path = fig_path
    )
  }

  # Hours over time
  for (grp in .group_specs) {
    plot_overtime_hours(
      df = df,
      by = grp$by,
      out_file = paste0("hours_", grp$stem, "_overtime", suffix, ".png"),
      fig_path = fig_path
    )
  }

  # Additional continuous time-use plots
  for (num_spec in .numeric_overtime_specs) {
    if (!num_spec$var %in% names(df)) next

    plot_covid_numeric_overtime(
      df = df,
      var = num_spec$var,
      y_lab = num_spec$y_lab,
      title = num_spec$title,
      by = NULL,
      out_file = paste0(num_spec$stem, "_overtime", suffix, ".png"),
      fig_path = fig_path
    )

    for (grp in .group_specs) {
      plot_covid_numeric_overtime(
        df = df,
        var = num_spec$var,
        y_lab = num_spec$y_lab,
        title = paste0(num_spec$title, ", by ", grp$by),
        by = grp$by,
        out_file = paste0(num_spec$stem, "_overtime_", grp$stem, suffix, ".png"),
        fig_path = fig_path
      )
    }
  }

  # Work outside over time (overall + grouped)
  plot_workoutside_overtime(
    df = df,
    by = NULL,
    out_file = paste0("workoutside_overtime", suffix, ".png"),
    fig_path = fig_path
  )
  for (grp in .group_specs) {
    plot_workoutside_overtime(
      df = df,
      by = grp$by,
      out_file = paste0("workoutside_overtime_", grp$stem, suffix, ".png"),
      fig_path = fig_path
    )
  }

  # WFH-some over time (overall + grouped)
  plot_wfh_some_overtime(
    df = df,
    by = NULL,
    out_file = paste0("wfh_some_overtime", suffix, ".png"),
    fig_path = fig_path
  )
  for (grp in .group_specs) {
    plot_wfh_some_overtime(
      df = df,
      by = grp$by,
      out_file = paste0("wfh_some_overtime_", grp$stem, suffix, ".png"),
      fig_path = fig_path
    )
  }

  # WFH intensity over time (faceted bars; grouped)
  for (grp in .group_specs) {
    plot_wfh_overtime_facets(
      df = df,
      by = grp$by,
      out_file = paste0("wfh_", grp$stem, "_overtime", suffix, ".png"),
      fig_path = fig_path
    )
  }

  # Furlough over time: same categories as April/May conventions
  plot_furlough_overtime_facets(
    df = df,
    by = NULL,
    out_file = paste0("furlough_overtime", suffix, ".png"),
    fig_path = fig_path
  )
  for (grp in .group_specs) {
    plot_furlough_overtime_facets(
      df = df,
      by = grp$by,
      out_file = paste0("furlough_overtime_", grp$stem, suffix, ".png"),
      fig_path = fig_path
    )
  }

  # Work schedule adapted because of childcare / home schooling
  for (bin_spec in .binary_childcare_specs) {
    if (!bin_spec$var %in% names(df)) next

    plot_workchsch_overtime(
      df = df,
      var = bin_spec$var,
      by = NULL,
      out_file = paste0(bin_spec$stem, "_overtime", suffix, ".png"),
      fig_path = fig_path
    )

    for (grp in .group_specs) {
      plot_workchsch_overtime(
        df = df,
        var = bin_spec$var,
        by = grp$by,
        out_file = paste0(bin_spec$stem, "_overtime_", grp$stem, suffix, ".png"),
        fig_path = fig_path
      )
    }
  }

  # Childcare responsibility: couples sample with baseline children only
  if (spec$name == "couples" && "husits_cv" %in% names(df)) {
    plot_husits_cv_overtime(
      df = df,
      by = NULL,
      out_file = paste0("husits_cv_overtime", suffix, ".png"),
      fig_path = fig_path
    )

    for (grp in .group_specs) {
      plot_husits_cv_overtime(
        df = df,
        by = grp$by,
        out_file = paste0("husits_cv_overtime_", grp$stem, suffix, ".png"),
        fig_path = fig_path
      )
    }
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
  for (grp in .group_specs) {
    
    # April 2020: binary self-reported keyworker status
    plot_keyworker_definition_compare(
      df = df,
      wave_code = "ca",
      by = grp$by,
      detailed_fill = FALSE,
      out_file = paste0("keyworker_def_explore_", grp$stem, "_april20", suffix, ".png"),
      fig_path = fig_path
    )
    
    # May 2020: binary self-reported keyworker status
    plot_keyworker_definition_compare(
      df = df,
      wave_code = "cb",
      by = grp$by,
      detailed_fill = FALSE,
      out_file = paste0("keyworker_def_explore_", grp$stem, "_may20", suffix, ".png"),
      fig_path = fig_path
    )
  }
  
  # May 2020: detailed self-reported keywork sector vs detailed groups only
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
}

cat("\nDescriptives complete (full + couples).\n")
cat("Figures saved to: ", fig_path, "\n")
