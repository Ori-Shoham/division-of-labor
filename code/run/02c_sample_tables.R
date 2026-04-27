# =============================================================================
# Script: code/run/02c_sample_tables.R
#
# Purpose:
#   Create sample-composition outputs for the updated couple samples:
#     (1) Baseline couple sample
#     (2) Couples observed in the strict COVID couple-wave panel
#     (3) Couples observed in the future-outcomes couple-wave panel
#
# Outputs created:
#   A. LaTeX tables
#      - Wife 3-group x husband 3-group frequency tables
#      - Wife detailed 5-group x husband detailed 5-group frequency tables
#      - Frequency table for age of youngest child in 2019
#
#   B. Figures
#      - Distribution of joint husband-wife workoutside status over time
#      - Distribution of joint husband-wife WFH-some status over time
#      - For COVID study: by COVID-study wave
#      - For future study: by wave and by year
#
# Notes:
#   - Assumes the build pipeline has already been corrected so that:
#       * s2019_baseline_couplelevel.rds is a rich baseline couple dataset
#       * future_outcomes_couple_long_lmo.rds exists
#       * both long couple datasets carry baseline group variables through
#   - Long datasets are collapsed to one row per couple for the frequency tables
#   - The workoutside and WFH-some figures keep the full couple-wave structure
#   - Wave labels are drawn from code/lib/wave_labels.R
#   - This version produces outputs for all couples and for the subsample with
#     a youngest child age <= 10 in 2019
# =============================================================================

suppressPackageStartupMessages({
  library(tidyverse)
})

rm(list = ls())

# ---- Source libs -------------------------------------------------------------
source("code/lib/config.R")
source("code/lib/utils.R")
source("code/lib/wave_labels.R")
source("code/lib/sample_tables.R")

# ---- Ensure output folders exist ---------------------------------------------
dir.create(tab_path, showWarnings = FALSE, recursive = TRUE)
dir.create(fig_path, showWarnings = FALSE, recursive = TRUE)

# =============================================================================
# Load updated couple-level datasets
# =============================================================================

df_base <- readRDS(
  file.path(samples_path, "s2019_baseline_couplelevel.rds")
)

df_covid_couple_long <- readRDS(
  file.path(der_path, "df_sample_long_covid_couplelevel.rds")
)

df_future_couple_long <- readRDS(
  file.path(der_path, "future_outcomes_couple_long_lmo.rds")
)

# =============================================================================
# Prepare one-row-per-couple versions for tabulation
# =============================================================================

tab_base_all <- df_base %>%
  prep_sample_table_vars()

tab_covid_all <- df_covid_couple_long %>%
  collapse_to_unique_couples() %>%
  prep_sample_table_vars()

tab_future_all <- df_future_couple_long %>%
  collapse_to_unique_couples() %>%
  prep_sample_table_vars()

# =============================================================================
# Treatment x child-age comparison tables: baseline demographics and history
# =============================================================================

TREATMENT_BALANCE_VARS <- c(
  "treat_wife_key_notedu_husb_not_or_edu",
  "treat_wife_key_notedu_any",
  "treat_husb_shutdown_wife_not"
)

for (tr in TREATMENT_BALANCE_VARS) {
  if (!tr %in% names(tab_base_all)) next

  t_balance <- make_treatment_child_balance_table(
    df = tab_base_all,
    treatment_var = tr,
    include_sd_rows = TRUE
  )

  readr::write_csv(
    t_balance,
    file.path(tab_path, paste0("sample_table_treatment_child_balance_", tr, ".csv"))
  )

  write_treatment_child_balance_table(
    df = t_balance,
    file = file.path(tab_path, paste0("sample_table_treatment_child_balance_", tr, ".tex")),
    title = paste0(
      "Baseline and history characteristics by treatment and child-age group: ",
      tr
    )
  )
}

subset_specs <- list(
  list(
    suffix = "",
    label = "all couples",
    panel_titles = c(
      "Panel A. Baseline couples",
      "Panel B. COVID sample",
      "Panel C. Future sample"
    ),
    tab_base = tab_base_all,
    tab_covid = tab_covid_all,
    tab_future = tab_future_all,
    covid_plot = df_covid_couple_long,
    future_plot = df_future_couple_long
  ),
  list(
    suffix = "_child_u10",
    label = "couples with youngest child age 10 or under in 2019",
    panel_titles = c(
      "Panel A. Baseline couples: youngest child $\\leq$ 10",
      "Panel B. COVID sample: youngest child $\\leq$ 10",
      "Panel C. Future sample: youngest child $\\leq$ 10"
    ),
    tab_base = tab_base_all %>% subset_couples_with_young_child_2019(max_age = 10),
    tab_covid = tab_covid_all %>% subset_couples_with_young_child_2019(max_age = 10),
    tab_future = tab_future_all %>% subset_couples_with_young_child_2019(max_age = 10),
    covid_plot = df_covid_couple_long %>%
      prep_sample_table_vars() %>%
      subset_couples_with_young_child_2019(max_age = 10),
    future_plot = df_future_couple_long %>%
      prep_sample_table_vars() %>%
      subset_couples_with_young_child_2019(max_age = 10)
  )
)

for (spec in subset_specs) {

  tab_base <- spec$tab_base
  tab_covid <- spec$tab_covid
  tab_future <- spec$tab_future
  suffix <- spec$suffix

  df_covid_plot <- spec$covid_plot %>%
    dplyr::filter(wave %in% covid_waves)

  df_future_plot <- spec$future_plot %>%
    dplyr::filter(wave %in% future_waves)

  t_3x3_base <- make_crosstab(tab_base, wife_group_3, husband_group_3)
  t_3x3_covid <- make_crosstab(tab_covid, wife_group_3, husband_group_3)
  t_3x3_future <- make_crosstab(tab_future, wife_group_3, husband_group_3)

  colnames(t_3x3_base)[1] <- "Wife group / Husband group"
  colnames(t_3x3_covid)[1] <- "Wife group / Husband group"
  colnames(t_3x3_future)[1] <- "Wife group / Husband group"

  write_latex_table(
    t_3x3_base,
    file = file.path(tab_path, paste0("sample_table_3x3_baseline", suffix, ".tex")),
    title = "Joint distribution of spouses by industry group"
  )
  write_latex_table(
    t_3x3_covid,
    file = file.path(tab_path, paste0("sample_table_3x3_covid", suffix, ".tex")),
    title = "Joint distribution of spouses by industry group"
  )
  write_latex_table(
    t_3x3_future,
    file = file.path(tab_path, paste0("sample_table_3x3_future", suffix, ".tex")),
    title = "Joint distribution of spouses by industry group"
  )
  write_three_panel_table(
    df_a = t_3x3_base,
    df_b = t_3x3_covid,
    df_c = t_3x3_future,
    panel_titles = spec$panel_titles,
    file = file.path(tab_path, paste0("sample_table_3x3_all", suffix, ".tex"))
  )

  t_5x5_base <- make_crosstab(tab_base, wife_group_5, husband_group_5)
  t_5x5_covid <- make_crosstab(tab_covid, wife_group_5, husband_group_5)
  t_5x5_future <- make_crosstab(tab_future, wife_group_5, husband_group_5)

  colnames(t_5x5_base)[1] <- "Wife group / Husband group"
  colnames(t_5x5_covid)[1] <- "Wife group / Husband group"
  colnames(t_5x5_future)[1] <- "Wife group / Husband group"

  write_latex_table(
    t_5x5_base,
    file = file.path(tab_path, paste0("sample_table_5x5_baseline", suffix, ".tex")),
    title = "Joint distribution of spouses by detailed industry group"
  )
  write_latex_table(
    t_5x5_covid,
    file = file.path(tab_path, paste0("sample_table_5x5_covid", suffix, ".tex")),
    title = "Joint distribution of spouses by detailed industry group"
  )
  write_latex_table(
    t_5x5_future,
    file = file.path(tab_path, paste0("sample_table_5x5_future", suffix, ".tex")),
    title = "Joint distribution of spouses by detailed industry group"
  )
  write_three_panel_table(
    df_a = t_5x5_base,
    df_b = t_5x5_covid,
    df_c = t_5x5_future,
    panel_titles = spec$panel_titles,
    file = file.path(tab_path, paste0("sample_table_5x5_all", suffix, ".tex"))
  )

  t_child_exact_base <- make_child_age_table_exact(tab_base)
  t_child_exact_covid <- make_child_age_table_exact(tab_covid)
  t_child_exact_future <- make_child_age_table_exact(tab_future)

  t_child_exact_all <- combine_three_count_tables(
    df_a = t_child_exact_base,
    df_b = t_child_exact_covid,
    df_c = t_child_exact_future,
    sample_names = c("Baseline", "COVID", "Future")
  )

  write_latex_table(
    t_child_exact_all,
    file = file.path(tab_path, paste0("sample_table_youngest_child_exact_all", suffix, ".tex")),
    title = "Distribution of youngest child age in 2019"
  )

  t_child_binned_base <- make_child_age_table_binned(tab_base)
  t_child_binned_covid <- make_child_age_table_binned(tab_covid)
  t_child_binned_future <- make_child_age_table_binned(tab_future)

  t_child_binned_all <- combine_three_count_tables(
    df_a = t_child_binned_base,
    df_b = t_child_binned_covid,
    df_c = t_child_binned_future,
    sample_names = c("Baseline", "COVID", "Future")
  )

  write_latex_table(
    t_child_binned_all,
    file = file.path(tab_path, paste0("sample_table_youngest_child_binned_all", suffix, ".tex")),
    title = "Distribution of youngest child age in 2019"
  )

  p_covid_share <- plot_workoutside_composition(
    df         = df_covid_plot,
    time_var   = wave,
    time_scale = "covid_wave",
    use_shares = TRUE,
    title      = paste(
      "Couple workoutside composition across COVID study waves:",
      spec$label
    )
  )

  ggplot2::ggsave(
    filename = file.path(fig_path, paste0("couple_workoutside_covid_wave_share", suffix, ".png")),
    plot = p_covid_share,
    width = 10,
    height = 6
  )

  p_covid_N <- plot_workoutside_composition(
    df         = df_covid_plot,
    time_var   = wave,
    time_scale = "covid_wave",
    use_shares = FALSE,
    title      = paste(
      "Number of couples by workoutside composition across COVID study waves:",
      spec$label
    )
  )

  ggplot2::ggsave(
    filename = file.path(fig_path, paste0("couple_workoutside_covid_wave_N", suffix, ".png")),
    plot = p_covid_N,
    width = 10,
    height = 6
  )

  p_future_wave_share <- plot_workoutside_composition(
    df         = df_future_plot,
    time_var   = wave,
    time_scale = "future_wave",
    use_shares = TRUE,
    title      = paste(
      "Couple workoutside composition across future study waves:",
      spec$label
    )
  )

  ggplot2::ggsave(
    filename = file.path(fig_path, paste0("couple_workoutside_future_wave_share", suffix, ".png")),
    plot = p_future_wave_share,
    width = 10,
    height = 6
  )

  p_future_wave_N <- plot_workoutside_composition(
    df         = df_future_plot,
    time_var   = wave,
    time_scale = "future_wave",
    use_shares = FALSE,
    title      = paste(
      "Number of couples by workoutside composition across future study waves:",
      spec$label
    )
  )

  ggplot2::ggsave(
    filename = file.path(fig_path, paste0("couple_workoutside_future_wave_N", suffix, ".png")),
    plot = p_future_wave_N,
    width = 10,
    height = 6
  )

  p_future_year_share <- plot_workoutside_composition(
    df         = df_future_plot,
    time_var   = year,
    time_scale = "year",
    use_shares = TRUE,
    title      = paste(
      "Couple workoutside composition across future study years:",
      spec$label
    )
  )

  ggplot2::ggsave(
    filename = file.path(fig_path, paste0("couple_workoutside_future_year_share", suffix, ".png")),
    plot = p_future_year_share,
    width = 10,
    height = 6
  )

  p_future_year_N <- plot_workoutside_composition(
    df         = df_future_plot,
    time_var   = year,
    time_scale = "year",
    use_shares = FALSE,
    title      = paste(
      "Number of couples by workoutside composition across future study years:",
      spec$label
    )
  )

  ggplot2::ggsave(
    filename = file.path(fig_path, paste0("couple_workoutside_future_year_N", suffix, ".png")),
    plot = p_future_year_N,
    width = 10,
    height = 6
  )

  p_covid_wfh_some_share <- plot_wfh_some_composition(
    df         = df_covid_plot,
    time_var   = wave,
    time_scale = "covid_wave",
    use_shares = TRUE,
    title      = paste(
      "Couple work-from-home-some composition across COVID study waves:",
      spec$label
    )
  )

  ggplot2::ggsave(
    filename = file.path(fig_path, paste0("couple_wfh_some_covid_wave_share", suffix, ".png")),
    plot = p_covid_wfh_some_share,
    width = 11,
    height = 7
  )

  p_covid_wfh_some_N <- plot_wfh_some_composition(
    df         = df_covid_plot,
    time_var   = wave,
    time_scale = "covid_wave",
    use_shares = FALSE,
    title      = paste(
      "Number of couples by work-from-home-some composition across COVID study waves:",
      spec$label
    )
  )

  ggplot2::ggsave(
    filename = file.path(fig_path, paste0("couple_wfh_some_covid_wave_N", suffix, ".png")),
    plot = p_covid_wfh_some_N,
    width = 11,
    height = 7
  )

  p_future_wfh_some_wave_share <- plot_wfh_some_composition(
    df         = df_future_plot,
    time_var   = wave,
    time_scale = "future_wave",
    use_shares = TRUE,
    title      = paste(
      "Couple work-from-home-some composition across future study waves:",
      spec$label
    )
  )

  ggplot2::ggsave(
    filename = file.path(fig_path, paste0("couple_wfh_some_future_wave_share", suffix, ".png")),
    plot = p_future_wfh_some_wave_share,
    width = 11,
    height = 7
  )

  p_future_wfh_some_wave_N <- plot_wfh_some_composition(
    df         = df_future_plot,
    time_var   = wave,
    time_scale = "future_wave",
    use_shares = FALSE,
    title      = paste(
      "Number of couples by work-from-home-some composition across future study waves:",
      spec$label
    )
  )

  ggplot2::ggsave(
    filename = file.path(fig_path, paste0("couple_wfh_some_future_wave_N", suffix, ".png")),
    plot = p_future_wfh_some_wave_N,
    width = 11,
    height = 7
  )

  p_future_wfh_some_year_share <- plot_wfh_some_composition(
    df         = df_future_plot,
    time_var   = year,
    time_scale = "year",
    use_shares = TRUE,
    title      = paste(
      "Couple work-from-home-some composition across future study years:",
      spec$label
    )
  )

  ggplot2::ggsave(
    filename = file.path(fig_path, paste0("couple_wfh_some_future_year_share", suffix, ".png")),
    plot = p_future_wfh_some_year_share,
    width = 11,
    height = 7
  )

  p_future_wfh_some_year_N <- plot_wfh_some_composition(
    df         = df_future_plot,
    time_var   = year,
    time_scale = "year",
    use_shares = FALSE,
    title      = paste(
      "Number of couples by work-from-home-some composition across future study years:",
      spec$label
    )
  )

  ggplot2::ggsave(
    filename = file.path(fig_path, paste0("couple_wfh_some_future_year_N", suffix, ".png")),
    plot = p_future_wfh_some_year_N,
    width = 11,
    height = 7
  )
}

cat("\nSaved updated sample-table outputs to ", tab_path, " and ", fig_path, ".\n", sep = "")
