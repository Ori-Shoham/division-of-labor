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

tab_base <- df_base %>%
  prep_sample_table_vars()

tab_covid <- df_covid_couple_long %>%
  collapse_to_unique_couples() %>%
  prep_sample_table_vars()

tab_future <- df_future_couple_long %>%
  collapse_to_unique_couples() %>%
  prep_sample_table_vars()

# =============================================================================
# SECTION A: Frequency tables
# =============================================================================

# -----------------------------------------------------------------------------
# A1. Wife 3-group x husband 3-group frequency tables
# -----------------------------------------------------------------------------

t_3x3_base <- make_crosstab(tab_base, wife_group_3, husband_group_3)
t_3x3_covid <- make_crosstab(tab_covid, wife_group_3, husband_group_3)
t_3x3_future <- make_crosstab(tab_future, wife_group_3, husband_group_3)

colnames(t_3x3_base)[1] <- "Wife group / Husband group"
colnames(t_3x3_covid)[1] <- "Wife group / Husband group"
colnames(t_3x3_future)[1] <- "Wife group / Husband group"

# Save single-panel tables
write_latex_table(
  t_3x3_base,
  file = file.path(tab_path, "sample_table_3x3_baseline.tex"),
  title = "Joint distribution of spouses by industry group"
)

write_latex_table(
  t_3x3_covid,
  file = file.path(tab_path, "sample_table_3x3_covid.tex"),
  title = "Joint distribution of spouses by industry group"
)

write_latex_table(
  t_3x3_future,
  file = file.path(tab_path, "sample_table_3x3_future.tex"),
  title = "Joint distribution of spouses by industry group"
)

# Save combined three-panel table
write_three_panel_table(
  df_a = t_3x3_base,
  df_b = t_3x3_covid,
  df_c = t_3x3_future,
  panel_titles = c(
    "Panel A. Baseline couples",
    "Panel B. COVID sample",
    "Panel C. Future sample"
  ),
  title = "Joint distribution of spouses by industry group",
  file = file.path(tab_path, "sample_table_3x3_all.tex")
)

# -----------------------------------------------------------------------------
# A2. Wife 5-group x husband 5-group frequency tables
# -----------------------------------------------------------------------------

t_5x5_base <- make_crosstab(tab_base, wife_group_5, husband_group_5)
t_5x5_covid <- make_crosstab(tab_covid, wife_group_5, husband_group_5)
t_5x5_future <- make_crosstab(tab_future, wife_group_5, husband_group_5)

colnames(t_5x5_base)[1] <- "Wife group / Husband group"
colnames(t_5x5_covid)[1] <- "Wife group / Husband group"
colnames(t_5x5_future)[1] <- "Wife group / Husband group"

# Save single-panel tables
write_latex_table(
  t_5x5_base,
  file = file.path(tab_path, "sample_table_5x5_baseline.tex"),
  title = "Joint distribution of spouses by detailed industry group"
)

write_latex_table(
  t_5x5_covid,
  file = file.path(tab_path, "sample_table_5x5_covid.tex"),
  title = "Joint distribution of spouses by detailed industry group"
)

write_latex_table(
  t_5x5_future,
  file = file.path(tab_path, "sample_table_5x5_future.tex"),
  title = "Joint distribution of spouses by detailed industry group"
)

# Save combined three-panel table
write_three_panel_table(
  df_a = t_5x5_base,
  df_b = t_5x5_covid,
  df_c = t_5x5_future,
  panel_titles = c(
    "Panel A. Baseline couples",
    "Panel B. COVID sample",
    "Panel C. Future sample"
  ),
  title = "Joint distribution of spouses by detailed industry group",
  file = file.path(tab_path, "sample_table_5x5_all.tex")
)

# -----------------------------------------------------------------------------
# A3. Youngest child age in 2019
#
# For child-age tables, use columns rather than separate panels so the three
# samples can be compared side by side more easily.
#
# Two versions are created:
#   (1) Exact ages, with 18+ pooled
#   (2) Coarser bins: None, 0-6, 7-10, 11-15, 16-17, 18+
# -----------------------------------------------------------------------------

# ---- Exact-age version -------------------------------------------------------

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
  file = file.path(tab_path, "sample_table_youngest_child_exact_all.tex"),
  title = "Distribution of youngest child age in 2019"
)

# ---- Binned version ----------------------------------------------------------

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
  file = file.path(tab_path, "sample_table_youngest_child_binned_all.tex"),
  title = "Distribution of youngest child age in 2019"
)

# =============================================================================
# SECTION B: Couple workoutside composition over time
#
# This is the graphical counterpart to a sequence of 2x2 husband-wife
# workoutside tables by period. Rather than saving many separate tables, we plot
# the composition over time.
# =============================================================================

# -----------------------------------------------------------------------------
# Restrict to intended study periods
#
# COVID:
#   Only the actual COVID study waves ca-ci
#   (exclude synthetic pre-period rows such as "2019" or "baseline" if present)
#
# Future:
#   Only the configured future study waves
# -----------------------------------------------------------------------------

df_covid_plot <- df_covid_couple_long %>%
  dplyr::filter(wave %in% covid_waves)

df_future_plot <- df_future_couple_long %>%
  dplyr::filter(wave %in% future_waves)

# -----------------------------------------------------------------------------
# B1. COVID study waves
# -----------------------------------------------------------------------------

p_covid_share <- plot_workoutside_composition(
  df         = df_covid_plot,
  time_var   = wave,
  time_scale = "covid_wave",
  use_shares = TRUE,
  title      = "Couple workoutside composition across COVID study waves"
)

ggplot2::ggsave(
  filename = file.path(fig_path, "couple_workoutside_covid_wave_share.png"),
  plot = p_covid_share,
  width = 10,
  height = 6
)

p_covid_N <- plot_workoutside_composition(
  df         = df_covid_plot,
  time_var   = wave,
  time_scale = "covid_wave",
  use_shares = FALSE,
  title      = "Number of couples by workoutside composition across COVID study waves"
)

ggplot2::ggsave(
  filename = file.path(fig_path, "couple_workoutside_covid_wave_N.png"),
  plot = p_covid_N,
  width = 10,
  height = 6
)

# -----------------------------------------------------------------------------
# B2. Future study waves
# -----------------------------------------------------------------------------

p_future_wave_share <- plot_workoutside_composition(
  df         = df_future_plot,
  time_var   = wave,
  time_scale = "future_wave",
  use_shares = TRUE,
  title      = "Couple workoutside composition across future study waves"
)

ggplot2::ggsave(
  filename = file.path(fig_path, "couple_workoutside_future_wave_share.png"),
  plot = p_future_wave_share,
  width = 10,
  height = 6
)

p_future_wave_N <- plot_workoutside_composition(
  df         = df_future_plot,
  time_var   = wave,
  time_scale = "future_wave",
  use_shares = FALSE,
  title      = "Number of couples by workoutside composition across future study waves"
)

ggplot2::ggsave(
  filename = file.path(fig_path, "couple_workoutside_future_wave_N.png"),
  plot = p_future_wave_N,
  width = 10,
  height = 6
)

# -----------------------------------------------------------------------------
# B3. Future study years
# -----------------------------------------------------------------------------

p_future_year_share <- plot_workoutside_composition(
  df         = df_future_plot,
  time_var   = year,
  time_scale = "year",
  use_shares = TRUE,
  title      = "Couple workoutside composition across future study years"
)

ggplot2::ggsave(
  filename = file.path(fig_path, "couple_workoutside_future_year_share.png"),
  plot = p_future_year_share,
  width = 10,
  height = 6
)

p_future_year_N <- plot_workoutside_composition(
  df         = df_future_plot,
  time_var   = year,
  time_scale = "year",
  use_shares = FALSE,
  title      = "Number of couples by workoutside composition across future study years"
)

ggplot2::ggsave(
  filename = file.path(fig_path, "couple_workoutside_future_year_N.png"),
  plot = p_future_year_N,
  width = 10,
  height = 6
)

# =============================================================================
# Console output
# =============================================================================

cat("\nSaved LaTeX sample tables to ", tab_path, ":\n", sep = "")
cat(" - sample_table_3x3_baseline.tex\n")
cat(" - sample_table_3x3_covid.tex\n")
cat(" - sample_table_3x3_future.tex\n")
cat(" - sample_table_3x3_all.tex\n")
cat(" - sample_table_5x5_baseline.tex\n")
cat(" - sample_table_5x5_covid.tex\n")
cat(" - sample_table_5x5_future.tex\n")
cat(" - sample_table_5x5_all.tex\n")
cat(" - sample_table_youngest_child_exact_all.tex\n")
cat(" - sample_table_youngest_child_binned_all.tex\n")

cat("\nSaved couple workoutside composition figures to ", fig_path, ":\n", sep = "")
cat(" - couple_workoutside_covid_wave_share.png\n")
cat(" - couple_workoutside_covid_wave_N.png\n")
cat(" - couple_workoutside_future_wave_share.png\n")
cat(" - couple_workoutside_future_wave_N.png\n")
cat(" - couple_workoutside_future_year_share.png\n")
cat(" - couple_workoutside_future_year_N.png\n")

# =============================================================================
# SECTION C: Couple WFH-some composition over time
#
# This is the graphical counterpart to a sequence of 2x2 husband-wife
# WFH-some tables by period. Rather than saving many separate tables, we plot
# the composition over time.
# =============================================================================

# -----------------------------------------------------------------------------
# C1. COVID study waves
# -----------------------------------------------------------------------------

p_covid_wfh_some_share <- plot_wfh_some_composition(
  df         = df_covid_plot,
  time_var   = wave,
  time_scale = "covid_wave",
  use_shares = TRUE,
  title      = "Couple work-from-home-some composition across COVID study waves"
)

ggplot2::ggsave(
  filename = file.path(fig_path, "couple_wfh_some_covid_wave_share.png"),
  plot = p_covid_wfh_some_share,
  width = 11,
  height = 7
)

p_covid_wfh_some_N <- plot_wfh_some_composition(
  df         = df_covid_plot,
  time_var   = wave,
  time_scale = "covid_wave",
  use_shares = FALSE,
  title      = "Number of couples by work-from-home-some composition across COVID study waves"
)

ggplot2::ggsave(
  filename = file.path(fig_path, "couple_wfh_some_covid_wave_N.png"),
  plot = p_covid_wfh_some_N,
  width = 11,
  height = 7
)

# -----------------------------------------------------------------------------
# C2. Future study waves
# -----------------------------------------------------------------------------

p_future_wfh_some_wave_share <- plot_wfh_some_composition(
  df         = df_future_plot,
  time_var   = wave,
  time_scale = "future_wave",
  use_shares = TRUE,
  title      = "Couple work-from-home-some composition across future study waves"
)

ggplot2::ggsave(
  filename = file.path(fig_path, "couple_wfh_some_future_wave_share.png"),
  plot = p_future_wfh_some_wave_share,
  width = 11,
  height = 7
)

p_future_wfh_some_wave_N <- plot_wfh_some_composition(
  df         = df_future_plot,
  time_var   = wave,
  time_scale = "future_wave",
  use_shares = FALSE,
  title      = "Number of couples by work-from-home-some composition across future study waves"
)

ggplot2::ggsave(
  filename = file.path(fig_path, "couple_wfh_some_future_wave_N.png"),
  plot = p_future_wfh_some_wave_N,
  width = 11,
  height = 7
)

# -----------------------------------------------------------------------------
# C3. Future study years
# -----------------------------------------------------------------------------

p_future_wfh_some_year_share <- plot_wfh_some_composition(
  df         = df_future_plot,
  time_var   = year,
  time_scale = "year",
  use_shares = TRUE,
  title      = "Couple work-from-home-some composition across future study years"
)

ggplot2::ggsave(
  filename = file.path(fig_path, "couple_wfh_some_future_year_share.png"),
  plot = p_future_wfh_some_year_share,
  width = 11,
  height = 7
)

p_future_wfh_some_year_N <- plot_wfh_some_composition(
  df         = df_future_plot,
  time_var   = year,
  time_scale = "year",
  use_shares = FALSE,
  title      = "Number of couples by work-from-home-some composition across future study years"
)

ggplot2::ggsave(
  filename = file.path(fig_path, "couple_wfh_some_future_year_N.png"),
  plot = p_future_wfh_some_year_N,
  width = 11,
  height = 7
)

cat("
Saved couple WFH-some composition figures to ", fig_path, ":
", sep = "")
cat(" - couple_wfh_some_covid_wave_share.png
")
cat(" - couple_wfh_some_covid_wave_N.png
")
cat(" - couple_wfh_some_future_wave_share.png
")
cat(" - couple_wfh_some_future_wave_N.png
")
cat(" - couple_wfh_some_future_year_share.png
")
cat(" - couple_wfh_some_future_year_N.png
")
