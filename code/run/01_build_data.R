# =============================================================================
# Script: code/run/01_build_data.R
#
# Purpose:
#   Build derived datasets and analytic samples.
#
# IMPORTANT:
#   All derived outputs are saved OUTSIDE the repo under data_out_root:
#     der_path     = <data_out_root>/derived
#     samples_path = <data_out_root>/samples
#
# This version:
#   - uses policies_keyworkers.R for SOC/SIC + keyworker crosswalks
#   - keeps the existing person-level outputs used by later scripts
#   - builds richer couple-level outputs:
#       * baseline_couple_roster.rds
#       * s2019_baseline_couplelevel.rds
#       * df_sample_long_covid_couplelevel.rds
#       * future_outcomes_couple_long_lmo.rds
#       * future_outcomes_couple_wide_lmo.rds
#       * s2019_baseline_couplelevel_plus_lmo.rds
#   - adds baseline couple treatment flags and child-age subgroup flags
# =============================================================================

suppressPackageStartupMessages({
  library(tidyverse)
  library(haven)
  library(readxl)
})

rm(list = ls())

# ---- Source libs --------------------------------------------------------------
source("code/lib/config.R")
source("code/lib/utils.R")
source("code/lib/harmonize_outcomes.R")
source("code/lib/work_groups.R")
source("code/lib/policies_keyworkers.R")

source("code/lib/family_baseline.R")
source("code/lib/covid_loader.R")
source("code/lib/covid_panel.R")
source("code/lib/future_outcomes.R")
source("code/lib/samples.R")

# ---- Ensure output folders exist (outside repo) -------------------------------
dir.create(data_out_root, showWarnings = FALSE, recursive = TRUE)
dir.create(der_path,      showWarnings = FALSE, recursive = TRUE)
dir.create(samples_path,  showWarnings = FALSE, recursive = TRUE)
dir.create(cache_path,    showWarnings = FALSE, recursive = TRUE)

# =============================================================================
# Step 0: Load policy files and keyworker crosswalk
# =============================================================================
cat("\n--- Step 0: Load policies and keyworker crosswalk ---\n")

# Load SOC and SIC lookup files
pols <- load_policies(pol_path)
SOC  <- pols$SOC
SIC  <- pols$SIC

# Build condensed SOC x SIC keyworker crosswalk
# Requires KEYWORKER_XLSX to be defined in config.R
key_inds <- build_keyworker_crosswalk(
  xlsx_path = KEYWORKER_XLSX,
  sheet     = 4,
  SOC       = SOC,
  SIC       = SIC
)

cat("Policies loaded.\n")
cat("SOC rows: ", nrow(SOC), "\n", sep = "")
cat("SIC rows: ", nrow(SIC), "\n", sep = "")
cat("Keyworker crosswalk rows: ", nrow(key_inds), "\n", sep = "")

# =============================================================================
# Step 1: Baseline J/K/I composite
# =============================================================================
cat("\n--- Step 1: Build baseline (I/J/K composite) ---\n")

df_baseline <- build_baseline(path_main) %>%
  add_baseline_work_groups()

saveRDS(df_baseline, file.path(der_path, "baseline.rds"))

cat("Baseline saved to: ", file.path(der_path, "baseline.rds"), "\n", sep = "")

# Analytic baseline restriction used throughout downstream samples:
#   - employed / self-employed at baseline
#   - observed baseline SIC and SOC
#   - negative SIC/SOC values treated as missing
df_baseline_analytic <- df_baseline %>%
  dplyr::filter(
    base_jbstat %in% 1:2
  )

cat("Analytic baseline rows (valid baseline worker + SIC/SOC): ",
    nrow(df_baseline_analytic), "\n", sep = "")

# =============================================================================
# Step 2: COVID wide (ca–ci)
# =============================================================================
cat("\n--- Step 2: Merge COVID waves wide (ca–ci) ---\n")

df_covid_wide <- merge_covid_waves_wide(path_covid, covid_waves)

saveRDS(df_covid_wide, file.path(der_path, "covid_all_wide.rds"))

cat("COVID wide saved to: ", file.path(der_path, "covid_all_wide.rds"), "\n", sep = "")

# =============================================================================
# Step 3: COVID long panel
# =============================================================================
cat("\n--- Step 3: Build COVID long panel ---\n")

df_sample_long_covid <- build_covid_long_panel(
  df_baseline   = df_baseline_analytic,
  df_covid_wide = df_covid_wide,
  SOC           = SOC,
  SIC           = SIC,
  key_inds      = key_inds
)

# Save full person-level COVID panel
saveRDS(df_sample_long_covid, file.path(der_path, "df_sample_long_covid.rds"))

cat("COVID long saved to: ",
    file.path(der_path, "df_sample_long_covid.rds"), "\n", sep = "")

# Backward-compatible person-level couples subset
df_sample_long_covid_couples <- df_sample_long_covid %>%
  dplyr::filter(!is.na(base_partner_pidp))

saveRDS(
  df_sample_long_covid_couples,
  file.path(der_path, "df_sample_long_covid_couples.rds")
)

cat("COVID long (person-level baseline couples subset) saved to: ",
    file.path(der_path, "df_sample_long_covid_couples.rds"), "\n", sep = "")

# PIDP list for defining COVID-observed samples
pidp_in_covid <- df_sample_long_covid %>%
  dplyr::distinct(pidp) %>%
  dplyr::pull(pidp)

# =============================================================================
# Step 4: Person-level analytic samples
# =============================================================================
cat("\n--- Step 4: Build baseline-defined person-level analytic samples ---\n")

samples <- build_samples_2019(
  df_baseline_analytic,
  pidp_in_covid = pidp_in_covid
)

saveRDS(samples$s2019_all,           file.path(samples_path, "s2019_all.rds"))
saveRDS(samples$s2019_couples,       file.path(samples_path, "s2019_couples.rds"))
saveRDS(samples$s2019_covid,         file.path(samples_path, "s2019_covid.rds"))
saveRDS(samples$s2019_covid_couples, file.path(samples_path, "s2019_covid_couples.rds"))

cat("Person-level samples saved to: ", samples_path, "\n", sep = "")

# =============================================================================
# Step 4b: Couple roster and couple-level COVID sample
# =============================================================================
cat("\n--- Step 4b: Build couple roster and couple-level samples ---\n")

# Build one-row-per-couple roster using baseline partner links only
# Restrict to heterosexual baseline couples (one male, one female)
couple_roster <- build_baseline_couple_roster(
  df_baseline   = df_baseline_analytic,
  pidp_in_covid = pidp_in_covid
)

# Structural baseline roster
saveRDS(
  couple_roster,
  file.path(samples_path, "baseline_couple_roster.rds")
)

cat("Baseline couple roster saved to: ",
    file.path(samples_path, "baseline_couple_roster.rds"), "\n", sep = "")

s2019_baseline_couplelevel <- build_baseline_couple_dataset(
  df_baseline = df_baseline_analytic,
  roster      = couple_roster
) %>%
  add_couple_baseline_treatments()

saveRDS(
  s2019_baseline_couplelevel,
  file.path(samples_path, "s2019_baseline_couplelevel.rds")
)

cat("Baseline couple-level sample saved to: ",
    file.path(samples_path, "s2019_baseline_couplelevel.rds"), "\n", sep = "")

# Optional restricted roster: couples where both spouses appear in COVID
couple_roster_covid_both <- couple_roster %>%
  dplyr::filter(both_in_covid)

saveRDS(
  couple_roster_covid_both,
  file.path(samples_path, "baseline_couple_roster_both_in_covid.rds")
)

cat("COVID-observed couple roster saved to: ",
    file.path(samples_path, "baseline_couple_roster_both_in_covid.rds"), "\n", sep = "")

# Build strict couple-wave COVID panel:
# one row per couple x wave, keeping only waves where both spouses are observed
df_covid_couple_long <- build_covid_couple_long(
  df_covid_long = df_sample_long_covid,
  roster        = couple_roster
) %>%
  dplyr::left_join(
    s2019_baseline_couplelevel %>%
      dplyr::select(
        couple_id,
        youngest_child_2019,
        has_child_u10_2019,
        has_child_11_17_2019,
        child_age_group_2019,
        treat_wife_key_notedu_husb_not_or_edu,
        treat_wife_key_notedu_any,
        sample_husb_notkey_or_edu,
        treat_husb_shutdown_wife_not
      ),
    by = "couple_id"
  )

saveRDS(
  df_covid_couple_long,
  file.path(der_path, "df_sample_long_covid_couplelevel.rds")
)

cat("COVID couple-level panel saved to: ",
    file.path(der_path, "df_sample_long_covid_couplelevel.rds"), "\n", sep = "")

# =============================================================================
# Step 5: Future outcomes L–O
# =============================================================================
cat("\n--- Step 5: Build future outcomes (L/M/N or L–O as configured) ---\n")

df_future_long <- build_future_outcomes_long(
  path_main    = path_main,
  future_waves = future_waves,
  min_ym = future_outcomes_start
)

# Add indicators comparing later partner status to baseline partner
df_future_long <- add_baseline_couple_evolution(
  df_future_long,
  df_baseline
)

# Restrict future outcomes to the analytic baseline sample before attaching
# baseline-defined groups and covariates.
df_future_long <- df_future_long %>%
  dplyr::semi_join(
    df_baseline_analytic %>% dplyr::select(pidp),
    by = "pidp"
  )

# Add baseline variables before constructing baseline-based groups
df_future_long <- df_future_long %>%
  dplyr::left_join(
    df_baseline_analytic %>%
      dplyr::select(
        pidp,
        starts_with("base_"),
        group_industry_based,
        group_industry_based_detailed
      ),
    by = "pidp"
  )

# Create harmonized WFH and workoutside variables
tmp_wfh <- combine_wfh(df_future_long$jbpl, df_future_long$jbwah)

df_future_long <- df_future_long %>%
  add_baseline_work_groups() %>%
  dplyr::mutate(
    wfh_code = tmp_wfh$wfh_code,
    wfh_cat  = tmp_wfh$wfh_cat,
    wfh_some = make_wfh_some_future(
      jbstat = jbstat,
      jbhrs  = jbhrs,
      jbpl   = jbpl,
      jbwah  = jbwah
    ),
    health_sf = combine_health(sf1, scsf1),
    
    health_sf = factor(
      dplyr::case_when(
        health_sf == 1 ~ "Excellent",
        health_sf == 2 ~ "Very good",
        health_sf == 3 ~ "Good",
        health_sf == 4 ~ "Fair",
        health_sf == 5 ~ "Poor",
        TRUE ~ NA_character_
      ),
      levels = c("Excellent", "Very good", "Good", "Fair", "Poor"),
      ordered = TRUE
    ),
    workoutside = make_workoutside_future(
      jbstat   = jbstat,
      jbhrs    = jbhrs,
      wfh_code = wfh_code
    )
  )

saveRDS(
  df_future_long,
  file.path(der_path, "future_outcomes_long_lmo.rds")
)

cat("Future outcomes long saved to: ",
    file.path(der_path, "future_outcomes_long_lmo.rds"), "\n", sep = "")

# =============================================================================
# Step 5b: Couple-level future outcomes long
# =============================================================================
cat("\n--- Step 5b: Build couple-level future outcomes long ---\n")

df_future_couple_long <- build_future_couple_long(
  df_future_long = df_future_long,
  roster         = couple_roster
) %>%
  dplyr::left_join(
    s2019_baseline_couplelevel %>%
      dplyr::select(
        couple_id,
        youngest_child_2019,
        has_child_u10_2019,
        has_child_11_17_2019,
        child_age_group_2019,
        treat_wife_key_notedu_husb_not_or_edu,
        treat_wife_key_notedu_any,
        sample_husb_notkey_or_edu,
        treat_husb_shutdown_wife_not
      ),
    by = "couple_id"
  )

saveRDS(
  df_future_couple_long,
  file.path(der_path, "future_outcomes_couple_long_lmo.rds")
)

cat("Future couple-level long saved to: ",
    file.path(der_path, "future_outcomes_couple_long_lmo.rds"), "\n", sep = "")

# =============================================================================
# Step 6: Future wide by year-month (ym)
# =============================================================================
cat("\n--- Step 6: Build future outcomes wide by year-month ---\n")

# Make a clean suffix for columns: YYYY_MM
df_future_long <- df_future_long %>%
  dplyr::mutate(
    ym_str = format(ym, "%Y_%m")
  )

# Identify baseline columns
base_cols <- names(df_future_long)[startsWith(names(df_future_long), "base_")]

df_future_wide <- df_future_long %>%
  dplyr::select(-hidp) %>%
  tidyr::pivot_wider(
    id_cols = pidp,
    names_from = ym_str,
    values_from = -c(
      pidp, wave, intdaty_dv, intdatm_dv, ym, year, ym_str,
      dplyr::all_of(base_cols)
    ),
    names_glue = "{.value}_{ym_str}"
  )

# Attach baseline columns once
if (length(base_cols) > 0) {
  df_future_wide <- df_future_wide %>%
    dplyr::left_join(
      df_future_long %>%
        dplyr::select(pidp, dplyr::all_of(base_cols)) %>%
        dplyr::distinct(pidp, .keep_all = TRUE),
      by = "pidp"
    )
}

saveRDS(df_future_wide, file.path(der_path, "future_outcomes_wide_ym.rds"))
saveRDS(df_future_wide, file.path(der_path, "future_outcomes_wide_lmo.rds"))

cat("Future outcomes wide saved to: ",
    file.path(der_path, "future_outcomes_wide_lmo.rds"), "\n", sep = "")

# =============================================================================
# Step 6b: Couple-level future outcomes wide
# =============================================================================
cat("\n--- Step 6b: Build couple-level future outcomes wide ---\n")

df_future_couple_wide <- build_future_couple_wide(
  df_future_wide = df_future_wide,
  roster         = couple_roster
) %>%
  dplyr::left_join(
    s2019_baseline_couplelevel %>%
      dplyr::select(
        couple_id,
        youngest_child_2019,
        has_child_u10_2019,
        has_child_11_17_2019,
        child_age_group_2019,
        treat_wife_key_notedu_husb_not_or_edu,
        treat_wife_key_notedu_any,
        sample_husb_notkey_or_edu,
        treat_husb_shutdown_wife_not
      ),
    by = "couple_id"
  )

saveRDS(
  df_future_couple_wide,
  file.path(der_path, "future_outcomes_couple_wide_lmo.rds")
)

cat("Future couple-level wide saved to: ",
    file.path(der_path, "future_outcomes_couple_wide_lmo.rds"), "\n", sep = "")

# Same baseline couple sample, now with future outcomes attached where available
s2019_baseline_couplelevel_plus_lmo <- df_future_couple_wide

saveRDS(
  s2019_baseline_couplelevel_plus_lmo,
  file.path(samples_path, "s2019_baseline_couplelevel_plus_lmo.rds")
)

cat("Baseline couple-level sample + future wide saved to: ",
    file.path(samples_path, "s2019_baseline_couplelevel_plus_lmo.rds"), "\n", sep = "")

# Optional: future couple-wide subset where both spouses appear in COVID
df_future_couple_wide_covidboth <- df_future_couple_wide %>%
  dplyr::filter(both_in_covid)

saveRDS(
  df_future_couple_wide_covidboth,
  file.path(der_path, "future_outcomes_couple_wide_lmo_both_in_covid.rds")
)

cat("Future couple-level wide (both spouses in COVID) saved to: ",
    file.path(der_path, "future_outcomes_couple_wide_lmo_both_in_covid.rds"),
    "\n", sep = "")

# =============================================================================
# Step 7: Merge future wide into person-level samples
# =============================================================================
cat("\n--- Step 7: Merge future wide into person-level samples ---\n")

samples_plus <- merge_future_wide_into_samples(
  samples,
  df_future_wide
)

saveRDS(samples_plus$s2019_all,           file.path(samples_path, "s2019_all_plus_lmo.rds"))
saveRDS(samples_plus$s2019_couples,       file.path(samples_path, "s2019_couples_plus_lmo.rds"))
saveRDS(samples_plus$s2019_covid,         file.path(samples_path, "s2019_covid_plus_lmo.rds"))
saveRDS(samples_plus$s2019_covid_couples, file.path(samples_path, "s2019_covid_couples_plus_lmo.rds"))

cat("Person-level samples + future wide saved to: ", samples_path, "\n", sep = "")

# =============================================================================
# Done
# =============================================================================
cat("\nBUILD COMPLETE.\n")
cat("Derived datasets: ", der_path, "\n", sep = "")
cat("Samples:          ", samples_path, "\n", sep = "")