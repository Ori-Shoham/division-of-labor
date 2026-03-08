# =============================================================================
# Script: code/run/01_build_data.R
#
# Purpose:
#   End-to-end build:
#     - baseline J/K composite (K restricted to 2019)
#     - COVID waves ca–ci merged wide + long panel
#     - four baseline-defined analytic samples
#     - future main waves L–O outcomes + family structure + couple evolution
#     - unified event-study long dataset
#
# Outputs (RDS):
#   derived/baseline.rds
#   derived/covid_all_wide.rds
#   derived/df_sample_long_covid.rds
#   derived/samples/*.rds
#   derived/future_outcomes_long_lmo.rds
#   derived/future_outcomes_wide_lmo.rds
#   derived/eventstudy_long_2019_baseline_covid_lmo.rds
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

source("code/lib/family_baseline.R")
source("code/lib/covid_loader.R")
source("code/lib/covid_panel.R")
source("code/lib/future_outcomes.R")
source("code/lib/samples.R")

# ---- Ensure output folders exist ---------------------------------------------
dir.create(der_path, showWarnings = FALSE, recursive = TRUE)
dir.create(file.path(der_path, "samples"), showWarnings = FALSE, recursive = TRUE)

# ---- Policies: SOC/SIC label crosswalks --------------------------------------
SOC <- read.csv(soc_path)
SIC <- read.csv(sic_path)

# ---- Key worker crosswalk (your excel pipeline; kept here as data input step) -
key_workers_raw <- readxl::read_excel(
  "C:/Users/USER/Dropbox/WFH_covid/UK project/division-of-labor/policies/keyworkersreferencetableupdated2.xlsx",
  sheet = 4
)

sics <- tibble(
  SIC_4 = colnames(key_workers_raw)[4:599],
  industry = unlist(key_workers_raw[1, 4:599])
)

key_workers <- key_workers_raw %>%
  filter(!is.na(Group)) %>%
  pivot_longer(
    !c(Group, `SOC10M / INDC07M`, `SOC_label / SIC_label`),
    names_to = "SIC_4",
    values_to = "key"
  ) %>%
  rename(
    SOC_4      = `SOC10M / INDC07M`,
    occ_group  = Group,
    occupation = `SOC_label / SIC_label`
  ) %>%
  left_join(sics, by = "SIC_4") %>%
  select(occ_group, occupation, SOC_4, industry, SIC_4, key) %>%
  mutate(across(c(SIC_4, SOC_4, key), as.numeric))

key_inds <- key_workers %>%
  group_by(SIC = floor(SIC_4 / 100), SOC = floor(SOC_4 / 10)) %>%
  summarise(any_key = max(key), .groups = "drop") %>%
  left_join(SOC, by = "SOC") %>%
  left_join(SIC, by = "SIC")

# ---- Step 1: baseline ---------------------------------------------------------
cat("\n--- Step 1: Build baseline (J/K composite; K restricted to 2019) ---\n")
df_baseline <- build_baseline(path_main)
saveRDS(df_baseline, file.path(der_path, "baseline.rds"))
cat("Baseline N=", nrow(df_baseline), "\n")

# ---- Step 2: COVID wide -------------------------------------------------------
cat("\n--- Step 2: Merge COVID waves wide (ca–ci) ---\n")
df_covid_wide <- merge_covid_waves_wide(path_covid, covid_waves)
saveRDS(df_covid_wide, file.path(der_path, "covid_all_wide.rds"))
cat("COVID wide N=", nrow(df_covid_wide), "\n")

# ---- Step 3: COVID long panel -------------------------------------------------
cat("\n--- Step 3: Build COVID long panel ---\n")
df_sample_long_covid <- build_covid_long_panel(
  df_baseline = df_baseline,
  df_covid_wide = df_covid_wide,
  SOC = SOC,
  SIC = SIC,
  key_inds = key_inds
)
saveRDS(df_sample_long_covid, file.path(der_path, "df_sample_long_covid.rds"))
cat("COVID long rows=", nrow(df_sample_long_covid), "\n")

pidp_in_covid <- df_sample_long_covid %>% distinct(pidp) %>% pull(pidp)

# ---- Step 4: samples ----------------------------------------------------------
cat("\n--- Step 4: Build baseline-defined analytic samples ---\n")
samples <- build_samples_2019(df_baseline, pidp_in_covid = pidp_in_covid)

saveRDS(samples$s2019_all,           file.path(der_path, "samples", "s2019_all.rds"))
saveRDS(samples$s2019_couples,       file.path(der_path, "samples", "s2019_couples.rds"))
saveRDS(samples$s2019_covid,         file.path(der_path, "samples", "s2019_covid.rds"))
saveRDS(samples$s2019_covid_couples, file.path(der_path, "samples", "s2019_covid_couples.rds"))

# ---- Step 5: future outcomes L–O ---------------------------------------------
cat("\n--- Step 5: Build future outcomes (L–O) ---\n")
df_future_long <- build_future_outcomes_long(path_main, future_waves = future_waves)
df_future_long <- add_baseline_couple_evolution(df_future_long, df_baseline)

tmp_wfh <- combine_wfh(df_future_long$jbpl, df_future_long$jbwah)
df_future_long <- df_future_long %>%
  mutate(
    wfh_code = tmp_wfh$wfh_code,
    wfh_cat  = tmp_wfh$wfh_cat,
    health_sf = combine_health(sf1, scsf1)
  )

saveRDS(df_future_long, file.path(der_path, "future_outcomes_long_lmo.rds"))

df_future_wide <- df_future_long %>%
  select(-hidp) %>%
  pivot_wider(
    id_cols = pidp,
    names_from = wave,
    values_from = -c(pidp, wave),
    names_glue = "{.value}_{wave}"
  )

saveRDS(df_future_wide, file.path(der_path, "future_outcomes_wide_lmo.rds"))

# ---- Step 6: unified event-study long ----------------------------------------
cat("\n--- Step 6: Unified event-study long dataset ---\n")

df_event_covid <- df_sample_long_covid %>%
  mutate(
    wfh_code_all = as.numeric(wah),
    wfh_cat_all = case_when(
      wfh_code_all == 1 ~ "Always",
      wfh_code_all == 2 ~ "Often",
      wfh_code_all == 3 ~ "Sometimes",
      wfh_code_all == 4 ~ "Never",
      TRUE ~ NA_character_
    ),
    source = "covid_panel"
  )

df_event_future <- df_future_long %>%
  mutate(
    wfh_code_all = wfh_code,
    wfh_cat_all  = wfh_cat,
    source = "main_future"
  )

df_eventstudy_long <- bind_rows(df_event_covid, df_event_future)
saveRDS(df_eventstudy_long, file.path(der_path, "eventstudy_long_2019_baseline_covid_lmo.rds"))

# ---- Step 7: merge future outcomes into samples ------------------------------
cat("\n--- Step 7: Merge future outcomes into samples ---\n")
samples_plus <- merge_future_wide_into_samples(samples, df_future_wide)

saveRDS(samples_plus$s2019_all,           file.path(der_path, "samples", "s2019_all_plus_lmo.rds"))
saveRDS(samples_plus$s2019_couples,       file.path(der_path, "samples", "s2019_couples_plus_lmo.rds"))
saveRDS(samples_plus$s2019_covid,         file.path(der_path, "samples", "s2019_covid_plus_lmo.rds"))
saveRDS(samples_plus$s2019_covid_couples, file.path(der_path, "samples", "s2019_covid_couples_plus_lmo.rds"))

cat("\nBUILD COMPLETE.\n")