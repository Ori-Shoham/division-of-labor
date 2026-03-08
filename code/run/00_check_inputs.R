# =============================================================================
# Script: code/run/00_check_inputs.R
#
# Purpose:
#   Pre-flight checks before running code/run/01_build_data.R.
#   - Verifies that required folders exist
#   - Verifies that key policy files exist (SOC/SIC)
#   - Verifies that UKHLS MAIN files exist for baseline (J/K) and future (L–O):
#       {w}_indresp.dta, {w}_egoalt.dta, {w}_indall.dta
#   - Verifies that UKHLS COVID files exist for ca–ci:
#       {cw}_indresp_w.dta
#   - Prints a clean checklist and stops with an informative error if missing.
#
# Notes:
#   - Data is outside project folder (storage/confidentiality).
#   - policies/, figures/, tables/ already exist (but we still check).
# =============================================================================

suppressPackageStartupMessages({
  library(tidyverse)
})

rm(list = ls())

# ---- Source config ------------------------------------------------------------
source("code/lib/config.R")

# ---- Helper: check path exists ------------------------------------------------
check_exists <- function(path, label) {
  ok <- file.exists(path)
  tibble(
    item = label,
    path = path,
    exists = ok
  )
}

# ---- Helper: check a set of wave files ---------------------------------------
check_main_wave_files <- function(path_main, waves) {
  # For each wave w, we require:
  #   w_indresp.dta  : individual response (main outcomes/covariates)
  #   w_egoalt.dta   : relationship grid (partner/children links)
  #   w_indall.dta   : all individuals in household (ages incl. babies)
  out <- list()
  
  for (w in waves) {
    out[[length(out) + 1]] <- check_exists(
      file.path(path_main, paste0(w, "_indresp.dta")),
      paste0("MAIN ", toupper(w), ": indresp")
    )
    out[[length(out) + 1]] <- check_exists(
      file.path(path_main, paste0(w, "_egoalt.dta")),
      paste0("MAIN ", toupper(w), ": egoalt")
    )
    out[[length(out) + 1]] <- check_exists(
      file.path(path_main, paste0(w, "_indall.dta")),
      paste0("MAIN ", toupper(w), ": indall")
    )
  }
  
  bind_rows(out)
}

check_covid_wave_files <- function(path_covid, covid_waves) {
  # For each COVID wave cw, we require:
  #   {cw}_indresp_w.dta
  out <- list()
  
  for (cw in covid_waves) {
    out[[length(out) + 1]] <- check_exists(
      file.path(path_covid, paste0(cw, "_indresp_w.dta")),
      paste0("COVID ", cw, ": indresp_w")
    )
  }
  
  bind_rows(out)
}

# =============================================================================
# Step 0: Check project folders
# =============================================================================

cat("\n--- Checking project folders ---\n")

folders_to_check <- tibble(
  label = c("policies/", "figures/", "tables/", "derived/ (will be created if missing)"),
  path  = c("policies", "figures", "tables", "derived")
) %>%
  mutate(
    exists = dir.exists(path)
  )

print(folders_to_check)

# Create derived/ if missing (harmless)
if (!dir.exists("derived")) dir.create("derived", recursive = TRUE)

# =============================================================================
# Step 1: Check policy files (SOC/SIC)
# =============================================================================

cat("\n--- Checking policy files ---\n")

policy_checks <- bind_rows(
  check_exists(soc_path, "SOC.csv"),
  check_exists(sic_path, "SIC.csv")
)

print(policy_checks)

# =============================================================================
# Step 2: Check data root folders
# =============================================================================

cat("\n--- Checking data root folders ---\n")

data_root_checks <- tibble(
  item = c("path_main (UKHLS main)", "path_covid (UKHLS COVID)"),
  path = c(path_main, path_covid),
  exists = c(dir.exists(path_main), dir.exists(path_covid))
)

print(data_root_checks)

# =============================================================================
# Step 3: Check required MAIN wave files
#   Baseline requires J/K; Future outcomes require L–O
# =============================================================================

cat("\n--- Checking MAIN wave files (baseline J/K + future L–O) ---\n")

main_required_waves <- c("j", "k", future_waves)
main_checks <- check_main_wave_files(path_main, main_required_waves)

# Summaries
main_missing <- main_checks %>% filter(!exists)
cat("\nMAIN files: ", nrow(main_checks) - nrow(main_missing), "/", nrow(main_checks), " found.\n")

# Print only missing (cleaner)
if (nrow(main_missing) > 0) {
  cat("\nMissing MAIN files:\n")
  print(main_missing)
} else {
  cat("All required MAIN files found.\n")
}

# =============================================================================
# Step 4: Check required COVID wave files
# =============================================================================

cat("\n--- Checking COVID wave files (ca–ci) ---\n")

covid_checks <- check_covid_wave_files(path_covid, covid_waves)
covid_missing <- covid_checks %>% filter(!exists)

cat("\nCOVID files: ", nrow(covid_checks) - nrow(covid_missing), "/", nrow(covid_checks), " found.\n")

if (nrow(covid_missing) > 0) {
  cat("\nMissing COVID files:\n")
  print(covid_missing)
} else {
  cat("All required COVID files found.\n")
}

# =============================================================================
# Step 5: Final decision (stop if critical missing inputs)
# =============================================================================

critical_missing <- bind_rows(
  policy_checks %>% filter(!exists) %>% mutate(type = "policy"),
  data_root_checks %>% filter(!exists) %>% mutate(type = "root"),
  main_missing %>% mutate(type = "main"),
  covid_missing %>% mutate(type = "covid")
)

if (nrow(critical_missing) > 0) {
  cat("\n--- CHECK FAILED: Missing inputs detected ---\n")
  cat("Fix the missing items above, then re-run 00_check_inputs.R.\n")
  stop("Missing required inputs. See printed checklist.")
} else {
  cat("\n--- CHECK PASSED ---\n")
  cat("You should be able to run: source('code/run/01_build_data.R')\n")
}