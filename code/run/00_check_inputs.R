# =============================================================================
# Script: code/run/00_check_inputs.R
#
# Purpose:
#   Pre-flight checks before running code/run/01_build_data.R.
#
#   Verifies that required project files, folders, policy files, and raw UKHLS
#   input files are available before building derived data.
#
# Current pipeline requirements:
#   - Baseline main-study composite: I/J/K
#   - Pre-baseline history: history_waves from config.R, usually A-K
#   - Future regular-wave outcomes: future_waves from config.R, usually J-O
#   - COVID study waves: covid_waves from config.R, usually ca-ci
#
# Required MAIN files for each checked regular wave:
#   {w}_indresp.dta
#   {w}_egoalt.dta
#   {w}_indall.dta
#
# Required COVID files for each checked COVID wave:
#   {cw}_indresp_w.dta
#
# Notes:
#   - Raw data live outside the project folder.
#   - Derived outputs live under data_out_root, also outside the repo.
#   - This script creates derived/sample/cache output folders if missing.
#   - It does not validate individual variable availability inside Stata files;
#     the build scripts use any_of()/safe loaders where appropriate.
# =============================================================================

suppressPackageStartupMessages({
  library(tidyverse)
})

rm(list = ls())

# ---- Source config ------------------------------------------------------------
source("code/lib/config.R")

# ---- Defaults for backward compatibility -------------------------------------
# Older config.R versions may not define history_waves, soc_path, or sic_path.
if (!exists("history_waves")) {
  history_waves <- c("a", "b", "c", "d", "e", "f", "g", "h", "i", "j", "k")
}

if (!exists("soc_path")) {
  soc_path <- file.path(pol_path, "SOC.csv")
}

if (!exists("sic_path")) {
  sic_path <- file.path(pol_path, "SIC.csv")
}

if (!exists("KEYWORKER_XLSX")) {
  KEYWORKER_XLSX <- file.path(pol_path, "keyworkersreferencetableupdated2.xlsx")
}

# ---- Helper: check path exists ------------------------------------------------
check_exists <- function(path, label, expected = c("file", "dir", "either")) {
  expected <- match.arg(expected)
  ok <- dplyr::case_when(
    expected == "file"   ~ file.exists(path) && !dir.exists(path),
    expected == "dir"    ~ dir.exists(path),
    expected == "either" ~ file.exists(path),
    TRUE                 ~ FALSE
  )
  
  tibble::tibble(
    item = label,
    path = path,
    expected = expected,
    exists = ok
  )
}

# ---- Helper: check required local project files -------------------------------
check_project_files <- function() {
  required_libs <- c(
    "code/lib/config.R",
    "code/lib/utils.R",
    "code/lib/harmonize_outcomes.R",
    "code/lib/work_groups.R",
    "code/lib/policies_keyworkers.R",
    "code/lib/family_baseline.R",
    "code/lib/covid_loader.R",
    "code/lib/covid_panel.R",
    "code/lib/future_outcomes.R",
    "code/lib/history.R",
    "code/lib/samples.R",
    "code/lib/wave_labels.R",
    "code/lib/descriptives_plots.R",
    "code/lib/future_descriptives_plots.R",
    "code/lib/couple_plot_helpers.R",
    "code/lib/couple_treatment_plots.R",
    "code/lib/sample_tables.R"
  )
  
  required_runs <- c(
    "code/run/00_master.R",
    "code/run/01_build_data.R",
    "code/run/02_make_descriptives.R",
    "code/run/02b_make_future_descriptives.R",
    "code/run/02c_sample_tables.R",
    "code/run/02d_make_couple_treatment_descriptives.R",
    "code/run/03_models_workoutside.R",
    "code/run/04_lasso_workoutside.R",
    "code/run/99_session_info.R"
  )
  
  bind_rows(
    purrr::map_dfr(required_libs, ~ check_exists(.x, paste0("LIB: ", basename(.x)), expected = "file")),
    purrr::map_dfr(required_runs, ~ check_exists(.x, paste0("RUN: ", basename(.x)), expected = "file"))
  )
}

# ---- Helper: check a set of regular-wave files --------------------------------
check_main_wave_files <- function(path_main, waves) {
  waves <- unique(tolower(waves))
  out <- list()
  
  for (w in waves) {
    out[[length(out) + 1]] <- check_exists(
      file.path(path_main, paste0(w, "_indresp.dta")),
      paste0("MAIN ", toupper(w), ": indresp"),
      expected = "file"
    )
    out[[length(out) + 1]] <- check_exists(
      file.path(path_main, paste0(w, "_egoalt.dta")),
      paste0("MAIN ", toupper(w), ": egoalt"),
      expected = "file"
    )
    out[[length(out) + 1]] <- check_exists(
      file.path(path_main, paste0(w, "_indall.dta")),
      paste0("MAIN ", toupper(w), ": indall"),
      expected = "file"
    )
  }
  
  bind_rows(out)
}

check_covid_wave_files <- function(path_covid, covid_waves) {
  covid_waves <- unique(tolower(covid_waves))
  out <- list()
  
  for (cw in covid_waves) {
    out[[length(out) + 1]] <- check_exists(
      file.path(path_covid, paste0(cw, "_indresp_w.dta")),
      paste0("COVID ", cw, ": indresp_w"),
      expected = "file"
    )
  }
  
  bind_rows(out)
}

print_missing_or_success <- function(checks, label) {
  missing <- checks %>% dplyr::filter(!exists)
  cat("\n", label, ": ", nrow(checks) - nrow(missing), "/", nrow(checks), " found.\n", sep = "")
  
  if (nrow(missing) > 0) {
    cat("\nMissing ", label, ":\n", sep = "")
    print(missing)
  } else {
    cat("All required ", label, " found.\n", sep = "")
  }
  
  invisible(missing)
}

# =============================================================================
# Step 0: Check local project folders and scripts
# =============================================================================

cat("\n--- Checking local project folders ---\n")

repo_folder_checks <- tibble::tibble(
  item = c("policies/", "figures/", "tables/"),
  path = c(pol_path, fig_path, tab_path),
  expected = "dir",
  exists = c(dir.exists(pol_path), dir.exists(fig_path), dir.exists(tab_path))
)

print(repo_folder_checks)

# Create figure/table folders if missing; they contain shareable outputs.
if (!dir.exists(fig_path)) dir.create(fig_path, recursive = TRUE)
if (!dir.exists(tab_path)) dir.create(tab_path, recursive = TRUE)

cat("\n--- Checking local project scripts ---\n")

project_file_checks <- check_project_files()
project_file_missing <- print_missing_or_success(project_file_checks, "project scripts")

# =============================================================================
# Step 1: Check policy files
# =============================================================================

cat("\n--- Checking policy files ---\n")

policy_checks <- bind_rows(
  check_exists(soc_path, "SOC.csv", expected = "file"),
  check_exists(sic_path, "SIC.csv", expected = "file"),
  check_exists(KEYWORKER_XLSX, "keyworker reference Excel", expected = "file")
)

policy_missing <- print_missing_or_success(policy_checks, "policy files")

# =============================================================================
# Step 2: Check data root and output folders
# =============================================================================

cat("\n--- Checking data root folders ---\n")

data_root_checks <- bind_rows(
  check_exists(path_main, "path_main (UKHLS main)", expected = "dir"),
  check_exists(path_covid, "path_covid (UKHLS COVID)", expected = "dir"),
  check_exists(data_out_root, "data_out_root", expected = "dir")
)

print(data_root_checks)

# Create derived output folders if data_out_root exists.
if (dir.exists(data_out_root)) {
  dir.create(der_path, showWarnings = FALSE, recursive = TRUE)
  dir.create(samples_path, showWarnings = FALSE, recursive = TRUE)
  dir.create(cache_path, showWarnings = FALSE, recursive = TRUE)
}

output_folder_checks <- bind_rows(
  check_exists(der_path, "der_path", expected = "dir"),
  check_exists(samples_path, "samples_path", expected = "dir"),
  check_exists(cache_path, "cache_path", expected = "dir")
)

cat("\n--- Checking derived output folders ---\n")
print(output_folder_checks)

# =============================================================================
# Step 3: Check required MAIN wave files
# =============================================================================

cat("\n--- Checking MAIN wave files ---\n")
cat("Baseline waves: I/J/K\n")
cat("History waves: ", paste(toupper(history_waves), collapse = ", "), "\n", sep = "")
cat("Future waves:  ", paste(toupper(future_waves), collapse = ", "), "\n", sep = "")

baseline_waves <- c("i", "j", "k")
main_required_waves <- unique(tolower(c(
  history_waves,
  baseline_waves,
  future_waves
)))

main_checks <- check_main_wave_files(path_main, main_required_waves)
main_missing <- print_missing_or_success(main_checks, "MAIN files")

# Also print by-purpose summaries; useful when history adds many earlier waves.
main_checks_by_wave <- main_checks %>%
  dplyr::mutate(
    wave = stringr::str_match(item, "MAIN ([A-Z]+):")[, 2]
  ) %>%
  dplyr::group_by(wave) %>%
  dplyr::summarise(
    files_found = sum(exists),
    files_required = dplyr::n(),
    complete = all(exists),
    .groups = "drop"
  ) %>%
  dplyr::arrange(wave)

cat("\nMAIN file completeness by wave:\n")
print(main_checks_by_wave)

# =============================================================================
# Step 4: Check required COVID wave files
# =============================================================================

cat("\n--- Checking COVID wave files ---\n")
cat("COVID waves: ", paste(covid_waves, collapse = ", "), "\n", sep = "")

covid_checks <- check_covid_wave_files(path_covid, covid_waves)
covid_missing <- print_missing_or_success(covid_checks, "COVID files")

# =============================================================================
# Step 5: Final decision
# =============================================================================

critical_missing <- bind_rows(
  project_file_missing %>% dplyr::mutate(type = "project_file"),
  policy_missing %>% dplyr::mutate(type = "policy"),
  data_root_checks %>% dplyr::filter(!exists) %>% dplyr::mutate(type = "root"),
  output_folder_checks %>% dplyr::filter(!exists) %>% dplyr::mutate(type = "output_folder"),
  main_missing %>% dplyr::mutate(type = "main"),
  covid_missing %>% dplyr::mutate(type = "covid")
)

if (nrow(critical_missing) > 0) {
  cat("\n--- CHECK FAILED: Missing inputs detected ---\n")
  cat("Fix the missing items above, then re-run code/run/00_check_inputs.R.\n")
  cat("\nMissing critical inputs by type:\n")
  print(
    critical_missing %>%
      dplyr::count(type, name = "n_missing") %>%
      dplyr::arrange(type)
  )
  stop("Missing required inputs. See printed checklist.")
} else {
  cat("\n--- CHECK PASSED ---\n")
  cat("You should be able to run: source('code/run/01_build_data.R')\n")
}
