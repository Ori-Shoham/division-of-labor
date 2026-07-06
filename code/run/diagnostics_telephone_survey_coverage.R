# =============================================================================
# Script: code/run/diagnostics_telephone_survey_coverage.R
#
# Purpose:
#   One-off diagnostic (not part of the staged pipeline) answering three
#   questions about the Understanding Society COVID-19 Study (SN 8644),
#   entirely from the raw .dta files already on disk under path_covid.
#   No internet access, no PDF parsing, no documentation scraping — this
#   reads the actual data the same way code/lib/covid_loader.R does
#   (read_dta_clean()) and inspects it directly.
#
#   Per wave in `covid_waves` (ca..ci):
#
#   1. Does a telephone-mode file exist for this wave?
#      -> file.exists() on "{wave}_indresp_t.dta" in path_covid, alongside
#         the web-mode "{wave}_indresp_w.dta" that covid_loader.R already
#         reads. (ONBOARDING.md §2.3 notes the _t files exist on disk for
#         at least some waves but are not currently loaded by the pipeline.)
#   2. Of the COVID-study variables this repo actually uses (parsed straight
#      out of code/lib/covid_loader.R's vars_to_keep block), which ones exist
#      as columns in that wave's telephone file (vs. the web file)?
#      -> read both files with read_dta_clean() and compare names().
#   3. How many observations would we gain in each wave by including the
#      telephone-mode file — i.e. how many pidps appear in the telephone
#      file but not already in the web file for that wave?
#      -> read both files fully and compare pidp sets.
#
# Requirements:
#   Must be run on a machine where path_covid (code/lib/config.R) actually
#   points at the licensed UKDA-8644 files. On a machine without the data
#   (e.g. an AI-coding machine per ONBOARDING.md §12), this script reports
#   that path_covid is missing and stops without fabricating any numbers.
#
# Output:
#   Printed to console only. No files written, no figures/tables touched.
# =============================================================================

suppressPackageStartupMessages({
  library(tidyverse)
  library(haven)
})

rm(list = ls())

source("code/lib/config.R")
source("code/lib/utils.R")       # read_dta_clean()
source("code/lib/wave_labels.R") # covid_wave_label_lookup()

if (!dir.exists(path_covid)) {
  stop(
    "path_covid ('", path_covid, "') does not exist on this machine.\n",
    "This script only reads raw COVID-study .dta files already on disk; ",
    "it does not download anything. Run it on a machine where path_covid ",
    "points at the real UKDA-8644 files (see ONBOARDING.md §5.4)."
  )
}

# =============================================================================
# Step 0: Variables this repo actually pulls from the COVID study
#
# Parsed directly out of covid_loader.R's vars_to_keep block so this list can
# never silently drift from the code that consumes it.
# =============================================================================

parse_repo_covid_variables <- function(loader_path = "code/lib/covid_loader.R") {
  lines <- readLines(loader_path, warn = FALSE)
  # Lines look like:   paste0(wave_prefix, "_furlough"),
  hits <- str_match(lines, 'paste0\\(wave_prefix,\\s*"_([A-Za-z0-9_]+)"\\)')[, 2]
  hits <- hits[!is.na(hits)]
  unique(hits)
}

repo_vars <- parse_repo_covid_variables()

cat("--- COVID-study variables used by this repo (from covid_loader.R) ---\n")
print(repo_vars)

# =============================================================================
# Steps 1-3: per wave, check file presence, variable coverage, and
# observation counts, straight from the .dta files.
# =============================================================================

wave_labels <- covid_wave_label_lookup()

check_wave <- function(wave) {
  web_path <- file.path(path_covid, paste0(wave, "_indresp_w.dta"))
  tel_path <- file.path(path_covid, paste0(wave, "_indresp_t.dta"))

  has_web <- file.exists(web_path)
  has_tel <- file.exists(tel_path)

  wave_label <- wave_labels$wave_label_full[wave_labels$wave == wave]
  if (length(wave_label) == 0) wave_label <- NA_character_

  cat("\n=== Wave ", wave, " (", wave_label, ") ===\n", sep = "")
  cat("  web file (", basename(web_path), "): ", if (has_web) "found" else "NOT FOUND", "\n", sep = "")
  cat("  telephone file (", basename(tel_path), "): ", if (has_tel) "found" else "not present this wave", "\n", sep = "")

  if (!has_tel) {
    return(tibble::tibble(
      wave = wave,
      wave_label = wave_label,
      has_telephone_file = FALSE,
      n_web = NA_integer_,
      n_telephone = NA_integer_,
      n_gained_from_telephone = NA_integer_,
      n_repo_vars = length(repo_vars),
      n_repo_vars_in_telephone = NA_integer_,
      repo_vars_missing_in_telephone = NA_character_
    ))
  }

  df_tel <- read_dta_clean(tel_path)
  df_web <- if (has_web) read_dta_clean(web_path) else NULL

  # ---- Step 2: which of our target variables exist as columns? -------------
  target_prefixed <- paste0(wave, "_", repo_vars)
  in_telephone <- target_prefixed %in% names(df_tel)
  missing_vars <- repo_vars[!in_telephone]

  cat("  repo variables present in telephone file: ",
      sum(in_telephone), " / ", length(repo_vars),
      if (length(missing_vars) > 0) paste0(" (missing: ", paste(missing_vars, collapse = ", "), ")") else "",
      "\n", sep = "")

  # ---- Step 3: observations gained by including telephone-mode rows -------
  n_web <- if (!is.null(df_web)) nrow(df_web) else NA_integer_
  n_tel <- nrow(df_tel)

  n_gained <- NA_integer_
  if (!is.null(df_web) && "pidp" %in% names(df_web) && "pidp" %in% names(df_tel)) {
    n_gained <- length(setdiff(unique(df_tel$pidp), unique(df_web$pidp)))
    n_overlap <- length(intersect(unique(df_tel$pidp), unique(df_web$pidp)))
    if (n_overlap > 0) {
      cat("  NOTE: ", n_overlap,
          " pidp(s) appear in BOTH the web and telephone files this wave ",
          "(unexpected if each respondent completes one mode) — check before using n_gained.\n",
          sep = "")
    }
  }

  cat("  n (web) = ", n_web, "; n (telephone) = ", n_tel,
      "; additional pidps gained by including telephone file = ", n_gained, "\n", sep = "")

  tibble::tibble(
    wave = wave,
    wave_label = wave_label,
    has_telephone_file = TRUE,
    n_web = n_web,
    n_telephone = n_tel,
    n_gained_from_telephone = n_gained,
    n_repo_vars = length(repo_vars),
    n_repo_vars_in_telephone = sum(in_telephone),
    repo_vars_missing_in_telephone = paste(missing_vars, collapse = ", ")
  )
}

results <- purrr::map_dfr(covid_waves, check_wave)

cat("\n\n--- Summary across all COVID waves ---\n")
print(results, n = Inf)

cat("\n--- Done ---\n")
