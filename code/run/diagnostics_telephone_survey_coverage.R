# =============================================================================
# Script: code/run/diagnostics_telephone_survey_coverage.R
#
# Purpose:
#   One-off diagnostic (not part of the staged pipeline) answering three
#   questions about the Understanding Society COVID-19 Study (SN 8644):
#
#   1. Which COVID waves (ca-ci) fielded a separate telephone (CATI)
#      questionnaire, alongside the web questionnaire?
#      -> determined by scraping the public questionnaires index page,
#         not hardcoded.
#   2. Of the COVID-study variables this repo actually uses (parsed straight
#      out of code/lib/covid_loader.R), which ones were included in each
#      wave's telephone questionnaire?
#      -> determined by downloading each telephone questionnaire PDF and
#         text-searching it, not by manual inspection.
#   3. How many observations would we gain/lose in each such wave if
#      telephone-mode respondents were included/excluded?
#      -> requires the actual licensed COVID indresp_w.dta files and a
#         mode-of-interview variable inside them. Per CLAUDE.md, the raw/
#         derived data are deliberately NOT present on this machine, so this
#         section is written to run correctly wherever path_covid does
#         exist, but it will only print counts (not fabricate them) when the
#         files are actually there. On this machine it will report that the
#         data are unavailable and skip.
#
# Output:
#   Printed to console only. No files written, no figures/tables touched.
# =============================================================================

suppressPackageStartupMessages({
  library(tidyverse)
  library(rvest)
  library(httr)
})

rm(list = ls())

source("code/lib/config.R")
source("code/lib/wave_labels.R")

# =============================================================================
# Step 0: Variables this repo actually pulls from the COVID study
#
# Parsed directly out of covid_loader.R's vars_to_keep block so this list
# can never silently drift from the code that consumes it.
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
# Step 1: Which waves have a telephone questionnaire?
#
# Scrape the public questionnaires index page rather than hardcoding wave
# numbers. Understanding Society links each wave's PDFs from this page, and
# telephone versions are labelled/filed distinctly (URL contains
# "telephone-questionnaire").
# =============================================================================

questionnaires_url <- "https://www.understandingsociety.ac.uk/documentation/covid-19/questionnaires/"

get_questionnaire_links <- function(url) {
  resp <- httr::GET(url, httr::timeout(30))
  httr::stop_for_status(resp)
  page <- xml2::read_html(httr::content(resp, as = "text", encoding = "UTF-8"))
  hrefs <- page %>% rvest::html_elements("a") %>% rvest::html_attr("href")
  hrefs <- hrefs[!is.na(hrefs) & str_detect(hrefs, "questionnaires/.*\\.pdf$")]
  hrefs <- ifelse(str_starts(hrefs, "http"), hrefs,
                   paste0("https://www.understandingsociety.ac.uk", hrefs))
  unique(hrefs)
}

links <- tryCatch(get_questionnaire_links(questionnaires_url), error = function(e) {
  message("Could not reach questionnaires page: ", conditionMessage(e))
  character(0)
})

questionnaire_index <- tibble::tibble(url = links) %>%
  dplyr::mutate(
    file = basename(url),
    wave_num = str_match(file, "-w0?(\\d+)\\.pdf$")[, 2] %>% as.integer(),
    is_telephone = str_detect(file, "telephone"),
    is_youth = str_detect(file, "youth")
  ) %>%
  dplyr::filter(!is.na(wave_num), !is_youth) %>%
  dplyr::arrange(wave_num, is_telephone)

cat("\n--- Questionnaire PDFs found on the Understanding Society site ---\n")
print(questionnaire_index %>% dplyr::select(wave_num, is_telephone, file))

waves_with_telephone <- questionnaire_index %>%
  dplyr::filter(is_telephone) %>%
  dplyr::pull(wave_num) %>%
  unique() %>%
  sort()

cat("\nWaves with a telephone (CATI) questionnaire: ",
    if (length(waves_with_telephone) == 0) "none found" else paste(waves_with_telephone, collapse = ", "),
    "\n", sep = "")

# Map "Wave N" (as used on the website / in the PDF filenames) onto this
# repo's internal ca..ci letter codes, using the same wave-order this repo's
# wave_labels.R already documents (ca = wave 1 = Apr 2020, ... ci = wave 9 =
# Sep 2021). Built here, not hand-typed a second time, from covid_waves.
covid_wave_map <- tibble::tibble(
  wave_letter = covid_waves,
  wave_num = seq_along(covid_waves)
) %>%
  dplyr::left_join(covid_wave_label_lookup(), by = c("wave_letter" = "wave")) %>%
  dplyr::left_join(
    questionnaire_index %>% dplyr::filter(is_telephone) %>% dplyr::select(wave_num, telephone_pdf = url),
    by = "wave_num"
  )

cat("\n--- Repo wave codes cross-referenced with telephone availability ---\n")
print(covid_wave_map %>% dplyr::select(wave_letter, wave_num, wave_label_full, telephone_pdf))

# =============================================================================
# Step 2: Of the repo's target variables, which appear in each telephone
# questionnaire?
# =============================================================================

extract_pdf_variable_names <- function(pdf_url) {
  if (!requireNamespace("pdftools", quietly = TRUE)) {
    stop("pdftools package is required to parse questionnaire PDFs.")
  }
  tmp <- tempfile(fileext = ".pdf")
  httr::GET(pdf_url, httr::write_disk(tmp, overwrite = TRUE), httr::timeout(60))
  txt <- pdftools::pdf_text(tmp)
  file.remove(tmp)
  lines <- unlist(str_split(txt, "\n"))
  # Question boxes look like: "hcond_cv [Baseline: health condition]"
  vars <- str_match(str_trim(lines), "^([A-Za-z][A-Za-z0-9_]*)\\s*\\[")[, 2]
  unique(na.omit(vars))
}

telephone_waves <- covid_wave_map %>% dplyr::filter(!is.na(telephone_pdf))

if (nrow(telephone_waves) == 0) {
  cat("\nNo telephone questionnaires found to check variable coverage against.\n")
} else {
  coverage <- purrr::pmap_dfr(telephone_waves, function(wave_letter, wave_num, wave_label_full,
                                                         telephone_pdf, ...) {
    cat("\nDownloading + parsing telephone questionnaire for wave ", wave_letter,
        " (", wave_label_full, "): ", telephone_pdf, "\n", sep = "")
    pdf_vars <- tryCatch(
      extract_pdf_variable_names(telephone_pdf),
      error = function(e) {
        message("  Failed to parse PDF: ", conditionMessage(e))
        character(0)
      }
    )
    tibble::tibble(
      wave_letter = wave_letter,
      wave_label_full = wave_label_full,
      repo_variable = repo_vars,
      in_telephone_questionnaire = tolower(repo_vars) %in% tolower(pdf_vars)
    )
  })

  cat("\n--- Repo-variable coverage in each telephone questionnaire ---\n")
  print(coverage %>% dplyr::arrange(wave_letter, dplyr::desc(in_telephone_questionnaire)), n = Inf)

  cat("\n--- Summary: how many of our variables each telephone questionnaire covers ---\n")
  print(
    coverage %>%
      dplyr::group_by(wave_letter, wave_label_full) %>%
      dplyr::summarise(
        n_repo_vars = dplyr::n(),
        n_covered = sum(in_telephone_questionnaire),
        n_missing = n_repo_vars - n_covered,
        missing_vars = paste(repo_variable[!in_telephone_questionnaire], collapse = ", "),
        .groups = "drop"
      )
  )
}

# =============================================================================
# Step 3: Observations gained/lost by wave from telephone-mode respondents
#
# This section needs the actual licensed COVID indresp_w.dta files, which are
# deliberately not present on this machine (see CLAUDE.md). It is written to
# run correctly once path_covid points at real data; here it will detect that
# and skip rather than guess at numbers.
# =============================================================================

cat("\n--- Step 3: observations gained/lost by interview mode, per wave ---\n")

if (!dir.exists(path_covid)) {
  cat("path_covid ('", path_covid, "') does not exist on this machine.\n", sep = "")
  cat("This is expected: raw/derived UKHLS data are intentionally kept off this\n")
  cat("machine (see CLAUDE.md). Re-run this script on a machine where path_covid\n")
  cat("points at the real UKDA-8644 files to get actual observation counts.\n")
} else {

  find_mode_variable <- function(df) {
    labels <- purrr::map_chr(df, ~ attr(.x, "label") %||% "")
    cand_by_label <- names(df)[str_detect(tolower(labels), "mode of (data collection|interview)|interview mode")]
    cand_by_name <- names(df)[str_detect(tolower(names(df)), "^[a-z]{2}_(ivmode|mode|intmode)$")]
    unique(c(cand_by_label, cand_by_name))
  }

  for (i in seq_len(nrow(telephone_waves))) {
    w <- telephone_waves$wave_letter[i]
    lbl <- telephone_waves$wave_label_full[i]
    fpath <- file.path(path_covid, paste0(w, "_indresp_w.dta"))

    cat("\nWave ", w, " (", lbl, "): ", fpath, "\n", sep = "")

    if (!file.exists(fpath)) {
      cat("  File not found, skipping.\n")
      next
    }

    df <- haven::read_dta(fpath)
    mode_vars <- find_mode_variable(df)

    if (length(mode_vars) == 0) {
      cat("  Could not auto-detect a mode-of-interview variable in this file.\n")
      cat("  Inspect variable labels manually, e.g.:\n")
      cat("    labels <- purrr::map_chr(df, ~ attr(.x, 'label') %||% '')\n")
      cat("    labels[str_detect(tolower(labels), 'mode')]\n")
      next
    }

    for (mv in mode_vars) {
      cat("  Mode variable candidate: ", mv, "\n", sep = "")
      tab <- df %>%
        dplyr::mutate(mode_lab = haven::as_factor(.data[[mv]])) %>%
        dplyr::count(mode_lab, name = "n")
      print(tab)

      wave_repo_vars <- coverage %>%
        dplyr::filter(wave_letter == w, in_telephone_questionnaire) %>%
        dplyr::pull(repo_variable)
      wave_repo_vars <- intersect(paste0(w, "_", wave_repo_vars), names(df))

      if (length(wave_repo_vars) > 0) {
        gained <- df %>%
          dplyr::mutate(
            mode_lab = haven::as_factor(.data[[mv]]),
            any_target_nonmissing = rowSums(!is.na(dplyr::across(dplyr::all_of(wave_repo_vars)))) > 0
          ) %>%
          dplyr::group_by(mode_lab) %>%
          dplyr::summarise(
            n = dplyr::n(),
            n_with_target_data = sum(any_target_nonmissing),
            .groups = "drop"
          )
        cat("  Observations with non-missing data on repo target variables, by mode:\n")
        print(gained)
      }
    }
  }
}

cat("\n--- Done ---\n")
