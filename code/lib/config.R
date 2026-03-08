# =============================================================================
# File: code/lib/config.R
#
# Purpose:
#   Central configuration for the project:
#     - load packages
#     - define paths (external raw data vs repo-safe outputs)
#     - define wave lists and missing code conventions
#
# Confidentiality / storage design:
#   - Raw data folders are stored outside the repo.
#   - The ONLY place raw data paths should appear is in this file.
#
# Repo folders:
#   - policies/   : crosswalks and policy definitions (safe to store)
#   - figures/    : output figures (safe to store)
#   - tables/     : output tables (safe to store)
#   - data/derived/: derived datasets (you decide if safe; can also redirect)
# =============================================================================

suppressPackageStartupMessages({
  library(tidyverse)  # dplyr, tidyr, ggplot2, stringr, readr, tibble, ...
  library(haven)      # read_dta
  library(forcats)    # fct_* helpers
  library(scales)     # percent_format
})

# ---- External raw data roots (EDIT THESE) -----------------------------------
# Place raw/large/confidential data OUTSIDE repo.
DATA_ROOT <- "C:/Users/USER/Dropbox/WFH_covid/UK project/understanding society uk all data"

# UKHLS main (SN 6614):
# Contains waves like j_indresp.dta, k_indresp.dta, and corresponding egoalt/indall.
path_main <- file.path(DATA_ROOT, "UKDA-6614-stata/stata/stata13_se/ukhls")

# UKHLS COVID study (SN 8644):
# Contains ca_indresp_w.dta ... ci_indresp_w.dta.
path_covid <- file.path(DATA_ROOT, "UKDA-8644-stata/stata/stata13_se")

# ---- Policy / crosswalk inputs (repo-safe) ----------------------------------
# SOC.csv and SIC.csv are expected in policies/
SOC_CSV <- file.path("policies", "SOC.csv")
SIC_CSV <- file.path("policies", "SIC.csv")

# Keyworker reference table:
# Recommendation: copy your excel into policies/ and rename it to keyworkers.xlsx.
KEYWORKER_XLSX  <- file.path("policies", "keyworkers.xlsx")
KEYWORKER_SHEET <- 4

# ---- Repo-safe outputs -------------------------------------------------------
pol_path <- "policies"
fig_path <- "figures"
tbl_path <- "tables"

# Derived data path inside repo (change if you want derived elsewhere)
der_path <- file.path("data", "derived")

if (!dir.exists(der_path)) dir.create(der_path, recursive = TRUE, showWarnings = FALSE)
if (!dir.exists(file.path(der_path, "samples"))) dir.create(file.path(der_path, "samples"), recursive = TRUE, showWarnings = FALSE)

# ---- Waves ------------------------------------------------------------------
# COVID monthly/periodic waves (April 2020 -> Sept 2021 in your usage)
covid_waves <- c("ca","cb","cc","cd","ce","cf","cg","ch","ci")

# Future main-survey waves you want for outcomes
future_waves <- c("l", "m", "n", "o")

# ---- UKHLS missing/NIU codes ------------------------------------------------
# UKHLS uses negative integer codes for missing/NIU/refusal; common ones:
#   -9: Missing
#   -8: Inapplicable / does not apply (often used as "NIU")
#   -2: Refusal
#   -1: Don't know
UKHLS_MISS <- c(-9, -8, -2, -1)

is_ukhls_missing <- function(x) x %in% UKHLS_MISS
to_na_ukhls <- function(x) ifelse(is_ukhls_missing(x), NA, x)

# Small infix helper: if a is NULL use b
`%||%` <- function(a, b) if (!is.null(a)) a else b