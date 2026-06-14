# =============================================================================
# Script: code/run/03a_make_couple_treatment_event_studies.R
#
# Purpose:
#   Create regression/event-study analogs of the couple-treatment figures.
#
# Outputs:
#   figures/event_studies/<short_treatment_slug>/<fe_slug>/*.png
#   derived/event_study_results/<short_treatment_slug>/<fe_slug>/*.csv
#   derived/event_study_results/<short_treatment_slug>/<fe_slug>/*.rds
#
# Design:
#   - Main-study, monthly main-study, and COVID-study event studies are
#     estimated separately.
#   - Spouses are estimated separately.
#   - Baseline child-age groups are estimated separately, but plotted together.
#   - Treatments:
#       treat_wife_key_notedu_husb_not_or_edu
#       treat_wife_key_notedu_any
#       treat_husb_shutdown_wife_not
#   - Each treatment definition is saved in its own output folder.
#   - Each fixed-effects version is saved in its own subfolder:
#       nfe = no couple fixed effects
#       cfe = couple fixed effects
#   - Controls versions:
#       nfe:
#         * none
#         * baseline
#       cfe:
#         * none only
#     Baseline controls are time-invariant and therefore absorbed by couple FE.
#   - For wife-key treatments, both comparison samples are produced:
#       all
#       restricted to sample_husb_notkey_or_edu
#   - Standard errors clustered by pidp.
# =============================================================================

suppressPackageStartupMessages({
  library(tidyverse)
})

rm(list = ls())

# ---- Source libs -------------------------------------------------------------
source("code/lib/config.R")
source("code/lib/utils.R")
source("code/lib/event_study_regressions.R")

# ---- Output folders ----------------------------------------------------------
event_fig_dir <- file.path(fig_path, "event_studies")
event_results_dir <- file.path(der_path, "event_study_results")

dir.create(event_fig_dir, showWarnings = FALSE, recursive = TRUE)
dir.create(event_results_dir, showWarnings = FALSE, recursive = TRUE)

# =============================================================================
# Settings
# =============================================================================

RUN_COUPLE_FE_SET <- if (isTRUE(MAKE_EVENT_STUDIES_COUPLE_FE)) {
  c(FALSE, TRUE)
} else {
  FALSE
}

TREATMENT_VARS <- c(
  "treat_wife_key_notedu_husb_not_or_edu",
  "treat_husb_shutdown_wife_not"
)

if (isTRUE(MAKE_WIFE_KEY_ANY_TREATMENT)) {
  TREATMENT_VARS <- c(
    "treat_wife_key_notedu_husb_not_or_edu",
    "treat_wife_key_notedu_any",
    "treat_husb_shutdown_wife_not"
  )
}

CHILD_GROUPS <- c("u10", "11_17")
SPOUSES <- c("wife", "husband")

# Important:
#   - baseline controls are run only without couple FE.
#   - with couple FE, baseline controls are collinear with couple fixed effects.
CONTROLS_SET_NO_FE <- if (isTRUE(MAKE_EVENT_STUDIES_BASELINE_CONTROLS)) {
  c("none", "baseline")
} else {
  "none"
}
CONTROLS_SET_COUPLE_FE <- c("none")

WIFE_TREATMENT_EXTRA_RESTRICTION_VARS <- if (isTRUE(MAKE_RESTRICTED_HUSB_NOTKEY_VARIANTS)) {
  intersect(
    c(
      "treat_wife_key_notedu_husb_not_or_edu",
      "treat_wife_key_notedu_any"
    ),
    TREATMENT_VARS
  )
} else {
  character(0)
}

MAIN_SAMPLE_VARIANTS <- c("all", "both_in_covid")

MAIN_MONTHLY_REFERENCE_YM <- as.Date("2019-12-01")
MAIN_MONTHLY_START_YM <- as.Date("2019-01-01")
MAIN_MONTHLY_END_YM <- as.Date("2021-12-01")

MAIN_OUTCOMES <- c(
  "any_work",
  "jbhrs",
  "paygu_dv_real",
  "fimnlabgrs_dv_real",
  "fimngrs_dv_real",
  "howlng"
)

COVID_OUTCOMES <- c(
  "workoutside",
  "wfh_some",
  "any_work",
  "hours",
  "howlng",
  "husits_wife_main_both"
)

# =============================================================================
# Load data
# =============================================================================

df_base_couple <- readRDS(
  file.path(samples_path, "s2019_baseline_couplelevel.rds")
)

df_base_couple_both_in_covid <- readRDS(
  file.path(samples_path, "s2019_baseline_couplelevel_both_in_covid.rds")
)

df_main_couple <- readRDS(
  file.path(der_path, "couple_history_future_mainonly_long.rds")
)

df_main_couple_both_in_covid <- readRDS(
  file.path(der_path, "couple_history_future_mainonly_long_both_in_covid.rds")
)

main_monthly_file <- file.path(der_path, "couple_history_future_mainonly_monthly_long.rds")
df_main_monthly_couple <- if (file.exists(main_monthly_file)) {
  readRDS(main_monthly_file)
} else {
  stop(
    "Monthly main-study panel not found: ",
    main_monthly_file,
    ". Rerun code/run/01_build_data.R before making monthly event-study plots."
  )
}

main_monthly_both_in_covid_file <- file.path(
  der_path,
  "couple_history_future_mainonly_monthly_long_both_in_covid.rds"
)
df_main_monthly_couple_both_in_covid <- if (file.exists(main_monthly_both_in_covid_file)) {
  readRDS(main_monthly_both_in_covid_file)
} else {
  stop(
    "Monthly COVID-observed main-study panel not found: ",
    main_monthly_both_in_covid_file,
    ". Rerun code/run/01_build_data.R before making monthly event-study plots."
  )
}

df_covid_couple <- readRDS(
  file.path(der_path, "df_sample_long_covid_couplelevel.rds")
)

# =============================================================================
# Prepare main-study spouse panel
# =============================================================================

df_main_couple_common <- df_main_couple %>%
  attach_event_baseline_controls(df_base_couple) %>%
  add_husits_wife_main_both()

df_main_couple_prepped <- df_main_couple_common %>%
  add_main_event_time(
    reference_year = 2019,
    exclude_jan_feb_2020 = TRUE
  )

df_main_spouse <- make_spouse_event_panel(
  df = df_main_couple_prepped,
  outcomes = MAIN_OUTCOMES,
  study = "main"
)

saveRDS(
  df_main_spouse,
  file.path(event_results_dir, "main_spouse_panel.rds")
)

df_main_couple_both_in_covid_common <- df_main_couple_both_in_covid %>%
  attach_event_baseline_controls(df_base_couple_both_in_covid) %>%
  add_husits_wife_main_both()

df_main_couple_both_in_covid_prepped <- df_main_couple_both_in_covid_common %>%
  add_main_event_time(
    reference_year = 2019,
    exclude_jan_feb_2020 = TRUE
  )

df_main_spouse_both_in_covid <- make_spouse_event_panel(
  df = df_main_couple_both_in_covid_prepped,
  outcomes = MAIN_OUTCOMES,
  study = "main"
)

saveRDS(
  df_main_spouse_both_in_covid,
  file.path(event_results_dir, "main_spouse_panel_both_in_covid.rds")
)

# =============================================================================
# Prepare monthly main-study spouse panel
# =============================================================================

df_main_monthly_couple_common <- df_main_monthly_couple %>%
  attach_event_baseline_controls(df_base_couple) %>%
  add_husits_wife_main_both()

df_main_monthly_couple_prepped <- df_main_monthly_couple_common %>%
  add_main_monthly_event_time(
    reference_ym = MAIN_MONTHLY_REFERENCE_YM,
    start_ym = MAIN_MONTHLY_START_YM,
    end_ym = MAIN_MONTHLY_END_YM
  )

df_main_monthly_spouse <- make_spouse_event_panel(
  df = df_main_monthly_couple_prepped,
  outcomes = MAIN_OUTCOMES,
  study = "main_monthly"
)

saveRDS(
  df_main_monthly_spouse,
  file.path(event_results_dir, "main_monthly_spouse_panel.rds")
)

df_main_monthly_couple_both_in_covid_common <- df_main_monthly_couple_both_in_covid %>%
  attach_event_baseline_controls(df_base_couple_both_in_covid) %>%
  add_husits_wife_main_both()

df_main_monthly_couple_both_in_covid_prepped <- df_main_monthly_couple_both_in_covid_common %>%
  add_main_monthly_event_time(
    reference_ym = MAIN_MONTHLY_REFERENCE_YM,
    start_ym = MAIN_MONTHLY_START_YM,
    end_ym = MAIN_MONTHLY_END_YM
  )

df_main_monthly_spouse_both_in_covid <- make_spouse_event_panel(
  df = df_main_monthly_couple_both_in_covid_prepped,
  outcomes = MAIN_OUTCOMES,
  study = "main_monthly"
)

saveRDS(
  df_main_monthly_spouse_both_in_covid,
  file.path(event_results_dir, "main_monthly_spouse_panel_both_in_covid.rds")
)

# =============================================================================
# Prepare COVID-study spouse panel
# =============================================================================

df_covid_couple_prepped <- df_covid_couple %>%
  attach_event_baseline_controls(df_base_couple) %>%
  add_husits_wife_main_both() %>%
  add_covid_event_time()

df_covid_spouse <- make_spouse_event_panel(
  df = df_covid_couple_prepped,
  outcomes = COVID_OUTCOMES,
  study = "covid"
)

saveRDS(
  df_covid_spouse,
  file.path(event_results_dir, "covid_spouse_panel.rds")
)

# =============================================================================
# Duplicate pidp-year diagnostics
# =============================================================================

main_dup_diag <- diagnose_pidp_year_duplicates(
  df_spouse = df_main_spouse,
  outcomes = MAIN_OUTCOMES
)

saveRDS(
  main_dup_diag,
  file.path(event_results_dir, "diag_main_pidp_year_dups.rds")
)

if (is.list(main_dup_diag)) {
  readr::write_csv(
    main_dup_diag$overall,
    file.path(event_results_dir, "diag_main_pidp_year_dups_overall.csv")
  )

  readr::write_csv(
    main_dup_diag$by_outcome,
    file.path(event_results_dir, "diag_main_pidp_year_dups_by_outcome.csv")
  )
}

main_dup_diag_both_in_covid <- diagnose_pidp_year_duplicates(
  df_spouse = df_main_spouse_both_in_covid,
  outcomes = MAIN_OUTCOMES
)

saveRDS(
  main_dup_diag_both_in_covid,
  file.path(event_results_dir, "diag_main_pidp_year_dups_both_in_covid.rds")
)

if (is.list(main_dup_diag_both_in_covid)) {
  readr::write_csv(
    main_dup_diag_both_in_covid$overall,
    file.path(event_results_dir, "diag_main_pidp_year_dups_overall_both_in_covid.csv")
  )

  readr::write_csv(
    main_dup_diag_both_in_covid$by_outcome,
    file.path(event_results_dir, "diag_main_pidp_year_dups_by_outcome_both_in_covid.csv")
  )
}

# =============================================================================
# Duplicate pidp-month diagnostics
# =============================================================================

main_monthly_dup_diag <- diagnose_pidp_ym_duplicates(
  df_spouse = df_main_monthly_spouse,
  outcomes = MAIN_OUTCOMES
)

saveRDS(
  main_monthly_dup_diag,
  file.path(event_results_dir, "diag_main_monthly_pidp_ym_dups.rds")
)

if (is.list(main_monthly_dup_diag)) {
  readr::write_csv(
    main_monthly_dup_diag$overall,
    file.path(event_results_dir, "diag_main_monthly_pidp_ym_dups_overall.csv")
  )

  readr::write_csv(
    main_monthly_dup_diag$by_outcome,
    file.path(event_results_dir, "diag_main_monthly_pidp_ym_dups_by_outcome.csv")
  )
}

main_monthly_dup_diag_both_in_covid <- diagnose_pidp_ym_duplicates(
  df_spouse = df_main_monthly_spouse_both_in_covid,
  outcomes = MAIN_OUTCOMES
)

saveRDS(
  main_monthly_dup_diag_both_in_covid,
  file.path(event_results_dir, "diag_main_monthly_pidp_ym_dups_both_in_covid.rds")
)

if (is.list(main_monthly_dup_diag_both_in_covid)) {
  readr::write_csv(
    main_monthly_dup_diag_both_in_covid$overall,
    file.path(event_results_dir, "diag_main_monthly_pidp_ym_dups_overall_both_in_covid.csv")
  )

  readr::write_csv(
    main_monthly_dup_diag_both_in_covid$by_outcome,
    file.path(event_results_dir, "diag_main_monthly_pidp_ym_dups_by_outcome_both_in_covid.csv")
  )
}

# =============================================================================
# Run event studies treatment-by-treatment and FE-by-FE
# =============================================================================

main_results_by_treatment <- list()
main_monthly_results_by_treatment <- list()
covid_results_by_treatment <- list()

for (tr in TREATMENT_VARS) {

  tr_slug <- event_study_treatment_slug(tr)

  cat("\n============================================================\n")
  cat("Running event studies for treatment: ", tr, "\n", sep = "")
  cat("Folder slug: ", tr_slug, "\n", sep = "")
  cat("============================================================\n")

  treatment_fig_dir <- file.path(event_fig_dir, tr_slug)
  treatment_results_dir <- file.path(event_results_dir, tr_slug)

  dir.create(treatment_fig_dir, showWarnings = FALSE, recursive = TRUE)
  dir.create(treatment_results_dir, showWarnings = FALSE, recursive = TRUE)

  main_results_by_fe <- list()
  main_monthly_results_by_fe <- list()
  covid_results_by_fe <- list()

  for (run_couple_fe in RUN_COUPLE_FE_SET) {

    fe_slug <- event_study_fe_slug(run_couple_fe)

    controls_set_this_fe <- if (isTRUE(run_couple_fe)) {
      CONTROLS_SET_COUPLE_FE
    } else {
      CONTROLS_SET_NO_FE
    }

    cat("\n------------------------------------------------------------\n")
    cat("Couple FE version: ", fe_slug, "\n", sep = "")
    cat("Controls run: ", paste(controls_set_this_fe, collapse = ", "), "\n", sep = "")
    cat("------------------------------------------------------------\n")

    treatment_fe_fig_dir <- file.path(treatment_fig_dir, fe_slug)
    treatment_fe_results_dir <- file.path(treatment_results_dir, fe_slug)

    dir.create(treatment_fe_fig_dir, showWarnings = FALSE, recursive = TRUE)
    dir.create(treatment_fe_results_dir, showWarnings = FALSE, recursive = TRUE)

    # -------------------------------------------------------------------------
    # Main-study event studies for this treatment and FE version
    # -------------------------------------------------------------------------

    cat("\nRunning main-study event studies for ", tr, " / ", fe_slug, "...\n", sep = "")

    main_results_tr_fe <- run_event_study_batch(
      df_spouse = df_main_spouse,
      study = "main",
      outcomes = MAIN_OUTCOMES,
      treatment_vars = tr,
      child_groups = CHILD_GROUPS,
      spouses = SPOUSES,
      controls_set = controls_set_this_fe,
      couple_fe = run_couple_fe,
      fig_dir = treatment_fe_fig_dir,
      results_dir = treatment_fe_results_dir,
      sample_variants = MAIN_SAMPLE_VARIANTS,
      wife_treatment_extra_restriction_vars = WIFE_TREATMENT_EXTRA_RESTRICTION_VARS,
      save_model = TRUE,
      save_individual_child_plots = FALSE,
      save_combined_child_plots = TRUE
    )

    saveRDS(
      main_results_tr_fe,
      file.path(
        treatment_fe_results_dir,
        paste0("main_results_", tr_slug, "_", fe_slug, ".rds")
      )
    )

    readr::write_csv(
      main_results_tr_fe,
      file.path(
        treatment_fe_results_dir,
        paste0("main_results_", tr_slug, "_", fe_slug, ".csv")
      )
    )

    main_results_by_fe[[fe_slug]] <- main_results_tr_fe

    # -------------------------------------------------------------------------
    # Monthly main-study event studies for this treatment and FE version
    # -------------------------------------------------------------------------

    cat("\nRunning monthly main-study event studies for ", tr, " / ", fe_slug, "...\n", sep = "")

    main_monthly_results_tr_fe <- run_event_study_batch(
      df_spouse = df_main_monthly_spouse,
      study = "main_monthly",
      outcomes = MAIN_OUTCOMES,
      treatment_vars = tr,
      child_groups = CHILD_GROUPS,
      spouses = SPOUSES,
      controls_set = controls_set_this_fe,
      couple_fe = run_couple_fe,
      fig_dir = treatment_fe_fig_dir,
      results_dir = treatment_fe_results_dir,
      sample_variants = MAIN_SAMPLE_VARIANTS,
      wife_treatment_extra_restriction_vars = WIFE_TREATMENT_EXTRA_RESTRICTION_VARS,
      save_model = TRUE,
      save_individual_child_plots = FALSE,
      save_combined_child_plots = TRUE,
      monthly_reference_ym = MAIN_MONTHLY_REFERENCE_YM,
      monthly_start_ym = MAIN_MONTHLY_START_YM,
      monthly_end_ym = MAIN_MONTHLY_END_YM
    )

    saveRDS(
      main_monthly_results_tr_fe,
      file.path(
        treatment_fe_results_dir,
        paste0("main_monthly_results_", tr_slug, "_", fe_slug, ".rds")
      )
    )

    readr::write_csv(
      main_monthly_results_tr_fe,
      file.path(
        treatment_fe_results_dir,
        paste0("main_monthly_results_", tr_slug, "_", fe_slug, ".csv")
      )
    )

    main_monthly_results_by_fe[[fe_slug]] <- main_monthly_results_tr_fe

    # -------------------------------------------------------------------------
    # COVID-study event studies for this treatment and FE version
    # -------------------------------------------------------------------------

    cat("\nRunning COVID-study event studies for ", tr, " / ", fe_slug, "...\n", sep = "")

    covid_results_tr_fe <- run_event_study_batch(
      df_spouse = df_covid_spouse,
      study = "covid",
      outcomes = COVID_OUTCOMES,
      treatment_vars = tr,
      child_groups = CHILD_GROUPS,
      spouses = SPOUSES,
      controls_set = controls_set_this_fe,
      couple_fe = run_couple_fe,
      fig_dir = treatment_fe_fig_dir,
      results_dir = treatment_fe_results_dir,
      sample_variants = "all",
      wife_treatment_extra_restriction_vars = WIFE_TREATMENT_EXTRA_RESTRICTION_VARS,
      save_model = TRUE,
      save_individual_child_plots = FALSE,
      save_combined_child_plots = TRUE
    )

    saveRDS(
      covid_results_tr_fe,
      file.path(
        treatment_fe_results_dir,
        paste0("covid_results_", tr_slug, "_", fe_slug, ".rds")
      )
    )

    readr::write_csv(
      covid_results_tr_fe,
      file.path(
        treatment_fe_results_dir,
        paste0("covid_results_", tr_slug, "_", fe_slug, ".csv")
      )
    )

    covid_results_by_fe[[fe_slug]] <- covid_results_tr_fe

    # -------------------------------------------------------------------------
    # Combined treatment-specific FE file
    # -------------------------------------------------------------------------

    all_results_tr_fe <- dplyr::bind_rows(
      main_results_tr_fe,
      covid_results_tr_fe
    )

    all_with_main_monthly_results_tr_fe <- dplyr::bind_rows(
      main_results_tr_fe,
      main_monthly_results_tr_fe,
      covid_results_tr_fe
    )

    saveRDS(
      all_results_tr_fe,
      file.path(
        treatment_fe_results_dir,
        paste0("all_results_", tr_slug, "_", fe_slug, ".rds")
      )
    )

    readr::write_csv(
      all_results_tr_fe,
      file.path(
        treatment_fe_results_dir,
        paste0("all_results_", tr_slug, "_", fe_slug, ".csv")
      )
    )

    saveRDS(
      all_with_main_monthly_results_tr_fe,
      file.path(
        treatment_fe_results_dir,
        paste0("all_with_main_monthly_results_", tr_slug, "_", fe_slug, ".rds")
      )
    )

    readr::write_csv(
      all_with_main_monthly_results_tr_fe,
      file.path(
        treatment_fe_results_dir,
        paste0("all_with_main_monthly_results_", tr_slug, "_", fe_slug, ".csv")
      )
    )
  }

  # ---------------------------------------------------------------------------
  # Combined treatment-specific files across FE versions
  # ---------------------------------------------------------------------------

  main_results_tr <- dplyr::bind_rows(main_results_by_fe)
  main_monthly_results_tr <- dplyr::bind_rows(main_monthly_results_by_fe)
  covid_results_tr <- dplyr::bind_rows(covid_results_by_fe)
  all_results_tr <- dplyr::bind_rows(main_results_tr, covid_results_tr)
  all_with_main_monthly_results_tr <- dplyr::bind_rows(
    main_results_tr,
    main_monthly_results_tr,
    covid_results_tr
  )

  saveRDS(
    main_results_tr,
    file.path(treatment_results_dir, paste0("main_results_", tr_slug, ".rds"))
  )

  readr::write_csv(
    main_results_tr,
    file.path(treatment_results_dir, paste0("main_results_", tr_slug, ".csv"))
  )

  saveRDS(
    main_monthly_results_tr,
    file.path(
      treatment_results_dir,
      paste0("main_monthly_results_", tr_slug, ".rds")
    )
  )

  readr::write_csv(
    main_monthly_results_tr,
    file.path(
      treatment_results_dir,
      paste0("main_monthly_results_", tr_slug, ".csv")
    )
  )

  saveRDS(
    covid_results_tr,
    file.path(treatment_results_dir, paste0("covid_results_", tr_slug, ".rds"))
  )

  readr::write_csv(
    covid_results_tr,
    file.path(treatment_results_dir, paste0("covid_results_", tr_slug, ".csv"))
  )

  saveRDS(
    all_results_tr,
    file.path(treatment_results_dir, paste0("all_results_", tr_slug, ".rds"))
  )

  readr::write_csv(
    all_results_tr,
    file.path(treatment_results_dir, paste0("all_results_", tr_slug, ".csv"))
  )

  saveRDS(
    all_with_main_monthly_results_tr,
    file.path(
      treatment_results_dir,
      paste0("all_with_main_monthly_results_", tr_slug, ".rds")
    )
  )

  readr::write_csv(
    all_with_main_monthly_results_tr,
    file.path(
      treatment_results_dir,
      paste0("all_with_main_monthly_results_", tr_slug, ".csv")
    )
  )

  main_results_by_treatment[[tr]] <- main_results_tr
  main_monthly_results_by_treatment[[tr]] <- main_monthly_results_tr
  covid_results_by_treatment[[tr]] <- covid_results_tr
}

# =============================================================================
# Top-level combined coefficient files
# =============================================================================

main_results <- dplyr::bind_rows(main_results_by_treatment)

saveRDS(
  main_results,
  file.path(event_results_dir, "main_results_all_treatments.rds")
)

readr::write_csv(
  main_results,
  file.path(event_results_dir, "main_results_all_treatments.csv")
)

main_monthly_results <- dplyr::bind_rows(main_monthly_results_by_treatment)

saveRDS(
  main_monthly_results,
  file.path(event_results_dir, "main_monthly_results_all_treatments.rds")
)

readr::write_csv(
  main_monthly_results,
  file.path(event_results_dir, "main_monthly_results_all_treatments.csv")
)

covid_results <- dplyr::bind_rows(covid_results_by_treatment)

saveRDS(
  covid_results,
  file.path(event_results_dir, "covid_results_all_treatments.rds")
)

readr::write_csv(
  covid_results,
  file.path(event_results_dir, "covid_results_all_treatments.csv")
)

all_results <- dplyr::bind_rows(
  main_results,
  covid_results
)

saveRDS(
  all_results,
  file.path(event_results_dir, "all_results_all_treatments.rds")
)

readr::write_csv(
  all_results,
  file.path(event_results_dir, "all_results_all_treatments.csv")
)

all_with_main_monthly_results <- dplyr::bind_rows(
  main_results,
  main_monthly_results,
  covid_results
)

saveRDS(
  all_with_main_monthly_results,
  file.path(event_results_dir, "all_with_main_monthly_results_all_treatments.rds")
)

readr::write_csv(
  all_with_main_monthly_results,
  file.path(event_results_dir, "all_with_main_monthly_results_all_treatments.csv")
)

# =============================================================================
# Coverage diagnostic
# =============================================================================

cat("\nEvent-study result coverage:\n")
print(
  all_results %>%
    dplyr::count(study, treatment_var, couple_fe, controls, sample_variant)
)

cat("\nEvent-study result coverage including monthly main-study:\n")
print(
  all_with_main_monthly_results %>%
    dplyr::count(study, treatment_var, couple_fe, controls, sample_variant)
)

cat("\nEvent-study regressions complete.\n")
cat("Figures saved under: ", event_fig_dir, "\n", sep = "")
cat("Results saved under: ", event_results_dir, "\n", sep = "")
