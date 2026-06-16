# =============================================================================
# Script: code/explore_baseline_year_2018_2019.R
#
# OUT-OF-PIPELINE CHECK (exploratory; not part of 00_master.R / 01_build_data.R).
#
# Question:
#   How many more couple x period observations, by treatment group, become
#   available if the pre-COVID baseline is chosen PER COUPLE BY CALENDAR YEAR:
#     - prefer 2019 when the WHOLE couple qualifies there
#       (both spouses observed, both working, both with valid SIC AND SOC),
#     - otherwise fall back to 2018,
#   compared with a 2019-only baseline built the same way.
#
# Design notes:
#   - This script does NOT modify the production pipeline, libraries, decks, or
#     any figure/table consumed by TeX. It only reuses existing builders.
#   - Candidate baseline waves: h/i/j/k. i/j/k cover 2019; h/i/j cover 2018.
#     Within a chosen year, the LATEST available wave is preferred (k>j>i>h).
#   - The only genuinely new logic is the per-couple year selection; everything
#     downstream re-runs existing pipeline builders with an alternative baseline.
#   - Counts are produced BY PERIOD for two panels:
#       * COVID couple panel (waves ca-ci)
#       * main-study history + future stacked couple panel (yearly)
#     broken down by treatment group, overall and by child-age facet.
#   - Outputs:
#       * figures + tidy counts CSV -> figures/explore/baseline_year_check/
#       * optional minimal couple panels (.rds) -> data_out_root/scratch/...
#
# Run (on a machine where the licensed UKHLS data are present):
#   source("code/explore_baseline_year_2018_2019.R")
# =============================================================================

suppressPackageStartupMessages({
  library(tidyverse)
  library(haven)
  library(readxl)
})

# ---- Source libs (same set the pipeline uses) --------------------------------
source("code/lib/config.R")
source("code/lib/utils.R")
source("code/lib/harmonize_outcomes.R")
source("code/lib/husits_harmonization.R")
source("code/lib/work_groups.R")
source("code/lib/policies_keyworkers.R")
source("code/lib/real_pay.R")
source("code/lib/family_baseline.R")
source("code/lib/covid_loader.R")
source("code/lib/covid_panel.R")
source("code/lib/future_outcomes.R")
source("code/lib/history.R")
source("code/lib/samples.R")
source("code/lib/wave_labels.R")
source("code/lib/couple_plot_helpers.R")
source("code/lib/couple_treatment_plots.R")

# ---- Output locations --------------------------------------------------------
fig_out <- file.path(fig_path, "explore", "baseline_year_check")
dir.create(fig_out, showWarnings = FALSE, recursive = TRUE)

scratch_out <- file.path(data_out_root, "scratch", "baseline_year_check")
dir.create(scratch_out, showWarnings = FALSE, recursive = TRUE)

# ---- Parameters --------------------------------------------------------------
CAND_WAVES <- c("h", "i", "j", "k")            # h/i/j -> 2018; i/j/k -> 2019
WAVE_RANK  <- c(h = 1L, i = 2L, j = 3L, k = 4L) # latest wave preferred within a year
TREATMENTS <- c(
  "treat_wife_key_notedu_husb_not_or_edu",
  "treat_husb_shutdown_wife_not"
)

# Columns carried from the couple-baseline object onto the long panels.
# Mirrors `couple_analysis_attach_cols` in code/run/01_build_data.R.
COUPLE_ATTACH_COLS <- c(
  "couple_id",
  "youngest_child_2019",
  "has_child_u10_2019",
  "has_child_11_17_2019",
  "child_age_group_2019",
  "treat_wife_key_notedu_husb_not_or_edu",
  "treat_wife_key_notedu_any",
  "sample_husb_notkey_or_edu",
  "treat_husb_shutdown_wife_not"
)

# Couple-level husits-direction helper, replicated from code/run/01_build_data.R
# (it is defined inline there, not in a library).
add_baseline_couple_husits_direction <- function(df) {
  if (!("husits_h" %in% names(df))) {
    df$husits_h <- if ("base_husits_h" %in% names(df)) df$base_husits_h else NA_real_
  }
  if (!("husits_w" %in% names(df))) {
    df$husits_w <- if ("base_husits_w" %in% names(df)) df$base_husits_w else NA_real_
  }
  add_couple_husits_direction_vars(df)
}

# =============================================================================
# Step A: Build-once, baseline-INDEPENDENT inputs
# =============================================================================
cat("\n--- Step A: Load policies and baseline-independent raw inputs ---\n")

pols <- load_policies(pol_path)
SOC  <- pols$SOC
SIC  <- pols$SIC
key_inds <- build_keyworker_crosswalk(
  xlsx_path = KEYWORKER_XLSX,
  sheet     = 4,
  SOC       = SOC,
  SIC       = SIC
)

# COVID wide merge (independent of the baseline definition). Reuse the cached
# copy from the pipeline if present, otherwise build it.
covid_wide_cache <- file.path(der_path, "covid_all_wide.rds")
if (file.exists(covid_wide_cache)) {
  cat("Reusing cached COVID wide: ", covid_wide_cache, "\n", sep = "")
  df_covid_wide <- readRDS(covid_wide_cache)
} else {
  df_covid_wide <- merge_covid_waves_wide(path_covid, covid_waves)
}

# Raw future-outcome long panel (independent of baseline; harmonized per-scenario
# below). Monthly start so Jan-Feb 2020 are available; the per-scenario prep
# restricts to the March-2020-onward main future sample, mirroring the pipeline.
df_future_raw <- build_future_outcomes_long(
  path_main    = path_main,
  future_waves = future_waves,
  min_ym       = future_outcomes_monthly_start
)

# Candidate pre-baseline history waves (same discovery logic as the pipeline).
history_waves_to_use <- discover_main_wave_prefixes(path_main)
if (length(history_waves_to_use) == 0 && exists("history_waves")) {
  history_waves_to_use <- history_waves
}

# =============================================================================
# Step B: Per-couple year selection (the only new logic)
# =============================================================================
cat("\n--- Step B: Per-couple baseline-year selection (2019 priority, 2018 fallback) ---\n")

# Pooled person x year candidate records from waves h/i/j/k.
build_year_pool <- function(path_main, cand_waves) {
  dfs <- lapply(cand_waves, function(w) {
    clean_baseline_wave(path_main, w) %>%
      dplyr::mutate(source_wave = w)
  })
  dplyr::bind_rows(dfs)
}

# One person-level baseline per calendar year: keep the latest wave within the
# year, apply the base_ prefix, and attach baseline work groups (SIC/SOC valid
# flags + industry groups). Real-pay/health recodes from build_baseline() are
# deliberately skipped (not needed for counts).
build_year_baseline_person <- function(pool, year) {
  pool %>%
    dplyr::filter(intdaty_dv == year) %>%
    dplyr::mutate(.wrank = unname(WAVE_RANK[source_wave])) %>%
    dplyr::group_by(pidp) %>%
    dplyr::slice_max(.wrank, n = 1, with_ties = FALSE) %>%
    dplyr::ungroup() %>%
    dplyr::select(-.wrank) %>%
    dplyr::rename_with(~ paste0("base_", .), -pidp) %>%
    add_baseline_work_groups()
}

pool <- build_year_pool(path_main, CAND_WAVES)

df_base_person_by_year <- list(
  `2019` = build_year_baseline_person(pool, 2019L),
  `2018` = build_year_baseline_person(pool, 2018L)
)

# For each year: roster (both spouses observed + working + reciprocal couple),
# couple dataset, and the set of couples that ALSO have valid SIC and SOC for
# both spouses ("we can identify industry and occupation").
year_couples <- lapply(df_base_person_by_year, function(dbp) {
  roster <- build_baseline_couple_roster(dbp)
  cds <- build_baseline_couple_dataset(dbp, roster)
  # baseline_group_info_ok = (valid SIC AND valid SOC), i.e. industry AND
  # occupation identifiable for that spouse. add_baseline_work_groups() drops
  # the raw has_valid_sic/has_valid_soc flags but keeps this combined flag.
  qualified_ids <- cds %>%
    dplyr::filter(
      baseline_group_info_ok_h,
      baseline_group_info_ok_w
    ) %>%
    dplyr::pull(couple_id)
  list(roster = roster, qualified_ids = qualified_ids)
})

qualified_2019 <- year_couples[["2019"]]$qualified_ids
qualified_2018 <- year_couples[["2018"]]$qualified_ids

chosen_2019_couples <- qualified_2019
chosen_2018_couples <- setdiff(qualified_2018, qualified_2019) # fallback only

cat("Qualified couples in 2019: ", length(qualified_2019), "\n", sep = "")
cat("Qualified couples in 2018: ", length(qualified_2018), "\n", sep = "")
cat("Added by 2018 fallback:    ", length(chosen_2018_couples), "\n", sep = "")

# Assemble the two scenarios' rosters and person-level baselines.
# couple_id is canonical from the spouse-pidp pair, so it is stable across years.
roster_2019_year <- year_couples[["2019"]]$roster
roster_2018_year <- year_couples[["2018"]]$roster

roster_2019only <- roster_2019_year %>%
  dplyr::filter(couple_id %in% chosen_2019_couples)

roster_combined <- dplyr::bind_rows(
  roster_2019_year %>% dplyr::filter(couple_id %in% chosen_2019_couples),
  roster_2018_year %>% dplyr::filter(couple_id %in% chosen_2018_couples)
)

# Person-level baseline for each scenario: each spouse takes their record from
# the couple's chosen year. The two pidp sets in the combined scenario are
# disjoint (a couple is in exactly one chosen set; a pidp is in exactly one
# couple), so the bind yields one row per pidp.
pidps_of_roster <- function(roster) {
  unique(c(roster$husband_pidp, roster$wife_pidp))
}

df_baseline_2019only_person <- df_base_person_by_year[["2019"]] %>%
  dplyr::semi_join(tibble::tibble(pidp = pidps_of_roster(roster_2019only)), by = "pidp") %>%
  dplyr::mutate(baseline_year_used = 2019L)

df_baseline_combined_person <- dplyr::bind_rows(
  df_base_person_by_year[["2019"]] %>%
    dplyr::semi_join(
      tibble::tibble(pidp = pidps_of_roster(
        roster_2019_year %>% dplyr::filter(couple_id %in% chosen_2019_couples)
      )),
      by = "pidp"
    ) %>%
    dplyr::mutate(baseline_year_used = 2019L),
  df_base_person_by_year[["2018"]] %>%
    dplyr::semi_join(
      tibble::tibble(pidp = pidps_of_roster(
        roster_2018_year %>% dplyr::filter(couple_id %in% chosen_2018_couples)
      )),
      by = "pidp"
    ) %>%
    dplyr::mutate(baseline_year_used = 2018L)
)

# Sanity checks.
stopifnot(!anyDuplicated(df_baseline_2019only_person$pidp))
stopifnot(!anyDuplicated(df_baseline_combined_person$pidp))
stopifnot(!anyDuplicated(roster_2019only$couple_id))
stopifnot(!anyDuplicated(roster_combined$couple_id))
stopifnot(nrow(roster_combined) >= nrow(roster_2019only))

# =============================================================================
# Step C: Per-scenario panel assembly (reuses pipeline builders unchanged)
# =============================================================================

# Replicate the pipeline's future-long harmonization (01_build_data.R Step 5),
# parameterized by a scenario baseline. df_future_raw is built once above.
prepare_future_long <- function(df_future_raw, df_baseline_person) {
  d <- add_baseline_couple_evolution(df_future_raw, df_baseline_person)

  d <- d %>%
    dplyr::semi_join(dplyr::select(df_baseline_person, pidp), by = "pidp") %>%
    dplyr::left_join(
      df_baseline_person %>%
        dplyr::select(
          pidp,
          dplyr::starts_with("base_"),
          group_industry_based,
          group_industry_based_detailed
        ),
      by = "pidp"
    )

  tmp_wfh <- combine_wfh(d$jbpl, d$jbwah)

  d <- d %>%
    add_baseline_work_groups() %>%
    dplyr::mutate(
      base_any_work = make_any_work_future(jbhas = base_jbhas),
      any_work      = make_any_work_future(jbhas = jbhas),
      base_work_last_week_status = make_work_last_week_status_main(
        jbhas = base_jbhas, jboff = base_jboff
      ),
      work_last_week_status = make_work_last_week_status_main(
        jbhas = jbhas, jboff = jboff
      ),
      wfh_code = tmp_wfh$wfh_code,
      wfh_cat  = tmp_wfh$wfh_cat,
      wfh_some = make_wfh_some_future(
        jbstat = jbstat, jbhrs = jbhrs, jbpl = jbpl, jbwah = jbwah
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
        jbstat = jbstat, jbhrs = jbhrs, wfh_code = wfh_code
      )
    )

  # Main future sample starts March 2020 (mirrors the pipeline's df_future_long).
  d %>% dplyr::filter(!is.na(ym), ym >= future_outcomes_start)
}

# Build the two couple-level long panels (COVID + main history/future) for a
# scenario, attaching the baseline treatment/child columns by couple_id.
assemble_scenario <- function(tag, df_baseline_person, roster) {
  cat("\n--- Step C: Assemble panels for scenario '", tag, "' ---\n", sep = "")

  # Couple-level baseline + treatments + child-age flags.
  s_couple <- build_baseline_couple_dataset(df_baseline_person, roster) %>%
    add_couple_baseline_treatments() %>%
    add_baseline_couple_husits_direction()

  # Pre-baseline couple history (window depends on each person's base_source_wave).
  df_hist_long <- build_prebaseline_history_long(
    path_main     = path_main,
    df_baseline   = df_baseline_person,
    history_waves = history_waves_to_use
  )
  df_couple_hist_long <- build_prebaseline_couple_history_long(
    df_history_long = df_hist_long,
    roster          = roster
  )
  # NB: the pre-baseline history *summary* (hist_* columns) is not needed for
  # observation counts. The stacked panel is built from df_couple_hist_long, and
  # the count plots only use treatment group + outcomes + time. So we skip
  # summarise_prebaseline_couple_history() and its three-key join here.

  attach_tbl <- s_couple %>%
    dplyr::select(
      dplyr::all_of(COUPLE_ATTACH_COLS),
      dplyr::starts_with("hist_")
    )

  # COVID couple long panel.
  df_covid_long <- build_covid_long_panel(
    df_baseline   = df_baseline_person,
    df_covid_wide = df_covid_wide,
    SOC           = SOC,
    SIC           = SIC,
    key_inds      = key_inds
  )
  df_covid_couple <- build_covid_couple_long(df_covid_long, roster) %>%
    add_couple_husits_direction_vars() %>%
    dplyr::left_join(attach_tbl, by = "couple_id")

  # Future couple long panel (main future sample, March 2020 onward).
  df_future_long <- prepare_future_long(df_future_raw, df_baseline_person)
  df_future_couple <- build_future_couple_long(df_future_long, roster) %>%
    dplyr::left_join(attach_tbl, by = "couple_id")

  # Main-study history + future stacked couple panel (no COVID rows).
  df_hf_couple <- build_couple_history_future_long(
    df_couple_history_long = df_couple_hist_long,
    df_baseline_couple     = s_couple,
    df_covid_couple_long   = NULL,
    df_future_couple_long  = df_future_couple,
    include_covid          = FALSE
  )

  list(
    covid           = df_covid_couple,
    history_future  = df_hf_couple,
    baseline_couple = s_couple
  )
}

scenarios <- list(
  `2019_only` = assemble_scenario("2019_only", df_baseline_2019only_person, roster_2019only),
  combined    = assemble_scenario("combined",  df_baseline_combined_person, roster_combined)
)

# =============================================================================
# Step D: Per-period counts by treatment group + comparison outputs
# =============================================================================
cat("\n--- Step D: Counts by period and treatment group ---\n")

# Count chains mirror the internals of plot_covid_treatment_group_counts() and
# plot_main_history_future_treatment_group_counts() so the CSV matches the
# figures exactly.
count_covid <- function(df, treatment_var) {
  df %>%
    filter_observed_couple_rows_for_counts(
      vars = covid_count_outcome_vars(),
      require_both_spouses = FALSE
    ) %>%
    dplyr::distinct(
      couple_id, wave, .data[[treatment_var]],
      has_child_u10_2019, has_child_11_17_2019
    ) %>%
    add_treatment_group_label(treatment_var = treatment_var) %>%
    expand_couple_samples_for_counts() %>%
    dplyr::filter(!is.na(treatment_group), !is.na(wave)) %>%
    dplyr::group_by(sample_group, period = wave, treatment_group) %>%
    dplyr::summarise(n_couples = dplyr::n_distinct(couple_id), .groups = "drop") %>%
    dplyr::mutate(period = as.character(period), panel = "covid_wave")
}

count_hf_year <- function(df, treatment_var) {
  df %>%
    dplyr::filter(!(as.character(period) %in% c("covid_baseline", "covid"))) %>%
    { if ("source" %in% names(.)) dplyr::filter(., !grepl("^covid", as.character(source))) else . } %>%
    filter_observed_couple_rows_for_counts(
      vars = future_count_outcome_vars(),
      require_both_spouses = FALSE
    ) %>%
    dplyr::distinct(
      couple_id, year, .data[[treatment_var]],
      has_child_u10_2019, has_child_11_17_2019
    ) %>%
    dplyr::rename(time = year) %>%
    add_treatment_group_label(treatment_var = treatment_var) %>%
    expand_couple_samples_for_counts() %>%
    dplyr::filter(!is.na(treatment_group), !is.na(time), time != 2025) %>%
    dplyr::group_by(sample_group, period = time, treatment_group) %>%
    dplyr::summarise(n_couples = dplyr::n_distinct(couple_id), .groups = "drop") %>%
    dplyr::mutate(period = as.character(period), panel = "history_future_year")
}

counts_all <- purrr::map_dfr(names(scenarios), function(scn) {
  s <- scenarios[[scn]]
  purrr::map_dfr(TREATMENTS, function(tr) {
    dplyr::bind_rows(
      count_covid(s$covid, tr),
      count_hf_year(s$history_future, tr)
    ) %>%
      dplyr::mutate(scenario = scn, treatment_var = tr)
  })
})

counts_all <- counts_all %>%
  dplyr::select(scenario, treatment_var, panel, sample_group, period, treatment_group, n_couples)

readr::write_csv(counts_all, file.path(fig_out, "baseline_year_counts.csv"))
cat("Counts CSV saved to: ", file.path(fig_out, "baseline_year_counts.csv"), "\n", sep = "")

# Per-scenario per-treatment figures using the existing plotters.
for (scn in names(scenarios)) {
  s <- scenarios[[scn]]
  for (tr in TREATMENTS) {
    plot_covid_treatment_group_counts(
      df            = s$covid,
      treatment_var = tr,
      out_file      = paste0("covid_counts_wave_", tr, "_", scn, ".png"),
      fig_path      = fig_out,
      include_title = TRUE
    )
    plot_main_history_future_treatment_group_counts(
      df            = s$history_future,
      treatment_var = tr,
      agg           = "year",
      out_file      = paste0("main_history_future_counts_year_", tr, "_", scn, ".png"),
      fig_path      = fig_out,
      include_title = TRUE
    )
  }
}

# Combined-vs-2019-only delta figures (the headline "gain" per treatment).
delta_tbl <- counts_all %>%
  tidyr::pivot_wider(
    names_from = scenario,
    values_from = n_couples,
    values_fill = 0
  ) %>%
  dplyr::mutate(gain = combined - `2019_only`)

readr::write_csv(delta_tbl, file.path(fig_out, "baseline_year_counts_delta.csv"))

for (tr in TREATMENTS) {
  p <- delta_tbl %>%
    dplyr::filter(treatment_var == tr) %>%
    ggplot(aes(
      x = period,
      y = gain,
      fill = treatment_group
    )) +
    geom_col(position = position_dodge(width = 0.8)) +
    facet_grid(sample_group ~ panel, scales = "free_x") +
    theme_minimal() +
    labs(
      x = NULL,
      y = "Added couples (combined - 2019-only)",
      fill = NULL,
      title = paste0("Observation gain from 2018 fallback | ", tr)
    ) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))

  ggsave(
    filename = paste0("gain_", tr, ".png"),
    plot = p,
    path = fig_out,
    width = 12,
    height = 8
  )
}

# =============================================================================
# Step E: Optional minimal scratch datasets (outside the repo)
# =============================================================================
for (scn in names(scenarios)) {
  s <- scenarios[[scn]]
  keep_cols <- c(
    "couple_id", "wave", "ym", "year", "period",
    COUPLE_ATTACH_COLS
  )
  saveRDS(
    s$covid %>% dplyr::select(dplyr::any_of(keep_cols), dplyr::matches("_(h|w)$")),
    file.path(scratch_out, paste0("covid_couple_long_", scn, ".rds"))
  )
  saveRDS(
    s$history_future %>% dplyr::select(dplyr::any_of(keep_cols), dplyr::matches("_(h|w)$")),
    file.path(scratch_out, paste0("history_future_couple_long_", scn, ".rds"))
  )
}

# =============================================================================
# Step F: Headline summary to console
# =============================================================================
cat("\n--- Step F: Baseline-year selection summary ---\n")
cat("Couples (2019-only roster): ", nrow(roster_2019only), "\n", sep = "")
cat("Couples (combined roster):  ", nrow(roster_combined), "\n", sep = "")
cat("Of which from 2018 fallback: ", length(chosen_2018_couples), "\n", sep = "")

cat("\nCouple-period observation totals by scenario/panel/treatment group:\n")
print(
  counts_all %>%
    dplyr::filter(sample_group == "All couples") %>%
    dplyr::group_by(scenario, panel, treatment_var, treatment_group) %>%
    dplyr::summarise(total_couple_periods = sum(n_couples), .groups = "drop") %>%
    dplyr::arrange(treatment_var, panel, treatment_group, scenario)
)

cat("\nFigures + CSVs written to: ", fig_out, "\n", sep = "")
cat("Scratch couple panels written to: ", scratch_out, "\n", sep = "")
cat("\nDone.\n")
