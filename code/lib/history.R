# =============================================================================
# File: code/lib/history.R
#
# Purpose:
#   Build pre-baseline UKHLS main-wave history files for the analytic sample.
#
# Outputs produced by these helpers:
#   1) person-wave history long file:
#        one row per pidp x prior main-study wave
#        includes all baseline/future outcome variables loaded by
#        load_main_wave_with_family(), plus derived variables such as any_work,
#        wfh_some, workoutside, health_sf, and cohabitation-with-baseline-partner
#        indicators.
#
#   2) person-level history summary:
#        compact history variables prefixed with hist_ that can be merged onto
#        baseline person-level samples.
#
#   3) couple-wave history long file:
#        one row per baseline couple x prior wave where at least one spouse is
#        observed in that wave. Husband variables have suffix _h and wife
#        variables have suffix _w. Includes spouse-specific interview dates and
#        a couple-level date/year for plotting.
#
#   4) couple-level history summary:
#        compact history variables prefixed with hist_ that can be merged onto
#        baseline couple-level samples.
#
# Important interpretation:
#   The cohabitation timing variables identify the first wave/date where the
#   baseline partners are OBSERVED as cohabiting / partnered in the available
#   UKHLS data. They do not identify the literal start date of cohabitation.
# =============================================================================

# -----------------------------------------------------------------------------
# Wave ordering
# -----------------------------------------------------------------------------
main_wave_order_lookup <- function() {
  stats::setNames(seq_along(letters), letters)
}

standardize_wave_prefix <- function(x) {
  tolower(as.character(x))
}

wave_order_value <- function(wave) {
  ord <- main_wave_order_lookup()
  unname(ord[standardize_wave_prefix(wave)])
}

# -----------------------------------------------------------------------------
# Discover available main-study waves on disk
# -----------------------------------------------------------------------------
discover_main_wave_prefixes <- function(path_main, max_wave = "o") {
  files <- list.files(
    path = path_main,
    pattern = "^[a-z]_indresp\\.dta$",
    full.names = FALSE
  )

  waves <- sub("_indresp\\.dta$", "", files)
  waves <- standardize_wave_prefix(waves)

  ord <- wave_order_value(waves)
  max_ord <- wave_order_value(max_wave)

  waves <- waves[!is.na(ord) & ord <= max_ord]
  waves[order(wave_order_value(waves))]
}

resolve_history_waves <- function(path_main, history_waves = NULL) {
  if (!is.null(history_waves)) {
    history_waves <- standardize_wave_prefix(history_waves)
    history_waves <- history_waves[order(wave_order_value(history_waves))]
    return(history_waves)
  }

  waves <- discover_main_wave_prefixes(path_main)
  if (length(waves) == 0) {
    # Fallback for environments where list.files() cannot see the raw data yet.
    waves <- letters[1:11]
  }
  waves
}

# -----------------------------------------------------------------------------
# Small safe summary helpers
# -----------------------------------------------------------------------------
valid_numeric <- function(x) {
  x[!is.na(x) & x >= 0]
}

mean_valid_numeric <- function(x) {
  x <- valid_numeric(x)
  if (length(x) == 0) return(NA_real_)
  mean(x)
}

last_valid_numeric <- function(x) {
  x <- valid_numeric(x)
  if (length(x) == 0) return(NA_real_)
  dplyr::last(x)
}

first_nonmissing_scalar <- function(x) {
  x <- x[!is.na(x)]
  if (length(x) == 0) return(NA)
  x[[1]]
}

last_nonmissing_scalar <- function(x) {
  x <- x[!is.na(x)]
  if (length(x) == 0) return(NA)
  x[[length(x)]]
}

first_nonmissing_date <- function(x) {
  x <- x[!is.na(x)]
  if (length(x) == 0) return(as.Date(NA))
  min(x)
}

last_nonmissing_date <- function(x) {
  x <- x[!is.na(x)]
  if (length(x) == 0) return(as.Date(NA))
  max(x)
}

any_flag <- function(x) {
  if (all(is.na(x))) return(NA_real_)
  if (any(x == 1, na.rm = TRUE)) return(1)
  0
}

count_flag <- function(x) {
  if (all(is.na(x))) return(NA_real_)
  sum(x == 1, na.rm = TRUE)
}

paste_unique_nonmissing <- function(x) {
  x <- sort(unique(x[!is.na(x)]))
  if (length(x) == 0) return(NA_character_)
  paste(x, collapse = ", ")
}

ensure_columns <- function(df, cols) {
  for (cc in cols) {
    if (!(cc %in% names(df))) {
      df[[cc]] <- NA_real_
    }
  }
  df
}

# -----------------------------------------------------------------------------
# Load one main wave for the history branch
# -----------------------------------------------------------------------------
load_history_wave <- function(path_main, prefix) {
  out <- tryCatch(
    load_main_wave_with_family(path_main, prefix),
    error = function(e) {
      warning("Could not load history wave ", prefix, ": ", conditionMessage(e))
      NULL
    }
  )
  if (is.null(out)) return(NULL)
  out
}

# -----------------------------------------------------------------------------
# Add derived variables to the regular-wave history panel
# -----------------------------------------------------------------------------
add_history_derived_variables <- function(df) {
  needed <- c(
    "jbstat", "jbhrs", "jbpl", "jbwah", "sf1", "scsf1",
    "partner_pidp", "partner_rel", "base_partner_pidp", "hidp"
  )
  df <- ensure_columns(df, needed)
  
  tmp_wfh <- combine_wfh(df$jbpl, df$jbwah)
  
  df %>%
    dplyr::mutate(
      any_work = make_any_work_future(
        jbstat = jbstat,
        jbhrs  = jbhrs
      ),
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
}

# -----------------------------------------------------------------------------
# Build person-wave pre-baseline history long file
# -----------------------------------------------------------------------------
build_prebaseline_history_long <- function(path_main,
                                           df_baseline,
                                           history_waves = NULL) {
  history_waves <- resolve_history_waves(
    path_main = path_main,
    history_waves = history_waves
  )

  if (length(history_waves) == 0) {
    stop("No candidate history waves were supplied or discovered.")
  }

  base_ref <- df_baseline %>%
    dplyr::transmute(
      pidp,
      base_source_wave = standardize_wave_prefix(base_source_wave),
      base_wave_order  = wave_order_value(base_source_wave),
      base_partner_pidp = base_partner_pidp,
      base_partner_rel  = base_partner_rel,
      base_family_id    = base_family_id,
      base_intdatm_dv   = base_intdatm_dv,
      base_intdaty_dv   = base_intdaty_dv,
      base_ym = as.Date(sprintf("%d-%02d-01", base_intdaty_dv, base_intdatm_dv))
    )
  
  dfs <- lapply(history_waves, function(w) load_history_wave(path_main, w))
  dfs <- dfs[!vapply(dfs, is.null, logical(1))]
  if (length(dfs) == 0) stop("No history waves could be loaded.")
  
  hist_all <- dplyr::bind_rows(dfs) %>%
    dplyr::mutate(
      wave = standardize_wave_prefix(wave),
      wave_order = wave_order_value(wave)
    )
  
  hist <- hist_all %>%
    dplyr::inner_join(base_ref, by = "pidp") %>%
    dplyr::filter(
      !is.na(base_wave_order),
      !is.na(wave_order),
      wave_order < base_wave_order
    ) %>%
    add_history_derived_variables()
  
  # To identify whether the baseline partner is observed in the same household in
  # a prior wave, join in the baseline partner's wave-specific household id.
  partner_hh <- hist_all %>%
    dplyr::select(
      base_partner_pidp = pidp,
      wave,
      partner_hidp_history = hidp
    ) %>%
    dplyr::filter(!is.na(base_partner_pidp)) %>%
    dplyr::distinct(base_partner_pidp, wave, .keep_all = TRUE)
  
  hist %>%
    dplyr::left_join(partner_hh, by = c("base_partner_pidp", "wave")) %>%
    dplyr::mutate(
      observed_base_partner_link = dplyr::case_when(
        is.na(base_partner_pidp) ~ NA_real_,
        !is.na(partner_pidp) &
          partner_pidp == base_partner_pidp &
          partner_rel %in% c(1, 2, 3) ~ 1,
        TRUE ~ 0
      ),
      observed_same_hh_base_partner = dplyr::case_when(
        is.na(base_partner_pidp) ~ NA_real_,
        !is.na(hidp) &
          !is.na(partner_hidp_history) &
          hidp == partner_hidp_history ~ 1,
        TRUE ~ 0
      ),
      observed_cohabit_base_partner = dplyr::case_when(
        is.na(base_partner_pidp) ~ NA_real_,
        observed_base_partner_link == 1 |
          observed_same_hh_base_partner == 1 ~ 1,
        TRUE ~ 0
      )
    ) %>%
    dplyr::arrange(pidp, ym, wave_order) %>%
    dplyr::select(
      pidp,
      wave,
      wave_order,
      ym,
      year,
      intdaty_dv,
      intdatm_dv,
      dplyr::everything()
    )
}

# -----------------------------------------------------------------------------
# Variables for compact person-level summaries
# -----------------------------------------------------------------------------
history_numeric_summary_vars <- function(df) {
  vars <- c(
    "age_dv",
    "sex",
    "gor_dv",
    "isced11_dv",
    "jbstat",
    "jbhas",
    "jbsic07_cc",
    "jbsoc10_cc",
    "jbft_dv",
    "jbhrs",
    "jbot",
    "jbsect",
    "jbterm1",
    "basrate",
    "fimngrs_dv",
    "fihhmngrs_dv",
    "paygu_dv",
    "fimnlabgrs_dv",
    "jbpl",
    "jbwah",
    "mastat_dv",
    "sppid",
    "nchild_dv",
    "ndepchl_dv",
    "howlng",
    "chcare",
    "aidadhrs",
    "scghq2_dv",
    "scghq1_dv",
    "sclfsato",
    "sf1",
    "scsf1",
    "any_work",
    "wfh_some",
    "workoutside",
    "observed_base_partner_link",
    "observed_same_hh_base_partner",
    "observed_cohabit_base_partner"
  )
  vars <- intersect(vars, names(df))
  vars[vapply(df[vars], is.numeric, logical(1))]
}

# -----------------------------------------------------------------------------
# Build compact person-level pre-baseline history summary
# -----------------------------------------------------------------------------
summarise_prebaseline_history <- function(df_history_long) {
  if (nrow(df_history_long) == 0) {
    return(tibble::tibble(pidp = numeric()))
  }
  
  df <- df_history_long %>%
    dplyr::arrange(pidp, ym, wave_order)
  
  numeric_vars <- history_numeric_summary_vars(df)
  
  core <- df %>%
    dplyr::group_by(pidp) %>%
    dplyr::summarise(
      hist_n_obs = dplyr::n(),
      hist_first_obs_ym = first_nonmissing_date(ym),
      hist_first_obs_year = suppressWarnings(as.integer(format(hist_first_obs_ym, "%Y"))),
      hist_first_obs_wave = first_nonmissing_scalar(wave),
      hist_last_obs_ym = last_nonmissing_date(ym),
      hist_last_obs_year = suppressWarnings(as.integer(format(hist_last_obs_ym, "%Y"))),
      hist_last_obs_wave = last_nonmissing_scalar(wave),
      hist_years_observed = paste_unique_nonmissing(year),
      hist_waves_observed = paste_unique_nonmissing(wave),
      hist_ever_observed_base_partner_link = any_flag(observed_base_partner_link),
      hist_ever_observed_same_hh_base_partner = any_flag(observed_same_hh_base_partner),
      hist_ever_observed_cohabit_base_partner = any_flag(observed_cohabit_base_partner),
      hist_n_waves_observed_base_partner_link = count_flag(observed_base_partner_link),
      hist_n_waves_same_hh_base_partner = count_flag(observed_same_hh_base_partner),
      hist_n_waves_cohabit_base_partner = count_flag(observed_cohabit_base_partner),
      hist_first_observed_base_partner_link_ym = first_nonmissing_date(ym[observed_base_partner_link == 1]),
      hist_first_observed_base_partner_link_year = suppressWarnings(as.integer(format(hist_first_observed_base_partner_link_ym, "%Y"))),
      hist_first_observed_base_partner_link_wave = first_nonmissing_scalar(wave[observed_base_partner_link == 1]),
      hist_first_observed_same_hh_base_partner_ym = first_nonmissing_date(ym[observed_same_hh_base_partner == 1]),
      hist_first_observed_same_hh_base_partner_year = suppressWarnings(as.integer(format(hist_first_observed_same_hh_base_partner_ym, "%Y"))),
      hist_first_observed_same_hh_base_partner_wave = first_nonmissing_scalar(wave[observed_same_hh_base_partner == 1]),
      hist_first_observed_cohabit_base_partner_ym = first_nonmissing_date(ym[observed_cohabit_base_partner == 1]),
      hist_first_observed_cohabit_base_partner_year = suppressWarnings(as.integer(format(hist_first_observed_cohabit_base_partner_ym, "%Y"))),
      hist_first_observed_cohabit_base_partner_wave = first_nonmissing_scalar(wave[observed_cohabit_base_partner == 1]),
      .groups = "drop"
    )
  
  if (length(numeric_vars) == 0) {
    return(core)
  }
  
  numeric_summary <- df %>%
    dplyr::group_by(pidp) %>%
    dplyr::summarise(
      dplyr::across(
        dplyr::all_of(numeric_vars),
        ~ sum(!is.na(.x) & .x >= 0),
        .names = "hist_n_nonmiss_{.col}"
      ),
      dplyr::across(
        dplyr::all_of(numeric_vars),
        mean_valid_numeric,
        .names = "hist_mean_{.col}"
      ),
      dplyr::across(
        dplyr::all_of(numeric_vars),
        last_valid_numeric,
        .names = "hist_last_{.col}"
      ),
      .groups = "drop"
    )
  
  core %>%
    dplyr::left_join(numeric_summary, by = "pidp")
}

# -----------------------------------------------------------------------------
# Add person history summary to a list of person-level samples
# -----------------------------------------------------------------------------
merge_history_summary_into_samples <- function(samples, df_history_summary) {
  lapply(samples, function(s) {
    dplyr::left_join(s, df_history_summary, by = "pidp")
  })
}

# -----------------------------------------------------------------------------
# Build baseline couple x prior-wave history long file
# -----------------------------------------------------------------------------
build_prebaseline_couple_history_long <- function(df_history_long, roster) {
  husband <- df_history_long %>%
    dplyr::rename(husband_pidp = pidp) %>%
    dplyr::rename_with(
      ~ ifelse(.x %in% c("husband_pidp", "wave", "wave_order"),
               .x,
               paste0(.x, "_h"))
    )

  wife <- df_history_long %>%
    dplyr::rename(wife_pidp = pidp) %>%
    dplyr::rename_with(
      ~ ifelse(.x %in% c("wife_pidp", "wave", "wave_order"),
               .x,
               paste0(.x, "_w"))
    )

  husband_rows <- roster %>%
    dplyr::select(couple_id, husband_pidp, wife_pidp, dplyr::everything()) %>%
    dplyr::inner_join(husband, by = "husband_pidp")

  wife_rows <- roster %>%
    dplyr::select(couple_id, husband_pidp, wife_pidp, dplyr::everything()) %>%
    dplyr::inner_join(wife, by = "wife_pidp")

  dplyr::full_join(
    husband_rows,
    wife_rows,
    by = c("couple_id", "husband_pidp", "wife_pidp", "wave", "wave_order"),
    suffix = c("", ".wifejoin")
  ) %>%
    dplyr::select(-dplyr::ends_with(".wifejoin")) %>%
    dplyr::mutate(
      couple_ym = dplyr::case_when(
        !is.na(ym_h) & !is.na(ym_w) & ym_h >= ym_w ~ ym_h,
        !is.na(ym_h) & !is.na(ym_w) & ym_w > ym_h ~ ym_w,
        !is.na(ym_h) ~ ym_h,
        !is.na(ym_w) ~ ym_w,
        TRUE ~ as.Date(NA)
      ),
      couple_year = suppressWarnings(as.integer(format(couple_ym, "%Y"))),
      observed_husband_history = dplyr::if_else(!is.na(ym_h), 1, 0),
      observed_wife_history = dplyr::if_else(!is.na(ym_w), 1, 0),
      observed_both_spouses_history = dplyr::if_else(
        observed_husband_history == 1 & observed_wife_history == 1,
        1,
        0
      ),
      couple_observed_base_partner_link = dplyr::case_when(
        observed_base_partner_link_h == 1 |
          observed_base_partner_link_w == 1 ~ 1,
        is.na(observed_base_partner_link_h) &
          is.na(observed_base_partner_link_w) ~ NA_real_,
        TRUE ~ 0
      ),
      couple_observed_same_hh_base_partner = dplyr::case_when(
        observed_same_hh_base_partner_h == 1 |
          observed_same_hh_base_partner_w == 1 ~ 1,
        is.na(observed_same_hh_base_partner_h) &
          is.na(observed_same_hh_base_partner_w) ~ NA_real_,
        TRUE ~ 0
      ),
      couple_observed_cohabit_base_partner = dplyr::case_when(
        observed_cohabit_base_partner_h == 1 |
          observed_cohabit_base_partner_w == 1 ~ 1,
        is.na(observed_cohabit_base_partner_h) &
          is.na(observed_cohabit_base_partner_w) ~ NA_real_,
        TRUE ~ 0
      )
    ) %>%
    dplyr::arrange(couple_id, couple_ym, wave_order)
}

# -----------------------------------------------------------------------------
# Build compact couple-level history summary
# -----------------------------------------------------------------------------
summarise_prebaseline_couple_history <- function(df_couple_history_long,
                                                 df_history_summary = NULL,
                                                 roster = NULL) {
  if (nrow(df_couple_history_long) == 0) {
    out <- tibble::tibble(couple_id = character())
  } else {
    out <- df_couple_history_long %>%
      dplyr::group_by(couple_id) %>%
      dplyr::summarise(
        hist_couple_n_joint_waves = dplyr::n_distinct(wave),
        hist_couple_first_joint_obs_ym = first_nonmissing_date(couple_ym),
        hist_couple_first_joint_obs_year = suppressWarnings(as.integer(format(hist_couple_first_joint_obs_ym, "%Y"))),
        hist_couple_first_joint_obs_wave = first_nonmissing_scalar(wave),
        hist_couple_last_joint_obs_ym = last_nonmissing_date(couple_ym),
        hist_couple_last_joint_obs_year = suppressWarnings(as.integer(format(hist_couple_last_joint_obs_ym, "%Y"))),
        hist_couple_last_joint_obs_wave = last_nonmissing_scalar(wave),
        hist_couple_years_joint_observed = paste_unique_nonmissing(couple_year),
        hist_couple_waves_joint_observed = paste_unique_nonmissing(wave),
        hist_couple_ever_observed_base_partner_link = any_flag(couple_observed_base_partner_link),
        hist_couple_ever_same_hh_base_partner = any_flag(couple_observed_same_hh_base_partner),
        hist_couple_ever_observed_cohabit_base_partner = any_flag(couple_observed_cohabit_base_partner),
        hist_couple_n_waves_observed_base_partner_link = count_flag(couple_observed_base_partner_link),
        hist_couple_n_waves_same_hh_base_partner = count_flag(couple_observed_same_hh_base_partner),
        hist_couple_n_waves_cohabit_base_partner = count_flag(couple_observed_cohabit_base_partner),
        hist_couple_first_observed_base_partner_link_ym = first_nonmissing_date(couple_ym[couple_observed_base_partner_link == 1]),
        hist_couple_first_observed_base_partner_link_year = suppressWarnings(as.integer(format(hist_couple_first_observed_base_partner_link_ym, "%Y"))),
        hist_couple_first_observed_base_partner_link_wave = first_nonmissing_scalar(wave[couple_observed_base_partner_link == 1]),
        hist_couple_first_same_hh_base_partner_ym = first_nonmissing_date(couple_ym[couple_observed_same_hh_base_partner == 1]),
        hist_couple_first_same_hh_base_partner_year = suppressWarnings(as.integer(format(hist_couple_first_same_hh_base_partner_ym, "%Y"))),
        hist_couple_first_same_hh_base_partner_wave = first_nonmissing_scalar(wave[couple_observed_same_hh_base_partner == 1]),
        hist_couple_first_observed_cohabit_base_partner_ym = first_nonmissing_date(couple_ym[couple_observed_cohabit_base_partner == 1]),
        hist_couple_first_observed_cohabit_base_partner_year = suppressWarnings(as.integer(format(hist_couple_first_observed_cohabit_base_partner_ym, "%Y"))),
        hist_couple_first_observed_cohabit_base_partner_wave = first_nonmissing_scalar(wave[couple_observed_cohabit_base_partner == 1]),
        .groups = "drop"
      )
  }
  
  if (!is.null(df_history_summary) && !is.null(roster)) {
    husband_hist <- df_history_summary %>%
      dplyr::rename(husband_pidp = pidp) %>%
      dplyr::rename_with(
        ~ ifelse(.x == "husband_pidp", .x, paste0(.x, "_h"))
      )
    
    wife_hist <- df_history_summary %>%
      dplyr::rename(wife_pidp = pidp) %>%
      dplyr::rename_with(
        ~ ifelse(.x == "wife_pidp", .x, paste0(.x, "_w"))
      )
    
    out <- roster %>%
      dplyr::select(couple_id, husband_pidp, wife_pidp) %>%
      dplyr::left_join(out, by = "couple_id") %>%
      dplyr::left_join(husband_hist, by = "husband_pidp") %>%
      dplyr::left_join(wife_hist, by = "wife_pidp")
  }
  
  out
}

# -----------------------------------------------------------------------------
# Calendar dates for synthetic/COVID-study rows used in stacked plotting panels
# -----------------------------------------------------------------------------
covid_wave_ym_lookup <- function() {
  tibble::tibble(
    wave = c(
      "baseline",
      "ca", "cb", "cc", "cd", "ce", "cf", "cg", "ch", "ci"
    ),
    ym = as.Date(c(
      "2020-02-01",
      "2020-04-01",
      "2020-05-01",
      "2020-06-01",
      "2020-07-01",
      "2020-09-01",
      "2020-11-01",
      "2021-01-01",
      "2021-03-01",
      "2021-09-01"
    ))
  ) %>%
    dplyr::mutate(year = suppressWarnings(as.integer(format(ym, "%Y"))))
}

# -----------------------------------------------------------------------------
# Convert baseline person rows from base_* names to regular-wave names
# -----------------------------------------------------------------------------
build_baseline_person_plot_rows <- function(df_baseline) {
  base_cols <- names(df_baseline)[startsWith(names(df_baseline), "base_")]
  keep_cols <- setdiff(
    names(df_baseline),
    base_cols
  )
  keep_cols <- setdiff(keep_cols, c("pidp"))
  keep_cols <- keep_cols[!grepl("^hist_", keep_cols)]
  keep_cols <- keep_cols[!grepl("_(h|w)$", keep_cols)]
  keep_cols <- keep_cols[keep_cols %in% names(df_baseline)]

  base_part <- df_baseline %>%
    dplyr::select(pidp, dplyr::all_of(base_cols)) %>%
    dplyr::rename_with(
      ~ sub("^base_", "", .x),
      dplyr::starts_with("base_")
    )

  extra_part <- df_baseline %>%
    dplyr::select(pidp, dplyr::all_of(keep_cols))

  out <- base_part %>%
    dplyr::left_join(extra_part, by = "pidp") %>%
    ensure_columns(c(
      "source_wave", "intdaty_dv", "intdatm_dv", "jbstat", "jbhrs",
      "jbpl", "jbwah", "sf1", "scsf1", "partner_pidp", "partner_rel",
      "hidp"
    )) %>%
    dplyr::mutate(
      wave = standardize_wave_prefix(source_wave),
      wave_order = wave_order_value(wave),
      ym = dplyr::case_when(
        !is.na(intdaty_dv) & !is.na(intdatm_dv) ~
          as.Date(sprintf("%d-%02d-01", intdaty_dv, intdatm_dv)),
        TRUE ~ as.Date(NA)
      ),
      year = suppressWarnings(as.integer(format(ym, "%Y")))
    ) %>%
    add_history_derived_variables() %>%
    dplyr::mutate(
      period = "baseline",
      source = "main_baseline",
      time_order = 2L
    ) %>%
    dplyr::select(
      pidp,
      period,
      source,
      time_order,
      wave,
      wave_order,
      ym,
      year,
      dplyr::everything()
    )

  out
}

# -----------------------------------------------------------------------------
# Prepare person-level COVID rows for the stacked plotting panel
# -----------------------------------------------------------------------------
prepare_covid_person_plot_rows <- function(df_covid_long) {
  if (is.null(df_covid_long) || nrow(df_covid_long) == 0) {
    return(tibble::tibble())
  }

  df_covid_long %>%
    dplyr::filter(wave != "2019") %>%
    dplyr::left_join(covid_wave_ym_lookup(), by = "wave") %>%
    dplyr::mutate(
      period = dplyr::if_else(wave == "baseline", "covid_baseline", "covid"),
      source = dplyr::if_else(wave == "baseline", "covid_janfeb_baseline", "covid_study"),
      time_order = dplyr::if_else(wave == "baseline", 3L, 4L),
      wave_order = dplyr::case_when(
        wave == "baseline" ~ 100L,
        wave == "ca" ~ 101L,
        wave == "cb" ~ 102L,
        wave == "cc" ~ 103L,
        wave == "cd" ~ 104L,
        wave == "ce" ~ 105L,
        wave == "cf" ~ 106L,
        wave == "cg" ~ 107L,
        wave == "ch" ~ 108L,
        wave == "ci" ~ 109L,
        TRUE ~ NA_integer_
      ),
      jbstat = sempderived,
      jbhrs = hours
    ) %>%
    dplyr::select(
      pidp,
      period,
      source,
      time_order,
      wave,
      wave_order,
      ym,
      year,
      dplyr::everything()
    )
}

# -----------------------------------------------------------------------------
# Prepare regular-wave future rows for the stacked plotting panel
# -----------------------------------------------------------------------------
prepare_future_person_plot_rows <- function(df_future_long) {
  if (is.null(df_future_long) || nrow(df_future_long) == 0) {
    return(tibble::tibble())
  }

  df_future_long %>%
    dplyr::mutate(
      period = "future",
      source = "main_future",
      time_order = 5L,
      wave = standardize_wave_prefix(wave),
      wave_order = wave_order_value(wave)
    ) %>%
    dplyr::select(
      pidp,
      period,
      source,
      time_order,
      wave,
      wave_order,
      ym,
      year,
      dplyr::everything()
    )
}

# -----------------------------------------------------------------------------
# Build person-level stacked history/baseline/COVID/future panel for plotting
# -----------------------------------------------------------------------------
build_person_history_future_long <- function(df_history_long,
                                             df_baseline,
                                             df_covid_long = NULL,
                                             df_future_long = NULL,
                                             include_covid = TRUE) {
  hist_rows <- df_history_long %>%
    dplyr::mutate(
      period = "history",
      source = "main_history",
      time_order = 1L,
      wave = standardize_wave_prefix(wave)
    ) %>%
    dplyr::select(
      pidp,
      period,
      source,
      time_order,
      wave,
      wave_order,
      ym,
      year,
      dplyr::everything()
    )

  baseline_rows <- build_baseline_person_plot_rows(df_baseline)

  pieces <- list(hist_rows, baseline_rows)

  if (include_covid && !is.null(df_covid_long)) {
    pieces <- c(pieces, list(prepare_covid_person_plot_rows(df_covid_long)))
  }

  if (!is.null(df_future_long)) {
    pieces <- c(pieces, list(prepare_future_person_plot_rows(df_future_long)))
  }

  dplyr::bind_rows(pieces) %>%
    dplyr::arrange(pidp, ym, time_order, wave_order) %>%
    dplyr::mutate(
      period = factor(
        period,
        levels = c("history", "baseline", "covid_baseline", "covid", "future")
      )
    )
}

# -----------------------------------------------------------------------------
# Convert baseline couple rows from base_*_h/base_*_w to regular suffixes
# -----------------------------------------------------------------------------
build_baseline_couple_plot_rows <- function(df_baseline_couple) {
  base_spouse_cols <- names(df_baseline_couple)[
    grepl("^base_.*_(h|w)$", names(df_baseline_couple))
  ]

  non_history_cols <- names(df_baseline_couple)[
    !startsWith(names(df_baseline_couple), "hist_")
  ]
  non_history_cols <- setdiff(non_history_cols, base_spouse_cols)

  out <- df_baseline_couple %>%
    dplyr::select(
      dplyr::all_of(non_history_cols),
      dplyr::all_of(base_spouse_cols)
    ) %>%
    dplyr::rename_with(
      ~ sub("^base_(.*)_(h|w)$", "\\1_\\2", .x),
      dplyr::all_of(base_spouse_cols)
    )

  needed <- c(
    "source_wave_h", "source_wave_w",
    "intdaty_dv_h", "intdatm_dv_h",
    "intdaty_dv_w", "intdatm_dv_w",
    "jbstat_h", "jbhrs_h", "jbpl_h", "jbwah_h", "sf1_h", "scsf1_h",
    "jbstat_w", "jbhrs_w", "jbpl_w", "jbwah_w", "sf1_w", "scsf1_w"
  )
  out <- ensure_columns(out, needed)

  tmp_wfh_h <- combine_wfh(out$jbpl_h, out$jbwah_h)
  tmp_wfh_w <- combine_wfh(out$jbpl_w, out$jbwah_w)

  out %>%
    dplyr::mutate(
      wave = dplyr::coalesce(
        standardize_wave_prefix(source_wave_h),
        standardize_wave_prefix(source_wave_w)
      ),
      wave_order = wave_order_value(wave),
      ym_h = dplyr::case_when(
        !is.na(intdaty_dv_h) & !is.na(intdatm_dv_h) ~
          as.Date(sprintf("%d-%02d-01", intdaty_dv_h, intdatm_dv_h)),
        TRUE ~ as.Date(NA)
      ),
      ym_w = dplyr::case_when(
        !is.na(intdaty_dv_w) & !is.na(intdatm_dv_w) ~
          as.Date(sprintf("%d-%02d-01", intdaty_dv_w, intdatm_dv_w)),
        TRUE ~ as.Date(NA)
      ),
      ym = dplyr::case_when(
        !is.na(ym_h) & !is.na(ym_w) & ym_h >= ym_w ~ ym_h,
        !is.na(ym_h) & !is.na(ym_w) & ym_w > ym_h ~ ym_w,
        !is.na(ym_h) ~ ym_h,
        !is.na(ym_w) ~ ym_w,
        TRUE ~ as.Date(NA)
      ),
      year = suppressWarnings(as.integer(format(ym, "%Y"))),
      any_work_h = make_any_work_future(jbstat = jbstat_h, jbhrs = jbhrs_h),
      any_work_w = make_any_work_future(jbstat = jbstat_w, jbhrs = jbhrs_w),
      wfh_code_h = tmp_wfh_h$wfh_code,
      wfh_cat_h = tmp_wfh_h$wfh_cat,
      wfh_code_w = tmp_wfh_w$wfh_code,
      wfh_cat_w = tmp_wfh_w$wfh_cat,
      wfh_some_h = make_wfh_some_future(
        jbstat = jbstat_h,
        jbhrs = jbhrs_h,
        jbpl = jbpl_h,
        jbwah = jbwah_h
      ),
      wfh_some_w = make_wfh_some_future(
        jbstat = jbstat_w,
        jbhrs = jbhrs_w,
        jbpl = jbpl_w,
        jbwah = jbwah_w
      ),
      workoutside_h = make_workoutside_future(
        jbstat = jbstat_h,
        jbhrs = jbhrs_h,
        wfh_code = wfh_code_h
      ),
      workoutside_w = make_workoutside_future(
        jbstat = jbstat_w,
        jbhrs = jbhrs_w,
        wfh_code = wfh_code_w
      ),
      period = "baseline",
      source = "main_baseline",
      time_order = 2L
    ) %>%
    dplyr::select(
      couple_id,
      husband_pidp,
      wife_pidp,
      period,
      source,
      time_order,
      wave,
      wave_order,
      ym,
      year,
      dplyr::everything()
    )
}

# -----------------------------------------------------------------------------
# Prepare couple-level COVID rows for the stacked plotting panel
# -----------------------------------------------------------------------------
prepare_covid_couple_plot_rows <- function(df_covid_couple_long) {
  if (is.null(df_covid_couple_long) || nrow(df_covid_couple_long) == 0) {
    return(tibble::tibble())
  }

  df_covid_couple_long %>%
    dplyr::filter(wave != "2019") %>%
    dplyr::left_join(covid_wave_ym_lookup(), by = "wave") %>%
    dplyr::mutate(
      period = dplyr::if_else(wave == "baseline", "covid_baseline", "covid"),
      source = dplyr::if_else(wave == "baseline", "covid_janfeb_baseline", "covid_study"),
      time_order = dplyr::if_else(wave == "baseline", 3L, 4L),
      wave_order = dplyr::case_when(
        wave == "baseline" ~ 100L,
        wave == "ca" ~ 101L,
        wave == "cb" ~ 102L,
        wave == "cc" ~ 103L,
        wave == "cd" ~ 104L,
        wave == "ce" ~ 105L,
        wave == "cf" ~ 106L,
        wave == "cg" ~ 107L,
        wave == "ch" ~ 108L,
        wave == "ci" ~ 109L,
        TRUE ~ NA_integer_
      ),
      jbstat_h = sempderived_h,
      jbhrs_h = hours_h,
      jbstat_w = sempderived_w,
      jbhrs_w = hours_w
    ) %>%
    dplyr::select(
      couple_id,
      husband_pidp,
      wife_pidp,
      period,
      source,
      time_order,
      wave,
      wave_order,
      ym,
      year,
      dplyr::everything()
    )
}

# -----------------------------------------------------------------------------
# Prepare couple-level future rows for the stacked plotting panel
# -----------------------------------------------------------------------------
prepare_future_couple_plot_rows <- function(df_future_couple_long) {
  if (is.null(df_future_couple_long) || nrow(df_future_couple_long) == 0) {
    return(tibble::tibble())
  }

  df_future_couple_long %>%
    dplyr::mutate(
      period = "future",
      source = "main_future",
      time_order = 5L,
      wave = standardize_wave_prefix(wave),
      wave_order = wave_order_value(wave)
    ) %>%
    dplyr::select(
      couple_id,
      husband_pidp,
      wife_pidp,
      period,
      source,
      time_order,
      wave,
      wave_order,
      ym,
      year,
      dplyr::everything()
    )
}

# -----------------------------------------------------------------------------
# Baseline couple variables that should be carried to every stacked row
#
# These variables are defined at the 2019/baseline-couple level and are used for
# plotting/grouping. They must therefore be attached to history rows as well as
# baseline/future/COVID rows; otherwise earlier history years are dropped by
# plotting functions when treatment_group or child-group variables are missing.
# -----------------------------------------------------------------------------
baseline_couple_stacked_group_vars <- function(df_baseline_couple) {
  candidate_vars <- names(df_baseline_couple)[
    startsWith(names(df_baseline_couple), "treat_") |
      startsWith(names(df_baseline_couple), "sample_") |
      startsWith(names(df_baseline_couple), "has_child_") |
      names(df_baseline_couple) %in% c(
        "youngest_child_2019",
        "child_age_group_2019",
        "both_in_covid"
      )
  ]

  unique(c("couple_id", candidate_vars))
}

attach_baseline_couple_stacked_group_vars <- function(df, df_baseline_couple) {
  attach_cols <- baseline_couple_stacked_group_vars(df_baseline_couple)
  attach_cols <- intersect(attach_cols, names(df_baseline_couple))

  if (!"couple_id" %in% attach_cols) {
    return(df)
  }

  # Avoid duplicate columns if a component already carries any of these variables.
  vars_to_add <- setdiff(attach_cols, c("couple_id", names(df)))

  if (length(vars_to_add) == 0) {
    return(df)
  }

  df %>%
    dplyr::left_join(
      df_baseline_couple %>%
        dplyr::select(couple_id, dplyr::all_of(vars_to_add)) %>%
        dplyr::distinct(couple_id, .keep_all = TRUE),
      by = "couple_id"
    )
}

# -----------------------------------------------------------------------------
# Build couple-level stacked history/baseline/COVID/future panel for plotting
# -----------------------------------------------------------------------------
build_couple_history_future_long <- function(df_couple_history_long,
                                             df_baseline_couple,
                                             df_covid_couple_long = NULL,
                                             df_future_couple_long = NULL,
                                             include_covid = TRUE) {
  hist_rows <- df_couple_history_long %>%
    dplyr::mutate(
      period = "history",
      source = "main_history",
      time_order = 1L,
      ym = couple_ym,
      year = couple_year,
      wave = standardize_wave_prefix(wave)
    ) %>%
    dplyr::select(
      couple_id,
      husband_pidp,
      wife_pidp,
      period,
      source,
      time_order,
      wave,
      wave_order,
      ym,
      year,
      dplyr::everything()
    ) %>%
    attach_baseline_couple_stacked_group_vars(df_baseline_couple)

  baseline_rows <- build_baseline_couple_plot_rows(df_baseline_couple) %>%
    attach_baseline_couple_stacked_group_vars(df_baseline_couple)

  pieces <- list(hist_rows, baseline_rows)

  if (include_covid && !is.null(df_covid_couple_long)) {
    pieces <- c(
      pieces,
      list(
        prepare_covid_couple_plot_rows(df_covid_couple_long) %>%
          attach_baseline_couple_stacked_group_vars(df_baseline_couple)
      )
    )
  }

  if (!is.null(df_future_couple_long)) {
    pieces <- c(
      pieces,
      list(
        prepare_future_couple_plot_rows(df_future_couple_long) %>%
          attach_baseline_couple_stacked_group_vars(df_baseline_couple)
      )
    )
  }

  dplyr::bind_rows(pieces) %>%
    dplyr::arrange(couple_id, ym, time_order, wave_order) %>%
    dplyr::mutate(
      period = factor(
        period,
        levels = c("history", "baseline", "covid_baseline", "covid", "future")
      )
    )
}
