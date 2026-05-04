# =============================================================================
# File: code/lib/event_study_regressions.R
#
# Purpose:
#   Helpers for regression/event-study analogs of the couple-treatment figures.
#
# Design:
#   - Runs spouse-specific event studies.
#   - Estimates separately by baseline child-age category.
#   - Plots younger/older child-group estimates in the same figure.
#   - Supports main-study and COVID-study panels.
#   - Supports no-controls and baseline-demographic-controls versions.
#   - Supports no couple fixed effects and couple fixed effects.
#   - Clusters standard errors at pidp level.
#
# First-round controlled specification:
#   Controls:
#     - baseline age of wife and husband
#     - categorical baseline education of wife and husband
#     - number of children under 18 at baseline
#     - number of children under 10 at baseline
#     - baseline region
# Key outcome-cleaning rule:
#   Event-study outcomes are cleaned to mirror the descriptive plots.
#
# Not included in first-round controls:
#     - baseline work hours
#     - baseline pay
#   Main-study / history-future:
#     - jbhrs, paygu_dv, fimnlabgrs_dv, fimngrs_dv are zeroed for non-workers.
#     - working people with missing outcome remain NA.
#     - missing employment status remains NA.
#
#   COVID:
#     - non-binary outcomes are cleaned for negative missing codes.
#     - they are not zeroed by work status.
#
# Required package:
#   fixest
# =============================================================================

suppressPackageStartupMessages({
  library(tidyverse)
  library(scales)
})

# =============================================================================
# Package checks
# =============================================================================

check_event_study_packages <- function() {
  if (!requireNamespace("fixest", quietly = TRUE)) {
    stop(
      "Package 'fixest' is required for event-study regressions. ",
      "Install it with install.packages('fixest')."
    )
  }
  invisible(TRUE)
}

# =============================================================================
# General helpers
# =============================================================================

clean_numeric_nonnegative <- function(x) {
  if (requireNamespace("haven", quietly = TRUE)) {
    x <- haven::zap_labels(x)
  }
  x <- suppressWarnings(as.numeric(x))
  dplyr::if_else(!is.na(x) & x >= 0, x, NA_real_)
}

clean_binary_01 <- function(x) {
  x <- suppressWarnings(as.numeric(x))
  dplyr::case_when(
    is.na(x) ~ NA_real_,
    x == 1 ~ 1,
    x == 0 ~ 0,
    x %in% TRUE ~ 1,
    x %in% FALSE ~ 0,
    TRUE ~ NA_real_
  )
}

clean_factor_nonnegative <- function(x, prefix = "cat") {
  if (requireNamespace("haven", quietly = TRUE)) {
    x <- haven::zap_labels(x)
  }
  x <- suppressWarnings(as.numeric(x))
  x <- dplyr::case_when(
    is.na(x) ~ NA_real_,
    x < 0 ~ NA_real_,
    TRUE ~ x
  )
  factor(
    dplyr::case_when(
      is.na(x) ~ NA_character_,
      TRUE ~ paste0(prefix, "_", x)
    )
  )
}

row_coalesce_numeric <- function(x, y) {
  x <- clean_numeric_nonnegative(x)
  y <- clean_numeric_nonnegative(y)
  
  dplyr::case_when(
    !is.na(x) ~ x,
    !is.na(y) ~ y,
    TRUE ~ NA_real_
  )
}

row_max_numeric <- function(x, y) {
  x <- clean_numeric_nonnegative(x)
  y <- clean_numeric_nonnegative(y)
  
  dplyr::case_when(
    !is.na(x) & !is.na(y) ~ pmax(x, y),
    !is.na(x) ~ x,
    !is.na(y) ~ y,
    TRUE ~ NA_real_
  )
}

sanitize_name <- function(x) {
  x %>%
    stringr::str_replace_all("[^A-Za-z0-9]+", "_") %>%
    stringr::str_replace_all("_+", "_") %>%
    stringr::str_replace_all("^_|_$", "")
}

# =============================================================================
# Outcome cleaning for event-study regressions
# =============================================================================

event_study_zero_if_not_working_outcomes <- function() {
  c(
    "jbhrs",
    "jbot",
    "basrate",
    "paygu_dv",
    "fimnlabgrs_dv",
    "fimngrs_dv"
  )
}

event_study_binary_outcomes <- function() {
  c(
    "any_work",
    "workoutside",
    "wfh_some",
    "husits_wife_main_both"
  )
}

event_study_clean_negative_codes <- function(x) {
  if (requireNamespace("haven", quietly = TRUE)) {
    x <- haven::zap_labels(x)
  }
  
  x <- suppressWarnings(as.numeric(x))
  x[x %in% c(-9, -8, -7, -2, -1)] <- NA_real_
  x
}

event_study_clean_binary <- function(x) {
  x <- event_study_clean_negative_codes(x)
  
  dplyr::case_when(
    is.na(x) ~ NA_real_,
    x == 1 ~ 1,
    x == 0 ~ 0,
    TRUE ~ NA_real_
  )
}

event_study_clean_main_outcome <- function(df, outcome) {
  stopifnot(outcome %in% names(df))
  
  x <- event_study_clean_negative_codes(df[[outcome]])
  
  if (outcome %in% event_study_binary_outcomes()) {
    return(event_study_clean_binary(df[[outcome]]))
  }
  
  if (outcome %in% event_study_zero_if_not_working_outcomes()) {
    if (!("jbstat" %in% names(df))) {
      stop(
        "Outcome '", outcome, "' requires spouse-level jbstat for ",
        "event-study cleaning, but jbstat is not in the event-study panel."
      )
    }
    
    emp <- event_study_clean_negative_codes(df$jbstat)
    
    x <- dplyr::case_when(
      is.na(emp) ~ NA_real_,
      !(emp %in% c(1, 2)) ~ 0,
      TRUE ~ x
    )
  }
  
  x
}

event_study_clean_covid_outcome <- function(df, outcome) {
  stopifnot(outcome %in% names(df))
  
  if (outcome %in% event_study_binary_outcomes()) {
    return(event_study_clean_binary(df[[outcome]]))
  }
  
  # Mirrors COVID descriptive plots:
  # non-binary COVID outcomes are cleaned by dropping negative missing codes.
  # They are not zeroed by work status.
  event_study_clean_negative_codes(df[[outcome]])
}

event_study_clean_outcome <- function(df,
                                      outcome,
                                      study = c("main", "covid")) {
  study <- match.arg(study)
  
  if (study == "main") {
    return(event_study_clean_main_outcome(df, outcome))
  }
  
  if (study == "covid") {
    return(event_study_clean_covid_outcome(df, outcome))
  }
  
  stop("Unknown study: ", study)
}

# =============================================================================
# Baseline controls and treatment variables
# =============================================================================

make_event_baseline_controls <- function(df_baseline_couple) {
  df_baseline_couple %>%
    dplyr::transmute(
      couple_id,
      husband_pidp,
      wife_pidp,
      
      # Treatment variables
      treat_wife_key_notedu_husb_not_or_edu =
        clean_binary_01(treat_wife_key_notedu_husb_not_or_edu),
      treat_wife_key_notedu_any =
        clean_binary_01(treat_wife_key_notedu_any),
      treat_husb_shutdown_wife_not =
        clean_binary_01(treat_husb_shutdown_wife_not),
      
      # Treatment-sample restriction used in robustness versions
      sample_husb_notkey_or_edu = dplyr::case_when(
        is.na(sample_husb_notkey_or_edu) ~ NA,
        sample_husb_notkey_or_edu %in% TRUE ~ TRUE,
        sample_husb_notkey_or_edu %in% FALSE ~ FALSE,
        suppressWarnings(as.numeric(sample_husb_notkey_or_edu)) == 1 ~ TRUE,
        suppressWarnings(as.numeric(sample_husb_notkey_or_edu)) == 0 ~ FALSE,
        TRUE ~ NA
      ),
      
      # Baseline child groups
      has_child_u10_2019 = dplyr::case_when(
        is.na(has_child_u10_2019) ~ FALSE,
        has_child_u10_2019 %in% TRUE ~ TRUE,
        suppressWarnings(as.numeric(has_child_u10_2019)) == 1 ~ TRUE,
        TRUE ~ FALSE
      ),
      has_child_11_17_2019 = dplyr::case_when(
        is.na(has_child_11_17_2019) ~ FALSE,
        has_child_11_17_2019 %in% TRUE ~ TRUE,
        suppressWarnings(as.numeric(has_child_11_17_2019)) == 1 ~ TRUE,
        TRUE ~ FALSE
      ),
      
      # Baseline controls: both spouses' age
      base_age_dv_h = clean_numeric_nonnegative(base_age_dv_h),
      base_age_dv_w = clean_numeric_nonnegative(base_age_dv_w),
      
      # Baseline controls: both spouses' categorical education
      base_isced11_cat_h = clean_factor_nonnegative(base_isced11_dv_h, "isced"),
      base_isced11_cat_w = clean_factor_nonnegative(base_isced11_dv_w, "isced"),
      
      # Baseline controls: children
      n_children_under18_baseline = row_max_numeric(
        base_n_children_18_under_h,
        base_n_children_18_under_w
      ),
      n_children_under10_baseline = row_max_numeric(
        base_n_children_10_under_h,
        base_n_children_10_under_w
      ),
      
      # Baseline controls: region.
      # In most couples this should be identical for both spouses, but coalesce
      # keeps the code robust.
      base_region = clean_factor_nonnegative(
        row_coalesce_numeric(base_gor_dv_h, base_gor_dv_w),
        "region"
      )
    ) %>%
    dplyr::distinct(couple_id, .keep_all = TRUE)
}

attach_event_baseline_controls <- function(df, df_baseline_couple) {
  controls <- make_event_baseline_controls(df_baseline_couple)
  
  # Avoid duplicate-name problems by replacing any stale copies in df.
  drop_cols <- setdiff(names(controls), "couple_id")
  
  df %>%
    dplyr::select(-dplyr::any_of(drop_cols)) %>%
    dplyr::left_join(controls, by = "couple_id")
}

# =============================================================================
# husits event-study binary outcome
# =============================================================================

add_husits_wife_main_both <- function(df,
                                      out_var = "husits_wife_main_both") {
  if (all(c("husits_wife_main_w", "husits_wife_main_h") %in% names(df))) {
    wife_says_wife_main <- clean_binary_01(df$husits_wife_main_w)
    husband_says_wife_main <- clean_binary_01(df$husits_wife_main_h)
    
    df[[out_var]] <- dplyr::case_when(
      !is.na(wife_says_wife_main) & !is.na(husband_says_wife_main) ~
        as.numeric(wife_says_wife_main == 1 & husband_says_wife_main == 1),
      TRUE ~ NA_real_
    )
    
    return(df)
  }
  
  if (all(c("husits_w", "husits_h") %in% names(df))) {
    husits_w <- suppressWarnings(as.numeric(df$husits_w))
    husits_h <- suppressWarnings(as.numeric(df$husits_h))
    
    husits_w <- dplyr::case_when(
      is.na(husits_w) ~ NA_real_,
      husits_w < 0 ~ NA_real_,
      husits_w %in% c(1, 2, 3, 4) ~ husits_w,
      TRUE ~ NA_real_
    )
    
    husits_h <- dplyr::case_when(
      is.na(husits_h) ~ NA_real_,
      husits_h < 0 ~ NA_real_,
      husits_h %in% c(1, 2, 3, 4) ~ husits_h,
      TRUE ~ NA_real_
    )
    
    wife_says_wife_main <- dplyr::case_when(
      is.na(husits_w) ~ NA_real_,
      husits_w == 1 ~ 1,
      husits_w %in% c(2, 3, 4) ~ 0,
      TRUE ~ NA_real_
    )
    
    husband_says_wife_main <- dplyr::case_when(
      is.na(husits_h) ~ NA_real_,
      husits_h == 2 ~ 1,
      husits_h %in% c(1, 3, 4) ~ 0,
      TRUE ~ NA_real_
    )
    
    df[[out_var]] <- dplyr::case_when(
      !is.na(wife_says_wife_main) & !is.na(husband_says_wife_main) ~
        as.numeric(wife_says_wife_main == 1 & husband_says_wife_main == 1),
      TRUE ~ NA_real_
    )
    
    return(df)
  }
  
  df[[out_var]] <- NA_real_
  df
}

# =============================================================================
# Study-specific time variables
# =============================================================================

add_main_event_time <- function(df,
                                reference_year = 2019,
                                exclude_jan_feb_2020 = TRUE,
                                drop_2025 = TRUE) {
  out <- df %>%
    dplyr::mutate(
      ym = as.Date(ym),
      year = suppressWarnings(as.integer(year)),
      month = suppressWarnings(as.integer(format(ym, "%m")))
    )
  
  # Match yearly descriptive figures: exclude Jan-Feb 2020 from 2020 points.
  if (exclude_jan_feb_2020) {
    out <- out %>%
      dplyr::filter(!(year == 2020 & !is.na(month) & month <= 2))
  }
  
  # Match yearly descriptive figures: drop 2025.
  if (drop_2025) {
    out <- out %>%
      dplyr::filter(year != 2025 | is.na(year))
  }
  
  out %>%
    dplyr::mutate(
      event_time = year - reference_year,
      event_label = as.character(year),
      reference_event_time = 0
    )
}

covid_wave_order_lookup <- function() {
  tibble::tibble(
    wave = c(
      "2019",
      "baseline",
      "ca",
      "cb",
      "cc",
      "cd",
      "ce",
      "cf",
      "cg",
      "ch",
      "ci"
    ),
    covid_event_time = c(-1, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9),
    covid_event_label = c(
      "2019",
      "Jan-Feb 2020",
      "Apr 2020",
      "May 2020",
      "Jun 2020",
      "Jul 2020",
      "Sep 2020",
      "Nov 2020",
      "Jan 2021",
      "Mar 2021",
      "Sep 2021"
    )
  )
}

add_covid_event_time <- function(df) {
  df %>%
    dplyr::mutate(wave = as.character(wave)) %>%
    dplyr::left_join(covid_wave_order_lookup(), by = "wave") %>%
    dplyr::mutate(
      event_time = covid_event_time,
      event_label = covid_event_label
    ) %>%
    dplyr::select(-covid_event_time, -covid_event_label)
}

choose_covid_reference_event_time <- function(df,
                                              outcome,
                                              treatment_var) {
  outcome_clean <- event_study_clean_outcome(
    df = df,
    outcome = outcome,
    study = "covid"
  )
  
  dd <- df %>%
    dplyr::mutate(outcome_clean = outcome_clean)
  
  ref_2019_available <- dd %>%
    dplyr::filter(
      event_time == -1,
      !is.na(outcome_clean),
      !is.na(.data[[treatment_var]])
    ) %>%
    nrow() > 0
  
  if (ref_2019_available) return(-1)
  
  ref_baseline_available <- dd %>%
    dplyr::filter(
      event_time == 0,
      !is.na(outcome_clean),
      !is.na(.data[[treatment_var]])
    ) %>%
    nrow() > 0
  
  if (ref_baseline_available) return(0)
  
  NA_real_
}

# =============================================================================
# Spouse-long construction
# =============================================================================

make_spouse_event_panel <- function(df,
                                    outcomes,
                                    study = c("main", "covid")) {
  study <- match.arg(study)
  
  common_cols <- names(df)[
    !stringr::str_detect(names(df), "_h$|_w$")
  ]
  
  common_cols <- union(
    common_cols,
    c(
      "couple_id",
      "husband_pidp",
      "wife_pidp",
      "wave",
      "year",
      "ym",
      "event_time",
      "event_label"
    )
  )
  common_cols <- intersect(common_cols, names(df))
  
  aux_vars <- c(
    "jbstat",
    "sempderived"
  )
  
  make_one_spouse <- function(suffix, spouse_label, pidp_var) {
    pidp_vec <- if (pidp_var %in% names(df)) df[[pidp_var]] else NA_real_
    
    out <- df %>%
      dplyr::select(dplyr::all_of(common_cols)) %>%
      dplyr::mutate(
        spouse = spouse_label,
        pidp = pidp_vec
      )
    
    for (vv in union(outcomes, aux_vars)) {
      suffixed_var <- paste0(vv, "_", suffix)
      
      if (suffixed_var %in% names(df)) {
        out[[vv]] <- df[[suffixed_var]]
      } else if (vv %in% names(df)) {
        out[[vv]] <- df[[vv]]
      } else if (!(vv %in% names(out))) {
        out[[vv]] <- NA_real_
      }
    }
    
    out
  }
  
  dplyr::bind_rows(
    make_one_spouse("w", "wife", "wife_pidp"),
    make_one_spouse("h", "husband", "husband_pidp")
  ) %>%
    dplyr::filter(!is.na(pidp))
}

# =============================================================================
# Sample restrictions
# =============================================================================

filter_child_group <- function(df,
                               child_group = c("u10", "11_17")) {
  child_group <- match.arg(child_group)
  
  if (child_group == "u10") {
    return(df %>% dplyr::filter(has_child_u10_2019))
  }
  
  if (child_group == "11_17") {
    return(df %>% dplyr::filter(has_child_11_17_2019))
  }
  
  df
}

filter_treatment_sample <- function(df,
                                    treatment_var,
                                    sample_variant = c("all", "husb_notkey_or_edu")) {
  sample_variant <- match.arg(sample_variant)
  
  out <- df
  
  if (sample_variant == "husb_notkey_or_edu") {
    if (!("sample_husb_notkey_or_edu" %in% names(out))) {
      return(out %>% dplyr::slice(0))
    }
    
    out <- out %>%
      dplyr::filter(sample_husb_notkey_or_edu %in% TRUE)
  }
  
  out %>%
    dplyr::filter(
      !is.na(.data[[treatment_var]]),
      .data[[treatment_var]] %in% c(0, 1)
    ) %>%
    dplyr::mutate(
      treated = as.numeric(.data[[treatment_var]])
    )
}

# =============================================================================
# Regression specification
# =============================================================================

event_study_control_terms <- function() {
  c(
    "base_age_dv_h",
    "base_age_dv_w",
    "factor(base_isced11_cat_h)",
    "factor(base_isced11_cat_w)",
    "n_children_under18_baseline",
    "n_children_under10_baseline",
    "factor(base_region)"
  )
}

build_event_study_formula <- function(outcome,
                                      controls = c("none", "baseline"),
                                      couple_fe = FALSE,
                                      ref_event_time = 0) {
  controls <- match.arg(controls)
  
  rhs_terms <- c(
    "treated",
    paste0("i(event_time, treated, ref = ", ref_event_time, ")")
  )
  
  if (controls == "baseline") {
    rhs_terms <- c(rhs_terms, event_study_control_terms())
  }
  
  fixed_effects <- if (couple_fe) {
    "event_time + couple_id"
  } else {
    "event_time"
  }
  
  if (couple_fe) {
    # treated is time-invariant and collinear with couple FE.
    rhs_terms <- setdiff(rhs_terms, "treated")
  }
  
  stats::as.formula(
    paste0(
      outcome,
      " ~ ",
      paste(rhs_terms, collapse = " + "),
      " | ",
      fixed_effects
    )
  )
}

estimate_event_study <- function(df,
                                 outcome,
                                 treatment_var,
                                 ref_event_time,
                                 controls = c("none", "baseline"),
                                 couple_fe = FALSE,
                                 study = c("main", "covid")) {
  check_event_study_packages()
  
  controls <- match.arg(controls)
  study <- match.arg(study)
  
  outcome_value <- event_study_clean_outcome(
    df = df,
    outcome = outcome,
    study = study
  )
  
  dd <- df %>%
    dplyr::mutate(outcome_value = outcome_value) %>%
    dplyr::filter(
      !is.na(outcome_value),
      !is.na(event_time),
      !is.na(treated),
      !is.na(pidp)
    ) %>%
    dplyr::mutate(
      "{outcome}" := outcome_value
    )
  
  if (nrow(dd) == 0) return(NULL)
  if (dplyr::n_distinct(dd$treated) < 2) return(NULL)
  if (!any(dd$event_time == ref_event_time)) return(NULL)
  
  fml <- build_event_study_formula(
    outcome = outcome,
    controls = controls,
    couple_fe = couple_fe,
    ref_event_time = ref_event_time
  )
  
  mod <- tryCatch(
    fixest::feols(
      fml = fml,
      data = dd,
      cluster = ~ pidp,
      notes = FALSE,
      warn = FALSE
    ),
    error = function(e) NULL
  )
  
  mod
}

extract_event_study_coefs <- function(model,
                                      ref_event_time,
                                      study = c("main", "covid")) {
  study <- match.arg(study)
  
  if (is.null(model)) {
    return(tibble::tibble())
  }
  
  ct <- as.data.frame(fixest::coeftable(model))
  ct$term <- rownames(ct)
  
  names(ct) <- names(ct) %>%
    stringr::str_replace_all("Std\\. Error", "std_error") %>%
    stringr::str_replace_all("t value", "statistic") %>%
    stringr::str_replace_all("Pr\\(>\\|t\\|\\)", "p_value") %>%
    stringr::str_replace_all("Estimate", "estimate")
  
  out <- ct %>%
    tibble::as_tibble() %>%
    dplyr::filter(stringr::str_detect(term, "^event_time::")) %>%
    dplyr::mutate(
      event_time_chr = stringr::str_match(
        term,
        "^event_time::([^:]+):treated$"
      )[, 2],
      event_time = suppressWarnings(as.numeric(event_time_chr)),
      conf_low = estimate - 1.96 * std_error,
      conf_high = estimate + 1.96 * std_error
    ) %>%
    dplyr::filter(!is.na(event_time)) %>%
    dplyr::select(
      event_time,
      estimate,
      std_error,
      statistic,
      p_value,
      conf_low,
      conf_high,
      term
    )
  
  ref_row <- tibble::tibble(
    event_time = ref_event_time,
    estimate = 0,
    std_error = NA_real_,
    statistic = NA_real_,
    p_value = NA_real_,
    conf_low = 0,
    conf_high = 0,
    term = "reference"
  )
  
  dplyr::bind_rows(out, ref_row) %>%
    dplyr::arrange(event_time)
}

# =============================================================================
# Labels
# =============================================================================

event_study_outcome_label <- function(outcome) {
  labels <- c(
    workoutside = "Works outside home",
    wfh_some = "Works from home at least sometimes",
    any_work = "Any work last week",
    hours = "Weekly hours worked",
    jbhrs = "Weekly hours worked",
    howlng = "Housework hours",
    timechcare = "Childcare hours",
    husits_wife_main_both = "Both spouses report wife mainly responsible for childcare",
    paygu_dv = "Usual gross pay",
    fimnlabgrs_dv = "Gross monthly labour pay",
    fimngrs_dv = "Gross monthly income"
  )
  
  if (outcome %in% names(labels)) labels[[outcome]] else outcome
}

treatment_label <- function(treatment_var) {
  labels <- c(
    treat_wife_key_notedu_husb_not_or_edu =
      "Wife key worker non-education; husband not key / education only",
    treat_wife_key_notedu_any =
      "Wife key worker non-education",
    treat_husb_shutdown_wife_not =
      "Husband shutdown sector; wife not"
  )
  
  if (treatment_var %in% names(labels)) labels[[treatment_var]] else treatment_var
}

child_group_label <- function(child_group) {
  dplyr::case_when(
    child_group == "u10" ~ "Youngest child 0--10",
    child_group == "11_17" ~ "Youngest child 11--17",
    TRUE ~ child_group
  )
}

# =============================================================================
# Short file/folder names
# =============================================================================

event_study_treatment_slug <- function(treatment_var) {
  dplyr::case_when(
    treatment_var == "treat_wife_key_notedu_husb_not_or_edu" ~ "wife_key_husb_notedu",
    treatment_var == "treat_wife_key_notedu_any" ~ "wife_key_any",
    treatment_var == "treat_husb_shutdown_wife_not" ~ "husb_shutdown",
    TRUE ~ sanitize_name(treatment_var)
  )
}

event_study_sample_slug <- function(sample_variant) {
  dplyr::case_when(
    sample_variant == "all" ~ "all",
    sample_variant == "husb_notkey_or_edu" ~ "hne",
    TRUE ~ sanitize_name(sample_variant)
  )
}

event_study_controls_slug <- function(controls) {
  dplyr::case_when(
    controls == "none" ~ "noc",
    controls == "baseline" ~ "bctrl",
    TRUE ~ sanitize_name(controls)
  )
}

event_study_spouse_slug <- function(spouse) {
  dplyr::case_when(
    spouse == "wife" ~ "w",
    spouse == "husband" ~ "h",
    TRUE ~ sanitize_name(spouse)
  )
}

event_study_fe_slug <- function(couple_fe) {
  ifelse(couple_fe, "cfe", "nfe")
}

make_event_study_file_stem <- function(study,
                                       outcome,
                                       treatment_var,
                                       child_group,
                                       spouse,
                                       controls,
                                       sample_variant,
                                       couple_fe) {
  paste(
    "es",
    study,
    sanitize_name(outcome),
    event_study_treatment_slug(treatment_var),
    child_group,
    event_study_spouse_slug(spouse),
    event_study_controls_slug(controls),
    event_study_sample_slug(sample_variant),
    event_study_fe_slug(couple_fe),
    sep = "_"
  ) %>%
    sanitize_name()
}

safe_save_rds <- function(object, file) {
  dir.create(dirname(file), showWarnings = FALSE, recursive = TRUE)
  
  ok <- tryCatch(
    {
      saveRDS(object, file)
      TRUE
    },
    error = function(e) {
      warning("Could not save RDS file: ", file, "\nReason: ", conditionMessage(e))
      FALSE
    }
  )
  
  invisible(ok)
}

safe_write_csv <- function(object, file) {
  dir.create(dirname(file), showWarnings = FALSE, recursive = TRUE)
  
  ok <- tryCatch(
    {
      readr::write_csv(object, file)
      TRUE
    },
    error = function(e) {
      warning("Could not write CSV file: ", file, "\nReason: ", conditionMessage(e))
      FALSE
    }
  )
  
  invisible(ok)
}

# =============================================================================
# Plotting
# =============================================================================

plot_event_study <- function(coefs,
                             study,
                             outcome,
                             treatment_var,
                             spouse,
                             child_group,
                             controls,
                             sample_variant,
                             couple_fe = FALSE,
                             ref_event_time = 0) {
  if (is.null(coefs) || nrow(coefs) == 0) {
    return(NULL)
  }
  
  caption_text <- paste(
    paste0("Treatment: ", treatment_label(treatment_var)),
    paste0("Controls: ", controls),
    paste0("Sample: ", sample_variant),
    paste0("Study: ", study),
    ifelse(couple_fe, "Couple FE: yes", "Couple FE: no"),
    sep = " | "
  )
  
  p <- ggplot2::ggplot(
    coefs,
    ggplot2::aes(x = event_time, y = estimate)
  ) +
    ggplot2::geom_hline(yintercept = 0, linetype = "dashed") +
    ggplot2::geom_vline(xintercept = ref_event_time, linetype = "dotted") +
    ggplot2::geom_errorbar(
      ggplot2::aes(ymin = conf_low, ymax = conf_high),
      width = 0.15,
      na.rm = TRUE
    ) +
    ggplot2::geom_point(size = 2.4, na.rm = TRUE) +
    ggplot2::geom_line(na.rm = TRUE) +
    ggplot2::labs(
      x = NULL,
      y = "Treatment-control difference relative to reference",
      title = event_study_outcome_label(outcome),
      subtitle = paste(
        stringr::str_to_title(spouse),
        "|",
        child_group_label(child_group)
      ),
      caption = caption_text
    ) +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(
      plot.subtitle = ggplot2::element_text(size = 11),
      plot.caption = ggplot2::element_text(
        size = 8,
        hjust = 0,
        color = "grey35",
        margin = ggplot2::margin(t = 8)
      ),
      axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)
    )
  
  if (study == "main") {
    main_breaks <- sort(unique(coefs$event_time))
    
    p <- p +
      ggplot2::scale_x_continuous(
        breaks = main_breaks,
        labels = main_breaks + 2019,
        minor_breaks = NULL
      ) +
      ggplot2::labs(x = "Calendar year")
  }
  
  if (study == "covid") {
    covid_axis <- covid_wave_order_lookup() %>%
      dplyr::filter(covid_event_time %in% sort(unique(coefs$event_time))) %>%
      dplyr::arrange(covid_event_time)
    
    p <- p +
      ggplot2::scale_x_continuous(
        breaks = covid_axis$covid_event_time,
        labels = covid_axis$covid_event_label,
        minor_breaks = NULL
      ) +
      ggplot2::labs(x = "COVID-study wave")
  }
  
  p
}

plot_event_study_child_groups <- function(coefs,
                                          study,
                                          outcome,
                                          treatment_var,
                                          spouse,
                                          controls,
                                          sample_variant,
                                          couple_fe = FALSE,
                                          ref_event_time = 0) {
  if (is.null(coefs) || nrow(coefs) == 0) {
    return(NULL)
  }
  
  coefs <- coefs %>%
    dplyr::filter(
      !is.na(event_time),
      child_group %in% c("u10", "11_17")
    ) %>%
    dplyr::mutate(
      child_group_plot = dplyr::case_when(
        child_group == "u10" ~ "Youngest child 0--10",
        child_group == "11_17" ~ "Youngest child 11--17",
        TRUE ~ child_group
      ),
      child_group_plot = factor(
        child_group_plot,
        levels = c("Youngest child 0--10", "Youngest child 11--17")
      )
    )
  
  if (nrow(coefs) == 0) {
    return(NULL)
  }
  
  caption_text <- paste(
    paste0("Treatment: ", treatment_label(treatment_var)),
    paste0("Controls: ", controls),
    paste0("Sample: ", sample_variant),
    paste0("Study: ", study),
    ifelse(couple_fe, "Couple FE: yes", "Couple FE: no"),
    sep = " | "
  )
  
  dodge <- ggplot2::position_dodge(width = 0.35)
  
  p <- ggplot2::ggplot(
    coefs,
    ggplot2::aes(
      x = event_time,
      y = estimate,
      color = child_group_plot,
      group = child_group_plot
    )
  ) +
    ggplot2::geom_hline(yintercept = 0, linetype = "dashed") +
    ggplot2::geom_vline(xintercept = ref_event_time, linetype = "dotted") +
    ggplot2::geom_errorbar(
      ggplot2::aes(ymin = conf_low, ymax = conf_high),
      width = 0.15,
      position = dodge,
      na.rm = TRUE
    ) +
    ggplot2::geom_point(
      size = 2.4,
      position = dodge,
      na.rm = TRUE
    ) +
    ggplot2::geom_line(
      position = dodge,
      na.rm = TRUE
    ) +
    ggplot2::labs(
      x = NULL,
      y = "Treatment-control difference relative to reference",
      color = NULL,
      title = event_study_outcome_label(outcome),
      subtitle = stringr::str_to_title(spouse),
      caption = caption_text
    ) +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(
      legend.position = "bottom",
      plot.subtitle = ggplot2::element_text(size = 11),
      plot.caption = ggplot2::element_text(
        size = 8,
        hjust = 0,
        color = "grey35",
        margin = ggplot2::margin(t = 8)
      ),
      axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)
    )
  
  if (study == "main") {
    main_breaks <- sort(unique(coefs$event_time))
    
    p <- p +
      ggplot2::scale_x_continuous(
        breaks = main_breaks,
        labels = main_breaks + 2019,
        minor_breaks = NULL
      ) +
      ggplot2::labs(x = "Calendar year")
  }
  
  if (study == "covid") {
    covid_axis <- covid_wave_order_lookup() %>%
      dplyr::filter(covid_event_time %in% sort(unique(coefs$event_time))) %>%
      dplyr::arrange(covid_event_time)
    
    p <- p +
      ggplot2::scale_x_continuous(
        breaks = covid_axis$covid_event_time,
        labels = covid_axis$covid_event_label,
        minor_breaks = NULL
      ) +
      ggplot2::labs(x = "COVID-study wave")
  }
  
  p
}

save_combined_child_group_event_study_plots <- function(results,
                                                        fig_dir) {
  if (is.null(results) || nrow(results) == 0) {
    return(invisible(NULL))
  }
  
  dir.create(fig_dir, showWarnings = FALSE, recursive = TRUE)
  
  plot_specs <- results %>%
    dplyr::distinct(
      study,
      outcome,
      treatment_var,
      spouse,
      controls,
      sample_variant,
      couple_fe,
      ref_event_time
    )
  
  purrr::pwalk(
    plot_specs,
    function(study,
             outcome,
             treatment_var,
             spouse,
             controls,
             sample_variant,
             couple_fe,
             ref_event_time) {
      
      dd <- results %>%
        dplyr::filter(
          study == !!study,
          outcome == !!outcome,
          treatment_var == !!treatment_var,
          spouse == !!spouse,
          controls == !!controls,
          sample_variant == !!sample_variant,
          couple_fe == !!couple_fe,
          ref_event_time == !!ref_event_time
        )
      
      if (nrow(dd) == 0) {
        return(NULL)
      }
      
      p <- plot_event_study_child_groups(
        coefs = dd,
        study = study,
        outcome = outcome,
        treatment_var = treatment_var,
        spouse = spouse,
        controls = controls,
        sample_variant = sample_variant,
        couple_fe = couple_fe,
        ref_event_time = ref_event_time
      )
      
      if (is.null(p)) {
        return(NULL)
      }
      
      stem <- paste(
        "es",
        study,
        sanitize_name(outcome),
        event_study_treatment_slug(treatment_var),
        event_study_spouse_slug(spouse),
        event_study_controls_slug(controls),
        event_study_sample_slug(sample_variant),
        event_study_fe_slug(couple_fe),
        "childgroups",
        sep = "_"
      ) %>%
        sanitize_name()
      
      ggplot2::ggsave(
        filename = file.path(fig_dir, paste0(stem, ".png")),
        plot = p,
        width = 9,
        height = 6
      )
      
      invisible(NULL)
    }
  )
  
  invisible(NULL)
}

# =============================================================================
# One regression task
# =============================================================================

run_one_event_study_task <- function(df_spouse,
                                     study,
                                     outcome,
                                     treatment_var,
                                     spouse,
                                     child_group,
                                     controls,
                                     sample_variant,
                                     ref_event_time,
                                     couple_fe,
                                     fig_dir,
                                     results_dir,
                                     save_model = TRUE,
                                     save_individual_child_plot = FALSE) {
  dd <- df_spouse %>%
    dplyr::filter(spouse == !!spouse) %>%
    filter_child_group(child_group) %>%
    filter_treatment_sample(
      treatment_var = treatment_var,
      sample_variant = sample_variant
    )
  
  if (nrow(dd) == 0) {
    return(tibble::tibble())
  }
  
  if (study == "covid" && is.na(ref_event_time)) {
    ref_event_time <- choose_covid_reference_event_time(
      df = dd,
      outcome = outcome,
      treatment_var = treatment_var
    )
  }
  
  if (is.na(ref_event_time)) {
    return(tibble::tibble())
  }
  
  model <- estimate_event_study(
    df = dd,
    outcome = outcome,
    treatment_var = treatment_var,
    ref_event_time = ref_event_time,
    controls = controls,
    couple_fe = couple_fe,
    study = study
  )
  
  if (is.null(model)) {
    return(tibble::tibble())
  }
  
  coefs <- extract_event_study_coefs(
    model = model,
    ref_event_time = ref_event_time,
    study = study
  ) %>%
    dplyr::mutate(
      study = study,
      outcome = outcome,
      treatment_var = treatment_var,
      treatment_slug = event_study_treatment_slug(treatment_var),
      spouse = spouse,
      child_group = child_group,
      controls = controls,
      sample_variant = sample_variant,
      couple_fe = couple_fe,
      ref_event_time = ref_event_time,
      n_obs = stats::nobs(model),
      n_pidp = dplyr::n_distinct(dd$pidp),
      n_couples = dplyr::n_distinct(dd$couple_id)
    )
  
  stem <- make_event_study_file_stem(
    study = study,
    outcome = outcome,
    treatment_var = treatment_var,
    child_group = child_group,
    spouse = spouse,
    controls = controls,
    sample_variant = sample_variant,
    couple_fe = couple_fe
  )
  
  if (save_model) {
    safe_save_rds(
      model,
      file.path(results_dir, paste0(stem, "_model.rds"))
    )
  }
  
  safe_write_csv(
    coefs,
    file.path(results_dir, paste0(stem, "_coefs.csv"))
  )
  
  safe_save_rds(
    coefs,
    file.path(results_dir, paste0(stem, "_coefs.rds"))
  )
  
  if (isTRUE(save_individual_child_plot)) {
    p <- plot_event_study(
      coefs = coefs,
      study = study,
      outcome = outcome,
      treatment_var = treatment_var,
      spouse = spouse,
      child_group = child_group,
      controls = controls,
      sample_variant = sample_variant,
      couple_fe = couple_fe,
      ref_event_time = ref_event_time
    )
    
    if (!is.null(p)) {
      dir.create(fig_dir, showWarnings = FALSE, recursive = TRUE)
      
      ggplot2::ggsave(
        filename = file.path(fig_dir, paste0(stem, ".png")),
        plot = p,
        width = 9,
        height = 6
      )
    }
  }
  
  coefs
}

# =============================================================================
# Batch runner
# =============================================================================

run_event_study_batch <- function(df_spouse,
                                  study,
                                  outcomes,
                                  treatment_vars,
                                  child_groups = c("u10", "11_17"),
                                  spouses = c("wife", "husband"),
                                  controls_set = c("none", "baseline"),
                                  couple_fe = FALSE,
                                  fig_dir,
                                  results_dir,
                                  wife_treatment_extra_restriction_vars =
                                    c(
                                      "treat_wife_key_notedu_husb_not_or_edu",
                                      "treat_wife_key_notedu_any"
                                    ),
                                  save_model = TRUE,
                                  save_individual_child_plots = FALSE,
                                  save_combined_child_plots = TRUE) {
  dir.create(fig_dir, showWarnings = FALSE, recursive = TRUE)
  dir.create(results_dir, showWarnings = FALSE, recursive = TRUE)
  
  all_results <- list()
  ii <- 0L
  
  for (outcome in outcomes) {
    if (!(outcome %in% names(df_spouse))) next
    if (all(is.na(df_spouse[[outcome]]))) next
    
    for (tr in treatment_vars) {
      if (!(tr %in% names(df_spouse))) next
      
      sample_variants <- "all"
      if (tr %in% wife_treatment_extra_restriction_vars) {
        sample_variants <- c("all", "husb_notkey_or_edu")
      }
      
      for (sample_variant in sample_variants) {
        for (child_group in child_groups) {
          for (sp in spouses) {
            for (cc in controls_set) {
              
              ref_event_time <- if (study == "main") 0 else NA_real_
              
              ii <- ii + 1L
              
              all_results[[ii]] <- run_one_event_study_task(
                df_spouse = df_spouse,
                study = study,
                outcome = outcome,
                treatment_var = tr,
                spouse = sp,
                child_group = child_group,
                controls = cc,
                sample_variant = sample_variant,
                ref_event_time = ref_event_time,
                couple_fe = couple_fe,
                fig_dir = fig_dir,
                results_dir = results_dir,
                save_model = save_model,
                save_individual_child_plot = save_individual_child_plots
              )
            }
          }
        }
      }
    }
  }
  
  out <- dplyr::bind_rows(all_results)
  
  safe_write_csv(
    out,
    file.path(results_dir, paste0("event_study_", study, "_all_coefficients.csv"))
  )
  
  safe_save_rds(
    out,
    file.path(results_dir, paste0("event_study_", study, "_all_coefficients.rds"))
  )
  
  if (isTRUE(save_combined_child_plots)) {
    save_combined_child_group_event_study_plots(
      results = out,
      fig_dir = fig_dir
    )
  }
  
  out
}

# =============================================================================
# Duplicate pidp-year diagnostics
# =============================================================================

diagnose_pidp_year_duplicates <- function(df_spouse,
                                          outcomes = NULL) {
  base <- df_spouse %>%
    dplyr::filter(!is.na(pidp), !is.na(year)) %>%
    dplyr::count(pidp, year, name = "n_obs") %>%
    dplyr::summarise(
      n_pidp_year = dplyr::n(),
      n_pidp_year_multiple = sum(n_obs > 1),
      share_pidp_year_multiple = mean(n_obs > 1),
      max_obs_in_pidp_year = max(n_obs),
      .groups = "drop"
    )
  
  if (is.null(outcomes)) {
    return(base)
  }
  
  by_outcome <- purrr::map_dfr(outcomes, function(vv) {
    if (!(vv %in% names(df_spouse))) {
      return(tibble::tibble())
    }
    
    df_spouse %>%
      dplyr::filter(!is.na(pidp), !is.na(year), !is.na(.data[[vv]])) %>%
      dplyr::count(pidp, year, name = "n_obs") %>%
      dplyr::summarise(
        outcome = vv,
        n_pidp_year = dplyr::n(),
        n_pidp_year_multiple = sum(n_obs > 1),
        share_pidp_year_multiple = mean(n_obs > 1),
        max_obs_in_pidp_year = max(n_obs),
        .groups = "drop"
      )
  })
  
  list(overall = base, by_outcome = by_outcome)
}