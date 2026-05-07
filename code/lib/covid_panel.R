# =============================================================================
# File: code/lib/covid_panel.R
#
# Purpose:
#   Construct the COVID long panel used in the current analysis:
#     - start from baseline (one row per pidp)
#     - merge COVID wide data by pidp
#     - add baseline-defined work groups
#     - reshape COVID waves to long panel (pidp x wave)
#     - add synthetic "2019" and "baseline" rows
#     - create COVID any-work, workoutside and WFH indicators
#
# Notes:
#   - The synthetic "2019" row uses regular-wave baseline variables.
#   - For the synthetic "2019" row, wah is intentionally set to missing,
#     because regular UKHLS waves before 2020 did not ask the COVID-style WFH
#     question.
#   - The synthetic "2019" row does include howlng, using base_howlng from the
#     regular-wave baseline composite.
#   - The synthetic "baseline" row uses Jan-Feb 2020 COVID baseline variables
#     where available: blwork, blhours, blwah. It does not have a Jan-Feb 2020
#     housework measure, so howlng is set to missing there.
# =============================================================================

build_covid_long_panel <- function(
    df_baseline,
    df_covid_wide,
    SOC,
    SIC,
    key_inds
) {
  
  # Merge baseline + COVID wide
  df_ind_panel <- df_baseline %>%
    dplyr::left_join(df_covid_wide, by = "pidp") %>%
    add_baseline_work_groups() %>%
    # Add SOC/SIC labels
    dplyr::left_join(SOC, by = c("base_jbsoc10_cc" = "SOC")) %>%
    dplyr::left_join(SIC, by = c("base_jbsic07_cc" = "SIC")) %>%
    # Add keyworker crosswalk at (SOC,SIC) condensed level
    dplyr::left_join(
      key_inds %>% dplyr::select(!c(industry, occupation)),
      by = c("base_jbsoc10_cc" = "SOC", "base_jbsic07_cc" = "SIC")
    )
  
  # Filter to current COVID analysis sample:
  #  - working in baseline main survey: base_jbstat in 1:2
  #  - observed in COVID: any variable with prefix "c" is non-missing
  #  - do NOT require valid SIC/SOC here; invalid/missing SIC/SOC are flagged
  #    by add_baseline_work_groups() and can be excluded in specific analyses
  #    where appropriate.
  df_sample <- df_ind_panel %>%
    dplyr::filter(
      base_jbstat %in% 1:2
    ) %>%
    dplyr::filter(rowSums(!is.na(dplyr::across(starts_with("c")))) > 0) %>%
    # Wide -> long by wave prefix
    tidyr::pivot_longer(
      cols = dplyr::matches("^[c][a-i]_"),
      names_to = c("wave", "var"),
      names_pattern = "^([a-z]{2})_(.*)",
      values_to = "val"
    ) %>%
    tidyr::pivot_wider(names_from = var, values_from = val) %>%
    dplyr::arrange(pidp, wave) %>%
    dplyr::group_by(pidp) %>%
    dplyr::mutate(dplyr::across(starts_with("bl"), first_valid)) %>%
    dplyr::ungroup()
  
  # Synthetic Jan-Feb 2020 baseline row from COVID baseline variables.
  # No Jan-Feb 2020 housework variable is available here, so howlng is missing.
  df_precovid <- df_sample %>%
    dplyr::group_by(pidp) %>%
    dplyr::slice_head(n = 1) %>%
    dplyr::mutate(
      wave = "baseline",
      sempderived = blwork,
      hours = blhours,
      wah = blwah,
      howlng = NA_real_,
      timechcare = NA_real_,
      husits = NA_real_
    ) %>%
    dplyr::ungroup() %>%
    dplyr::select(
      pidp,
      wave,
      sempderived,
      hours,
      wah,
      howlng,
      timechcare,
      husits,
      starts_with("base"),
      starts_with("group"),
      starts_with("key"),
      shutdown_sec
    )
  
  # Synthetic 2019 row from regular-wave baseline variables.
  # Set wah to missing because pre-2020 regular waves did not ask the relevant
  # WFH question. Keep 2019 housework hours from base_howlng.
  df_2019 <- df_sample %>%
    dplyr::group_by(pidp) %>%
    dplyr::slice_head(n = 1) %>%
    dplyr::mutate(
      wave = "2019",
      sempderived = base_jbstat,
      hours = base_jbhrs,
      wah = NA_real_,
      howlng = base_howlng,
      timechcare = NA_real_,
      husits = base_husits
    ) %>%
    dplyr::ungroup() %>%
    dplyr::select(
      pidp,
      wave,
      sempderived,
      hours,
      wah,
      howlng,
      timechcare,
      husits,
      starts_with("base"),
      starts_with("group"),
      starts_with("key"),
      shutdown_sec
    )
  
  df_sample_long <- dplyr::bind_rows(df_2019, df_precovid, df_sample) %>%
    dplyr::group_by(wave) %>%
    dplyr::mutate(
      # Wave-level availability of the COVID WFH question.
      # This must be computed before the not-employed -8 sentinel is inserted
      # into wah, so synthetic 2019 remains unavailable.
      covid_wfh_question_available = any(
        !is.na(suppressWarnings(as.numeric(wah))) &
          suppressWarnings(as.numeric(wah)) >= 0,
        na.rm = TRUE
      )
    ) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(
      # For not employed (sempderived==4), set missing hours/wah to -8 sentinel
      hours = dplyr::if_else(sempderived == 4 & is.na(hours), -8, hours),
      wah   = dplyr::if_else(sempderived == 4 & is.na(wah),   -8, wah),
      
      # Any work last week in COVID waves; analogous positive-hours measure in
      # synthetic pre-period rows.
      any_work = make_any_work_covid(
        sempderived = sempderived,
        hours = hours
      ),
      
      # Generic COVID work-from-home-some indicator
      wfh_some = make_wfh_some_covid(
        sempderived = sempderived,
        hours = hours,
        wah = wah,
        wfh_question_available = covid_wfh_question_available
      ),
      
      # Generic COVID workoutside
      workoutside = make_workoutside_covid(
        sempderived = sempderived,
        hours = hours,
        wah = wah,
        wfh_question_available = covid_wfh_question_available
      ),
      husits_raw = husits,
      husits = dplyr::case_when(
        wave == "2019" ~ clean_husits_main(husits_raw),
        wave == "baseline" ~ NA_real_,
        TRUE ~ clean_husits_covid(husits_raw, covid_code8_to_missing = TRUE)
      ),
      husits_cat = label_husits(husits)
    ) %>%
    dplyr::select(-covid_wfh_question_available)
  
  df_sample_long
}