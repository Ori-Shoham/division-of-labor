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
#     - create COVID workoutside
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
  #  - non-missing, non-negative baseline SIC and SOC
  #  - observed in COVID: any variable with prefix "c" is non-missing
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
  
  # Synthetic pre-period rows
  df_precovid <- df_sample %>%
    dplyr::group_by(pidp) %>%
    dplyr::slice_head(n = 1) %>%
    dplyr::mutate(
      wave = "baseline",
      sempderived = blwork,
      hours = blhours,
      wah = blwah
    ) %>%
    dplyr::ungroup() %>% 
    select(pidp, wave, sempderived, hours, wah, starts_with("base"), starts_with("group"), starts_with("key"), shutdown_sec)
  
  df_2019 <- df_sample %>%
    dplyr::group_by(pidp) %>%
    dplyr::slice_head(n = 1) %>%
    dplyr::mutate(
      wave = "2019",
      sempderived = base_jbstat,
      hours = base_jbhrs
    ) %>%
    dplyr::ungroup()%>% 
    select(pidp, wave, sempderived, hours, wah, starts_with("base"), starts_with("group"), starts_with("key"), shutdown_sec)
  
  df_sample_long <- dplyr::bind_rows(df_2019, df_precovid, df_sample) %>%
    dplyr::mutate(
      # For not employed (sempderived==4), set missing hours/wah to -8 sentinel
      hours = dplyr::if_else(sempderived == 4 & is.na(hours), -8, hours),
      wah   = dplyr::if_else(sempderived == 4 & is.na(wah),   -8, wah),
      
      # Generic COVID work-from-home-some indicator
      wfh_some = make_wfh_some_covid(
        sempderived = sempderived,
        hours = hours,
        wah = wah
      ),
      
      # Generic COVID workoutside
      workoutside = make_workoutside_covid(
        sempderived = sempderived,
        hours = hours,
        wah = wah
      )
    )
  
  df_sample_long
}