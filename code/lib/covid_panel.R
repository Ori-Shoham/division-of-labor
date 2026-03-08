# =============================================================================
# File: code/lib/covid_panel.R
#
# Purpose:
#   Construct the COVID long panel used in the current analysis:
#     - start from baseline (one row per pidp)
#     - merge COVID wide data by pidp
#     - define sector/keyworker groups using baseline SIC/SOC and COVID self-report
#     - reshape COVID waves to long panel (pidp x wave)
#     - add synthetic "2019" and "baseline" rows:
#         wave="2019" uses base_ main survey values
#         wave="baseline" uses COVID recall bl* (Jan-Feb 2020)
#
# Output columns include:
#   - base_* (baseline covariates and labels)
#   - wave in {"2019","baseline","ca"...,"ci"}
#   - COVID outcomes: hours, sempderived, wah, furlough, etc.
#   - workoutside (derived): 1 if worked>0 and not WFH, else 0.
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
    dplyr::mutate(
      # Shutdown sector definition (based on condensed SIC):
      # 55 accommodation; 56 food/bev; 79 travel; 90 arts; 91 museums; 92 gambling;
      # 93 sports; 96 other personal services
      shutdown_sec = ifelse(base_jbsic07_cc %in% c(55, 56, 79, 90, 91, 92, 93, 96), 1, 0),
      
      # Key worker definitions (baseline SIC/SOC rules)
      keyworker_health_social =
        dplyr::if_else(
          base_jbsic07_cc == 86 |
            base_jbsoc10_cc %in% c(124, 221, 222, 223, 321) |
            (base_jbsoc10_cc == 118 & base_jbsic07_cc %in% 86:88) |
            (base_jbsoc10_cc == 614 & base_jbsic07_cc %in% c(84, 86:88)),
          1, 0
        ),
      keyworker_education =
        dplyr::if_else(
          base_jbsic07_cc %in% c(85) |
            (base_jbsoc10_cc %in% c(323, 612, 623) & base_jbsic07_cc == 84) |
            (base_jbsoc10_cc == 231),
          1, 0
        ),
      keyworker_public_safety = dplyr::if_else(base_jbsoc10_cc == 331, 1, 0),
      keyworker_my_def = pmax(keyworker_health_social, keyworker_education, keyworker_public_safety)
    ) %>%
    # Add SOC/SIC labels
    dplyr::left_join(SOC, by = c("base_jbsoc10_cc" = "SOC")) %>%
    dplyr::left_join(SIC, by = c("base_jbsic07_cc" = "SIC")) %>%
    # Add keyworker crosswalk at (SOC,SIC) condensed level
    dplyr::left_join(key_inds %>% dplyr::select(!c(industry, occupation)),
                     by = c("base_jbsoc10_cc" = "SOC", "base_jbsic07_cc" = "SIC"))
  
  # Filter to current COVID analysis sample:
  #  - working in baseline main survey: base_jbstat in 1:2
  #  - observed in COVID: any variable with prefix "c" is non-missing
  df_sample <- df_ind_panel %>%
    dplyr::filter(base_jbstat %in% 1:2) %>%
    dplyr::filter(rowSums(!is.na(dplyr::across(starts_with("c")))) > 0) %>%
    # Wide -> long by wave prefix
    tidyr::pivot_longer(
      cols = dplyr::matches("^[c][a-i]_"),
      names_to = c("wave", "var"),
      names_pattern = "^([a-z]{2})_(.*)",
      values_to = "val"
    ) %>%
    tidyr::pivot_wider(names_from = var, values_from = val) %>%
    dplyr::mutate(
      group_self_report = dplyr::case_when(
        shutdown_sec == 1 ~ "shutdown sector",
        keyworker == 1 | keyworksector %in% 1:8 ~ "key worker",
        TRUE ~ "other"
      ),
      group_industry_based = dplyr::case_when(
        shutdown_sec == 1 ~ "shutdown sector",
        keyworker_my_def == 1 ~ "key worker",
        TRUE ~ "other"
      ),
      group_industry_based_detailed = dplyr::case_when(
        shutdown_sec == 1 ~ "shutdown sector",
        keyworker_health_social == 1 ~ "key worker - health and social services",
        keyworker_education == 1 ~ "key worker - education",
        keyworker_public_safety == 1 ~ "key worker - public safety",
        TRUE ~ "other"
      )
    ) %>%
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
    dplyr::ungroup()
  
  df_2019 <- df_sample %>%
    dplyr::group_by(pidp) %>%
    dplyr::slice_head(n = 1) %>%
    dplyr::mutate(
      wave = "2019",
      sempderived = base_jbstat,
      hours = base_jbhrs
    ) %>%
    dplyr::ungroup()
  
  df_sample_long <- dplyr::bind_rows(df_2019, df_precovid, df_sample) %>%
    dplyr::mutate(
      # For not employed (sempderived==4), set missing hours/wah to -8 sentinel
      hours = dplyr::if_else(sempderived == 4 & is.na(hours), -8, hours),
      wah   = dplyr::if_else(sempderived == 4 & is.na(wah),   -8, wah),
      
      # workoutside:
      #   1 if worked >0 hours AND not WFH always (wah in 2:4)
      #   0 if hours<=0 or wah==1
      #   NA if missing / invalid
      workoutside = dplyr::case_when(
        sempderived < 0 ~ NA_real_,
        hours <= 0 ~ 0,
        wah == 1 ~ 0,
        wah %in% 2:4 ~ 1,
        TRUE ~ NA_real_
      )
    )
  
  df_sample_long
}