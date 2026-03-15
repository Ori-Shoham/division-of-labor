# =============================================================================
# File: code/lib/family_baseline.R
#
# Purpose:
#   Build composite baseline (pre-pandemic) using UKHLS main waves I/J/K.
#   - Pull rich covariates and outcomes from INDRESP
#   - Build family structure from EGOALT + INDALL:
#       partner_pidp, partner_rel, number and ages of children, family_id
#   - Prefer wave K restricted to 2019; fill remaining pidps from wave J.
#
# Baseline variable meanings (subset):
#   pidp           : person id
#   hidp           : household id (wave-specific)
#   sex            : 1 male, 2 female (typical)
#   age_dv         : derived age
#   gor_dv         : region (derived)
#   isced11_dv     : education (derived)
#
# Employment:
#   jbstat         : labour market status (1 employed, 2 self-employed, etc.)
#   jbhas          : did paid work last week
#   jbsic07_cc     : industry (SIC 2007 condensed)
#   jbsoc10_cc     : occupation (SOC 2010 condensed)
#   jbft_dv        : full-time/part-time derived
#   jbhrs          : number of hours normally worked per week
#   jbot           : overtime hours in a normal week
#   jbsect         : public/private sector
#   jbterm1        : permanent/temporary
#   basrate        : basic hourly pay rate
#
# Income:
#   fimngrs_dv     : gross personal income per month
#   fihhmngrs_dv   : gross household income (month before interview)
#   paygu_dv       : usual gross pay per month from current job
#   fimnlabgrs_dv  : total monthly labour income gross
#
# Work location / WFH:
#   jbpl           : work location (1 home,2 employer,3 travel,4 other)
#   jbwah          : WAH frequency (1 always..4 never) if jbpl != 1
#
# Time use / caring:
#   howlng         : housework hours (normal week)
#   chcare         : childcare (often binary)
#   aidadhrs       : caring hours per week
#
# Mental health / wellbeing:
#   scghq2_dv      : GHQ-12 caseness
#   scghq1_dv      : GHQ-12 Likert score
#   sclfsato       : life satisfaction overall (1-7)
#
# General health:
#   sf1 / scsf1    : self-reported general health (routing; combine later)
#
# Timing:
#   intdatm_dv, intdaty_dv : interview month/year
# =============================================================================

build_family_data <- function(path_main, prefix) {
  
  egoalt_path <- file.path(path_main, paste0(prefix, "_egoalt.dta"))
  indall_path <- file.path(path_main, paste0(prefix, "_indall.dta"))
  
  indall <- haven::read_dta(indall_path) %>%
    dplyr::rename_with(~ stringr::str_remove(., paste0("^", prefix, "_"))) %>%
    dplyr::select(pidp, age_dv)
  
  egoalt <- haven::read_dta(egoalt_path) %>%
    dplyr::rename_with(~ stringr::str_remove(., paste0("^", prefix, "_"))) %>%
    dplyr::select(pidp, apidp, relationship_dv)
  
  # Couples: 1=Husband/Wife, 2=Partner/Cohabitee, 3=Civil partner
  couples <- egoalt %>%
    dplyr::filter(relationship_dv %in% c(1, 2, 3)) %>%
    dplyr::select(pidp, partner_pidp = apidp, partner_rel = relationship_dv) %>%
    dplyr::distinct(pidp, .keep_all = TRUE)
  
  # Children: your convention 9:12 (kept as-is)
  children <- egoalt %>%
    dplyr::filter(relationship_dv %in% 9:12) %>%
    dplyr::left_join(indall, by = c("apidp" = "pidp")) %>%
    dplyr::filter(age_dv >= 0) %>%
    dplyr::group_by(pidp) %>%
    dplyr::summarise(
      n_children = dplyr::n(),
      n_children_18_under = sum(age_dv <= 18, na.rm = TRUE),
      n_children_16_under = sum(age_dv <= 16, na.rm = TRUE),
      n_children_10_under = sum(age_dv <= 10, na.rm = TRUE),
      age_youngest_child = min(age_dv, na.rm = TRUE),
      all_children_ages = paste(sort(age_dv), collapse = ", "),
      .groups = "drop"
    )
  
  egoalt %>%
    dplyr::distinct(pidp) %>%
    dplyr::left_join(couples, by = "pidp") %>%
    dplyr::left_join(children, by = "pidp") %>%
    dplyr::mutate(
      n_children = tidyr::replace_na(n_children, 0),
      
      # family_id:
      #   - if no partner: fam_{pidp}
      #   - if partner: symmetric id fam_{min}_{max}
      family_id = dplyr::case_when(
        is.na(partner_pidp) ~ paste0("fam_", pidp),
        pidp < partner_pidp ~ paste0("fam_", pidp, "_", partner_pidp),
        TRUE ~ paste0("fam_", partner_pidp, "_", pidp)
      )
    ) %>%
    dplyr::rename_with(~ paste0(prefix, "_", .), -pidp)
}

clean_baseline_wave <- function(path_main, prefix) {
  
  file_path <- file.path(path_main, paste0(prefix, "_indresp.dta"))
  df_family <- build_family_data(path_main, prefix)
  
  cols_to_keep <- c(
    "pidp",
    paste0(prefix,"_hidp"),
    
    # Demographics
    paste0(prefix, "_sex"),
    paste0(prefix, "_age_dv"),
    paste0(prefix, "_gor_dv"),
    paste0(prefix, "_isced11_dv"),
    
    # Employment + outcomes you requested
    paste0(prefix, "_jbstat"),
    paste0(prefix, "_jbhas"),
    paste0(prefix, "_jbsic07_cc"),
    paste0(prefix, "_jbsoc10_cc"),
    paste0(prefix, "_jbft_dv"),
    paste0(prefix, "_jbhrs"),
    paste0(prefix, "_jbot"),
    paste0(prefix, "_jbsect"),
    paste0(prefix, "_jbterm1"),
    paste0(prefix, "_basrate"),
    
    # Income
    paste0(prefix, "_fimngrs_dv"),
    paste0(prefix, "_fihhmngrs_dv"),
    paste0(prefix, "_paygu_dv"),
    paste0(prefix, "_fimnlabgrs_dv"),
    
    # Work location / WFH
    paste0(prefix, "_jbpl"),
    paste0(prefix, "_jbwah"),
    
    # Family (from INDRESP)
    paste0(prefix, "_mastat_dv"),
    paste0(prefix, "_sppid"),
    paste0(prefix, "_nchild_dv"),
    paste0(prefix, "_ndepchl_dv"),
    
    # Time use / caring
    paste0(prefix, "_howlng"),
    paste0(prefix, "_chcare"),
    paste0(prefix, "_aidadhrs"),
    
    # Mental health / wellbeing
    paste0(prefix, "_scghq2_dv"),
    paste0(prefix, "_scghq1_dv"),
    paste0(prefix, "_sclfsato"),
    
    # General health
    paste0(prefix, "_sf1"),
    paste0(prefix, "_scsf1"),
    
    # Timing
    paste0(prefix, "_intdatm_dv"),
    paste0(prefix, "_intdaty_dv")
  )
  
  haven::read_dta(file_path) %>%
    dplyr::select(any_of(cols_to_keep)) %>%
    dplyr::left_join(df_family, by = "pidp") %>%
    dplyr::rename_with(~ stringr::str_remove(., paste0("^", prefix, "_")), -pidp)
}

build_baseline <- function(path_main) {
  
  df_i <- clean_baseline_wave(path_main, "i")
  df_j <- clean_baseline_wave(path_main, "j")
  df_k <- clean_baseline_wave(path_main, "k")
  
  # K baseline: keep strictly pre-2020 (your existing convention)
  df_k_2019 <- df_k %>%
    dplyr::filter(intdaty_dv < 2020) %>%
    dplyr::mutate(source_wave = "K")
  
  # J baseline: keep strictly pre-2020 (your existing convention)
  df_j_2019 <- df_j %>%
    dplyr::filter(intdaty_dv < 2020) %>%
    dplyr::mutate(source_wave = "J")
  
  # I spans 2017–2019, so REQUIRE 2019 explicitly
  df_i_2019only <- df_i %>%
    dplyr::filter(intdaty_dv == 2019) %>%
    dplyr::mutate(source_wave = "I")
  
  # Priority fill: K first, then J, then I
  df_j_fill <- df_j_2019 %>%
    dplyr::anti_join(df_k_2019, by = "pidp")
  
  df_i_fill <- df_i_2019only %>%
    dplyr::anti_join(dplyr::bind_rows(df_k_2019, df_j_fill), by = "pidp")
  
  dplyr::bind_rows(df_k_2019, df_j_fill, df_i_fill) %>%
    dplyr::rename_with(~ paste0("base_", .), -pidp) %>%
    dplyr::mutate(
      # Your education recode: merge PhD with MA; drop negative codes
      base_isced11_dv = dplyr::if_else(base_isced11_dv == 8, 7, base_isced11_dv),
      base_isced11_dv = dplyr::if_else(base_isced11_dv < 0, NA_real_, base_isced11_dv),
      
      # Harmonized baseline general health:
      # use base_scsf1 when available, otherwise base_sf1
      base_health_sf = combine_health(base_sf1, base_scsf1),
      
      base_health_sf = factor(
        dplyr::case_when(
          base_health_sf == 1 ~ "Excellent",
          base_health_sf == 2 ~ "Very good",
          base_health_sf == 3 ~ "Good",
          base_health_sf == 4 ~ "Fair",
          base_health_sf == 5 ~ "Poor",
          TRUE ~ NA_character_
        ),
        levels = c("Excellent", "Very good", "Good", "Fair", "Poor"),
        ordered = TRUE
      )    )
}