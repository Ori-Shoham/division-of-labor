# =============================================================================
# File: code/lib/future_outcomes.R
#
# Purpose:
#   Load UKHLS main-wave future outcomes and family structure at each wave.
#   - Outcomes from INDRESP
#   - Family structure from EGOALT + INDALL (same construction as baseline)
#   - Keeps interview year/month
#   - Supports adding J/K main-study interviews from March 2020 onward
#
# Also computes baseline-fixed couple evolution:
#   still_with_base_partner:
#     1 if wave partner_pidp matches baseline partner_pidp
#     0 if baseline couple but link no longer present / different
#     NA if not a baseline couple
#
# Notes:
#   - Requires build_family_data() from family_baseline.R to be sourced first.
# =============================================================================

load_main_wave_indresp <- function(path_main, prefix) {
  
  f <- file.path(path_main, paste0(prefix, "_indresp.dta"))
  if (!file.exists(f)) {
    warning("Missing file: ", f)
    return(NULL)
  }
  
  cols_to_keep <- c(
    "pidp",
    paste0(prefix,"_hidp"),
    
    # Demographics
    paste0(prefix, "_sex"),
    paste0(prefix, "_age_dv"),
    paste0(prefix, "_gor_dv"),
    paste0(prefix, "_isced11_dv"),
    
    # Employment + outcomes
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
    
    # Work location / WFH branching
    paste0(prefix, "_jbpl"),
    paste0(prefix, "_jbwah"),
    
    # Family/couple (from INDRESP)
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
  
  haven::read_dta(f) %>%
    dplyr::select(any_of(cols_to_keep)) %>%
    dplyr::rename_with(~ stringr::str_remove(., paste0("^", prefix, "_")), -pidp) %>%
    dplyr::mutate(
      wave = prefix,
      # Month-year as a proper Date (first of month). Easy to plot and aggregate.
      ym = as.Date(sprintf("%d-%02d-01", intdaty_dv, intdatm_dv)),
      year = as.integer(intdaty_dv)
    )
}

load_main_wave_with_family <- function(path_main, prefix) {
  
  df <- load_main_wave_indresp(path_main, prefix)
  if (is.null(df)) return(NULL)
  
  # build_family_data() returns columns prefixed with {prefix}_
  df_family <- build_family_data(path_main, prefix) %>%
    dplyr::rename_with(~ stringr::str_remove(., paste0("^", prefix, "_")), -pidp)
  
  df %>% dplyr::left_join(df_family, by = "pidp")
}

build_future_outcomes_long <- function(path_main, future_waves, min_ym = NULL) {
  
  dfs <- lapply(future_waves, \(w) load_main_wave_with_family(path_main, w))
  dfs <- dfs[!sapply(dfs, is.null)]
  if (length(dfs) == 0) stop("No future wave files found.")
  
  out <- dplyr::bind_rows(dfs)
  
  # When J/K are included as future-outcome waves, keep only post-February-2020
  # interviews so the future-outcomes branch starts after the pre-COVID baseline.
  if (!is.null(min_ym)) {
    out <- out %>%
      dplyr::filter(!is.na(ym), ym >= min_ym)
  }
  
  out
}

add_baseline_couple_evolution <- function(df_future_long, df_baseline) {
  
  base_couples <- df_baseline %>%
    dplyr::transmute(
      pidp,
      base_partner_pidp = base_partner_pidp,
      base_partner_rel  = base_partner_rel,
      base_family_id    = base_family_id,
      base_is_couple = !is.na(base_partner_pidp) & base_partner_rel %in% c(1, 2, 3)
    )
  
  df_future_long %>%
    dplyr::left_join(base_couples, by = "pidp") %>%
    dplyr::mutate(
      still_with_base_partner = dplyr::case_when(
        base_is_couple == FALSE ~ NA_real_,
        base_is_couple == TRUE & !is.na(partner_pidp) & partner_pidp == base_partner_pidp ~ 1,
        base_is_couple == TRUE ~ 0
      )
    )
}