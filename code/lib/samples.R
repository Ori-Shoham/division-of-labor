# =============================================================================
# File: code/lib/samples.R
#
# Purpose:
#   Construct analytic samples based on baseline-defined rules:
#
#   1) s2019_all: all baseline workers (base_jbstat in 1:2)
#   2) s2019_couples: same, restricted to baseline married/cohab/civil partners
#   3) s2019_covid: same as (1), restricted to those observed in COVID study
#   4) s2019_covid_couples: same as (2), restricted to those observed in COVID
#
# Couple status is fixed at baseline; later family structure changes are recorded
# in wave outcomes (endogenous).
# =============================================================================

build_samples_2019 <- function(df_baseline, pidp_in_covid = NULL) {
  
  s2019_all <- df_baseline %>% dplyr::filter(base_jbstat %in% 1:2)
  
  s2019_couples <- s2019_all %>%
    dplyr::filter(!is.na(base_partner_pidp) & base_partner_rel %in% c(1, 2, 3))
  
  if (is.null(pidp_in_covid)) {
    s2019_covid <- s2019_all[0, ]
    s2019_covid_couples <- s2019_all[0, ]
  } else {
    s2019_covid <- s2019_all %>% dplyr::filter(pidp %in% pidp_in_covid)
    s2019_covid_couples <- s2019_covid %>%
      dplyr::filter(!is.na(base_partner_pidp) & base_partner_rel %in% c(1, 2, 3))
  }
  
  list(
    s2019_all = s2019_all,
    s2019_couples = s2019_couples,
    s2019_covid = s2019_covid,
    s2019_covid_couples = s2019_covid_couples
  )
}

merge_future_wide_into_samples <- function(samples, df_future_wide) {
  lapply(samples, \(s) dplyr::left_join(s, df_future_wide, by = "pidp"))
}