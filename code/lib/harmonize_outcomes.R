# =============================================================================
# File: code/lib/harmonize_outcomes.R
#
# Purpose:
#   Harmonize outcome variables across waves where UKHLS uses branching:
#     - Work from home: combine jbpl and jbwah
#     - General health: combine sf1 and scsf1
#
# Key meanings:
#   jbpl  : Work location
#           1 At home
#           2 Employer premises
#           3 Driving/travelling around
#           4 One or more other places
#
#   jbwah : Working at home frequency (only asked if jbpl != 1)
#           1 always, 2 often, 3 sometimes, 4 never
#
#   We create:
#     - wfh_code: numeric 1..4 aligned with jbwah convention
#         If jbpl==1 (at home), we set wfh_code=1 (Always)
#     - wfh_cat: "Always"/"Often"/"Sometimes"/"Never"
#
# General health:
#   sf1/scsf1: Only one may exist for each respondent due to routing.
#   We create health_sf = scsf1 if present else sf1.
# =============================================================================

combine_wfh <- function(jbpl, jbwah) {
  
  wfh_code <- dplyr::case_when(
    !is.na(jbpl) & jbpl == 1 ~ 1,
    is.na(jbpl) | jbpl != 1  ~ as.numeric(jbwah),
    TRUE ~ NA_real_
  )
  
  wfh_cat <- dplyr::case_when(
    wfh_code == 1 ~ "Always",
    wfh_code == 2 ~ "Often",
    wfh_code == 3 ~ "Sometimes",
    wfh_code == 4 ~ "Never",
    TRUE ~ NA_character_
  )
  
  list(wfh_code = wfh_code, wfh_cat = wfh_cat)
}

combine_health <- function(sf1, scsf1) {
  out <- scsf1
  out[is.na(out)] <- sf1[is.na(out)]
  out
}