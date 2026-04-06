# =============================================================================
# File: code/lib/samples.R
#
# Purpose:
#   Construct analytic samples based on baseline-defined rules:
#
#   Person-level samples:
#   1) s2019_all: all baseline workers (base_jbstat in 1:2)
#   2) s2019_couples: same, restricted to baseline married/cohab/civil partners
#   3) s2019_covid: same as (1), restricted to those observed in COVID study
#   4) s2019_covid_couples: same as (2), restricted to those observed in COVID
#
#   Couple-level helpers:
#   5) baseline heterosexual couple roster (one row per couple)
#   6) rich baseline couple-level dataset (one row per couple, spouse vars side by side)
#   7) couple-level baseline treatment / child subgroup variables
#   8) COVID couple-wave panel (one row per couple x wave)
#   9) future outcomes couple-long panel (one row per couple x wave)
#   10) future outcomes couple-wide file (one row per couple)
#
# Notes:
#   - Couple status is fixed at baseline; later family structure changes are
#     recorded in outcomes rather than used to redefine the sample.
#   - Heterosexual couple restriction is imposed at baseline using base_sex:
#       1 = male, 2 = female
# =============================================================================

suppressPackageStartupMessages({
  library(tidyverse)
})

# -----------------------------------------------------------------------------
# Internal helper: valid baseline analytic sample
#
# Rules:
#   - employed / self-employed at baseline
#   - non-missing baseline industry and occupation
#   - negative SIC / SOC codes are treated as missing
# -----------------------------------------------------------------------------
filter_valid_baseline_workers <- function(df) {
  df %>%
    dplyr::filter(
      base_jbstat %in% 1:2
    )
}

# -----------------------------------------------------------------------------
# Baseline person-level samples
# -----------------------------------------------------------------------------
build_samples_2019 <- function(df_baseline, pidp_in_covid = NULL) {
  
  # All baseline workers with valid baseline industry + occupation
  s2019_all <- df_baseline %>%
    filter_valid_baseline_workers()
  
  # Baseline couples among baseline workers
  s2019_couples <- s2019_all %>%
    dplyr::filter(
      !is.na(base_partner_pidp) &
        base_partner_rel %in% c(1, 2, 3)
    )
  
  # COVID-observed subsamples
  if (is.null(pidp_in_covid)) {
    s2019_covid <- s2019_all[0, ]
    s2019_covid_couples <- s2019_all[0, ]
  } else {
    s2019_covid <- s2019_all %>%
      dplyr::filter(pidp %in% pidp_in_covid)
    
    s2019_covid_couples <- s2019_covid %>%
      dplyr::filter(
        !is.na(base_partner_pidp) &
          base_partner_rel %in% c(1, 2, 3)
      )
  }
  
  list(
    s2019_all = s2019_all,
    s2019_couples = s2019_couples,
    s2019_covid = s2019_covid,
    s2019_covid_couples = s2019_covid_couples
  )
}

# -----------------------------------------------------------------------------
# Merge future wide outcomes into a list of person-level samples
# -----------------------------------------------------------------------------
merge_future_wide_into_samples <- function(samples, df_future_wide) {
  lapply(samples, function(s) {
    dplyr::left_join(s, df_future_wide, by = "pidp")
  })
}

# -----------------------------------------------------------------------------
# Build a baseline heterosexual couple roster
#
# Output:
#   one row per couple with:
#     - couple_id
#     - husband_pidp
#     - wife_pidp
#     - optional COVID participation flags
#
# Rules:
#   - baseline worker sample only (base_jbstat in 1:2)
#   - must have a baseline partner
#   - must have reciprocal partner link
#   - one partner male and the other female
# -----------------------------------------------------------------------------
build_baseline_couple_roster <- function(df_baseline, pidp_in_covid = NULL) {
  
  # Keep baseline-eligible individuals with a baseline partner link
  x <- df_baseline %>%
    filter_valid_baseline_workers() %>%
    dplyr::filter(
      !is.na(base_partner_pidp),
      base_partner_rel %in% c(1, 2, 3)
    ) %>%
    dplyr::select(
      pidp,
      starts_with("base_"),
      source_wave = dplyr::any_of("base_source_wave")
    )
  
  # Create partner version for self-join
  x_partner <- x %>%
    dplyr::rename(partner_pidp = pidp) %>%
    dplyr::rename_with(
      ~ paste0("partner_", .x),
      -partner_pidp
    )
  
  # Join each person to their baseline partner record
  pairs <- x %>%
    dplyr::left_join(
      x_partner,
      by = c("base_partner_pidp" = "partner_pidp")
    ) %>%
    dplyr::filter(
      # reciprocal partner link
      partner_base_partner_pidp == pidp,
      
      # both sexes observed
      !is.na(base_sex),
      !is.na(partner_base_sex),
      
      # opposite-sex restriction
      (base_sex == 1 & partner_base_sex == 2) |
        (base_sex == 2 & partner_base_sex == 1)
    ) %>%
    dplyr::mutate(
      # Prefer symmetric family id if available; otherwise build one canonically
      couple_id = dplyr::coalesce(
        base_family_id,
        partner_base_family_id,
        ifelse(
          pidp < base_partner_pidp,
          paste0("fam_", pidp, "_", base_partner_pidp),
          paste0("fam_", base_partner_pidp, "_", pidp)
        )
      )
    )
  
  # Assign husband/wife based on baseline sex and deduplicate to one row per couple
  roster <- pairs %>%
    dplyr::transmute(
      couple_id,
      husband_pidp = dplyr::if_else(base_sex == 1, pidp, base_partner_pidp),
      wife_pidp    = dplyr::if_else(base_sex == 2, pidp, base_partner_pidp)
    ) %>%
    dplyr::distinct(couple_id, .keep_all = TRUE)
  
  # Add optional COVID participation flags
  if (!is.null(pidp_in_covid)) {
    roster <- roster %>%
      dplyr::mutate(
        husband_in_covid = husband_pidp %in% pidp_in_covid,
        wife_in_covid    = wife_pidp %in% pidp_in_covid,
        both_in_covid    = husband_in_covid & wife_in_covid
      )
  }
  
  # Sanity checks
  stopifnot(!anyDuplicated(roster$couple_id))
  stopifnot(!anyDuplicated(roster$husband_pidp))
  stopifnot(!anyDuplicated(roster$wife_pidp))
  
  roster
}

# -----------------------------------------------------------------------------
# Build rich baseline couple-level dataset
#
# Input:
#   df_baseline: one row per person
#   roster: one row per couple
#
# Output:
#   one row per couple with husband and wife baseline vars side by side
#
# Notes:
#   - All husband variables get suffix _h
#   - All wife variables get suffix _w
#   - Keeps roster flags (e.g. both_in_covid) if present
# -----------------------------------------------------------------------------
build_baseline_couple_dataset <- function(df_baseline, roster) {
  
  husband <- df_baseline %>%
    dplyr::rename(husband_pidp = pidp) %>%
    dplyr::rename_with(
      ~ ifelse(.x == "husband_pidp", .x, paste0(.x, "_h"))
    )
  
  wife <- df_baseline %>%
    dplyr::rename(wife_pidp = pidp) %>%
    dplyr::rename_with(
      ~ ifelse(.x == "wife_pidp", .x, paste0(.x, "_w"))
    )
  
  roster %>%
    dplyr::left_join(husband, by = "husband_pidp") %>%
    dplyr::left_join(wife, by = "wife_pidp")
}

# -----------------------------------------------------------------------------
# Add baseline couple-level treatment variables and child subgroup flags
#
# Output adds:
#   - youngest_child_2019
#   - has_child_u10_2019
#   - has_child_11_17_2019
#   - child_age_group_2019
#   - treat_wife_key_notedu_husb_not_or_edu
#   - treat_wife_key_notedu_any
#   - treat_husb_shutdown_wife_not
#
# Definitions:
#   treat_wife_key_notedu_husb_not_or_edu = 1 if
#     wife is a key worker outside education
#     AND husband is either:
#       (i) not a key worker, OR
#       (ii) a key worker in education
#
#   treat_wife_key_notedu_any = 1 if
#     wife is a key worker outside education, regardless of husband
#
#   treat_husb_shutdown_wife_not = 1 if
#     husband is in shutdown and wife is not
#
# Missingness rule:
#   Return NA only when the needed baseline group info is missing.
# -----------------------------------------------------------------------------
add_couple_baseline_treatments <- function(df_couple) {
  
  df_couple %>%
    dplyr::mutate(
      
      # ---- Couple child-age measure -----------------------------------------
      youngest_child_2019 = dplyr::coalesce(
        suppressWarnings(as.numeric(base_age_youngest_child_w)),
        suppressWarnings(as.numeric(base_age_youngest_child_h))
      ),
      
      has_child_u10_2019 = dplyr::case_when(
        is.na(youngest_child_2019) ~ FALSE,
        youngest_child_2019 < 0    ~ FALSE,
        youngest_child_2019 <= 10  ~ TRUE,
        TRUE                       ~ FALSE
      ),
      
      has_child_11_17_2019 = dplyr::case_when(
        is.na(youngest_child_2019) ~ FALSE,
        youngest_child_2019 < 11   ~ FALSE,
        youngest_child_2019 < 18   ~ TRUE,
        TRUE                       ~ FALSE
      ),
      
      child_age_group_2019 = dplyr::case_when(
        is.na(youngest_child_2019) | youngest_child_2019 < 0 ~ "no child / unknown",
        youngest_child_2019 <= 10                            ~ "youngest child 0-10",
        youngest_child_2019 < 18                             ~ "youngest child 11-17",
        TRUE                                                 ~ "youngest child 18+"
      ),
      
      child_age_group_2019 = factor(
        child_age_group_2019,
        levels = c(
          "youngest child 0-10",
          "youngest child 11-17",
          "youngest child 18+",
          "no child / unknown"
        )
      ),
      
      # ---- Baseline group observability -------------------------------------
      wife_group3_ok = !is.na(group_industry_based_w),
      husb_group3_ok = !is.na(group_industry_based_h),
      
      wife_group5_ok = !is.na(group_industry_based_detailed_w),
      husb_group5_ok = !is.na(group_industry_based_detailed_h),
      
      # ---- Shutdown ----------------------------------------------------------
      wife_is_shutdown = dplyr::case_when(
        !wife_group3_ok ~ NA,
        group_industry_based_w == "shutdown sector" ~ TRUE,
        TRUE ~ FALSE
      ),
      
      husb_is_shutdown = dplyr::case_when(
        !husb_group3_ok ~ NA,
        group_industry_based_h == "shutdown sector" ~ TRUE,
        TRUE ~ FALSE
      ),
      
      # ---- Any key worker ----------------------------------------------------
      wife_is_keyworker_any = dplyr::case_when(
        !wife_group3_ok ~ NA,
        group_industry_based_w == "key worker" ~ TRUE,
        TRUE ~ FALSE
      ),
      
      husb_is_keyworker_any = dplyr::case_when(
        !husb_group3_ok ~ NA,
        group_industry_based_h == "key worker" ~ TRUE,
        TRUE ~ FALSE
      ),
      
      # ---- Detailed key-worker groups ---------------------------------------
      wife_is_keyworker_education = dplyr::case_when(
        !wife_group5_ok ~ NA,
        group_industry_based_detailed_w == "key worker - education" ~ TRUE,
        TRUE ~ FALSE
      ),
      
      husb_is_keyworker_education = dplyr::case_when(
        !husb_group5_ok ~ NA,
        group_industry_based_detailed_h == "key worker - education" ~ TRUE,
        TRUE ~ FALSE
      ),
      
      wife_is_keyworker_nonedu = dplyr::case_when(
        !wife_group5_ok ~ NA,
        group_industry_based_detailed_w %in% c(
          "key worker - health\n and social services",
          "key worker - public safety\n and essential gvt. services"
        ) ~ TRUE,
        TRUE ~ FALSE
      ),
      
      # ---- Treatment 1a ------------------------------------------------------
      # Wife key worker outside education; husband either not key worker
      # or only education key worker.
      treat_wife_key_notedu_husb_not_or_edu = dplyr::case_when(
        is.na(wife_is_keyworker_nonedu) |
          is.na(husb_is_keyworker_any) |
          is.na(husb_is_keyworker_education) ~ NA_real_,
        
        wife_is_keyworker_nonedu &
          (!husb_is_keyworker_any | husb_is_keyworker_education) ~ 1,
        
        TRUE ~ 0
      ),
      
      # ---- Treatment 1b (robustness) ----------------------------------------
      # Wife key worker outside education, regardless of husband.
      treat_wife_key_notedu_any = dplyr::case_when(
        is.na(wife_is_keyworker_nonedu) ~ NA_real_,
        wife_is_keyworker_nonedu        ~ 1,
        TRUE                            ~ 0
      ),
      
      # ---- Treatment 2 -------------------------------------------------------
      # Husband shutdown; wife not shutdown.
      treat_husb_shutdown_wife_not = dplyr::case_when(
        is.na(husb_is_shutdown) | is.na(wife_is_shutdown) ~ NA_real_,
        husb_is_shutdown & !wife_is_shutdown              ~ 1,
        TRUE                                              ~ 0
      )
    ) 
}

# -----------------------------------------------------------------------------
# Build COVID couple-level long panel
#
# Input:
#   df_covid_long: one row per person x wave
#   roster: one row per couple
#
# Output:
#   one row per couple x wave
#
# Notes:
#   - Keeps only waves where both spouses are observed in the same wave
#   - All husband variables get suffix _h
#   - All wife variables get suffix _w
# -----------------------------------------------------------------------------
build_covid_couple_long <- function(df_covid_long, roster) {
  
  husband <- df_covid_long %>%
    dplyr::rename(husband_pidp = pidp) %>%
    dplyr::rename_with(
      ~ ifelse(.x %in% c("husband_pidp", "wave"),
               .x,
               paste0(.x, "_h"))
    )
  
  wife <- df_covid_long %>%
    dplyr::rename(wife_pidp = pidp) %>%
    dplyr::rename_with(
      ~ ifelse(.x %in% c("wife_pidp", "wave"),
               .x,
               paste0(.x, "_w"))
    )
  
  roster %>%
    dplyr::inner_join(husband, by = "husband_pidp") %>%
    dplyr::inner_join(wife, by = c("wife_pidp", "wave")) %>%
    dplyr::arrange(couple_id, wave)
}

# -----------------------------------------------------------------------------
# Build future-outcomes couple-level long panel
#
# Input:
#   df_future_long: one row per person x wave
#   roster: one row per couple
#
# Output:
#   one row per couple x wave
#
# Notes:
#   - Keeps only waves where both spouses are observed in the same wave
#   - All husband variables get suffix _h
#   - All wife variables get suffix _w
#   - Mirrors build_covid_couple_long()
# -----------------------------------------------------------------------------
build_future_couple_long <- function(df_future_long, roster) {
  
  husband <- df_future_long %>%
    dplyr::rename(husband_pidp = pidp) %>%
    dplyr::rename_with(
      ~ ifelse(.x %in% c("husband_pidp", "wave", "ym", "year", "intdaty_dv", "intdatm_dv"),
               .x,
               paste0(.x, "_h"))
    )
  
  wife <- df_future_long %>%
    dplyr::rename(wife_pidp = pidp) %>%
    dplyr::rename_with(
      ~ ifelse(.x %in% c("wife_pidp", "wave", "ym", "year", "intdaty_dv", "intdatm_dv"),
               .x,
               paste0(.x, "_w"))
    )
  
  roster %>%
    dplyr::inner_join(husband, by = "husband_pidp") %>%
    dplyr::inner_join(
      wife,
      by = c(
        "wife_pidp",
        "wave",
        "ym",
        "year",
        "intdaty_dv",
        "intdatm_dv"
      )
    ) %>%
    dplyr::arrange(couple_id, ym)
}

# -----------------------------------------------------------------------------
# Build future-outcomes couple-wide dataset
#
# Input:
#   df_future_wide: one row per individual
#   roster: one row per couple
#
# Output:
#   one row per couple, with husband and wife variables side by side
# -----------------------------------------------------------------------------
build_future_couple_wide <- function(df_future_wide, roster) {
  
  husband <- df_future_wide %>%
    dplyr::rename(husband_pidp = pidp) %>%
    dplyr::rename_with(
      ~ ifelse(.x == "husband_pidp",
               .x,
               paste0(.x, "_h"))
    )
  
  wife <- df_future_wide %>%
    dplyr::rename(wife_pidp = pidp) %>%
    dplyr::rename_with(
      ~ ifelse(.x == "wife_pidp",
               .x,
               paste0(.x, "_w"))
    )
  
  roster %>%
    dplyr::left_join(husband, by = "husband_pidp") %>%
    dplyr::left_join(wife, by = "wife_pidp")
}