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
#   7) COVID couple-wave panel (one row per couple x wave)
#   8) future outcomes couple-long panel (one row per couple x wave)
#   9) future outcomes couple-wide file (one row per couple)
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
# Baseline person-level samples
# -----------------------------------------------------------------------------
build_samples_2019 <- function(df_baseline, pidp_in_covid = NULL) {
  
  # All baseline workers
  s2019_all <- df_baseline %>%
    dplyr::filter(base_jbstat %in% 1:2)
  
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
    dplyr::filter(
      base_jbstat %in% 1:2,
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