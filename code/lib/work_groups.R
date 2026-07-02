# =============================================================================
# File: code/lib/work_groups.R
#
# Purpose:
#   Reusable helpers for:
#     - baseline-based shutdown / key worker group definitions
#     - generic "work outside" variables for COVID and future outcomes
#
# Notes:
#   These are NOT inherently COVID-specific:
#     - groups are defined from baseline SIC/SOC
#     - workoutside logic is based on work status + work location / WFH
#
# Classification systems used:
#   SIC 2007 (condensed) - industry codes
#   SOC 2010 (condensed) - occupation codes
# =============================================================================


# -----------------------------------------------------------------------------
# Detailed key-worker group labels and helpers (shared by EUL and SL)
# -----------------------------------------------------------------------------
# EUL keeps the original three detailed labels verbatim (byte-identical output).
# SL collapses the eight ONS key-worker groups to six types, merging "Key public
# services", "Public safety and national security" and "National and Local
# Government" into one. Every label keeps the "key worker - " prefix and the
# education label keeps the "education" token, so the downstream education tests
# in samples.R / sample_tables.R work unchanged across both editions.

# The three EUL detailed labels, in display order (must match historical strings).
KW_LABELS_EUL <- c(
  "key worker - health\n and social services",
  "key worker - education",
  "key worker - public safety\n and essential gvt. services"
)

# Map each ONS group (as returned by build_keyworker_crosswalk_detailed()) to its
# collapsed SL key-worker label.
KW_ONS_TO_LABEL_SL <- c(
  "Health and social care"              = "key worker - health and social care",
  "Education and childcare"             = "key worker - education and childcare",
  "Food and necessary goods"            = "key worker - food and necessary goods",
  "Transport"                           = "key worker - transport",
  "Utilities and communication"         = "key worker - utilities and communication",
  "Key public services"                 = "key worker - public services and government",
  "Public safety and national security" = "key worker - public services and government",
  "National and Local Government"       = "key worker - public services and government"
)

# Ordered factor levels for group_industry_based_detailed, per licence. This is
# the single source of truth for category ordering; sample tables and figures use
# it instead of hardcoding the label set.
group_detailed_levels <- function(license = if (exists("DATA_LICENSE")) DATA_LICENSE else "EUL") {
  if (license == "SL") {
    c(
      "shutdown sector",
      "key worker - health and social care",
      "key worker - education and childcare",
      "key worker - food and necessary goods",
      "key worker - transport",
      "key worker - utilities and communication",
      "key worker - public services and government",
      "other",
      "missing industry / occupation"
    )
  } else {
    c(
      "shutdown sector",
      KW_LABELS_EUL,
      "other",
      "missing industry / occupation"
    )
  }
}

# Shutdown sectors from the IFS BN278 endnote (four-digit SIC 2007 CLASS codes).
# Input is the normalized 4-digit class code; returns a logical vector (no NA for
# valid numeric input).
is_ifs_shutdown_sic <- function(sic4) {
  s <- suppressWarnings(as.numeric(sic4))
  in_range <- function(x, lo, hi) !is.na(x) & x >= lo & x <= hi

  (!is.na(s) & s == 4719) |
    in_range(s, 4730, 4772) |
    in_range(s, 4776, 4799) |
    (!is.na(s) & s == 4910) |
    in_range(s, 4931, 4939) |
    (!is.na(s) & s == 5010) |
    (!is.na(s) & s == 5030) |
    (!is.na(s) & s == 5110) |
    in_range(s, 5510, 5630) |   # accommodation + food service
    in_range(s, 7911, 7990) |   # travel
    (!is.na(s) & s == 8510) |   # pre-primary education / childcare
    (!is.na(s) & s == 8891) |   # child day-care
    (in_range(s, 9001, 9329) & s != 9003) |  # arts & leisure, except artistic creation
    (in_range(s, 9601, 9609) & s != 9603) |  # personal care, except funeral activities
    (!is.na(s) & s == 9700)     # domestic services
}


# -----------------------------------------------------------------------------
# Baseline-based group definitions
# -----------------------------------------------------------------------------
# Licence-aware entry point. EUL uses the historical condensed-code logic exactly
# as before. SL redefines key-worker (with type) and shutdown groups from the
# detailed jbsic07 / jbsoc10 codes via the ONS crosswalk and the IFS shutdown
# list; pass key_crosswalk = build_keyworker_crosswalk_detailed(...).
add_baseline_work_groups <- function(df,
                                     license = if (exists("DATA_LICENSE")) DATA_LICENSE else "EUL",
                                     key_crosswalk = NULL) {

  if (license == "SL") {
    return(add_baseline_work_groups_sl(df, key_crosswalk = key_crosswalk))
  }

  df %>%
    dplyr::mutate(
      
      # Clean baseline SIC / SOC separately
      base_sic_clean = dplyr::if_else(
        !is.na(base_jbsic07_cc) & base_jbsic07_cc >= 0,
        base_jbsic07_cc,
        NA_real_
      ),
      base_soc_clean = dplyr::if_else(
        !is.na(base_jbsoc10_cc) & base_jbsoc10_cc >= 0,
        base_jbsoc10_cc,
        NA_real_
      ),
      
      has_valid_sic = !is.na(base_sic_clean),
      has_valid_soc = !is.na(base_soc_clean),
      
      # "both valid" can still be useful downstream for diagnostics
      baseline_group_info_ok = has_valid_sic & has_valid_soc,
      
      # Shutdown sector: determinable from SIC alone
      shutdown_sec = dplyr::case_when(
        has_valid_sic & base_sic_clean %in% c(55, 56, 79, 90, 91, 92, 93, 96) ~ 1,
        has_valid_sic ~ 0,
        TRUE ~ NA_real_
      ),
      
      # Health / social care
      keyworker_health_social = dplyr::case_when(
        has_valid_sic & base_sic_clean == 86 ~ 1,
        has_valid_soc & base_soc_clean %in% c(124, 221, 222, 223, 321) ~ 1,
        has_valid_soc & has_valid_sic &
          base_soc_clean == 118 & base_sic_clean %in% 86:88 ~ 1,
        has_valid_soc & has_valid_sic &
          base_soc_clean == 614 & base_sic_clean %in% c(84, 86:88) ~ 1,
        
        # unambiguous negatives from SIC alone
        has_valid_sic & base_sic_clean %in% c(86, 87, 88) ~ 0,
        has_valid_sic & base_sic_clean == 84 ~ 0,
        
        # unambiguous negatives from SOC alone
        has_valid_soc & base_soc_clean %in% c(118, 124, 221, 222, 223, 321, 614) ~ 0,
        
        # if at least one dimension observed and none of the ambiguity-triggering
        # partial cases applies, classify as 0
        (has_valid_sic | has_valid_soc) ~ 0,
        
        TRUE ~ NA_real_
      ),
      
      # Education / childcare
      keyworker_education = dplyr::case_when(
        has_valid_sic & base_sic_clean == 85 ~ 1,
        has_valid_soc & base_soc_clean == 231 ~ 1,
        has_valid_soc & has_valid_sic &
          base_soc_clean %in% c(323, 612, 623) & base_sic_clean == 84 ~ 1,
        
        # negatives where ambiguous interaction cases are ruled out
        has_valid_soc & base_soc_clean == 231 ~ 1,
        has_valid_sic & base_sic_clean == 85 ~ 1,
        has_valid_soc & base_soc_clean %in% c(323, 612, 623) & has_valid_sic ~ 0,
        has_valid_sic & base_sic_clean == 84 & has_valid_soc ~ 0,
        
        (has_valid_sic | has_valid_soc) ~ 0,
        
        TRUE ~ NA_real_
      ),
      
      # Public safety / other essential
      keyworker_public_safety = dplyr::case_when(
        has_valid_soc & base_soc_clean == 331 ~ 1,
        has_valid_sic & base_sic_clean == 53 ~ 1,
        has_valid_sic | has_valid_soc ~ 0,
        TRUE ~ NA_real_
      ),
      
      # Combined key worker indicator:
      # if any subgroup is 1 => 1
      # if all observed subgroups are 0 and none are NA => 0
      # otherwise NA
      keyworker_my_def = dplyr::case_when(
        keyworker_health_social == 1 |
          keyworker_education == 1 |
          keyworker_public_safety == 1 ~ 1,
        
        !is.na(keyworker_health_social) &
          !is.na(keyworker_education) &
          !is.na(keyworker_public_safety) &
          keyworker_health_social == 0 &
          keyworker_education == 0 &
          keyworker_public_safety == 0 ~ 0,
        
        TRUE ~ NA_real_
      ),
      
      group_self_report = dplyr::case_when(
        shutdown_sec == 1 ~ "shutdown sector",
        "keyworker" %in% names(.) && "keyworksector" %in% names(.) &&
          (keyworker == 1 | keyworksector %in% 1:8) ~ "key worker",
        is.na(shutdown_sec) ~ "missing industry / occupation",
        TRUE ~ "other"
      ),
      
      group_industry_based = dplyr::case_when(
        shutdown_sec == 1 ~ "shutdown sector",
        keyworker_my_def == 1 ~ "key worker",
        is.na(shutdown_sec) | is.na(keyworker_my_def) ~ "missing industry / occupation",
        TRUE ~ "other"
      ),
      
      group_industry_based_detailed = dplyr::case_when(
        shutdown_sec == 1 ~ "shutdown sector",
        keyworker_health_social == 1 ~ "key worker - health\n and social services",
        keyworker_education == 1 ~ "key worker - education",
        keyworker_public_safety == 1 ~ "key worker - public safety\n and essential gvt. services",
        is.na(shutdown_sec) |
          is.na(keyworker_health_social) |
          is.na(keyworker_education) |
          is.na(keyworker_public_safety) ~ "missing industry / occupation",
        TRUE ~ "other"
      )
    ) %>%
    dplyr::select(-base_sic_clean, -base_soc_clean, -has_valid_sic, -has_valid_soc)
}


# -----------------------------------------------------------------------------
# Special-Licence baseline group definitions
# -----------------------------------------------------------------------------
# Uses the detailed jbsic07 / jbsoc10 codes carried as base_jbsic07 / base_jbsoc10:
#   - key worker (and its type) from the ONS (SOC x SIC) crosswalk: a job is key
#     only if the exact occupation-by-industry combination is flagged key, and its
#     type is the collapsed ONS group.
#   - shutdown sector from the IFS BN278 four-digit SIC list.
# Emits the same output columns as the EUL branch so downstream is unchanged.
add_baseline_work_groups_sl <- function(df, key_crosswalk = NULL) {

  if (is.null(key_crosswalk)) {
    stop("add_baseline_work_groups(license = 'SL') requires key_crosswalk = ",
         "build_keyworker_crosswalk_detailed(KEYWORKER_XLSX).")
  }
  if (!all(c("base_jbsic07", "base_jbsoc10") %in% names(df))) {
    stop("SL work groups need base_jbsic07 and base_jbsoc10. Did the loader read ",
         "the detailed SIC/SOC variables (is DATA_LICENSE = 'SL' and the SN 6931 ",
         "data in path_main)?")
  }

  key_cells <- key_crosswalk %>%
    dplyr::select(SOC_4, SIC_4, any_key, occ_group)

  df %>%
    dplyr::mutate(
      base_soc4 = dplyr::if_else(
        !is.na(base_jbsoc10) & base_jbsoc10 >= 0, as.numeric(base_jbsoc10), NA_real_
      ),
      base_sic_raw = dplyr::if_else(
        !is.na(base_jbsic07) & base_jbsic07 >= 0, as.numeric(base_jbsic07), NA_real_
      ),
      # Normalize SIC to a 4-digit class. jbsic07 may be stored at 5-digit subclass
      # level (e.g. 86900 -> class 8690); genuine 4-digit classes pass through.
      base_sic4 = dplyr::case_when(
        is.na(base_sic_raw)   ~ NA_real_,
        base_sic_raw >= 10000 ~ floor(base_sic_raw / 10),
        TRUE                  ~ base_sic_raw
      ),
      has_valid_sic = !is.na(base_sic4),
      has_valid_soc = !is.na(base_soc4),
      baseline_group_info_ok = has_valid_sic & has_valid_soc
    ) %>%
    dplyr::left_join(key_cells, by = c("base_soc4" = "SOC_4", "base_sic4" = "SIC_4")) %>%
    dplyr::mutate(
      # Shutdown sector: determinable from SIC alone.
      shutdown_sec = dplyr::case_when(
        has_valid_sic & is_ifs_shutdown_sic(base_sic4) ~ 1,
        has_valid_sic ~ 0,
        TRUE ~ NA_real_
      ),

      # Key worker requires BOTH detailed codes observed (the "combination").
      # An observed combination not present as key in the crosswalk is a 0.
      keyworker_my_def = dplyr::case_when(
        !baseline_group_info_ok ~ NA_real_,
        !is.na(any_key) & any_key == 1 ~ 1,
        TRUE ~ 0
      ),

      # Collapsed detailed key-worker label from the matched ONS group.
      kw_label_sl = dplyr::if_else(
        keyworker_my_def == 1 & !is.na(occ_group),
        unname(KW_ONS_TO_LABEL_SL[occ_group]),
        NA_character_
      ),

      group_self_report = dplyr::case_when(
        shutdown_sec == 1 ~ "shutdown sector",
        "keyworker" %in% names(.) && "keyworksector" %in% names(.) &&
          (keyworker == 1 | keyworksector %in% 1:8) ~ "key worker",
        is.na(shutdown_sec) ~ "missing industry / occupation",
        TRUE ~ "other"
      ),

      group_industry_based = dplyr::case_when(
        shutdown_sec == 1 ~ "shutdown sector",
        keyworker_my_def == 1 ~ "key worker",
        is.na(shutdown_sec) | is.na(keyworker_my_def) ~ "missing industry / occupation",
        TRUE ~ "other"
      ),

      # Shutdown wins the descriptive categorical (matches EUL precedence).
      group_industry_based_detailed = dplyr::case_when(
        shutdown_sec == 1 ~ "shutdown sector",
        keyworker_my_def == 1 ~ kw_label_sl,
        is.na(shutdown_sec) | is.na(keyworker_my_def) ~ "missing industry / occupation",
        TRUE ~ "other"
      )
    ) %>%
    dplyr::select(
      -base_soc4, -base_sic_raw, -base_sic4,
      -has_valid_sic, -has_valid_soc,
      -any_key, -occ_group, -kw_label_sl
    )
}


# -----------------------------------------------------------------------------
# COVID any-work indicator
#
# Logic:
#   - missing/invalid employment -> NA
#   - not employed -> 0
#   - missing/invalid hours -> NA
#   - zero hours -> 0
#   - positive hours -> 1
# -----------------------------------------------------------------------------
make_any_work_covid <- function(sempderived, hours) {
  sempderived <- suppressWarnings(as.numeric(sempderived))
  hours <- suppressWarnings(as.numeric(hours))
  
  dplyr::case_when(
    is.na(sempderived) ~ NA_real_,
    sempderived < 0 ~ NA_real_,
    sempderived == 4 ~ 0,
    is.na(hours) ~ NA_real_,
    hours < 0 ~ NA_real_,
    hours > 0 ~ 1,
    hours == 0 ~ 0,
    TRUE ~ NA_real_
  )
}


# -----------------------------------------------------------------------------
# Future/main-wave any-work analogue
#
# The regular UKHLS waves ask whether the respondent did any paid work last
# week in JBHAS. Use that direct last-week employment question rather than
# usual weekly hours.
#
# Logic:
#   - JBHAS == 1 -> 1
#   - JBHAS == 2 -> 0
#   - missing/invalid/inapplicable survey codes -> NA
# -----------------------------------------------------------------------------
make_any_work_future <- function(jbhas) {
  jbhas <- suppressWarnings(as.numeric(jbhas))
  
  dplyr::case_when(
    is.na(jbhas) ~ NA_real_,
    jbhas < 0 ~ NA_real_,
    jbhas == 1 ~ 1,
    jbhas == 2 ~ 0,
    TRUE ~ NA_real_
  )
}


# -----------------------------------------------------------------------------
# Three-category last-week work/job status
#
# Codes:
#   1 = worked last week
#   2 = did not work last week but has a job
#   3 = did not work last week and does not have a job
# -----------------------------------------------------------------------------
make_work_last_week_status_main <- function(jbhas, jboff) {
  jbhas <- suppressWarnings(as.numeric(jbhas))
  jboff <- suppressWarnings(as.numeric(jboff))
  
  dplyr::case_when(
    is.na(jbhas) ~ NA_real_,
    jbhas < 0 ~ NA_real_,
    jbhas == 1 ~ 1,
    jbhas == 2 & !is.na(jboff) & jboff == 1 ~ 2,
    jbhas == 2 & !is.na(jboff) & jboff == 2 ~ 3,
    jbhas == 2 & (is.na(jboff) | jboff < 0) ~ NA_real_,
    TRUE ~ NA_real_
  )
}

make_work_last_week_status_covid <- function(sempderived, hours) {
  sempderived <- suppressWarnings(as.numeric(sempderived))
  hours <- suppressWarnings(as.numeric(hours))
  
  dplyr::case_when(
    is.na(sempderived) ~ NA_real_,
    sempderived < 0 ~ NA_real_,
    sempderived == 4 ~ 3,
    !(sempderived %in% c(1, 2, 3)) ~ NA_real_,
    is.na(hours) ~ NA_real_,
    hours < 0 ~ NA_real_,
    hours > 0 ~ 1,
    hours == 0 ~ 2,
    TRUE ~ NA_real_
  )
}


# -----------------------------------------------------------------------------
# COVID workoutside
#
# Logic:
#   - missing/invalid employment -> NA
#   - not working / zero hours -> 0
#   - always WFH -> 0
#   - often/sometimes/never WFH -> 1
# Key point:
#   workoutside is only defined in waves where the COVID WFH question was asked.
#   If wfh_question_available == FALSE, return NA for everyone in that wave,
#   including non-workers. This avoids defining synthetic 2019 values.
# -----------------------------------------------------------------------------
make_workoutside_covid <- function(sempderived,
                                   hours,
                                   wah,
                                   wfh_question_available = NULL) {
  sempderived <- suppressWarnings(as.numeric(sempderived))
  hours <- suppressWarnings(as.numeric(hours))
  wah <- suppressWarnings(as.numeric(wah))
  
  if (is.null(wfh_question_available)) {
    wfh_question_available <- rep(TRUE, length(sempderived))
  }
  
  wfh_question_available <- as.logical(wfh_question_available)
  
  dplyr::case_when(
    # New rule only:
    # If WFH question was not asked in this wave, workoutside is unavailable
    # for everyone in the wave.
    is.na(wfh_question_available) | !wfh_question_available ~ NA_real_,
    
    # Original individual-level logic:
    is.na(sempderived) ~ NA_real_,
    sempderived < 0 ~ NA_real_,
    is.na(hours) ~ NA_real_,
    hours <= 0 ~ 0,
    wah == 1 ~ 0,
    wah %in% 2:4 ~ 1,
    TRUE ~ NA_real_
  )
}


# -----------------------------------------------------------------------------
# COVID work-from-home-some indicator
#
# Logic:
#   - missing/invalid employment -> NA
#   - not working / zero hours -> 0
#   - always/often/sometimes WFH -> 1
#   - never WFH -> 0
# Key point:
#   wfh_some is only defined in waves where the COVID WFH question was asked.
#   If wfh_question_available == FALSE, return NA for everyone in that wave,
#   including non-workers. This avoids defining synthetic 2019 values.
# -----------------------------------------------------------------------------
make_wfh_some_covid <- function(sempderived,
                                hours,
                                wah,
                                wfh_question_available = NULL) {
  sempderived <- suppressWarnings(as.numeric(sempderived))
  hours <- suppressWarnings(as.numeric(hours))
  wah <- suppressWarnings(as.numeric(wah))
  
  if (is.null(wfh_question_available)) {
    wfh_question_available <- rep(TRUE, length(sempderived))
  }
  
  wfh_question_available <- as.logical(wfh_question_available)
  
  dplyr::case_when(
    # New rule only:
    # If WFH question was not asked in this wave, wfh_some is unavailable
    # for everyone in the wave.
    is.na(wfh_question_available) | !wfh_question_available ~ NA_real_,
    
    # Original individual-level logic:
    is.na(sempderived) ~ NA_real_,
    sempderived < 0 ~ NA_real_,
    is.na(hours) ~ NA_real_,
    hours <= 0 ~ 0,
    wah %in% 1:3 ~ 1,
    wah == 4 ~ 0,
    TRUE ~ NA_real_
  )
}


# -----------------------------------------------------------------------------
# Future/main-wave work-from-home-some indicator
#
# Inputs:
#   jbstat   : labour-market status
#              1 employed
#              2 self-employed
#
#   jbhrs    : usual weekly hours worked
#
#   jbpl   : work location
#            1 at home
#            2 employer premises
#            3 driving / travelling around
#            4 one or more other places
#   wfh_code : harmonized WFH code from combine_wfh()
#              1 always
#              2 often
#              3 sometimes
#              4 never
#
#   jbwah  : working at home frequency
#            1 always
#            2 often
#            3 sometimes
#            4 never
# Backward-compatible inputs:
#   Existing scripts may still call this function with jbpl and jbwah instead
#   of wfh_code. In that case, this function computes wfh_code internally using
#   combine_wfh(jbpl, jbwah).
#
# Logic:
#   This intentionally mirrors make_workoutside_future().
#
#   - if employment status missing/invalid -> NA
#   - if harmonized WFH code is missing -> NA
#       This is the wave/question-availability gate. If jbwah was not asked in
#       a main-study wave, combine_wfh() returns NA, so wfh_some is unavailable
#       for everyone in that wave.
#   - if not employed/self-employed -> 0
#   - if hours missing -> NA
#   - if zero or negative hours -> 0
#   - if always/often/sometimes WFH -> 1
#   - if never WFH -> 0
# -----------------------------------------------------------------------------
make_wfh_some_future <- function(jbstat,
                                 jbhrs,
                                 wfh_code = NULL,
                                 jbpl = NULL,
                                 jbwah = NULL) {
  jbstat <- suppressWarnings(as.numeric(jbstat))
  jbhrs <- suppressWarnings(as.numeric(jbhrs))
  
  if (is.null(wfh_code)) {
    if (is.null(jbpl) || is.null(jbwah)) {
      stop("make_wfh_some_future() requires either wfh_code or both jbpl and jbwah.")
    }
    
    wfh_code <- combine_wfh(jbpl, jbwah)$wfh_code
  }
  
  wfh_code <- suppressWarnings(as.numeric(wfh_code))
  
  dplyr::case_when(
    is.na(jbstat) ~ NA_real_,
    jbstat < 0 ~ NA_real_,
    is.na(wfh_code) ~ NA_real_,
    !(jbstat %in% c(1, 2)) ~ 0,
    is.na(jbhrs) ~ NA_real_,
    jbhrs <= 0 ~ 0,
    wfh_code %in% 1:3 ~ 1,
    wfh_code == 4 ~ 0,
    TRUE ~ NA_real_
  )
}


# -----------------------------------------------------------------------------
# Future/main-wave workoutside
#
# Inputs:
#   jbstat   : labour-market status
#              1 employed
#              2 self-employed
#
#   jbhrs    : usual weekly hours worked
#
#   wfh_code : harmonized WFH code from combine_wfh()
#              1 always
#              2 often
#              3 sometimes
#              4 never
#
# Logic:
#   - if employment status missing/invalid -> NA
#   - if harmonized WFH code is missing -> NA
#       This is the wave/question-availability gate.
#   - if not employed/self-employed -> 0
#   - if hours missing -> NA
#   - if zero or negative hours -> 0
#   - if always WFH -> 0
#   - if often / sometimes / never WFH -> 1
#
# This makes workoutside depend on the harmonized WFH definition rather than
# reconstructing it separately from jbpl/jbwah.
# -----------------------------------------------------------------------------
make_workoutside_future <- function(jbstat, jbhrs, wfh_code) {
  jbstat <- suppressWarnings(as.numeric(jbstat))
  jbhrs <- suppressWarnings(as.numeric(jbhrs))
  wfh_code <- suppressWarnings(as.numeric(wfh_code))
  
  dplyr::case_when(
    is.na(jbstat) ~ NA_real_,
    jbstat < 0 ~ NA_real_,
    is.na(wfh_code) ~ NA_real_,
    !(jbstat %in% c(1, 2)) ~ 0,
    is.na(jbhrs) ~ NA_real_,
    jbhrs <= 0 ~ 0,
    wfh_code == 1 ~ 0,
    wfh_code %in% 2:4 ~ 1,
    TRUE ~ NA_real_
  )
}
