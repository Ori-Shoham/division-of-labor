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
#   SIC 2007 (condensed) – industry codes
#   SOC 2010 (condensed) – occupation codes
# =============================================================================


# -----------------------------------------------------------------------------
# Baseline-based group definitions
# -----------------------------------------------------------------------------
add_baseline_work_groups <- function(df) {
  
  df %>%
    dplyr::mutate(
      
      # -----------------------------------------------------------------------
      # Shutdown sector definition
      #
      # Based on sectors that were forced to close during lockdown
      # (following IFS classification).
      #
      # SIC 2007 codes:
      # 55  Accommodation
      # 56  Food and beverage service activities
      # 79  Travel agency, tour operator and reservation services
      # 90  Creative, arts and entertainment activities
      # 91  Libraries, archives, museums and other cultural activities
      # 92  Gambling and betting activities
      # 93  Sports activities and amusement and recreation activities
      # 96  Other personal service activities (e.g. hairdressing, beauty)
      #
      # These sectors experienced the strongest legal restrictions during
      # early COVID lockdowns and therefore represent "shutdown sectors".
      # -----------------------------------------------------------------------
      shutdown_sec = dplyr::if_else(
        base_jbsic07_cc %in% c(55, 56, 79, 90, 91, 92, 93, 96),
        1, 0
      ),
      
      
      # -----------------------------------------------------------------------
      # Baseline key worker definitions
      #
      # These follow a simplified version of the UK "essential worker"
      # classification combining industry (SIC) and occupation (SOC).
      # -----------------------------------------------------------------------
      
      # Health and social services
      keyworker_health_social =
        dplyr::if_else(
          
          # SIC 86: Human health activities (hospitals, medical practices)
          base_jbsic07_cc == 86 |
            
            # SOC 124  Health services and public health managers
            # SOC 221  Medical practitioners
            # SOC 222  Psychologists
            # SOC 223  Nurses
            # SOC 321  Paramedics
            base_jbsoc10_cc %in% c(124, 221, 222, 223, 321) |
            
            # SOC 118  Health and social services managers
            # when working in SIC 86–88 (health or social care)
            (base_jbsoc10_cc == 118 & base_jbsic07_cc %in% 86:88) |
            
            # SOC 614  Nursing auxiliaries and assistants
            # working in public administration or health sectors
            (base_jbsoc10_cc == 614 & base_jbsic07_cc %in% c(84, 86:88)),
          
          1, 0
        ),
      
      
      # -----------------------------------------------------------------------
      # Education key workers
      # -----------------------------------------------------------------------
      keyworker_education =
        dplyr::if_else(
          
          # SIC 85: Education
          base_jbsic07_cc %in% c(85) |
            
            # SOC 323  Teaching assistants
            # SOC 612  Childcare and related personal services
            # SOC 623  Teaching and educational professionals
            # when working in public administration (schools run by local gov)
            (base_jbsoc10_cc %in% c(323, 612, 623) & base_jbsic07_cc == 84) |
            
            # SOC 231  Teaching professionals
            (base_jbsoc10_cc == 231),
          
          1, 0
        ),
      
      
      # -----------------------------------------------------------------------
      # Public safety and essential government services
      #
      # SOC 331: Protective service occupations
      #          (police officers, firefighters, prison service officers)
      #
      # SIC 53 : Postal and courier activities
      #          (Royal Mail, parcel delivery, logistics operators)
      #          Treated as essential infrastructure during lockdown.
      # -----------------------------------------------------------------------
      keyworker_public_safety =
        dplyr::if_else(
          base_jbsoc10_cc == 331 |
            base_jbsic07_cc %in% c(
              53   # Postal and courier activities
            ),
          1, 0
        ),
      
      # Combined key worker indicator
      keyworker_my_def = pmax(
        keyworker_health_social,
        keyworker_education,
        keyworker_public_safety
      ),
      
      
      # -----------------------------------------------------------------------
      # Grouping variables used in analysis
      # -----------------------------------------------------------------------
      
      # Self-reported classification from the COVID module
      # (if those variables are present in the dataset)
      group_self_report = dplyr::case_when(
        shutdown_sec == 1 ~ "shutdown sector",
        "keyworker" %in% names(.) && "keyworksector" %in% names(.) &&
          (keyworker == 1 | keyworksector %in% 1:8) ~ "key worker",
        TRUE ~ "other"
      ),
      
      # Industry / occupation based classification
      group_industry_based = dplyr::case_when(
        shutdown_sec == 1 ~ "shutdown sector",
        keyworker_my_def == 1 ~ "key worker",
        TRUE ~ "other"
      ),
      
      # More detailed key worker breakdown
      group_industry_based_detailed = dplyr::case_when(
        shutdown_sec == 1 ~ "shutdown sector",
        keyworker_health_social == 1 ~ "key worker - health\n and social services",
        keyworker_education == 1 ~ "key worker - education",
        keyworker_public_safety == 1 ~ "key worker - public safety\n and essential gvt. services",
        TRUE ~ "other"
      )
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
# -----------------------------------------------------------------------------
make_workoutside_covid <- function(sempderived, hours, wah) {
  dplyr::case_when(
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
# Future-outcomes workoutside
#
# Inputs:
#   jbstat : labour-market status
#            1 employed
#            2 self-employed
#
#   jbhrs  : usual weekly hours worked
#
#   jbpl   : work location
#            1 home
#            2 employer premises
#            3 driving/travelling
#            4 other places
#
#   jbwah  : working-from-home frequency
#            1 always
#            2 often
#            3 sometimes
#            4 never
#
# Logic:
#   - if not employed/self-employed -> 0
#   - if no hours / zero hours -> 0
#   - if works at home (jbpl==1) -> 0
#   - otherwise, if works and not home-based -> 1
#
# This is the closest future-wave analogue to the COVID workoutside variable.
# -----------------------------------------------------------------------------
make_workoutside_future <- function(jbstat, jbhrs, jbpl, jbwah = NA) {
  dplyr::case_when(
    is.na(jbstat) ~ NA_real_,
    !(jbstat %in% c(1, 2)) ~ 0,
    is.na(jbhrs) ~ NA_real_,
    jbhrs <= 0 ~ 0,
    jbpl == 1 ~ 0,
    jbpl %in% 2:4 ~ 1,
    TRUE ~ NA_real_
  )
}