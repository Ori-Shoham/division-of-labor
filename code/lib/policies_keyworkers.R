# =============================================================================
# File: code/lib/policies_keyworkers.R
#
# Purpose:
#   Load SOC/SIC label tables and build a keyworker crosswalk.
#
# Inputs:
#   - policies/SOC.csv
#     Typical columns: SOC (code), SOC_label (text)
#   - policies/SIC.csv
#     Typical columns: SIC (code), SIC_label (text)
#   - policies/keyworkers.xlsx (sheet 4 by default)
#
# Outputs:
#   - load_policies(): list(SOC=..., SIC=...)
#   - build_keyworker_crosswalk(): tibble keyed by (SOC, SIC) with:
#       any_key : max keyworker flag within that SOC x SIC cell (0/1)
#
# Notes on meaning:
#   The keyworker excel is a detailed mapping of SOC (occupation) x SIC (industry)
#   to whether that job is considered "key"/essential under policy definitions.
# =============================================================================

load_policies <- function(pol_path) {
  SOC <- read.csv(file.path(pol_path, "SOC.csv"))
  SIC <- read.csv(file.path(pol_path, "SIC.csv"))
  list(SOC = SOC, SIC = SIC)
}

# build_keyworker_crosswalk()
# Reads keyworker excel and collapses to 2-digit-ish cells:
#   - SOC_4 is SOC10M code (often 4-digit); you collapse to floor(SOC_4/10)
#   - SIC_4 is SIC code (often 4-digit); you collapse to floor(SIC_4/100)
build_keyworker_crosswalk <- function(xlsx_path, sheet = 4, SOC, SIC) {
  
  if (!file.exists(xlsx_path)) {
    stop(
      "Keyworker excel not found: ", xlsx_path,
      "\nPlace it in policies/ as keyworkers.xlsx, or update KEYWORKER_XLSX in config.R"
    )
  }
  
  key_workers_raw <- readxl::read_excel(xlsx_path, sheet = sheet)
  
  # In your original code, industry columns are colnames[4:599]
  sics <- tibble(
    SIC_4 = colnames(key_workers_raw)[4:599],
    industry = unlist(key_workers_raw[1, 4:599])
  )
  
  key_workers <- key_workers_raw %>%
    filter(!is.na(Group)) %>%
    pivot_longer(
      !c(Group, `SOC10M / INDC07M`, `SOC_label / SIC_label`),
      names_to = "SIC_4",
      values_to = "key"
    ) %>%
    rename(
      # SOC_4: detailed occupation code in the sheet
      SOC_4 = `SOC10M / INDC07M`,
      # occ_group: grouping label within the sheet
      occ_group = Group,
      # occupation: occupation label text
      occupation = `SOC_label / SIC_label`
    ) %>%
    left_join(sics, by = "SIC_4") %>%
    select(occ_group, occupation, SOC_4, industry, SIC_4, key) %>%
    mutate(across(c(SIC_4, SOC_4, key), as.numeric))
  
  # Collapse to the level you use in analysis (SOC, SIC)
  key_inds <- key_workers %>%
    group_by(SIC = floor(SIC_4/100), SOC = floor(SOC_4/10)) %>%
    summarise(any_key = max(key, na.rm = TRUE), .groups = "drop") %>%
    left_join(SOC, by = "SOC") %>%
    left_join(SIC, by = "SIC")
  
  key_inds
}