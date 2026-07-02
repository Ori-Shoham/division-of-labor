# =============================================================================
# File: code/lib/config.R
#
# Purpose:
#   Central configuration:
#     - data inputs (path_main, path_covid) live outside repo
#     - project repo folders: policies/, figures/, tables/
#     - DATA OUTPUT root (data_out_root) lives outside repo:
#         - derived datasets (RDS)
#         - samples (RDS)
#         - any cached intermediate objects
#
# Why this matters:
#   You can move data_out_root to a protected/encrypted location later
#   without touching code.
# =============================================================================

#load data
user = Sys.info()[["user"]]
# print(user)

# ---- Data licence switch ------------------------------------------------------
# Single hyperparameter selecting which edition of the UKHLS MAIN-study data the
# pipeline runs on:
#   "EUL" = End User Licence, SN 6614 (condensed industry/occupation only).
#           Everything runs exactly as before.
#   "SL"  = Special Licence, SN 6931 (adds detailed 4-digit SIC/SOC). In SL mode
#           the main-wave data path changes, the detailed industry/occupation
#           variables are read, derived datasets / figures / tables are written to
#           SEPARATE locations (so EUL outputs are never overwritten), and the
#           key-worker and shutdown-sector groups are redefined from the detailed
#           codes via the policy files.
# The COVID study (SN 8644) is unchanged in both modes.
DATA_LICENSE <- "EUL"

if (!DATA_LICENSE %in% c("EUL", "SL")) {
  stop("DATA_LICENSE must be \"EUL\" or \"SL\"; got: ", DATA_LICENSE)
}

# ---- Data INPUT root folders (outside repo; confidential/raw) -----------------
# Main study: EUL = UKDA-6614, SL = UKDA-6931. The two editions share identical
# file names, wave prefixes and value coding; the SL edition simply adds the
# detailed variables. COVID study path is the same regardless of licence.
path_main_eul <- "C:/Users/USER/Dropbox/WFH_covid/UK project/understanding society uk all data/UKDA-6614-stata/stata/stata14_se/ukhls"
path_main_sl  <- "C:/Users/USER/Dropbox/WFH_covid/UK project/understanding society uk all data/UKDA-6931-stata/stata/stata14_se/ukhls"
path_covid    <- "C:/Users/USER/Dropbox/WFH_covid/UK project/understanding society uk all data/UKDA-8644-stata/stata/stata13_se/"

if(user == "orishoham"){
  dropbox_path <- "C:/Users/orishoham/Dropbox/"
  path_main_eul <- paste0(dropbox_path,"WFH_covid/UK project/understanding society uk all data/UKDA-6614-stata/stata/stata14_se/ukhls")
  path_main_sl  <- paste0(dropbox_path,"WFH_covid/UK project/understanding society uk all data/UKDA-6931-stata/stata/stata14_se/ukhls")
  path_covid    <- paste0(dropbox_path,"WFH_covid/UK project/understanding society uk all data/UKDA-8644-stata/stata/stata13_se/")
}

# Active main-wave path follows the licence (main waves only).
path_main <- if (DATA_LICENSE == "SL") path_main_sl else path_main_eul

# ---- Licence-specific variable names ------------------------------------------
# Industry / occupation are the only variables whose NAME differs between editions:
#   EUL ships only the condensed *_cc versions; the SL ships the detailed codes.
# All other variables keep identical names across editions, so no mapping needed.
VAR_SIC <- if (DATA_LICENSE == "SL") "jbsic07" else "jbsic07_cc"
VAR_SOC <- if (DATA_LICENSE == "SL") "jbsoc10" else "jbsoc10_cc"

# ---- Project REPO folders (inside repo; safe to sync) -------------------------
# Policies are shared. Figures/tables are isolated per licence so an SL run never
# overwrites the EUL outputs: EUL keeps the existing "figures"/"tables" roots;
# SL nests under "figures/SL"/"tables/SL". All fig_path_* below derive from
# fig_path, so they inherit the nesting automatically.
pol_path <- "policies"
fig_path <- if (DATA_LICENSE == "SL") file.path("figures", "SL") else "figures"
tab_path <- if (DATA_LICENSE == "SL") file.path("tables", "SL") else "tables"

# ---- Figure pipeline controls -------------------------------------------------
# Defaults keep the shareable pipeline lean. Flip these flags to regenerate the
# broader exploratory figure families without changing plotting helpers.
MAKE_EXPLORATORY_EXTRA <- FALSE
MAKE_FUTURE_ONLY_TREATMENT <- FALSE
MAKE_RESTRICTED_HUSB_NOTKEY_VARIANTS <- FALSE
MAKE_WIFE_KEY_ANY_TREATMENT <- FALSE
MAKE_COUPLE_TREATMENT_SPOUSEFACETS <- FALSE
MAKE_EVENT_STUDIES_COUPLE_FE <- TRUE
MAKE_EVENT_STUDIES_BASELINE_CONTROLS <- TRUE

# Organized figure output folders.
fig_path_descriptives_covid <- file.path(fig_path, "descriptives", "covid")
fig_path_descriptives_future <- file.path(fig_path, "descriptives", "future")
fig_path_sample_composition <- file.path(fig_path, "sample_composition")
fig_path_couple_treatment <- file.path(fig_path, "couple_treatment")
fig_path_couple_treatment_covid_childgrids <- file.path(
  fig_path_couple_treatment,
  "covid_childgrids"
)
fig_path_couple_treatment_future_childgrids <- file.path(
  fig_path_couple_treatment,
  "future_childgrids"
)
fig_path_couple_treatment_history_future_childgrids <- file.path(
  fig_path_couple_treatment,
  "history_future_childgrids"
)
fig_path_couple_treatment_spousefacets <- file.path(
  fig_path_couple_treatment,
  "spousefacets"
)
fig_path_couple_treatment_counts <- file.path(
  fig_path_couple_treatment,
  "counts"
)

# ---- Policy files -------------------------------------------------------------
soc_path <- file.path(pol_path, "SOC.csv")
sic_path <- file.path(pol_path, "SIC.csv")
KEYWORKER_XLSX <- file.path(pol_path, "keyworkersreferencetableupdated2.xlsx")
UK_PRICE_INDEX_MONTHLY_CSV <- file.path(pol_path, "uk_price_index_monthly.csv")
REAL_PAY_BASE_YM <- as.Date("2019-12-01")

# ---- Data OUTPUT root (outside repo; can be protected/encrypted) --------------
# Choose a location that is NOT inside your git repo.
# Examples:
#   "D:/protected_outputs/ukhls_project"
#   "E:/encrypted_drive/ukhls_outputs"
#   "C:/Users/USER/Documents/protected/ukhls_outputs"
data_out_root <- "C:/Users/USER/Dropbox/WFH_covid/UK project/understanding society uk all data" 

if(user == "orishoham"){
  data_out_root <- paste0(dropbox_path,"WFH_covid/UK project/understanding society uk all data")  

}

# Under data_out_root we keep all non-shareable derived products. These are
# isolated per licence so an SL build never overwrites the EUL datasets: EUL keeps
# the existing "derived"/"samples"/"cache" folders byte-for-byte; SL writes to
# "derived_SL"/"samples_SL"/"cache_SL". Because every saveRDS/readRDS in the
# pipeline goes through these variables, switching DATA_LICENSE redirects all
# reads and writes with no per-script edits.
out_suffix   <- if (DATA_LICENSE == "SL") "_SL" else ""
der_path     <- file.path(data_out_root, paste0("derived", out_suffix))
samples_path <- file.path(data_out_root, paste0("samples", out_suffix))
cache_path   <- file.path(data_out_root, paste0("cache",   out_suffix))

# ---- Waves -------------------------------------------------------------------
covid_waves  <- c("ca", "cb", "cc", "cd", "ce", "cf", "cg", "ch", "ci")
future_waves <- c("j", "k", "l", "m", "n", "o")
future_outcomes_start <- as.Date("2020-03-01")
future_outcomes_monthly_start <- as.Date("2020-01-01")

# Candidate waves used to construct pre-baseline histories.
# The history builder then keeps only waves strictly prior to each individual's
# own baseline source wave. For example:
#   base_source_wave == "k" -> keep a:j
#   base_source_wave == "j" -> keep a:i
#   base_source_wave == "i" -> keep a:h
history_waves <- c("a", "b", "c", "d", "e", "f", "g", "h", "i", "j", "k")
