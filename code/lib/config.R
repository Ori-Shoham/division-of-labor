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

# ---- Data INPUT root folders (outside repo; confidential/raw) -----------------
path_main  <- "C:/Users/USER/Dropbox/WFH_covid/UK project/understanding society uk all data/UKDA-6614-stata/stata/stata14_se/ukhls"
path_covid <- "C:/Users/USER/Dropbox/WFH_covid/UK project/understanding society uk all data/UKDA-8644-stata/stata/stata13_se/"

if(user == "orishoham"){
  dropbox_path <- "C:/Users/orishoham/Dropbox/"
  path_main  <- paste0(dropbox_path,"WFH_covid/UK project/understanding society uk all data/UKDA-6614-stata/stata/stata14_se/ukhls")
  path_covid <- paste0(dropbox_path,"WFH_covid/UK project/understanding society uk all data/UKDA-8644-stata/stata/stata13_se/")
}

# ---- Project REPO folders (inside repo; safe to sync) -------------------------
pol_path <- "policies"
fig_path <- "figures"
tab_path <- "tables"

# ---- Figure pipeline controls -------------------------------------------------
# Defaults keep the shareable pipeline lean. Flip these flags to regenerate the
# broader exploratory figure families without changing plotting helpers.
MAKE_EXPLORATORY_EXTRA <- FALSE
MAKE_FUTURE_ONLY_TREATMENT <- FALSE
MAKE_RESTRICTED_HUSB_NOTKEY_VARIANTS <- FALSE
MAKE_WIFE_KEY_ANY_TREATMENT <- FALSE
MAKE_COUPLE_TREATMENT_SPOUSEFACETS <- FALSE
MAKE_EVENT_STUDIES_COUPLE_FE <- FALSE
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

# Under data_out_root we keep all non-shareable derived products:
der_path     <- file.path(data_out_root, "derived")
samples_path <- file.path(data_out_root, "samples")
cache_path   <- file.path(data_out_root, "cache")

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
