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

# ---- Data INPUT root folders (outside repo; confidential/raw) -----------------
path_main  <- "C:/Users/USER/Dropbox/WFH_covid/UK project/understanding society uk all data/UKDA-6614-stata/stata/stata14_se/ukhls"
path_covid <- "C:/Users/USER/Dropbox/WFH_covid/UK project/understanding society uk all data/UKDA-8644-stata/stata/stata13_se/"

# ---- Project REPO folders (inside repo; safe to sync) -------------------------
pol_path <- "policies"
fig_path <- "figures"
tab_path <- "tables"

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
data_out_root <- "C:/Users/USER/Dropbox/WFH_covid/UK project/understanding society uk all data"  # <-- EDIT ME

# Under data_out_root we keep all non-shareable derived products:
der_path     <- file.path(data_out_root, "derived")
samples_path <- file.path(data_out_root, "samples")
cache_path   <- file.path(data_out_root, "cache")

# ---- Waves -------------------------------------------------------------------
covid_waves  <- c("ca", "cb", "cc", "cd", "ce", "cf", "cg", "ch", "ci")
future_waves <- c("l", "m", "n", "o") 