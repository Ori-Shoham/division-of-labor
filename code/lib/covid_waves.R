# =============================================================================
# File: code/lib/covid_waves.R
#
# Purpose:
#   Load and merge UKHLS COVID study waves (ca..ci) into a wide dataset keyed by pidp.
#
# Important column meanings (COVID variables you keep):
#
#   pidp                 : Person identifier
#
# For each wave prefix (e.g. ca_*, cb_*):
#   {w}_hours            : hours worked last week (COVID survey)
#   {w}_sempderived      : employment derived status in COVID survey
#                          (your note: 1 employed, 2 self-employed, 3 both, 4 not employed)
#   {w}_wah              : working from home frequency/status (codes 1..4 in your plots)
#
# Baseline recall variables inside COVID:
#   {w}_blwork           : baseline worked in Jan-Feb 2020 (as recorded in COVID module)
#   {w}_blhours          : baseline weekly hours in Jan-Feb 2020
#   {w}_blwah            : baseline WAH (worked at home) in Jan-Feb 2020
#
# Furlough:
#   {w}_furlough         : furlough status (1 yes, 2 no in your plots)
#   {w}_ff_furlough      : additional furlough variable (renamed to fffurlough)
#
# Keyworker:
#   {w}_keyworker        : self-reported key worker (wave ca uses this in your code)
#   {w}_keyworksector    : keywork sector classification (wave cb+ uses this in your code)
#
# Childcare / homeschooling:
#   {w}_timechcare       : hours childcare + homeschooling (continuous)
#   {w}_husits_cv        : childcare responsibility within couples
#   {w}_workchsch        : work pattern change due to homeschooling
#   {w}_workchsch2       : work pattern change due to homeschooling (variant)
#
# Output:
#   build_covid_all(): full-join across waves, preserving participants who dip in/out
# =============================================================================

load_covid_wave <- function(path_covid, wave_prefix) {
  
  filename <- paste0(wave_prefix, "_indresp_w.dta")
  fpath <- file.path(path_covid, filename)
  if (!file.exists(fpath)) return(NULL)
  
  data <- read_dta(fpath)
  
  vars_to_keep <- c(
    "pidp",
    paste0(wave_prefix, "_furlough"),
    paste0(wave_prefix, "_ff_furlough"),
    paste0(wave_prefix, "_hours"),
    paste0(wave_prefix, "_blwork"),
    paste0(wave_prefix, "_blhours"),
    paste0(wave_prefix, "_blwah"),
    paste0(wave_prefix, "_sempderived"),
    paste0(wave_prefix, "_wah"),
    paste0(wave_prefix, "_howlng"),
    paste0(wave_prefix, "_keyworker"),
    paste0(wave_prefix, "_keyworksector"),
    paste0(wave_prefix, "_timechcare"),
    paste0(wave_prefix, "_husits_cv"),
    paste0(wave_prefix, "_workchsch"),
    paste0(wave_prefix, "_workchsch2")
  )
  
  existing <- intersect(vars_to_keep, names(data))
  
  data %>%
    select(all_of(existing)) %>%
    # rename ff_furlough -> fffurlough to avoid the parser splitting it oddly later
    rename_with(\(x) str_replace(x, "ff_furlough", "fffurlough"), everything())
}

build_covid_all <- function(path_covid, covid_waves) {
  
  dfs <- lapply(covid_waves, \(w) load_covid_wave(path_covid, w))
  dfs <- dfs[!sapply(dfs, is.null)]
  
  if (length(dfs) == 0) {
    stop("No COVID wave files found in: ", path_covid)
  }
  
  reduce(dfs, full_join, by = "pidp")
}