# =============================================================================
# File: code/lib/covid_loader.R
#
# Purpose:
#   Load COVID study waves and merge them wide by pidp.
#
# Input file naming:
#   {wave}_indresp_w.dta  for wave in {ca,...,ci}
# =============================================================================

load_covid_wave <- function(path_covid, wave_prefix) {
  
  filename <- paste0(wave_prefix, "_indresp_w.dta")
  fpath <- file.path(path_covid, filename)
  if (!file.exists(fpath)) return(NULL)
  
  data <- haven::read_dta(fpath)
  
  # COVID variable meanings:
  #   furlough       : furlough status
  #   ff_furlough    : alternate furlough measure (renamed to fffurlough)
  #   hours          : hours worked last week
  #   blwork         : baseline worked in Jan-Feb 2020
  #   blhours        : baseline weekly hours in Jan-Feb 2020
  #   blwah          : baseline WAH frequency in Jan-Feb 2020
  #   sempderived    : 1 employed, 2 self-employed, 3 both, 4 not employed
  #   wah            : WFH frequency 1 always, 2 often, 3 sometimes, 4 never
  #   howlng         : housework hours
  #   keyworker      : self-reported key worker
  #   keyworksector  : keyworker sector (1..8 key, 9 not)
  #   timechcare     : childcare/homeschool hours (continuous)
  #   husits_cv      : responsibility for childcare (couples)
  #   workchsch(*)   : change in work due to homeschooling
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
    dplyr::select(dplyr::all_of(existing)) %>%
    dplyr::rename_with(\(x) stringr::str_replace(x, "ff_furlough", "fffurlough"), dplyr::everything())
}

merge_covid_waves_wide <- function(path_covid, covid_waves) {
  dfs <- lapply(covid_waves, \(w) load_covid_wave(path_covid, w))
  dfs <- dfs[!sapply(dfs, is.null)]
  if (length(dfs) == 0) stop("No COVID wave files found.")
  purrr::reduce(dfs, dplyr::full_join, by = "pidp")
}