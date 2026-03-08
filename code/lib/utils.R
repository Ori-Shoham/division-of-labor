# =============================================================================
# File: code/lib/utils.R
#
# Purpose:
#   Small helper functions reused across modules/scripts.
# =============================================================================

# first_valid()
# Used when the same "baseline" variable is repeated across waves:
#   - take the first non-missing and not equal to -8 (NIU)
first_valid <- function(x) {
  x2 <- x[!is.na(x) & x != -8]
  if (length(x2) == 0) NA else x2[1]
}

# wave_label_map()
# Consistent ordering + human-readable labels for plotting.
wave_label_map <- function() {
  tibble(
    wave = c("2019", "baseline", "ca", "cb", "cc", "cd", "ce", "cf", "cg", "ch", "ci"),
    wave_lab = c("2019", "Jan-Feb 20", "Apr 20", "May 20", "Jun 20", "Jul 20",
                 "Sep 20", "Nov 20", "Jan 21", "Mar 21", "Sep 21")
  )
}