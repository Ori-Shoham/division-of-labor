# =============================================================================
# File: code/lib/wave_labels.R
#
# Purpose:
#   Central mapping from internal wave codes to:
#     - short labels (axes)
#     - full labels (titles/captions)
#
# Notes:
#   - Keep wave codes in the data for merges/joins.
#   - Use wave_label_short for axes.
#   - Use wave_label_full for titles.
# =============================================================================

wave_label_lookup <- function() {
  tibble::tibble(
    wave = c(
      "2019", "baseline",
      "ca", "cb", "cc", "cd", "ce", "cf", "cg", "ch", "ci"
    ),
    
    # Short labels for axis ticks
    wave_label_short = c(
      "2019",
      "Jan–Feb 20",
      "Apr 20",
      "May 20",
      "Jun 20",
      "Jul 20",
      "Sep 20",
      "Nov 20",
      "Jan 21",
      "Mar 21",
      "Sep 21"
    ),
    
    # Full labels for titles/captions
    wave_label_full = c(
      "2019",
      "January–February 2020",
      "April 2020",
      "May 2020",
      "June 2020",
      "July 2020",
      "September 2020",
      "November 2020",
      "January 2021",
      "March 2021",
      "September 2021"
    )
  )
}