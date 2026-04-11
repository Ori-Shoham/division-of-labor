# =============================================================================
# File: code/lib/wave_labels.R
#
# Purpose:
#   Central mapping from internal wave/time codes to:
#     - short labels (axes)
#     - full labels (titles/captions)
#
# Notes:
#   - Keep wave codes in the data for merges/joins.
#   - Use wave_label_short for axes.
#   - Use wave_label_full for titles.
#   - This file is the single source of truth for:
#       * COVID-study waves
#       * Future/main-study waves
# =============================================================================

suppressPackageStartupMessages({
  library(tidyverse)
})

# -----------------------------------------------------------------------------
# COVID-study wave labels
#
# Includes:
#   - synthetic pre-period rows used in the COVID long panel
#   - actual COVID study waves ca-ci
# -----------------------------------------------------------------------------
covid_wave_label_lookup <- function() {
  tibble::tibble(
    wave = c(
      "2019", "baseline",
      "ca", "cb", "cc", "cd", "ce", "cf", "cg", "ch", "ci"
    ),
    
    # Short labels for axis ticks
    wave_label_short = c(
      "2019",
      "Jan-Feb 20",
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
      "January-February 2020",
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

# -----------------------------------------------------------------------------
# Future/main-study wave labels
#
# Current convention in the project:
#   j -> Wave 10
#   k -> Wave 11
#   l -> Wave 12
#   m -> Wave 13
#   n -> Wave 14
#   o -> Wave 15
#
# Also allow numeric string versions for safety.
# -----------------------------------------------------------------------------
future_wave_label_lookup <- function() {
  tibble::tibble(
    wave = c("j", "k", "l", "m", "n", "o", "10", "11", "12", "13", "14", "15"),
    wave_label_short = c(
      "Wave 10",
      "Wave 11",
      "Wave 12",
      "Wave 13",
      "Wave 14",
      "Wave 15",
      "Wave 10",
      "Wave 11",
      "Wave 12",
      "Wave 13",
      "Wave 14",
      "Wave 15"
    ),
    wave_label_full = c(
      "Wave 10",
      "Wave 11",
      "Wave 12",
      "Wave 13",
      "Wave 14",
      "Wave 15",
      "Wave 10",
      "Wave 11",
      "Wave 12",
      "Wave 13",
      "Wave 14",
      "Wave 15"
    )
  )
}

# -----------------------------------------------------------------------------
# Backward-compatible main lookup
#
# Historically this file exposed wave_label_lookup() for COVID-study labels.
# Keep that behavior so existing code continues to work unchanged.
# -----------------------------------------------------------------------------
wave_label_lookup <- function() {
  covid_wave_label_lookup()
}

# -----------------------------------------------------------------------------
# Generic lookup by scale
#
# Supported scales:
#   - "covid"
#   - "future"
# -----------------------------------------------------------------------------
time_label_lookup <- function(scale = c("covid", "future")) {
  scale <- match.arg(scale)
  
  if (scale == "covid") {
    return(covid_wave_label_lookup())
  }
  
  future_wave_label_lookup()
}

# -----------------------------------------------------------------------------
# Return a named character vector for axis relabeling
#
# Example:
#   time_label_map("covid", which = "short")
#   time_label_map("future", which = "short")
# -----------------------------------------------------------------------------
time_label_map <- function(scale = c("covid", "future"),
                           which = c("short", "full")) {
  scale <- match.arg(scale)
  which <- match.arg(which)
  
  x <- time_label_lookup(scale)
  
  label_col <- if (which == "short") "wave_label_short" else "wave_label_full"
  
  out <- x[[label_col]]
  names(out) <- x$wave
  out
}

# -----------------------------------------------------------------------------
# Safe relabel helper
#
# If a code is not found in the lookup, keep the original value.
# -----------------------------------------------------------------------------
label_time_values <- function(x,
                              scale = c("covid", "future"),
                              which = c("short", "full")) {
  scale <- match.arg(scale)
  which <- match.arg(which)
  
  x_chr <- as.character(x)
  map <- time_label_map(scale = scale, which = which)
  
  out <- unname(map[x_chr])
  out[is.na(out)] <- x_chr[is.na(out)]
  out
}