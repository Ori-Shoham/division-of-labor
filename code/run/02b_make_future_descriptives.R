# =============================================================================
# Script: code/run/02b_make_future_descriptives.R
#
# Purpose:
#   Descriptive figures for ALL future outcomes (waves L/M/N),
#   for (1) all baseline workers, and (2) baseline couples.
# =============================================================================

suppressPackageStartupMessages({
  library(tidyverse)
})

source("code/lib/config.R")
source("code/lib/future_descriptives_plots.R")

dir.create(fig_path, showWarnings = FALSE, recursive = TRUE)

# --- load future outcomes (long) ----------------------------------------------
df_future <- readRDS(file.path(der_path, "future_outcomes_long_lmo.rds"))

# --- load baseline-defined samples --------------------------------------------
s_all     <- readRDS(file.path(samples_path, "s2019_all.rds"))
s_couples <- readRDS(file.path(samples_path, "s2019_couples.rds"))

pidp_all     <- s_all$pidp
pidp_couples <- s_couples$pidp

df_future_all <- df_future %>% filter(pidp %in% pidp_all)
df_future_couples <- df_future %>% filter(pidp %in% pidp_couples)

# --- plot everything -----------------------------------------------------------
plot_all_future_outcomes(
  df = df_future_all,
  prefix = "future_all",
  fig_path = fig_path
)

plot_all_future_outcomes(
  df = df_future_couples,
  prefix = "future_couples",
  fig_path = fig_path
)

cat("\nFuture outcome descriptives complete.\n")
cat("Figures saved to: ", fig_path, "\n")