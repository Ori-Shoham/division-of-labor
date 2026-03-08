# =============================================================================
# Script: code/run/02_make_descriptives.R
#
# Purpose:
#   Produce descriptive figures (industry and group versions) for:
#     - worked at all last week
#     - work status
#     - furlough
#     - work-from-home distribution
#     - overtime plots: worked at all, hours worked
#     - workoutside over time
#
# Inputs:
#   derived/df_sample_long_covid.rds
#
# Outputs:
#   figures/*.png
# =============================================================================

suppressPackageStartupMessages({
  library(tidyverse)
  library(scales)
  library(forcats)
})

rm(list = ls())

source("code/lib/config.R")
source("code/lib/descriptives_plots.R")

df <- readRDS(file.path(der_path, "df_sample_long_covid.rds"))

# ---- Industry plots for April/May 2020 ---------------------------------------
for (w in c("ca", "cb")) {
  
  plot_worked_at_all_bar(df, w, by = "industry", perc = FALSE,
                         out_file = paste0("worked_at_all_", w, "_ind.png"),
                         fig_path = fig_path)
  
  plot_worked_at_all_bar(df, w, by = "industry", perc = TRUE,
                         out_file = paste0("worked_at_all_", w, "_ind_perc.png"),
                         fig_path = fig_path)
  
  plot_work_status_bar(df, w, by = "industry", perc = FALSE,
                       out_file = paste0("work_status_", w, "_ind.png"),
                       fig_path = fig_path)
  
  plot_work_status_bar(df, w, by = "industry", perc = TRUE,
                       out_file = paste0("work_status_", w, "_ind_perc.png"),
                       fig_path = fig_path)
  
  plot_wfh_bar(df, w, by = "industry",
               out_file = paste0("wfh_", w, "_ind_perc.png"),
               fig_path = fig_path)
}

# Furlough only makes sense where furlough variable exists (your original used April)
plot_furlough_bar(df, "ca", by = "industry",
                  out_file = "furlough_ca_ind_perc.png",
                  fig_path = fig_path)

# ---- Group plots for April/May 2020 ------------------------------------------
for (w in c("ca", "cb")) {
  
  plot_worked_at_all_bar(df, w, by = "group_industry_based", perc = TRUE,
                         out_file = paste0("worked_at_all_", w, "_groups_perc.png"),
                         fig_path = fig_path)
  
  plot_worked_at_all_bar(df, w, by = "group_industry_based_detailed", perc = TRUE,
                         out_file = paste0("worked_at_all_", w, "_detailed_groups_perc.png"),
                         fig_path = fig_path)
  
  plot_work_status_bar(df, w, by = "group_industry_based", perc = TRUE,
                       out_file = paste0("work_status_", w, "_groups_perc.png"),
                       fig_path = fig_path)
  
  plot_work_status_bar(df, w, by = "group_industry_based_detailed", perc = TRUE,
                       out_file = paste0("work_status_", w, "_detailed_groups_perc.png"),
                       fig_path = fig_path)
  
  plot_wfh_bar(df, w, by = "group_industry_based",
               out_file = paste0("wfh_", w, "_groups_perc.png"),
               fig_path = fig_path)
  
  plot_wfh_bar(df, w, by = "group_industry_based_detailed",
               out_file = paste0("wfh_", w, "_detailed_groups_perc.png"),
               fig_path = fig_path)
}

plot_furlough_bar(df, "ca", by = "group_industry_based",
                  out_file = "furlough_ca_groups_perc.png",
                  fig_path = fig_path)

plot_furlough_bar(df, "ca", by = "group_industry_based_detailed",
                  out_file = "furlough_ca_detailed_groups_perc.png",
                  fig_path = fig_path)

# ---- Over-time plots ----------------------------------------------------------
plot_overtime_worked(df, by = "group_industry_based",
                     out_file = "worked_at_all_groups_overtime.png",
                     fig_path = fig_path)

plot_overtime_worked(df, by = "group_industry_based_detailed",
                     out_file = "worked_at_all_detailed_groups_overtime.png",
                     fig_path = fig_path)

plot_overtime_hours(df, by = "group_industry_based",
                    out_file = "hours_groups_overtime.png",
                    fig_path = fig_path)

plot_overtime_hours(df, by = "group_industry_based_detailed",
                    out_file = "hours_detailed_groups_overtime.png",
                    fig_path = fig_path)

plot_workoutside_overtime(df, by = NULL,
                          out_file = "workoutside_overtime.png",
                          fig_path = fig_path)

plot_workoutside_overtime(df, by = "group_industry_based",
                          out_file = "workoutside_overtime_groups.png",
                          fig_path = fig_path)

plot_workoutside_overtime(df, by = "group_industry_based_detailed",
                          out_file = "workoutside_overtime_detailed_groups.png",
                          fig_path = fig_path)

cat("\nDescriptive figures saved to figures/.\n")