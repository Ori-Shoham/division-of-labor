# =============================================================================
# Script: code/run/diagnostics_any_work_hours_figures.R
#
# Purpose:
#   Ad hoc figure diagnostics for any_work and working hours.
#
# Focus:
#   - any_work distributions over time
#   - hours / jbhrs distributions over time
#   - missingness in any_work and hours
#   - consistency checks:
#       * any_work == 1 but hours <= 0
#       * any_work == 0 but hours > 0
#
# Notes:
#   - This script is for quality checks only.
#   - It is not part of the main pipeline.
#   - It reads existing derived .rds files.
# =============================================================================

suppressPackageStartupMessages({
  library(tidyverse)
  library(scales)
})

rm(list = ls())

source("code/lib/config.R")

diag_fig_path <- file.path(fig_path, "diagnostics_any_work_hours")
dir.create(diag_fig_path, showWarnings = FALSE, recursive = TRUE)

# =============================================================================
# Helpers
# =============================================================================

save_diag_plot <- function(p, filename, width = 10, height = 6) {
  ggplot2::ggsave(
    filename = filename,
    plot = p,
    path = diag_fig_path,
    width = width,
    height = height
  )
  invisible(p)
}

summarise_any_work_hours <- function(df,
                                     group_vars,
                                     any_work_var = "any_work",
                                     hours_var = "hours") {
  
  stopifnot(any_work_var %in% names(df))
  stopifnot(hours_var %in% names(df))
  
  df %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(group_vars))) %>%
    dplyr::summarise(
      n_rows = dplyr::n(),
      
      n_any_work_nonmissing = sum(!is.na(.data[[any_work_var]])),
      share_any_work_missing = mean(is.na(.data[[any_work_var]])),
      share_any_work = mean(.data[[any_work_var]], na.rm = TRUE),
      
      n_hours_nonmissing = sum(!is.na(.data[[hours_var]])),
      share_hours_missing = mean(is.na(.data[[hours_var]])),
      mean_hours = mean(.data[[hours_var]], na.rm = TRUE),
      p10_hours = stats::quantile(.data[[hours_var]], 0.10, na.rm = TRUE, names = FALSE),
      p25_hours = stats::quantile(.data[[hours_var]], 0.25, na.rm = TRUE, names = FALSE),
      p50_hours = stats::quantile(.data[[hours_var]], 0.50, na.rm = TRUE, names = FALSE),
      p75_hours = stats::quantile(.data[[hours_var]], 0.75, na.rm = TRUE, names = FALSE),
      p90_hours = stats::quantile(.data[[hours_var]], 0.90, na.rm = TRUE, names = FALSE),
      
      share_hours_zero = mean(.data[[hours_var]] == 0, na.rm = TRUE),
      share_hours_positive = mean(.data[[hours_var]] > 0, na.rm = TRUE),
      
      n_any_work_1_hours_nonpositive = sum(
        .data[[any_work_var]] == 1 &
          !is.na(.data[[hours_var]]) &
          .data[[hours_var]] <= 0,
        na.rm = TRUE
      ),
      n_any_work_0_hours_positive = sum(
        .data[[any_work_var]] == 0 &
          !is.na(.data[[hours_var]]) &
          .data[[hours_var]] > 0,
        na.rm = TRUE
      ),
      share_any_work_1_hours_nonpositive = n_any_work_1_hours_nonpositive / n_rows,
      share_any_work_0_hours_positive = n_any_work_0_hours_positive / n_rows,
      
      .groups = "drop"
    )
}

make_spouse_long_for_vars <- function(df,
                                      id_vars = c("couple_id", "wave", "year", "ym", "period", "source"),
                                      any_work_base = "any_work",
                                      hours_base = "hours") {
  
  available_id_vars <- intersect(id_vars, names(df))
  
  wife_vars <- intersect(
    c(paste0(any_work_base, "_w"), paste0(hours_base, "_w")),
    names(df)
  )
  
  husband_vars <- intersect(
    c(paste0(any_work_base, "_h"), paste0(hours_base, "_h")),
    names(df)
  )
  
  wife <- df %>%
    dplyr::select(
      dplyr::all_of(available_id_vars),
      dplyr::all_of(wife_vars)
    ) %>%
    dplyr::rename_with(
      ~ stringr::str_remove(.x, "_w$"),
      dplyr::ends_with("_w")
    ) %>%
    dplyr::mutate(spouse = "Wife")
  
  husband <- df %>%
    dplyr::select(
      dplyr::all_of(available_id_vars),
      dplyr::all_of(husband_vars)
    ) %>%
    dplyr::rename_with(
      ~ stringr::str_remove(.x, "_h$"),
      dplyr::ends_with("_h")
    ) %>%
    dplyr::mutate(spouse = "Husband")
  
  dplyr::bind_rows(wife, husband)
}

plot_share_any_work <- function(df_sum,
                                x_var,
                                filename,
                                title,
                                facet_var = NULL) {
  
  p <- ggplot(
    df_sum,
    aes(x = .data[[x_var]], y = share_any_work, group = 1)
  ) +
    geom_line() +
    geom_point() +
    scale_y_continuous(labels = percent_format(), limits = c(0, 1)) +
    labs(
      x = NULL,
      y = "Share worked positive hours",
      title = title
    ) +
    theme_minimal(base_size = 13)
  
  if (!is.null(facet_var)) {
    p <- p +
      aes(group = .data[[facet_var]]) +
      facet_wrap(stats::as.formula(paste("~", facet_var)))
  }
  
  save_diag_plot(p, filename)
}

plot_missingness <- function(df_sum,
                             x_var,
                             filename,
                             title,
                             facet_var = NULL) {
  
  dd <- df_sum %>%
    dplyr::select(
      dplyr::all_of(c(x_var, facet_var)),
      share_any_work_missing,
      share_hours_missing
    ) %>%
    tidyr::pivot_longer(
      cols = c(share_any_work_missing, share_hours_missing),
      names_to = "variable",
      values_to = "share_missing"
    ) %>%
    dplyr::mutate(
      variable = dplyr::case_when(
        variable == "share_any_work_missing" ~ "any_work missing",
        variable == "share_hours_missing" ~ "hours missing",
        TRUE ~ variable
      )
    )
  
  p <- ggplot(
    dd,
    aes(x = .data[[x_var]], y = share_missing, color = variable, group = variable)
  ) +
    geom_line() +
    geom_point() +
    scale_y_continuous(labels = percent_format(), limits = c(0, 1)) +
    labs(
      x = NULL,
      y = "Share missing",
      color = NULL,
      title = title
    ) +
    theme_minimal(base_size = 13) +
    theme(legend.position = "bottom")
  
  if (!is.null(facet_var)) {
    p <- p +
      facet_wrap(stats::as.formula(paste("~", facet_var)))
  }
  
  save_diag_plot(p, filename)
}

plot_hours_distribution <- function(df_sum,
                                    x_var,
                                    filename,
                                    title,
                                    facet_var = NULL) {
  
  dd <- df_sum %>%
    dplyr::select(
      dplyr::all_of(c(x_var, facet_var)),
      mean_hours,
      p10_hours,
      p25_hours,
      p50_hours,
      p75_hours,
      p90_hours
    )
  
  p <- ggplot(dd, aes(x = .data[[x_var]])) +
    geom_ribbon(
      aes(ymin = p10_hours, ymax = p90_hours),
      alpha = 0.15
    ) +
    geom_ribbon(
      aes(ymin = p25_hours, ymax = p75_hours),
      alpha = 0.25
    ) +
    geom_line(aes(y = p50_hours), linewidth = 0.8) +
    geom_point(aes(y = p50_hours)) +
    geom_line(aes(y = mean_hours), linetype = "dashed") +
    labs(
      x = NULL,
      y = "Hours",
      title = title,
      subtitle = "Solid = median; dashed = mean; darker band = p25-p75; lighter band = p10-p90"
    ) +
    theme_minimal(base_size = 13)
  
  if (!is.null(facet_var)) {
    p <- p +
      facet_wrap(stats::as.formula(paste("~", facet_var)))
  }
  
  save_diag_plot(p, filename)
}

plot_zero_positive_hours <- function(df_sum,
                                     x_var,
                                     filename,
                                     title,
                                     facet_var = NULL) {
  
  dd <- df_sum %>%
    dplyr::select(
      dplyr::all_of(c(x_var, facet_var)),
      share_hours_zero,
      share_hours_positive
    ) %>%
    tidyr::pivot_longer(
      cols = c(share_hours_zero, share_hours_positive),
      names_to = "variable",
      values_to = "share"
    ) %>%
    dplyr::mutate(
      variable = dplyr::case_when(
        variable == "share_hours_zero" ~ "Hours = 0",
        variable == "share_hours_positive" ~ "Hours > 0",
        TRUE ~ variable
      )
    )
  
  p <- ggplot(
    dd,
    aes(x = .data[[x_var]], y = share, color = variable, group = variable)
  ) +
    geom_line() +
    geom_point() +
    scale_y_continuous(labels = percent_format(), limits = c(0, 1)) +
    labs(
      x = NULL,
      y = "Share among non-missing hours",
      color = NULL,
      title = title
    ) +
    theme_minimal(base_size = 13) +
    theme(legend.position = "bottom")
  
  if (!is.null(facet_var)) {
    p <- p +
      facet_wrap(stats::as.formula(paste("~", facet_var)))
  }
  
  save_diag_plot(p, filename)
}

plot_consistency_issues <- function(df_sum,
                                    x_var,
                                    filename,
                                    title,
                                    facet_var = NULL) {
  
  dd <- df_sum %>%
    dplyr::select(
      dplyr::all_of(c(x_var, facet_var)),
      share_any_work_1_hours_nonpositive,
      share_any_work_0_hours_positive
    ) %>%
    tidyr::pivot_longer(
      cols = c(
        share_any_work_1_hours_nonpositive,
        share_any_work_0_hours_positive
      ),
      names_to = "issue",
      values_to = "share"
    ) %>%
    dplyr::mutate(
      issue = dplyr::case_when(
        issue == "share_any_work_1_hours_nonpositive" ~ "any_work = 1 and hours <= 0",
        issue == "share_any_work_0_hours_positive" ~ "any_work = 0 and hours > 0",
        TRUE ~ issue
      )
    )
  
  p <- ggplot(
    dd,
    aes(x = .data[[x_var]], y = share, color = issue, group = issue)
  ) +
    geom_line() +
    geom_point() +
    scale_y_continuous(labels = percent_format()) +
    labs(
      x = NULL,
      y = "Share of rows",
      color = NULL,
      title = title
    ) +
    theme_minimal(base_size = 13) +
    theme(legend.position = "bottom")
  
  if (!is.null(facet_var)) {
    p <- p +
      facet_wrap(stats::as.formula(paste("~", facet_var)))
  }
  
  save_diag_plot(p, filename)
}

make_all_diagnostic_plots <- function(df,
                                      group_vars,
                                      x_var,
                                      prefix,
                                      label,
                                      any_work_var = "any_work",
                                      hours_var = "hours",
                                      facet_var = NULL) {
  
  df_sum <- summarise_any_work_hours(
    df = df,
    group_vars = group_vars,
    any_work_var = any_work_var,
    hours_var = hours_var
  )
  
  plot_share_any_work(
    df_sum = df_sum,
    x_var = x_var,
    facet_var = facet_var,
    filename = paste0(prefix, "_share_worked_positive_hours.png"),
    title = paste0(label, ": share worked positive hours")
  )
  
  plot_missingness(
    df_sum = df_sum,
    x_var = x_var,
    facet_var = facet_var,
    filename = paste0(prefix, "_missingness.png"),
    title = paste0(label, ": missingness in any_work and hours")
  )
  
  plot_hours_distribution(
    df_sum = df_sum,
    x_var = x_var,
    facet_var = facet_var,
    filename = paste0(prefix, "_hours_distribution.png"),
    title = paste0(label, ": hours distribution")
  )
  
  plot_zero_positive_hours(
    df_sum = df_sum,
    x_var = x_var,
    facet_var = facet_var,
    filename = paste0(prefix, "_zero_positive_hours.png"),
    title = paste0(label, ": zero vs positive hours")
  )
  
  plot_consistency_issues(
    df_sum = df_sum,
    x_var = x_var,
    facet_var = facet_var,
    filename = paste0(prefix, "_consistency_issues.png"),
    title = paste0(label, ": consistency checks")
  )
  
  invisible(df_sum)
}

# =============================================================================
# 1. COVID person-level diagnostics by wave
# =============================================================================

covid_person_file <- file.path(der_path, "df_sample_long_covid.rds")

if (file.exists(covid_person_file)) {
  
  df_covid_person <- readRDS(covid_person_file)
  
  if (all(c("wave", "any_work", "hours") %in% names(df_covid_person))) {
    
    df_covid_person <- df_covid_person %>%
      dplyr::mutate(
        wave = factor(
          wave,
          levels = c("2019", "baseline", "ca", "cb", "cc", "cd", "ce", "cf", "cg", "ch", "ci")
        )
      )
    
    make_all_diagnostic_plots(
      df = df_covid_person,
      group_vars = c("wave"),
      x_var = "wave",
      prefix = "covid_person_by_wave",
      label = "COVID person-level by wave",
      any_work_var = "any_work",
      hours_var = "hours"
    )
  }
}

# =============================================================================
# 2. COVID couple/spouse diagnostics by wave
# =============================================================================

covid_couple_file <- file.path(der_path, "df_sample_long_covid_couplelevel.rds")

if (file.exists(covid_couple_file)) {
  
  df_covid_couple <- readRDS(covid_couple_file)
  
  df_covid_spouse <- make_spouse_long_for_vars(
    df = df_covid_couple,
    id_vars = c("couple_id", "wave"),
    any_work_base = "any_work",
    hours_base = "hours"
  )
  
  if (all(c("wave", "spouse", "any_work", "hours") %in% names(df_covid_spouse))) {
    
    df_covid_spouse <- df_covid_spouse %>%
      dplyr::mutate(
        wave = factor(
          wave,
          levels = c("2019", "baseline", "ca", "cb", "cc", "cd", "ce", "cf", "cg", "ch", "ci")
        )
      )
    
    make_all_diagnostic_plots(
      df = df_covid_spouse,
      group_vars = c("wave", "spouse"),
      x_var = "wave",
      prefix = "covid_couple_spouse_by_wave",
      label = "COVID couple-level by wave and spouse",
      any_work_var = "any_work",
      hours_var = "hours",
      facet_var = "spouse"
    )
  }
}

# =============================================================================
# 3. Future regular-wave person-level diagnostics by year
# =============================================================================

future_person_file <- file.path(der_path, "future_outcomes_long_lmo.rds")

if (file.exists(future_person_file)) {
  
  df_future_person <- readRDS(future_person_file)
  
  if (all(c("year", "any_work", "jbhrs") %in% names(df_future_person))) {
    
    make_all_diagnostic_plots(
      df = df_future_person,
      group_vars = c("year"),
      x_var = "year",
      prefix = "future_person_by_year",
      label = "Future regular-wave person-level by year",
      any_work_var = "any_work",
      hours_var = "jbhrs"
    )
  }
}

# =============================================================================
# 4. Future regular-wave couple/spouse diagnostics by year
# =============================================================================

future_couple_file <- file.path(der_path, "future_outcomes_couple_long_lmo.rds")

if (file.exists(future_couple_file)) {
  
  df_future_couple <- readRDS(future_couple_file)
  
  df_future_spouse <- make_spouse_long_for_vars(
    df = df_future_couple,
    id_vars = c("couple_id", "wave", "year", "ym"),
    any_work_base = "any_work",
    hours_base = "jbhrs"
  )
  
  if (all(c("year", "spouse", "any_work", "jbhrs") %in% names(df_future_spouse))) {
    
    make_all_diagnostic_plots(
      df = df_future_spouse,
      group_vars = c("year", "spouse"),
      x_var = "year",
      prefix = "future_couple_spouse_by_year",
      label = "Future regular-wave couple-level by year and spouse",
      any_work_var = "any_work",
      hours_var = "jbhrs",
      facet_var = "spouse"
    )
  }
}

# =============================================================================
# 5. Main-survey-only history + future couple/spouse diagnostics by year
# =============================================================================

mainonly_couple_file <- file.path(der_path, "couple_history_future_mainonly_long.rds")

if (file.exists(mainonly_couple_file)) {
  
  df_mainonly_couple <- readRDS(mainonly_couple_file)
  
  df_mainonly_spouse <- make_spouse_long_for_vars(
    df = df_mainonly_couple,
    id_vars = c("couple_id", "wave", "year", "ym", "period", "source"),
    any_work_base = "any_work",
    hours_base = "jbhrs"
  )
  
  if (all(c("year", "period", "spouse", "any_work", "jbhrs") %in% names(df_mainonly_spouse))) {
    
    make_all_diagnostic_plots(
      df = df_mainonly_spouse,
      group_vars = c("year", "spouse"),
      x_var = "year",
      prefix = "mainonly_history_future_couple_spouse_by_year",
      label = "Main-survey history + future couple-level by year and spouse",
      any_work_var = "any_work",
      hours_var = "jbhrs",
      facet_var = "spouse"
    )
    
    make_all_diagnostic_plots(
      df = df_mainonly_spouse,
      group_vars = c("period", "year", "spouse"),
      x_var = "year",
      prefix = "mainonly_history_future_couple_spouse_by_period_year",
      label = "Main-survey history + future couple-level by period, year, and spouse",
      any_work_var = "any_work",
      hours_var = "jbhrs",
      facet_var = "spouse"
    )
  }
}

# =============================================================================
# Done
# =============================================================================

cat("\nFigure diagnostics complete.\n")
cat("Figures saved to:\n")
cat("  ", diag_fig_path, "\n", sep = "")