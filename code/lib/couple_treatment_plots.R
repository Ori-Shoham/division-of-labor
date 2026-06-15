# =============================================================================
# File: code/lib/couple_treatment_plots.R
#
# Purpose:
#   Plot helpers for spouse-separated couple-treatment descriptives.
#
# Design choices:
#   - treatment shown in color / shape
#   - standard version: facet rows = spouse
#   - child-grid version: facet rows = child group, facet cols = spouse
# =============================================================================

suppressPackageStartupMessages({
  library(tidyverse)
})

# -----------------------------------------------------------------------------
# Theme tweak for clearer facet separation while staying close to theme_minimal
# -----------------------------------------------------------------------------
theme_couple_facets <- function(axis_text_size = 12,
                                axis_title_size = 13,
                                strip_text_size = 12,
                                legend_text_size = 11,
                                legend_title_size = 11,
                                title_size = 14) {
  ggplot2::theme(
    legend.position = "bottom",
    axis.text.x = ggplot2::element_text(
      angle = 90, hjust = 1, size = axis_text_size
    ),
    axis.text.y = ggplot2::element_text(size = axis_text_size),
    axis.title.x = ggplot2::element_text(size = axis_title_size),
    axis.title.y = ggplot2::element_text(size = axis_title_size),
    
    panel.spacing = grid::unit(0.9, "lines"),
    panel.border = ggplot2::element_rect(
      colour = "grey75",
      fill = NA,
      linewidth = 0.6
    ),
    
    strip.background = ggplot2::element_rect(
      fill = "grey96",
      colour = "grey75",
      linewidth = 0.6
    ),
    strip.text = ggplot2::element_text(
      face = "plain",
      size = strip_text_size
    ),
    
    legend.text = ggplot2::element_text(size = legend_text_size),
    legend.title = ggplot2::element_text(size = legend_title_size),
    plot.title = ggplot2::element_text(size = title_size)
  )
}

# -----------------------------------------------------------------------------
# Apply common treatment/control discrete scales
#
# This preserves the same mapping used by add_treatment_group_label():
# treatment is the first level and control is the second level.
# -----------------------------------------------------------------------------
add_treatment_group_scales <- function(p,
                                       treatment_var,
                                       treated_label = NULL,
                                       untreated_label = NULL) {
  group_levels <- treatment_group_levels(
    treatment_var = treatment_var,
    treated_label = treated_label,
    untreated_label = untreated_label
  )
  
  p +
    ggplot2::scale_color_discrete(
      limits = group_levels,
      breaks = group_levels,
      drop = FALSE
    ) +
    ggplot2::scale_shape_discrete(
      limits = group_levels,
      breaks = group_levels,
      drop = FALSE
    )
}

# -----------------------------------------------------------------------------
# Main-history/future yearly x-axis scale
#
# Used only for main-survey history + future year plots.
# Gives one major break per calendar year and no minor breaks.
# -----------------------------------------------------------------------------
.add_yearly_x_breaks <- function(p, dd, time_var = "time") {
  if (is.null(dd) || nrow(dd) == 0 || !(time_var %in% names(dd))) {
    return(p)
  }
  
  years <- suppressWarnings(as.integer(dd[[time_var]]))
  years <- years[!is.na(years)]
  
  if (length(years) == 0) {
    return(p)
  }
  
  year_breaks <- seq(
    from = min(years, na.rm = TRUE),
    to = max(years, na.rm = TRUE),
    by = 1
  )
  
  p +
    ggplot2::scale_x_continuous(
      breaks = year_breaks,
      minor_breaks = NULL
    )
}

.filter_monthly_window <- function(dd,
                                   time_var = "time",
                                   monthly_start_ym = as.Date("2019-01-01"),
                                   monthly_end_ym = as.Date("2021-12-01")) {
  if (is.null(dd) || nrow(dd) == 0 || !(time_var %in% names(dd))) {
    return(dd)
  }

  monthly_start_ym <- as.Date(monthly_start_ym)
  monthly_end_ym <- as.Date(monthly_end_ym)

  dd %>%
    dplyr::mutate("{time_var}" := as.Date(.data[[time_var]])) %>%
    dplyr::filter(
      !is.na(.data[[time_var]]),
      .data[[time_var]] >= monthly_start_ym,
      .data[[time_var]] <= monthly_end_ym
    )
}

.drop_jan_feb_2020_for_yearly <- function(dd, agg, ym_var = "ym") {
  if (
    agg != "year" ||
    is.null(dd) ||
    nrow(dd) == 0 ||
    !(ym_var %in% names(dd))
  ) {
    return(dd)
  }

  dd %>%
    dplyr::mutate(
      .plot_ym = as.Date(.data[[ym_var]]),
      .plot_year = suppressWarnings(as.integer(format(.plot_ym, "%Y"))),
      .plot_month = suppressWarnings(as.integer(format(.plot_ym, "%m")))
    ) %>%
    dplyr::filter(!(.plot_year == 2020 & !is.na(.plot_month) & .plot_month <= 2)) %>%
    dplyr::select(-.plot_ym, -.plot_year, -.plot_month)
}

.monthly_window_breaks <- function(monthly_start_ym = as.Date("2019-01-01"),
                                   monthly_end_ym = as.Date("2021-12-01")) {
  seq(
    from = as.Date(monthly_start_ym),
    to = as.Date(monthly_end_ym),
    by = "3 months"
  )
}

.monthly_effective_start <- function(dd = NULL,
                                     time_var = "time",
                                     monthly_start_ym = as.Date("2019-01-01"),
                                     monthly_end_ym = as.Date("2021-12-01")) {
  monthly_start_ym <- as.Date(monthly_start_ym)
  monthly_end_ym <- as.Date(monthly_end_ym)

  if (is.null(dd) || nrow(dd) == 0 || !(time_var %in% names(dd))) {
    return(monthly_start_ym)
  }

  first_observed_ym <- suppressWarnings(min(as.Date(dd[[time_var]]), na.rm = TRUE))
  if (is.na(first_observed_ym) || !is.finite(as.numeric(first_observed_ym))) {
    return(monthly_start_ym)
  }

  effective_start_ym <- max(monthly_start_ym, as.Date(first_observed_ym))
  min(effective_start_ym, monthly_end_ym)
}

.apply_monthly_window_x_scale <- function(p,
                                          dd = NULL,
                                          time_var = "time",
                                          monthly_start_ym = as.Date("2019-01-01"),
                                          monthly_end_ym = as.Date("2021-12-01")) {
  monthly_start_ym <- .monthly_effective_start(
    dd = dd,
    time_var = time_var,
    monthly_start_ym = monthly_start_ym,
    monthly_end_ym = monthly_end_ym
  )
  monthly_end_ym <- as.Date(monthly_end_ym)

  p +
    ggplot2::scale_x_date(
      limits = c(monthly_start_ym, monthly_end_ym),
      breaks = .monthly_window_breaks(
        monthly_start_ym = monthly_start_ym,
        monthly_end_ym = monthly_end_ym
      ),
      date_labels = "%b %Y"
    )
}

.apply_regular_wave_time_labels <- function(p,
                                            dd,
                                            agg,
                                            monthly_start_ym = as.Date("2019-01-01"),
                                            monthly_end_ym = as.Date("2021-12-01")) {
  if (agg == "year") {
    .add_yearly_x_breaks(p, dd, time_var = "time")
  } else if (agg == "ym") {
    .apply_monthly_window_x_scale(
      p,
      dd = dd,
      time_var = "time",
      monthly_start_ym = monthly_start_ym,
      monthly_end_ym = monthly_end_ym
    )
  } else {
    .apply_time_labels(p, dd, agg = agg)
  }
}

# -----------------------------------------------------------------------------
# Couple counts over time by treatment group: COVID waves
# -----------------------------------------------------------------------------
plot_covid_treatment_group_counts <- function(
    df,
    treatment_var,
    out_file,
    fig_path,
    outcome_vars = covid_count_outcome_vars(),
    include_title = FALSE,
    restriction = NULL,
    treated_label = NULL,
    require_both_spouses = FALSE,
    axis_text_size = 12,
    axis_title_size = 13,
    strip_text_size = 12,
    legend_text_size = 11,
    legend_title_size = 11,
    title_size = 14
) {
  stopifnot(treatment_var %in% names(df))
  
  wl <- wave_labels()
  
  df <- df %>%
    filter_couple_plot_restriction(restriction = restriction)
  
  dd <- df %>%
    filter_observed_couple_rows_for_counts(
      vars = outcome_vars,
      require_both_spouses = require_both_spouses
    ) %>%
    dplyr::distinct(
      couple_id,
      wave,
      .data[[treatment_var]],
      has_child_u10_2019,
      has_child_11_17_2019
    ) %>%
    add_treatment_group_label(
      treatment_var = treatment_var,
      treated_label = treated_label
    ) %>%
    expand_couple_samples_for_counts() %>%
    dplyr::filter(
      !is.na(treatment_group),
      !is.na(wave)
    ) %>%
    dplyr::group_by(sample_group, wave, treatment_group) %>%
    dplyr::summarise(
      n_couples = dplyr::n_distinct(couple_id),
      .groups = "drop"
    ) %>%
    dplyr::left_join(wl, by = "wave")
  
  p <- ggplot(
    dd,
    aes(
      x = factor(wave, levels = wl$wave, labels = wl$wave_label_short),
      y = n_couples,
      color = treatment_group,
      shape = treatment_group,
      group = treatment_group
    )
  ) +
    geom_line(na.rm = TRUE) +
    geom_point(size = 2.4, na.rm = TRUE) +
    facet_grid(sample_group ~ ., scales = "fixed") +
    theme_minimal() +
    labs(
      x = NULL,
      y = if (require_both_spouses) {
        "Number of couples (both spouses observed)"
      } else {
        "Number of couples"
      },
      color = NULL,
      shape = NULL,
      title = if (include_title) paste(
        "Couple counts over time |",
        if (require_both_spouses) "both spouses observed |" else "any spouse observed |",
        treatment_var
      ) else NULL
    ) +
    theme_couple_facets(
      axis_text_size = axis_text_size,
      axis_title_size = axis_title_size,
      strip_text_size = strip_text_size,
      legend_text_size = legend_text_size,
      legend_title_size = legend_title_size,
      title_size = title_size
    )
  
  p <- add_treatment_group_scales(
    p = p,
    treatment_var = treatment_var,
    treated_label = treated_label
  )
  
  ggsave(
    filename = out_file,
    plot = p,
    path = fig_path,
    width = 12,
    height = 9
  )
  
  p
}

# -----------------------------------------------------------------------------
# Couple counts over time by treatment group: future outcomes
#
# agg:
#   - "wave"
#   - "ym"
#   - "year"
#
# Uses couple-level long data (one row per couple x time point), not spouse-long.
# Each couple is counted once per time point.
# -----------------------------------------------------------------------------
plot_future_treatment_group_counts <- function(
    df,
    treatment_var,
    agg = c("wave", "ym", "year"),
    out_file,
    fig_path,
    outcome_vars = future_count_outcome_vars(),
    include_title = FALSE,
    restriction = NULL,
    treated_label = NULL,
    require_both_spouses = FALSE,
    axis_text_size = 12,
    axis_title_size = 13,
    strip_text_size = 12,
    legend_text_size = 11,
    legend_title_size = 11,
    title_size = 14,
    monthly_start_ym = as.Date("2019-01-01"),
    monthly_end_ym = as.Date("2021-12-01")
) {
  agg <- match.arg(agg)
  stopifnot(treatment_var %in% names(df))
  
  time_var <- agg
  
  df <- df %>%
    filter_couple_plot_restriction(restriction = restriction)
  
  dd <- df %>%
    .drop_jan_feb_2020_for_yearly(agg = agg) %>%
    filter_observed_couple_rows_for_counts(
      vars = outcome_vars,
      require_both_spouses = require_both_spouses
    ) %>%
    dplyr::distinct(
      couple_id,
      .data[[time_var]],
      .data[[treatment_var]],
      has_child_u10_2019,
      has_child_11_17_2019,
      .keep_all = FALSE
    ) %>%
    dplyr::rename(time = .data[[time_var]]) %>%
    add_treatment_group_label(
      treatment_var = treatment_var,
      treated_label = treated_label
    ) %>%
    expand_couple_samples_for_counts() %>%
    dplyr::filter(
      !is.na(treatment_group),
      !is.na(time)
    ) %>%
    { if (agg == "ym") {
      .filter_monthly_window(
        .,
        monthly_start_ym = monthly_start_ym,
        monthly_end_ym = monthly_end_ym
      )
    } else . } %>%
    dplyr::group_by(sample_group, time, treatment_group) %>%
    dplyr::summarise(
      n_couples = dplyr::n_distinct(couple_id),
      .groups = "drop"
    )
  
  p <- ggplot(
    dd,
    aes(
      x = time,
      y = n_couples,
      color = treatment_group,
      shape = treatment_group,
      group = treatment_group
    )
  ) +
    geom_line(na.rm = TRUE) +
    geom_point(size = 2.4, na.rm = TRUE) +
    facet_grid(sample_group ~ ., scales = "fixed") +
    theme_minimal() +
    labs(
      x = NULL,
      y = if (require_both_spouses) {
        "Number of couples (both spouses observed)"
      } else {
        "Number of couples"
      },
      color = NULL,
      shape = NULL,
      title = if (include_title) paste(
        "Couple counts over time |",
        if (require_both_spouses) "both spouses observed |" else "any spouse observed |",
        treatment_var
      ) else NULL
    ) +
    theme_couple_facets(
      axis_text_size = axis_text_size,
      axis_title_size = axis_title_size,
      strip_text_size = strip_text_size,
      legend_text_size = legend_text_size,
      legend_title_size = legend_title_size,
      title_size = title_size
    )
  
  p <- add_treatment_group_scales(
    p = p,
    treatment_var = treatment_var,
    treated_label = treated_label
  )
  
  if (agg == "wave") {
    wl_future <- wave_labels() %>%
      dplyr::filter(wave %in% unique(dd$time))
    
    p <- p +
      scale_x_discrete(
        limits = wl_future$wave,
        labels = wl_future$wave_label_short
      )
  } else {
    p <- .apply_regular_wave_time_labels(
      p,
      dd,
      agg = agg,
      monthly_start_ym = monthly_start_ym,
      monthly_end_ym = monthly_end_ym
    )
  }
  
  ggsave(
    filename = out_file,
    plot = p,
    path = fig_path,
    width = 12,
    height = 9
  )
  
  p
}

# -----------------------------------------------------------------------------
# COVID overtime plot: spouse facets
# -----------------------------------------------------------------------------
plot_covid_spouse_treatment_overtime <- function(
    df,
    var,
    treatment_var,
    child_subset = c("all", "u10", "11_17"),
    out_file,
    fig_path,
    include_title = FALSE,
    restriction = NULL,
    treated_label = NULL,
    axis_text_size = 12,
    axis_title_size = 13,
    strip_text_size = 12,
    legend_text_size = 11,
    legend_title_size = 11,
    title_size = 14
) {
  child_subset <- match.arg(child_subset)
  
  stopifnot(var %in% names(df))
  stopifnot(treatment_var %in% names(df))
  
  wl <- wave_labels()
  
  df <- df %>%
    filter_couple_plot_restriction(restriction = restriction)
  
  dd <- df %>%
    filter_couples_by_child_subset(child_subset = child_subset) %>%
    add_treatment_group_label(
      treatment_var = treatment_var,
      treated_label = treated_label
    ) %>%
    dplyr::mutate(
      value = if (couple_plot_is_binary(var)) {
        suppressWarnings(as.numeric(.data[[var]]))
      } else {
        clean_covid_numeric(.data[[var]])
      }
    ) %>%
    dplyr::filter(
      !is.na(value),
      !is.na(treatment_group),
      !is.na(spouse),
      !is.na(wave)
    ) %>%
    dplyr::group_by(wave, spouse, treatment_group) %>%
    dplyr::summarise(
      mean_y = mean(value, na.rm = TRUE),
      n = dplyr::n(),
      .groups = "drop"
    ) %>%
    dplyr::left_join(wl, by = "wave")
  
  p <- ggplot(
    dd,
    aes(
      x = factor(wave, levels = wl$wave, labels = wl$wave_label_short),
      y = mean_y,
      color = treatment_group,
      shape = treatment_group,
      group = treatment_group
    )
  ) +
    geom_line(na.rm = TRUE) +
    geom_point(size = 2.4, na.rm = TRUE) +
    facet_grid(spouse ~ ., scales = "fixed") +
    theme_minimal() +
    labs(
      x = NULL,
      y = couple_plot_var_units(var, is_binary = couple_plot_is_binary(var)),
      color = NULL,
      shape = NULL,
      title = if (include_title) paste(
        couple_plot_var_label(var),
        "| child subset:", child_subset
      ) else NULL
    ) +
    theme_couple_facets(
      axis_text_size = axis_text_size,
      axis_title_size = axis_title_size,
      strip_text_size = strip_text_size,
      legend_text_size = legend_text_size,
      legend_title_size = legend_title_size,
      title_size = title_size
    )
  
  p <- add_treatment_group_scales(
    p = p,
    treatment_var = treatment_var,
    treated_label = treated_label
  )
  
  if (couple_plot_is_binary(var)) {
    p <- p + scale_y_continuous(labels = scales::percent_format())
  }
  
  ggsave(
    filename = out_file,
    plot = p,
    path = fig_path,
    width = 12,
    height = 8
  )
  
  p
}

# -----------------------------------------------------------------------------
# COVID overtime plot: child-group x spouse facet grid
# -----------------------------------------------------------------------------
plot_covid_spouse_treatment_childgrid <- function(
    df,
    var,
    treatment_var,
    out_file,
    fig_path,
    include_title = FALSE,
    restriction = NULL,
    treated_label = NULL,
    axis_text_size = 12,
    axis_title_size = 13,
    strip_text_size = 12,
    legend_text_size = 11,
    legend_title_size = 11,
    title_size = 14
) {
  stopifnot(var %in% names(df))
  stopifnot(treatment_var %in% names(df))
  
  wl <- wave_labels()
  
  df <- df %>%
    filter_couple_plot_restriction(restriction = restriction)
  
  dd <- df %>%
    filter_couples_for_child_grid() %>%
    add_treatment_group_label(
      treatment_var = treatment_var,
      treated_label = treated_label
    ) %>%
    dplyr::mutate(
      value = if (couple_plot_is_binary(var)) {
        suppressWarnings(as.numeric(.data[[var]]))
      } else {
        clean_covid_numeric(.data[[var]])
      }
    ) %>%
    dplyr::filter(
      !is.na(value),
      !is.na(treatment_group),
      !is.na(spouse),
      !is.na(child_group_plot),
      !is.na(wave)
    ) %>%
    dplyr::group_by(wave, spouse, child_group_plot, treatment_group) %>%
    dplyr::summarise(
      mean_y = mean(value, na.rm = TRUE),
      n = dplyr::n(),
      .groups = "drop"
    ) %>%
    dplyr::left_join(wl, by = "wave")
  
  p <- ggplot(
    dd,
    aes(
      x = factor(wave, levels = wl$wave, labels = wl$wave_label_short),
      y = mean_y,
      color = treatment_group,
      shape = treatment_group,
      group = treatment_group
    )
  ) +
    geom_line(na.rm = TRUE) +
    geom_point(size = 2.0, na.rm = TRUE) +
    facet_grid(child_group_plot ~ spouse, scales = "fixed") +
    theme_minimal() +
    labs(
      x = NULL,
      y = couple_plot_var_units(var, is_binary = couple_plot_is_binary(var)),
      color = NULL,
      shape = NULL,
      title = if (include_title) couple_plot_var_label(var) else NULL
    ) +
    theme_couple_facets(
      axis_text_size = axis_text_size,
      axis_title_size = axis_title_size,
      strip_text_size = strip_text_size,
      legend_text_size = legend_text_size,
      legend_title_size = legend_title_size,
      title_size = title_size
    )
  
  p <- add_treatment_group_scales(
    p = p,
    treatment_var = treatment_var,
    treated_label = treated_label
  )
  
  if (couple_plot_is_binary(var)) {
    p <- p + scale_y_continuous(labels = scales::percent_format())
  }
  
  ggsave(
    filename = out_file,
    plot = p,
    path = fig_path,
    width = 13,
    height = 9
  )
  
  p
}

# -----------------------------------------------------------------------------
# Future numeric plot: spouse facets
# -----------------------------------------------------------------------------
plot_future_spouse_treatment_numeric <- function(
    df,
    var,
    treatment_var,
    child_subset = c("all", "u10", "11_17"),
    agg = c("wave", "ym", "year"),
    out_file,
    fig_path,
    include_title = FALSE,
    restriction = NULL,
    treated_label = NULL,
    axis_text_size = 12,
    axis_title_size = 13,
    strip_text_size = 12,
    legend_text_size = 11,
    legend_title_size = 11,
    title_size = 14,
    monthly_start_ym = as.Date("2019-01-01"),
    monthly_end_ym = as.Date("2021-12-01")
) {
  child_subset <- match.arg(child_subset)
  agg <- match.arg(agg)
  
  stopifnot(var %in% names(df))
  stopifnot(treatment_var %in% names(df))
  
  zero_if_not_working <- var %in% c(
    "jbhrs",
    "jbot",
    "basrate",
    "basrate_real",
    "paygu_dv",
    "paygu_dv_real",
    "fimnlabgrs_dv",
    "fimnlabgrs_dv_real",
    "fimngrs_dv",
    "fimngrs_dv_real"
  )
  
  include_baseline_2019 <- !(var %in% c("workoutside", "wfh_some", "wfh_cat"))
  
  df <- df %>%
    filter_couple_plot_restriction(restriction = restriction)
  
  dd <- df %>%
    filter_couples_by_child_subset(child_subset = child_subset) %>%
    .drop_jan_feb_2020_for_yearly(agg = agg) %>%
    add_treatment_group_label(
      treatment_var = treatment_var,
      treated_label = treated_label
    ) %>%
    dplyr::filter(
      !is.na(treatment_group),
      !is.na(spouse),
      !is.na(spouse_pidp)
    ) %>%
    dplyr::rename(pidp = spouse_pidp) %>%
    dplyr::mutate(
      spouse_treat_group = paste(spouse, treatment_group, sep = " || ")
    )
  
  prep <- .prepare_future_numeric_mean_data(
    df = dd,
    var = var,
    by = "spouse_treat_group",
    agg = agg,
    include_baseline_2019 = include_baseline_2019,
    baseline_var = paste0("base_", var),
    zero_if_not_working = zero_if_not_working,
    employment_var = "jbstat",
    exclude_2025 = TRUE
  )
  
  dd_plot <- prep$data %>%
    tidyr::separate(
      col = group,
      into = c("spouse", "treatment_group"),
      sep = " \\|\\| ",
      remove = TRUE
    ) %>%
    dplyr::mutate(
      spouse = factor(spouse, levels = c("Wife", "Husband")),
      treatment_group = factor(
        treatment_group,
        levels = treatment_group_levels(
          treatment_var = treatment_var,
          treated_label = treated_label
        )
      )
    )

  if (agg == "ym") {
    dd_plot <- .filter_monthly_window(
      dd_plot,
      monthly_start_ym = monthly_start_ym,
      monthly_end_ym = monthly_end_ym
    )
  }
  
  p <- ggplot(
    dd_plot,
    aes(
      x = time,
      y = mean_y,
      color = treatment_group,
      shape = treatment_group,
      group = treatment_group
    )
  ) +
    geom_line(na.rm = TRUE) +
    geom_point(size = 2.4, na.rm = TRUE) +
    facet_grid(spouse ~ ., scales = "fixed") +
    theme_minimal() +
    labs(
      x = NULL,
      y = couple_plot_var_units(var, is_binary = prep$is_binary),
      color = NULL,
      shape = NULL,
      title = if (include_title) paste(
        couple_plot_var_label(var),
        "| child subset:", child_subset
      ) else NULL
    ) +
    theme_couple_facets(
      axis_text_size = axis_text_size,
      axis_title_size = axis_title_size,
      strip_text_size = strip_text_size,
      legend_text_size = legend_text_size,
      legend_title_size = legend_title_size,
      title_size = title_size
    )
  
  p <- add_treatment_group_scales(
    p = p,
    treatment_var = treatment_var,
    treated_label = treated_label
  )
  
  p <- .apply_regular_wave_time_labels(
    p,
    dd_plot,
    agg = agg,
    monthly_start_ym = monthly_start_ym,
    monthly_end_ym = monthly_end_ym
  )
  
  if (prep$is_binary) {
    p <- p + scale_y_continuous(labels = scales::percent_format())
  }
  
  ggsave(
    filename = out_file,
    plot = p,
    path = fig_path,
    width = 12,
    height = 8
  )
  
  p
}

# -----------------------------------------------------------------------------
# Future numeric plot: child-group x spouse facet grid
# -----------------------------------------------------------------------------
plot_future_spouse_treatment_childgrid <- function(
    df,
    var,
    treatment_var,
    agg = c("wave", "ym", "year"),
    out_file,
    fig_path,
    include_title = FALSE,
    restriction = NULL,
    treated_label = NULL,
    axis_text_size = 12,
    axis_title_size = 13,
    strip_text_size = 12,
    legend_text_size = 11,
    legend_title_size = 11,
    title_size = 14,
    monthly_start_ym = as.Date("2019-01-01"),
    monthly_end_ym = as.Date("2021-12-01")
) {
  agg <- match.arg(agg)
  
  stopifnot(var %in% names(df))
  stopifnot(treatment_var %in% names(df))
  
  zero_if_not_working <- var %in% c(
    "jbhrs",
    "jbot",
    "basrate",
    "basrate_real",
    "paygu_dv",
    "paygu_dv_real",
    "fimnlabgrs_dv",
    "fimnlabgrs_dv_real",
    "fimngrs_dv",
    "fimngrs_dv_real"
  )
  
  include_baseline_2019 <- !(var %in% c("workoutside", "wfh_some", "wfh_cat"))
  
  df <- df %>%
    filter_couple_plot_restriction(restriction = restriction)
  
  dd <- df %>%
    filter_couples_for_child_grid() %>%
    .drop_jan_feb_2020_for_yearly(agg = agg) %>%
    add_treatment_group_label(
      treatment_var = treatment_var,
      treated_label = treated_label
    ) %>%
    dplyr::filter(
      !is.na(treatment_group),
      !is.na(spouse),
      !is.na(child_group_plot),
      !is.na(spouse_pidp)
    ) %>%
    dplyr::rename(pidp = spouse_pidp) %>%
    dplyr::mutate(
      child_spouse_treat_group = paste(
        child_group_plot,
        spouse,
        treatment_group,
        sep = " || "
      )
    )
  
  prep <- .prepare_future_numeric_mean_data(
    df = dd,
    var = var,
    by = "child_spouse_treat_group",
    agg = agg,
    include_baseline_2019 = include_baseline_2019,
    baseline_var = paste0("base_", var),
    zero_if_not_working = zero_if_not_working,
    employment_var = "jbstat",
    exclude_2025 = TRUE
  )
  
  dd_plot <- prep$data %>%
    tidyr::separate(
      col = group,
      into = c("child_group_plot", "spouse", "treatment_group"),
      sep = " \\|\\| ",
      remove = TRUE
    ) %>%
    dplyr::mutate(
      spouse = factor(spouse, levels = c("Wife", "Husband")),
      child_group_plot = factor(
        child_group_plot,
        levels = c("Young kids: 0-10", "Older kids: 11-17")
      ),
      treatment_group = factor(
        treatment_group,
        levels = treatment_group_levels(
          treatment_var = treatment_var,
          treated_label = treated_label
        )
      )
    )

  if (agg == "ym") {
    dd_plot <- .filter_monthly_window(
      dd_plot,
      monthly_start_ym = monthly_start_ym,
      monthly_end_ym = monthly_end_ym
    )
  }
  
  p <- ggplot(
    dd_plot,
    aes(
      x = time,
      y = mean_y,
      color = treatment_group,
      shape = treatment_group,
      group = treatment_group
    )
  ) +
    geom_line(na.rm = TRUE) +
    geom_point(size = 2.0, na.rm = TRUE) +
    facet_grid(child_group_plot ~ spouse, scales = "fixed") +
    theme_minimal() +
    labs(
      x = NULL,
      y = couple_plot_var_units(var, is_binary = prep$is_binary),
      color = NULL,
      shape = NULL,
      title = if (include_title) couple_plot_var_label(var) else NULL
    ) +
    theme_couple_facets(
      axis_text_size = axis_text_size,
      axis_title_size = axis_title_size,
      strip_text_size = strip_text_size,
      legend_text_size = legend_text_size,
      legend_title_size = legend_title_size,
      title_size = title_size
    )
  
  p <- add_treatment_group_scales(
    p = p,
    treatment_var = treatment_var,
    treated_label = treated_label
  )
  
  p <- .apply_regular_wave_time_labels(
    p,
    dd_plot,
    agg = agg,
    monthly_start_ym = monthly_start_ym,
    monthly_end_ym = monthly_end_ym
  )
  
  if (prep$is_binary) {
    p <- p + scale_y_continuous(labels = scales::percent_format())
  }
  
  ggsave(
    filename = out_file,
    plot = p,
    path = fig_path,
    width = 13,
    height = 9
  )
  
  p
}

# -----------------------------------------------------------------------------
# Main-survey history + future plot: spouse facets
#
# Intended input:
#   spouse-long version of couple_history_future_mainonly_long.rds
#   created with reshape_couple_long_to_spouse_long().
#
# This function uses regular UKHLS main-study rows only. If a stacked file that
# includes COVID rows is passed by accident, COVID rows are dropped by default.
# -----------------------------------------------------------------------------
plot_main_history_future_spouse_treatment_numeric <- function(
    df,
    var,
    treatment_var,
    child_subset = c("all", "u10", "11_17"),
    agg = c("year", "ym"),
    out_file,
    fig_path,
    include_title = FALSE,
    restriction = NULL,
    treated_label = NULL,
    drop_covid = TRUE,
    exclude_2025 = TRUE,
    axis_text_size = 12,
    axis_title_size = 13,
    strip_text_size = 12,
    legend_text_size = 11,
    legend_title_size = 11,
    title_size = 14,
    monthly_start_ym = as.Date("2019-01-01"),
    monthly_end_ym = as.Date("2021-12-01")
) {
  child_subset <- match.arg(child_subset)
  agg <- match.arg(agg)
  
  stopifnot(var %in% names(df))
  stopifnot(treatment_var %in% names(df))
  stopifnot(agg %in% names(df))
  
  zero_if_not_working <- var %in% c(
    "jbhrs",
    "jbot",
    "basrate",
    "basrate_real",
    "paygu_dv",
    "paygu_dv_real",
    "fimnlabgrs_dv",
    "fimnlabgrs_dv_real",
    "fimngrs_dv",
    "fimngrs_dv_real"
  )
  
  df <- df %>%
    filter_couple_plot_restriction(restriction = restriction)
  
  if (drop_covid && "period" %in% names(df)) {
    df <- df %>%
      dplyr::filter(!(as.character(period) %in% c("covid_baseline", "covid")))
  }
  
  if (drop_covid && "source" %in% names(df)) {
    df <- df %>%
      dplyr::filter(!grepl("^covid", as.character(source)))
  }
  
  dd <- df %>%
    filter_couples_by_child_subset(child_subset = child_subset) %>%
    .drop_jan_feb_2020_for_yearly(agg = agg) %>%
    add_treatment_group_label(
      treatment_var = treatment_var,
      treated_label = treated_label
    ) %>%
    dplyr::filter(
      !is.na(treatment_group),
      !is.na(spouse),
      !is.na(.data[[agg]])
    ) %>%
    dplyr::mutate(
      value = if (couple_plot_is_binary(var)) {
        suppressWarnings(as.numeric(.data[[var]]))
      } else {
        .clean_future_numeric_for_plot(
          x = .data[[var]],
          emp = if (zero_if_not_working) .data[["jbstat"]] else NULL,
          zero_if_not_working = zero_if_not_working
        )
      },
      time = .data[[agg]]
    ) %>%
    dplyr::filter(
      !is.na(value),
      !is.na(time),
      !(exclude_2025 & agg == "year" & time == 2025)
    ) %>%
    { if (agg == "ym") {
      .filter_monthly_window(
        .,
        monthly_start_ym = monthly_start_ym,
        monthly_end_ym = monthly_end_ym
      )
    } else . } %>%
    dplyr::group_by(time, spouse, treatment_group) %>%
    dplyr::summarise(
      mean_y = mean(value, na.rm = TRUE),
      n = dplyr::n(),
      .groups = "drop"
    )
  
  p <- ggplot(
    dd,
    aes(
      x = time,
      y = mean_y,
      color = treatment_group,
      shape = treatment_group,
      group = treatment_group
    )
  ) +
    geom_line(na.rm = TRUE) +
    geom_point(size = 2.4, na.rm = TRUE) +
    facet_grid(spouse ~ ., scales = "fixed") +
    theme_minimal() +
    labs(
      x = NULL,
      y = couple_plot_var_units(var, is_binary = couple_plot_is_binary(var)),
      color = NULL,
      shape = NULL,
      title = if (include_title) paste(
        couple_plot_var_label(var),
        "| history + future | child subset:", child_subset
      ) else NULL
    ) +
    theme_couple_facets(
      axis_text_size = axis_text_size,
      axis_title_size = axis_title_size,
      strip_text_size = strip_text_size,
      legend_text_size = legend_text_size,
      legend_title_size = legend_title_size,
      title_size = title_size
    )
  
  p <- add_treatment_group_scales(
    p = p,
    treatment_var = treatment_var,
    treated_label = treated_label
  )
  
  p <- .apply_regular_wave_time_labels(
    p,
    dd,
    agg = agg,
    monthly_start_ym = monthly_start_ym,
    monthly_end_ym = monthly_end_ym
  )
  
  if (couple_plot_is_binary(var)) {
    p <- p + scale_y_continuous(labels = scales::percent_format())
  }
  
  ggsave(
    filename = out_file,
    plot = p,
    path = fig_path,
    width = 12,
    height = 8
  )
  
  p
}

# -----------------------------------------------------------------------------
# Main-survey history + future plot: child-group x spouse facet grid
# -----------------------------------------------------------------------------
plot_main_history_future_spouse_treatment_childgrid <- function(
    df,
    var,
    treatment_var,
    agg = c("year", "ym"),
    out_file,
    fig_path,
    include_title = FALSE,
    restriction = NULL,
    treated_label = NULL,
    drop_covid = TRUE,
    exclude_2025 = TRUE,
    axis_text_size = 12,
    axis_title_size = 13,
    strip_text_size = 12,
    legend_text_size = 11,
    legend_title_size = 11,
    title_size = 14,
    monthly_start_ym = as.Date("2019-01-01"),
    monthly_end_ym = as.Date("2021-12-01")
) {
  agg <- match.arg(agg)
  
  stopifnot(var %in% names(df))
  stopifnot(treatment_var %in% names(df))
  stopifnot(agg %in% names(df))
  
  zero_if_not_working <- var %in% c(
    "jbhrs",
    "jbot",
    "basrate",
    "basrate_real",
    "paygu_dv",
    "paygu_dv_real",
    "fimnlabgrs_dv",
    "fimnlabgrs_dv_real",
    "fimngrs_dv",
    "fimngrs_dv_real"
  )
  
  df <- df %>%
    filter_couple_plot_restriction(restriction = restriction)
  
  if (drop_covid && "period" %in% names(df)) {
    df <- df %>%
      dplyr::filter(!(as.character(period) %in% c("covid_baseline", "covid")))
  }
  
  if (drop_covid && "source" %in% names(df)) {
    df <- df %>%
      dplyr::filter(!grepl("^covid", as.character(source)))
  }
  
  dd <- df %>%
    filter_couples_for_child_grid() %>%
    .drop_jan_feb_2020_for_yearly(agg = agg) %>%
    add_treatment_group_label(
      treatment_var = treatment_var,
      treated_label = treated_label
    ) %>%
    dplyr::filter(
      !is.na(treatment_group),
      !is.na(spouse),
      !is.na(child_group_plot),
      !is.na(.data[[agg]])
    ) %>%
    dplyr::mutate(
      value = if (couple_plot_is_binary(var)) {
        suppressWarnings(as.numeric(.data[[var]]))
      } else {
        .clean_future_numeric_for_plot(
          x = .data[[var]],
          emp = if (zero_if_not_working) .data[["jbstat"]] else NULL,
          zero_if_not_working = zero_if_not_working
        )
      },
      time = .data[[agg]]
    ) %>%
    dplyr::filter(
      !is.na(value),
      !is.na(time),
      !(exclude_2025 & agg == "year" & time == 2025)
    ) %>%
    { if (agg == "ym") {
      .filter_monthly_window(
        .,
        monthly_start_ym = monthly_start_ym,
        monthly_end_ym = monthly_end_ym
      )
    } else . } %>%
    dplyr::group_by(time, spouse, child_group_plot, treatment_group) %>%
    dplyr::summarise(
      mean_y = mean(value, na.rm = TRUE),
      n = dplyr::n(),
      .groups = "drop"
    )
  
  p <- ggplot(
    dd,
    aes(
      x = time,
      y = mean_y,
      color = treatment_group,
      shape = treatment_group,
      group = treatment_group
    )
  ) +
    geom_line(na.rm = TRUE) +
    geom_point(size = 2.0, na.rm = TRUE) +
    facet_grid(child_group_plot ~ spouse, scales = "fixed") +
    theme_minimal() +
    labs(
      x = NULL,
      y = couple_plot_var_units(var, is_binary = couple_plot_is_binary(var)),
      color = NULL,
      shape = NULL,
      title = if (include_title) paste(
        couple_plot_var_label(var),
        "| history + future"
      ) else NULL
    ) +
    theme_couple_facets(
      axis_text_size = axis_text_size,
      axis_title_size = axis_title_size,
      strip_text_size = strip_text_size,
      legend_text_size = legend_text_size,
      legend_title_size = legend_title_size,
      title_size = title_size
    )
  
  p <- add_treatment_group_scales(
    p = p,
    treatment_var = treatment_var,
    treated_label = treated_label
  )
  
  p <- .apply_regular_wave_time_labels(
    p,
    dd,
    agg = agg,
    monthly_start_ym = monthly_start_ym,
    monthly_end_ym = monthly_end_ym
  )
  
  if (couple_plot_is_binary(var)) {
    p <- p + scale_y_continuous(labels = scales::percent_format())
  }
  
  ggsave(
    filename = out_file,
    plot = p,
    path = fig_path,
    width = 13,
    height = 9
  )
  
  p
}

# -----------------------------------------------------------------------------
# Main-survey history + future couple counts by treatment group
# -----------------------------------------------------------------------------
plot_main_history_future_treatment_group_counts <- function(
    df,
    treatment_var,
    agg = c("year", "ym"),
    out_file,
    fig_path,
    outcome_vars = future_count_outcome_vars(),
    include_title = FALSE,
    restriction = NULL,
    treated_label = NULL,
    require_both_spouses = FALSE,
    drop_covid = TRUE,
    exclude_2025 = TRUE,
    axis_text_size = 12,
    axis_title_size = 13,
    strip_text_size = 12,
    legend_text_size = 11,
    legend_title_size = 11,
    title_size = 14,
    monthly_start_ym = as.Date("2019-01-01"),
    monthly_end_ym = as.Date("2021-12-01")
) {
  agg <- match.arg(agg)
  stopifnot(treatment_var %in% names(df))
  stopifnot(agg %in% names(df))
  
  df <- df %>%
    filter_couple_plot_restriction(restriction = restriction)
  
  if (drop_covid && "period" %in% names(df)) {
    df <- df %>%
      dplyr::filter(!(as.character(period) %in% c("covid_baseline", "covid")))
  }
  
  if (drop_covid && "source" %in% names(df)) {
    df <- df %>%
      dplyr::filter(!grepl("^covid", as.character(source)))
  }
  
  dd <- df %>%
    .drop_jan_feb_2020_for_yearly(agg = agg) %>%
    filter_observed_couple_rows_for_counts(
      vars = outcome_vars,
      require_both_spouses = require_both_spouses
    ) %>%
    dplyr::distinct(
      couple_id,
      .data[[agg]],
      .data[[treatment_var]],
      has_child_u10_2019,
      has_child_11_17_2019,
      .keep_all = FALSE
    ) %>%
    dplyr::rename(time = .data[[agg]]) %>%
    add_treatment_group_label(
      treatment_var = treatment_var,
      treated_label = treated_label
    ) %>%
    expand_couple_samples_for_counts() %>%
    dplyr::filter(
      !is.na(treatment_group),
      !is.na(time),
      !(exclude_2025 & agg == "year" & time == 2025)
    ) %>%
    { if (agg == "ym") {
      .filter_monthly_window(
        .,
        monthly_start_ym = monthly_start_ym,
        monthly_end_ym = monthly_end_ym
      )
    } else . } %>%
    dplyr::group_by(sample_group, time, treatment_group) %>%
    dplyr::summarise(
      n_couples = dplyr::n_distinct(couple_id),
      .groups = "drop"
    )
  
  p <- ggplot(
    dd,
    aes(
      x = time,
      y = n_couples,
      color = treatment_group,
      shape = treatment_group,
      group = treatment_group
    )
  ) +
    geom_line(na.rm = TRUE) +
    geom_point(size = 2.4, na.rm = TRUE) +
    facet_grid(sample_group ~ ., scales = "fixed") +
    theme_minimal() +
    labs(
      x = NULL,
      y = if (require_both_spouses) {
        "Number of couples (both spouses observed)"
      } else {
        "Number of couples"
      },
      color = NULL,
      shape = NULL,
      title = if (include_title) paste(
        "Couple counts over time | history + future |",
        if (require_both_spouses) "both spouses observed |" else "any spouse observed |",
        treatment_var
      ) else NULL
    ) +
    theme_couple_facets(
      axis_text_size = axis_text_size,
      axis_title_size = axis_title_size,
      strip_text_size = strip_text_size,
      legend_text_size = legend_text_size,
      legend_title_size = legend_title_size,
      title_size = title_size
    )
  
  p <- add_treatment_group_scales(
    p = p,
    treatment_var = treatment_var,
    treated_label = treated_label
  )
  
  p <- .apply_regular_wave_time_labels(
    p,
    dd,
    agg = agg,
    monthly_start_ym = monthly_start_ym,
    monthly_end_ym = monthly_end_ym
  )
  
  ggsave(
    filename = out_file,
    plot = p,
    path = fig_path,
    width = 12,
    height = 9
  )
  
  p
}
