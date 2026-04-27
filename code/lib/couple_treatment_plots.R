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
      x = factor(wave, levels = wl$wave, labels = wl$wave_lab_short),
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
#   - "year"
#
# Uses couple-level long data (one row per couple x time point), not spouse-long.
# Each couple is counted once per time point.
# -----------------------------------------------------------------------------
plot_future_treatment_group_counts <- function(
    df,
    treatment_var,
    agg = c("wave", "year"),
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
    title_size = 14
) {
  agg <- match.arg(agg)
  stopifnot(treatment_var %in% names(df))
  
  time_var <- agg
  
  df <- df %>%
    filter_couple_plot_restriction(restriction = restriction)
  
  dd <- df %>%
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
        labels = wl_future$wave_lab_short
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
      x = factor(wave, levels = wl$wave, labels = wl$wave_lab_short),
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
      x = factor(wave, levels = wl$wave, labels = wl$wave_lab_short),
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
    agg = c("wave", "year"),
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
  agg <- match.arg(agg)
  
  stopifnot(var %in% names(df))
  stopifnot(treatment_var %in% names(df))
  
  zero_if_not_working <- var %in% c(
    "jbhrs",
    "jbot",
    "basrate",
    "paygu_dv",
    "fimnlabgrs_dv",
    "fimngrs_dv"
  )
  
  include_baseline_2019 <- !(var %in% c("workoutside", "wfh_some", "wfh_cat"))
  
  df <- df %>%
    filter_couple_plot_restriction(restriction = restriction)
  
  # Build a combined group variable BEFORE calling the future helper,
  # because the helper returns aggregated data without pidp.
  dd <- df %>%
    filter_couples_by_child_subset(child_subset = child_subset) %>%
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
  
  p <- .apply_time_labels(p, dd_plot, agg = agg)
  
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
    agg = c("wave", "year"),
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
  agg <- match.arg(agg)
  
  stopifnot(var %in% names(df))
  stopifnot(treatment_var %in% names(df))
  
  zero_if_not_working <- var %in% c(
    "jbhrs",
    "jbot",
    "basrate",
    "paygu_dv",
    "fimnlabgrs_dv",
    "fimngrs_dv"
  )
  
  include_baseline_2019 <- !(var %in% c("workoutside", "wfh_some", "wfh_cat"))
  
  df <- df %>%
    filter_couple_plot_restriction(restriction = restriction)
  
  # Same logic: encode child group + spouse + treatment into one grouping var
  # before calling the summarization helper.
  dd <- df %>%
    filter_couples_for_child_grid() %>%
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
  
  p <- .apply_time_labels(p, dd_plot, agg = agg)
  
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
    agg = c("year"),
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
    title_size = 14
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
    "paygu_dv",
    "fimnlabgrs_dv",
    "fimngrs_dv"
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
    agg = c("year"),
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
    title_size = 14
) {
  agg <- match.arg(agg)

  stopifnot(var %in% names(df))
  stopifnot(treatment_var %in% names(df))
  stopifnot(agg %in% names(df))

  zero_if_not_working <- var %in% c(
    "jbhrs",
    "jbot",
    "basrate",
    "paygu_dv",
    "fimnlabgrs_dv",
    "fimngrs_dv"
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
      title = if (include_title) paste(couple_plot_var_label(var), "| history + future") else NULL
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
# Main-survey history + future couple counts by treatment group
# -----------------------------------------------------------------------------
plot_main_history_future_treatment_group_counts <- function(
    df,
    treatment_var,
    agg = c("year"),
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
    title_size = 14
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

  ggsave(
    filename = out_file,
    plot = p,
    path = fig_path,
    width = 12,
    height = 9
  )

  p
}
