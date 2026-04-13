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
theme_couple_facets <- function() {
  ggplot2::theme(
    legend.position = "bottom",
    axis.text.x = ggplot2::element_text(angle = 90, hjust = 1),
    
    # clearer separation between facets
    panel.spacing = grid::unit(0.9, "lines"),
    panel.border = ggplot2::element_rect(
      colour = "grey75",
      fill = NA,
      linewidth = 0.6
    ),
    
    # keep strip styling light / close to current look
    strip.background = ggplot2::element_rect(
      fill = "grey96",
      colour = "grey75",
      linewidth = 0.6
    ),
    strip.text = ggplot2::element_text(face = "plain")
  )
}

# -----------------------------------------------------------------------------
# Couple counts over time by treatment group: COVID waves
#
# -----------------------------------------------------------------------------
plot_covid_treatment_group_counts <- function(
    df,
    treatment_var,
    out_file,
    fig_path,
    outcome_vars = covid_count_outcome_vars(),
    include_title = FALSE,
    restriction = NULL,
    treated_label = NULL
) {
  stopifnot(treatment_var %in% names(df))
  
  wl <- wave_labels()
  df <- df %>%
    filter_couple_plot_restriction(restriction = restriction)
  dd <- df %>%
    filter_jointly_observed_couple_rows(vars = outcome_vars) %>%
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
      !is.na(wave),
      wave != "2019"
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
      y = "Number of couples",
      color = NULL,
      shape = NULL,
      title = if (include_title) paste("Couple counts over time |", treatment_var) else NULL
    ) +
    theme_couple_facets()
  
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
    treated_label = NULL
) {
  agg <- match.arg(agg)
  stopifnot(treatment_var %in% names(df))
  
  time_var <- agg
  df <- df %>%
    filter_couple_plot_restriction(restriction = restriction)
  dd <- df %>%
    filter_jointly_observed_couple_rows(vars = outcome_vars) %>%
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
      y = "Number of couples",
      color = NULL,
      shape = NULL,
      title = if (include_title) paste("Couple counts over time |", treatment_var) else NULL
    ) +
    theme_couple_facets()
  
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
    treated_label = NULL
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
      !is.na(wave),
      wave != "2019"
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
    theme_couple_facets()
  
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
    treated_label = NULL
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
      !is.na(wave),
      wave != "2019"
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
    theme_couple_facets()
  
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
    treated_label = NULL
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
      spouse = factor(spouse, levels = c("Wife", "Husband"))
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
    theme_couple_facets()
  
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
    treated_label = NULL
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
    theme_couple_facets()
  
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
# Expand couple-level data into sample facets for count plots
#
# Output:
#   original rows repeated across:
#     - All couples
#     - Young kids: 0-10
#     - Older kids: 11-17
# -----------------------------------------------------------------------------
expand_couple_samples_for_counts <- function(df) {
  
  dplyr::bind_rows(
    df %>%
      dplyr::mutate(sample_group = "All couples"),
    
    df %>%
      dplyr::filter(has_child_u10_2019) %>%
      dplyr::mutate(sample_group = "Young kids: 0-10"),
    
    df %>%
      dplyr::filter(has_child_11_17_2019) %>%
      dplyr::mutate(sample_group = "Older kids: 11-17")
  ) %>%
    dplyr::mutate(
      sample_group = factor(
        sample_group,
        levels = c("All couples", "Young kids: 0-10", "Older kids: 11-17")
      )
    )
}