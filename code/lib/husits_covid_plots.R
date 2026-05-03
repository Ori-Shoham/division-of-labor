# =============================================================================
# File: code/lib/husits_covid_plots.R
#
# Purpose:
#   Couple-level distribution plots for harmonized husits in the COVID section.
#
# These are not spouse-level outcome plots. They show the distribution of
# childcare responsibility categories over time.
#
# Categories:
#   - Wife mainly responsible
#   - Husband mainly responsible
#   - Shared
#   - Someone else / other
#
# Standard version:
#   - facets by treatment group
#
# Child-grid version:
#   - rows = child age group
#   - columns = treatment group
# =============================================================================

suppressPackageStartupMessages({
  library(tidyverse)
  library(scales)
})

.has_husits_distribution <- function(df) {
  !is.null(df) &&
    all(c(
      "husits_wife_main_avg",
      "husits_husband_main_avg",
      "husits_shared_avg",
      "husits_other_avg"
    ) %in% names(df)) &&
    any(!is.na(df$husits_wife_main_avg) |
          !is.na(df$husits_husband_main_avg) |
          !is.na(df$husits_shared_avg) |
          !is.na(df$husits_other_avg))
}

husits_distribution_vars <- function() {
  tibble::tibble(
    husits_category = c(
      "Wife mainly responsible",
      "Husband mainly responsible",
      "Shared",
      "Someone else / other"
    ),
    var = c(
      "husits_wife_main_avg",
      "husits_husband_main_avg",
      "husits_shared_avg",
      "husits_other_avg"
    )
  )
}

prepare_husits_covid_distribution <- function(df,
                                              treatment_var,
                                              restriction = NULL,
                                              treated_label = NULL,
                                              child_grid = FALSE) {
  stopifnot(treatment_var %in% names(df))
  
  hv <- husits_distribution_vars()
  existing_vars <- intersect(hv$var, names(df))
  
  if (length(existing_vars) == 0) {
    stop("No husits distribution variables found.")
  }
  
  dd <- df %>%
    filter_couple_plot_restriction(restriction = restriction)
  
  if (child_grid) {
    dd <- dd %>%
      filter_couples_for_child_grid()
  }
  
  dd %>%
    add_treatment_group_label(
      treatment_var = treatment_var,
      treated_label = treated_label
    ) %>%
    dplyr::select(
      dplyr::any_of(c(
        "couple_id",
        "wave",
        "child_group_plot"
      )),
      treatment_group,
      dplyr::all_of(existing_vars)
    ) %>%
    tidyr::pivot_longer(
      cols = dplyr::all_of(existing_vars),
      names_to = "var",
      values_to = "value"
    ) %>%
    dplyr::left_join(hv, by = "var") %>%
    dplyr::filter(
      !is.na(wave),
      !is.na(treatment_group),
      !is.na(husits_category),
      !is.na(value)
    ) %>%
    dplyr::mutate(
      husits_category = factor(
        husits_category,
        levels = hv$husits_category
      )
    )
}

husits_covid_wave_levels <- function(x) {
  all_levels <- c("2019", "baseline", "ca", "cb", "cc", "cd", "ce", "cf", "cg", "ch", "ci")
  intersect(all_levels, unique(as.character(x)))
}

husits_covid_wave_labels <- function(levels) {
  dplyr::case_when(
    levels == "2019" ~ "2019",
    levels == "baseline" ~ "Jan-Feb 2020",
    levels == "ca" ~ "Apr 2020",
    levels == "cb" ~ "May 2020",
    levels == "cc" ~ "Jun 2020",
    levels == "cd" ~ "Jul 2020",
    levels == "ce" ~ "Sep 2020",
    levels == "cf" ~ "Nov 2020",
    levels == "cg" ~ "Jan 2021",
    levels == "ch" ~ "Mar 2021",
    levels == "ci" ~ "Sep 2021",
    TRUE ~ levels
  )
}

plot_covid_husits_distribution <- function(
    df,
    treatment_var,
    out_file,
    fig_path,
    restriction = NULL,
    treated_label = NULL,
    include_title = FALSE,
    axis_text_size = 12,
    axis_title_size = 13,
    strip_text_size = 12,
    legend_text_size = 11,
    legend_title_size = 11,
    title_size = 14
) {
  
  dd <- prepare_husits_covid_distribution(
    df = df,
    treatment_var = treatment_var,
    restriction = restriction,
    treated_label = treated_label,
    child_grid = FALSE
  ) %>%
    dplyr::group_by(wave, treatment_group, husits_category) %>%
    dplyr::summarise(
      share = mean(value, na.rm = TRUE),
      n = dplyr::n(),
      .groups = "drop"
    )
  
  wlev <- husits_covid_wave_levels(dd$wave)
  wlab <- husits_covid_wave_labels(wlev)
  
  p <- ggplot(
    dd,
    aes(
      x = factor(wave, levels = wlev, labels = wlab),
      y = share,
      color = husits_category,
      shape = husits_category,
      group = husits_category
    )
  ) +
    geom_line(na.rm = TRUE) +
    geom_point(size = 2.0, na.rm = TRUE) +
    facet_wrap(~ treatment_group) +
    scale_y_continuous(labels = scales::percent_format(), limits = c(0, 1)) +
    labs(
      x = NULL,
      y = "Share of couples",
      color = NULL,
      shape = NULL,
      title = if (include_title) "Childcare responsibility" else NULL
    ) +
    theme_minimal() +
    theme_couple_facets(
      axis_text_size = axis_text_size,
      axis_title_size = axis_title_size,
      strip_text_size = strip_text_size,
      legend_text_size = legend_text_size,
      legend_title_size = legend_title_size,
      title_size = title_size
    ) +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1)
    )
  
  ggplot2::ggsave(
    filename = out_file,
    plot = p,
    path = fig_path,
    width = 12,
    height = 7
  )
  
  p
}

plot_covid_husits_distribution_childgrid <- function(
    df,
    treatment_var,
    out_file,
    fig_path,
    restriction = NULL,
    treated_label = NULL,
    include_title = FALSE,
    axis_text_size = 12,
    axis_title_size = 13,
    strip_text_size = 12,
    legend_text_size = 11,
    legend_title_size = 11,
    title_size = 14
) {
  
  dd <- prepare_husits_covid_distribution(
    df = df,
    treatment_var = treatment_var,
    restriction = restriction,
    treated_label = treated_label,
    child_grid = TRUE
  ) %>%
    dplyr::group_by(wave, child_group_plot, treatment_group, husits_category) %>%
    dplyr::summarise(
      share = mean(value, na.rm = TRUE),
      n = dplyr::n(),
      .groups = "drop"
    )
  
  wlev <- husits_covid_wave_levels(dd$wave)
  wlab <- husits_covid_wave_labels(wlev)
  
  p <- ggplot(
    dd,
    aes(
      x = factor(wave, levels = wlev, labels = wlab),
      y = share,
      color = husits_category,
      shape = husits_category,
      group = husits_category
    )
  ) +
    geom_line(na.rm = TRUE) +
    geom_point(size = 2.0, na.rm = TRUE) +
    facet_grid(child_group_plot ~ treatment_group) +
    scale_y_continuous(labels = scales::percent_format(), limits = c(0, 1)) +
    labs(
      x = NULL,
      y = "Share of couples",
      color = NULL,
      shape = NULL,
      title = if (include_title) "Childcare responsibility" else NULL
    ) +
    theme_minimal() +
    theme_couple_facets(
      axis_text_size = axis_text_size,
      axis_title_size = axis_title_size,
      strip_text_size = strip_text_size,
      legend_text_size = legend_text_size,
      legend_title_size = legend_title_size,
      title_size = title_size
    ) +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1)
    )
  
  ggplot2::ggsave(
    filename = out_file,
    plot = p,
    path = fig_path,
    width = 13,
    height = 9
  )
  
  p
}