# =============================================================================
# File: code/lib/husits_covid_plots.R
#
# Purpose:
#   Plot COVID-study / 2019 baseline distributions of harmonized husits.
#
# Design:
#   - husits is an individual report about who mainly looks after the children.
#   - Couple-level data can contain two reports:
#       husits_w : wife's report
#       husits_h : husband's report
#
#   - This script creates separate figures for wife reports and husband reports.
#   - Figures are bar charts of category shares by wave.
#   - The reporter is encoded in the output filename, not as an extra facet.
#
# Expected input:
#   Couple-level COVID panel, usually:
#     derived/df_sample_long_covid_couplelevel.rds
#
# Expected variables:
#   - couple_id
#   - wave
#   - treatment variables, e.g.
#       treat_wife_key_notedu_husb_not_or_edu
#       treat_wife_key_notedu_any
#       treat_husb_shutdown_wife_not
#   - has_child_u10_2019
#   - has_child_11_17_2019
#   - husits_w
#   - husits_h
#
# Harmonized husits coding:
#   The plotting code assumes the harmonized main-survey style codes:
#     1 = mainly self
#     2 = mainly partner
#     3 = shared
#     4 = someone else / other
#
# Directional interpretation:
#   Wife report:
#     1 -> Mainly wife
#     2 -> Mainly husband
#     3 -> Shared
#     4 -> Someone else / other
#
#   Husband report:
#     1 -> Mainly husband
#     2 -> Mainly wife
#     3 -> Shared
#     4 -> Someone else / other
#
# Notes:
#   - Negative values are treated as missing.
#   - Values outside 1:4 are treated as missing.
#   - Existing calls in 02d_make_couple_treatment_descriptives.R remain valid:
#       plot_covid_husits_distribution(...)
#       plot_covid_husits_distribution_childgrid(...)
#     Each call now writes two files: wife_report and husband_report.
# =============================================================================

suppressPackageStartupMessages({
  library(tidyverse)
})

# =============================================================================
# Availability helpers
# =============================================================================

.has_husits_distribution <- function(df) {
  !is.null(df) &&
    any(c("husits_w", "husits_h") %in% names(df)) &&
    any(
      vapply(
        intersect(c("husits_w", "husits_h"), names(df)),
        function(vv) any(!is.na(df[[vv]])),
        logical(1)
      )
    )
}

.husits_report_specs <- function(df) {
  specs <- tibble::tibble(
    report_var = c("husits_w", "husits_h"),
    reporter = c("wife_report", "husband_report"),
    reporter_label = c("Wife report", "Husband report")
  )
  
  specs %>%
    dplyr::filter(report_var %in% names(df))
}

# =============================================================================
# Filename helper
# =============================================================================

.add_reporter_to_out_file <- function(out_file, reporter) {
  if (grepl("\\.png$", out_file, ignore.case = TRUE)) {
    sub("\\.png$", paste0("_", reporter, ".png"), out_file, ignore.case = TRUE)
  } else {
    paste0(out_file, "_", reporter, ".png")
  }
}

# =============================================================================
# Harmonization for plotting
# =============================================================================

.clean_husits_code <- function(x) {
  if (requireNamespace("haven", quietly = TRUE)) {
    x <- haven::zap_labels(x)
  }
  
  x <- suppressWarnings(as.numeric(x))
  
  dplyr::case_when(
    is.na(x) ~ NA_real_,
    x < 0 ~ NA_real_,
    x %in% 1:4 ~ x,
    TRUE ~ NA_real_
  )
}

.husits_category_from_report <- function(x, reporter = c("wife_report", "husband_report")) {
  reporter <- match.arg(reporter)
  x <- .clean_husits_code(x)
  
  if (reporter == "wife_report") {
    out <- dplyr::case_when(
      x == 1 ~ "Mainly wife",
      x == 2 ~ "Mainly husband",
      x == 3 ~ "Shared",
      x == 4 ~ "Someone else / other",
      TRUE ~ NA_character_
    )
  } else {
    out <- dplyr::case_when(
      x == 1 ~ "Mainly husband",
      x == 2 ~ "Mainly wife",
      x == 3 ~ "Shared",
      x == 4 ~ "Someone else / other",
      TRUE ~ NA_character_
    )
  }
  
  factor(
    out,
    levels = c(
      "Mainly wife",
      "Mainly husband",
      "Shared",
      "Someone else / other"
    )
  )
}

# =============================================================================
# Small compatibility wrappers
# =============================================================================

.husits_filter_restriction <- function(df, restriction = NULL) {
  if (is.null(restriction)) {
    return(df)
  }
  
  if (exists("filter_couple_plot_restriction", mode = "function")) {
    return(
      filter_couple_plot_restriction(
        df = df,
        restriction = restriction
      )
    )
  }
  
  if (restriction == "husb_notkey_or_edu" &&
      "sample_husb_notkey_or_edu" %in% names(df)) {
    return(
      df %>%
        dplyr::filter(sample_husb_notkey_or_edu %in% TRUE)
    )
  }
  
  df
}

.husits_add_treatment_group <- function(df,
                                        treatment_var,
                                        treated_label = NULL,
                                        untreated_label = NULL) {
  if (exists("add_treatment_group_label", mode = "function")) {
    return(
      add_treatment_group_label(
        df = df,
        treatment_var = treatment_var,
        treated_label = treated_label,
        untreated_label = untreated_label
      )
    )
  }
  
  treated_lab <- if (!is.null(treated_label)) treated_label else "Treated"
  untreated_lab <- if (!is.null(untreated_label)) untreated_label else "Control"
  
  df %>%
    dplyr::mutate(
      treatment_group = dplyr::case_when(
        .data[[treatment_var]] == 1 ~ treated_lab,
        .data[[treatment_var]] == 0 ~ untreated_lab,
        TRUE ~ NA_character_
      ),
      treatment_group = factor(
        treatment_group,
        levels = c(treated_lab, untreated_lab)
      )
    )
}

.husits_treatment_group_levels <- function(treatment_var,
                                           treated_label = NULL,
                                           untreated_label = NULL) {
  if (exists("treatment_group_levels", mode = "function")) {
    return(
      treatment_group_levels(
        treatment_var = treatment_var,
        treated_label = treated_label,
        untreated_label = untreated_label
      )
    )
  }
  
  c(
    if (!is.null(treated_label)) treated_label else "Treated",
    if (!is.null(untreated_label)) untreated_label else "Control"
  )
}

.husits_add_treatment_group_scales <- function(p,
                                               treatment_var,
                                               treated_label = NULL,
                                               untreated_label = NULL) {
  if (exists("add_treatment_group_scales", mode = "function")) {
    return(
      add_treatment_group_scales(
        p = p,
        treatment_var = treatment_var,
        treated_label = treated_label,
        untreated_label = untreated_label
      )
    )
  }
  
  group_levels <- .husits_treatment_group_levels(
    treatment_var = treatment_var,
    treated_label = treated_label,
    untreated_label = untreated_label
  )
  
  p +
    ggplot2::scale_color_discrete(
      limits = group_levels,
      breaks = group_levels,
      drop = FALSE
    )
}

.husits_wave_labels <- function(df) {
  if (exists("wave_labels", mode = "function")) {
    wl <- wave_labels()
    
    if (all(c("wave", "wave_lab_short") %in% names(wl))) {
      return(wl)
    }
  }
  
  waves <- unique(as.character(df$wave))
  
  tibble::tibble(
    wave = waves,
    wave_lab_short = waves
  )
}

# =============================================================================
# Data prep: standard treatment-facet distribution
# =============================================================================

.prepare_husits_distribution_data <- function(df,
                                              report_var,
                                              reporter,
                                              treatment_var,
                                              restriction = NULL,
                                              treated_label = NULL) {
  stopifnot(report_var %in% names(df))
  stopifnot(treatment_var %in% names(df))
  
  wl <- .husits_wave_labels(df)
  
  df %>%
    .husits_filter_restriction(restriction = restriction) %>%
    .husits_add_treatment_group(
      treatment_var = treatment_var,
      treated_label = treated_label
    ) %>%
    dplyr::mutate(
      husits_category = .husits_category_from_report(
        .data[[report_var]],
        reporter = reporter
      )
    ) %>%
    dplyr::filter(
      !is.na(wave),
      !is.na(treatment_group),
      !is.na(husits_category)
    ) %>%
    dplyr::count(
      wave,
      treatment_group,
      husits_category,
      name = "n"
    ) %>%
    tidyr::complete(
      wave = wl$wave,
      treatment_group = .husits_treatment_group_levels(
        treatment_var = treatment_var,
        treated_label = treated_label
      ),
      husits_category = levels(.husits_category_from_report(1:4, reporter = reporter)),
      fill = list(n = 0)
    ) %>%
    dplyr::group_by(wave, treatment_group) %>%
    dplyr::mutate(
      total = sum(n),
      share = dplyr::if_else(total > 0, n / total, NA_real_)
    ) %>%
    dplyr::ungroup() %>%
    dplyr::left_join(wl, by = "wave") %>%
    dplyr::mutate(
      wave_plot = factor(
        wave,
        levels = wl$wave,
        labels = wl$wave_lab_short
      ),
      treatment_group = factor(
        treatment_group,
        levels = .husits_treatment_group_levels(
          treatment_var = treatment_var,
          treated_label = treated_label
        )
      ),
      husits_category = factor(
        husits_category,
        levels = c(
          "Mainly wife",
          "Mainly husband",
          "Shared",
          "Someone else / other"
        )
      )
    )
}

# =============================================================================
# Data prep: child-grid distribution
# =============================================================================

.prepare_husits_distribution_childgrid_data <- function(df,
                                                        report_var,
                                                        reporter,
                                                        treatment_var,
                                                        restriction = NULL,
                                                        treated_label = NULL) {
  stopifnot(report_var %in% names(df))
  stopifnot(treatment_var %in% names(df))
  
  wl <- .husits_wave_labels(df)
  
  df %>%
    .husits_filter_restriction(restriction = restriction) %>%
    dplyr::mutate(
      child_group_plot = dplyr::case_when(
        has_child_u10_2019 %in% TRUE ~ "Young kids: 0-10",
        has_child_11_17_2019 %in% TRUE ~ "Older kids: 11-17",
        TRUE ~ NA_character_
      ),
      child_group_plot = factor(
        child_group_plot,
        levels = c("Young kids: 0-10", "Older kids: 11-17")
      )
    ) %>%
    dplyr::filter(!is.na(child_group_plot)) %>%
    .husits_add_treatment_group(
      treatment_var = treatment_var,
      treated_label = treated_label
    ) %>%
    dplyr::mutate(
      husits_category = .husits_category_from_report(
        .data[[report_var]],
        reporter = reporter
      )
    ) %>%
    dplyr::filter(
      !is.na(wave),
      !is.na(treatment_group),
      !is.na(husits_category)
    ) %>%
    dplyr::count(
      wave,
      child_group_plot,
      treatment_group,
      husits_category,
      name = "n"
    ) %>%
    tidyr::complete(
      wave = wl$wave,
      child_group_plot = factor(
        c("Young kids: 0-10", "Older kids: 11-17"),
        levels = c("Young kids: 0-10", "Older kids: 11-17")
      ),
      treatment_group = .husits_treatment_group_levels(
        treatment_var = treatment_var,
        treated_label = treated_label
      ),
      husits_category = levels(.husits_category_from_report(1:4, reporter = reporter)),
      fill = list(n = 0)
    ) %>%
    dplyr::group_by(wave, child_group_plot, treatment_group) %>%
    dplyr::mutate(
      total = sum(n),
      share = dplyr::if_else(total > 0, n / total, NA_real_)
    ) %>%
    dplyr::ungroup() %>%
    dplyr::left_join(wl, by = "wave") %>%
    dplyr::mutate(
      wave_plot = factor(
        wave,
        levels = wl$wave,
        labels = wl$wave_lab_short
      ),
      treatment_group = factor(
        treatment_group,
        levels = .husits_treatment_group_levels(
          treatment_var = treatment_var,
          treated_label = treated_label
        )
      ),
      husits_category = factor(
        husits_category,
        levels = c(
          "Mainly wife",
          "Mainly husband",
          "Shared",
          "Someone else / other"
        )
      )
    )
}

# =============================================================================
# Theme helper
# =============================================================================

.husits_theme <- function(axis_text_size = 12,
                          axis_title_size = 13,
                          strip_text_size = 12,
                          legend_text_size = 11,
                          legend_title_size = 11,
                          title_size = 14) {
  ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(
      legend.position = "bottom",
      axis.text.x = ggplot2::element_text(
        angle = 90,
        hjust = 1,
        size = axis_text_size
      ),
      axis.text.y = ggplot2::element_text(size = axis_text_size),
      axis.title.x = ggplot2::element_text(size = axis_title_size),
      axis.title.y = ggplot2::element_text(size = axis_title_size),
      strip.text = ggplot2::element_text(size = strip_text_size),
      legend.text = ggplot2::element_text(size = legend_text_size),
      legend.title = ggplot2::element_text(size = legend_title_size),
      plot.title = ggplot2::element_text(size = title_size),
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
      )
    )
}

# =============================================================================
# Single-reporter plot builders
# =============================================================================

.plot_husits_distribution_one_reporter <- function(df,
                                                   report_var,
                                                   reporter,
                                                   reporter_label,
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
                                                   title_size = 14) {
  dd <- .prepare_husits_distribution_data(
    df = df,
    report_var = report_var,
    reporter = reporter,
    treatment_var = treatment_var,
    restriction = restriction,
    treated_label = treated_label
  )
  
  if (nrow(dd) == 0 || all(is.na(dd$share))) {
    return(NULL)
  }
  
  p <- ggplot2::ggplot(
    dd,
    ggplot2::aes(
      x = wave_plot,
      y = share,
      fill = husits_category
    )
  ) +
    ggplot2::geom_col(width = 0.85, na.rm = TRUE) +
    ggplot2::facet_grid(treatment_group ~ ., scales = "fixed") +
    ggplot2::scale_y_continuous(labels = scales::percent_format()) +
    ggplot2::labs(
      x = NULL,
      y = "Share of valid reports",
      fill = NULL,
      title = if (include_title) {
        paste0("Childcare responsibility distribution: ", reporter_label)
      } else {
        NULL
      }
    ) +
    .husits_theme(
      axis_text_size = axis_text_size,
      axis_title_size = axis_title_size,
      strip_text_size = strip_text_size,
      legend_text_size = legend_text_size,
      legend_title_size = legend_title_size,
      title_size = title_size
    )
  
  ggplot2::ggsave(
    filename = out_file,
    plot = p,
    path = fig_path,
    width = 12,
    height = 8
  )
  
  p
}

.plot_husits_distribution_childgrid_one_reporter <- function(df,
                                                             report_var,
                                                             reporter,
                                                             reporter_label,
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
                                                             title_size = 14) {
  dd <- .prepare_husits_distribution_childgrid_data(
    df = df,
    report_var = report_var,
    reporter = reporter,
    treatment_var = treatment_var,
    restriction = restriction,
    treated_label = treated_label
  )
  
  if (nrow(dd) == 0 || all(is.na(dd$share))) {
    return(NULL)
  }
  
  p <- ggplot2::ggplot(
    dd,
    ggplot2::aes(
      x = wave_plot,
      y = share,
      fill = husits_category
    )
  ) +
    ggplot2::geom_col(width = 0.85, na.rm = TRUE) +
    ggplot2::facet_grid(child_group_plot ~ treatment_group, scales = "fixed") +
    ggplot2::scale_y_continuous(labels = scales::percent_format()) +
    ggplot2::labs(
      x = NULL,
      y = "Share of valid reports",
      fill = NULL,
      title = if (include_title) {
        paste0("Childcare responsibility distribution: ", reporter_label)
      } else {
        NULL
      }
    ) +
    .husits_theme(
      axis_text_size = axis_text_size,
      axis_title_size = axis_title_size,
      strip_text_size = strip_text_size,
      legend_text_size = legend_text_size,
      legend_title_size = legend_title_size,
      title_size = title_size
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

# =============================================================================
# Public functions called by 02d
# =============================================================================

plot_covid_husits_distribution <- function(
    df,
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
  stopifnot(treatment_var %in% names(df))
  
  specs <- .husits_report_specs(df)
  
  if (nrow(specs) == 0) {
    warning("No husits_w or husits_h variables found; skipping husits distribution plot.")
    return(invisible(list()))
  }
  
  plots <- purrr::pmap(
    list(specs$report_var, specs$reporter, specs$reporter_label),
    function(report_var, reporter, reporter_label) {
      .plot_husits_distribution_one_reporter(
        df = df,
        report_var = report_var,
        reporter = reporter,
        reporter_label = reporter_label,
        treatment_var = treatment_var,
        out_file = .add_reporter_to_out_file(out_file, reporter),
        fig_path = fig_path,
        include_title = include_title,
        restriction = restriction,
        treated_label = treated_label,
        axis_text_size = axis_text_size,
        axis_title_size = axis_title_size,
        strip_text_size = strip_text_size,
        legend_text_size = legend_text_size,
        legend_title_size = legend_title_size,
        title_size = title_size
      )
    }
  )
  
  names(plots) <- specs$reporter
  invisible(plots)
}

plot_covid_husits_distribution_childgrid <- function(
    df,
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
  stopifnot(treatment_var %in% names(df))
  
  specs <- .husits_report_specs(df)
  
  if (nrow(specs) == 0) {
    warning("No husits_w or husits_h variables found; skipping husits child-grid plot.")
    return(invisible(list()))
  }
  
  plots <- purrr::pmap(
    list(specs$report_var, specs$reporter, specs$reporter_label),
    function(report_var, reporter, reporter_label) {
      .plot_husits_distribution_childgrid_one_reporter(
        df = df,
        report_var = report_var,
        reporter = reporter,
        reporter_label = reporter_label,
        treatment_var = treatment_var,
        out_file = .add_reporter_to_out_file(out_file, reporter),
        fig_path = fig_path,
        include_title = include_title,
        restriction = restriction,
        treated_label = treated_label,
        axis_text_size = axis_text_size,
        axis_title_size = axis_title_size,
        strip_text_size = strip_text_size,
        legend_text_size = legend_text_size,
        legend_title_size = legend_title_size,
        title_size = title_size
      )
    }
  )
  
  names(plots) <- specs$reporter
  invisible(plots)
}