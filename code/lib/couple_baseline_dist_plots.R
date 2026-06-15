# =============================================================================
# File: code/lib/couple_baseline_dist_plots.R
#
# Purpose:
#   Baseline distribution figures for the couple-treatment descriptive deck.
#
#   Two plot types:
#     - plot_baseline_dist_continuous(): density + boxplot for continuous
#       outcomes at the 2019 baseline, faceted by child age group (rows) and
#       spouse (columns).
#     - plot_baseline_share_binary(): bar chart of the outcome share (mean)
#       for binary outcomes, faceted by child age group (rows) and spouse
#       (columns).
#
#   Both functions draw from the full couples-with-children sample (no
#   treatment split). The x-axis is trimmed to the 1st--99th percentile for
#   continuous outcomes to avoid extreme outliers distorting the density.
#
# Dependencies:
#   code/lib/couple_plot_helpers.R  (must be sourced first)
# =============================================================================

suppressPackageStartupMessages({
  library(tidyverse)
})

# Clean negative UKHLS values to NA. All variables plotted here (hours, pay,
# binary 0/1) have no legitimate negative values, so we zero out everything
# below 0 rather than matching specific documented codes — this catches any
# undocumented negative codes present in the data.
.clean_ukhls_numeric <- function(x) {
  x <- suppressWarnings(as.numeric(x))
  x[!is.na(x) & x < 0] <- NA_real_
  x
}

# Variables for which non-employed observations should be coded 0 rather than
# dropped as NA — matches the zero_if_not_working list in couple_treatment_plots.R.
.ZERO_IF_NOT_WORKING_VARS <- c(
  "jbhrs", "jbot",
  "basrate", "basrate_real",
  "paygu_dv", "paygu_dv_real",
  "fimnlabgrs_dv", "fimnlabgrs_dv_real",
  "fimngrs_dv", "fimngrs_dv_real"
)

# -----------------------------------------------------------------------------
# Internal: build the 2x2 facet data for a spouse-long baseline data frame
#
# Returns a data frame with columns:
#   - <var>         the outcome
#   - spouse        "Wife" | "Husband"
#   - child_group_plot  factor with levels "Young kids: 0-10" | "Older kids: 11-17"
# -----------------------------------------------------------------------------
.baseline_dist_data <- function(df_spouse_long, var, zero_if_not_working = FALSE) {

  if (!var %in% names(df_spouse_long)) {
    warning("Variable '", var, "' not found in data; skipping.")
    return(NULL)
  }

  df <- df_spouse_long %>%
    filter_couples_for_child_grid() %>%
    dplyr::mutate(!!var := .clean_ukhls_numeric(.data[[var]]))

  if (zero_if_not_working && "jbstat" %in% names(df)) {
    df <- df %>%
      dplyr::mutate(
        .jbstat_clean = .clean_ukhls_numeric(jbstat),
        !!var := dplyr::if_else(
          !is.na(.jbstat_clean) & !(.jbstat_clean %in% c(1, 2)),
          0,
          .data[[var]]
        )
      ) %>%
      dplyr::select(-.jbstat_clean)
  }

  df %>% dplyr::filter(!is.na(.data[[var]]))
}

# -----------------------------------------------------------------------------
# plot_baseline_dist_continuous
#
# Args:
#   df_spouse_long : spouse-long data frame (output of
#                    reshape_couple_long_to_spouse_long applied to the baseline
#                    couple-level file filtered to year == 2019)
#   var            : outcome variable name (unquoted string)
#   out_file       : filename (no path)
#   fig_path       : output directory
#   width, height  : plot dimensions in inches
#   axis_text_size, axis_title_size, strip_text_size, title_size : ggplot sizes
# -----------------------------------------------------------------------------
plot_baseline_dist_continuous <- function(
    df_spouse_long,
    var,
    out_file,
    fig_path,
    zero_if_not_working = FALSE,
    width  = 14,
    height = 8,
    axis_text_size  = 13,
    axis_title_size = 14,
    strip_text_size = 13,
    title_size      = 13
) {

  df_plot <- .baseline_dist_data(df_spouse_long, var, zero_if_not_working = zero_if_not_working)
  if (is.null(df_plot) || nrow(df_plot) == 0) return(invisible(NULL))

  # Pooled percentiles (both spouses, both child age groups).
  pct_vals <- quantile(df_plot[[var]], probs = c(0.90, 0.95, 0.98, 0.99), na.rm = TRUE)
  pct_df <- data.frame(
    pct        = factor(c("p90", "p95", "p98", "p99"), levels = c("p90", "p95", "p98", "p99")),
    xintercept = as.numeric(pct_vals)
  )

  var_label <- couple_plot_var_label(var)
  x_units   <- couple_plot_var_units(var, is_binary = FALSE)

  p <- ggplot(df_plot, aes(x = .data[[var]])) +
    geom_histogram(
      aes(y = after_stat(density)),
      bins   = 40,
      fill   = "#4e81bd",
      colour = "white",
      alpha  = 0.85
    ) +
    geom_density(colour = "#1f4e79", linewidth = 0.8) +
    geom_vline(
      data     = pct_df,
      aes(xintercept = xintercept, linetype = pct, colour = pct),
      linewidth = 0.75
    ) +
    scale_linetype_manual(
      name   = "Percentile",
      values = c(p90 = "dashed", p95 = "dotdash", p98 = "longdash", p99 = "dotted"),
      labels = c(p90 = "90th", p95 = "95th", p98 = "98th", p99 = "99th")
    ) +
    scale_colour_manual(
      name   = "Percentile",
      values = c(p90 = "#e07b39", p95 = "#c0392b", p98 = "#8e1a0e", p99 = "#7b0000"),
      labels = c(p90 = "90th", p95 = "95th", p98 = "98th", p99 = "99th")
    ) +
    facet_grid(
      rows = vars(child_group_plot),
      cols = vars(spouse)
    ) +
    labs(
      title = NULL,
      x     = x_units,
      y     = "Density"
    ) +
    theme_bw(base_size = axis_text_size) +
    theme(
      axis.text    = element_text(size = axis_text_size),
      axis.title   = element_text(size = axis_title_size),
      strip.text   = element_text(size = strip_text_size),
      plot.title   = element_text(size = title_size),
      panel.grid.minor = element_blank(),
      legend.position  = "bottom",
      legend.text      = element_text(size = axis_text_size),
      legend.title     = element_text(size = axis_text_size)
    )

  out_path <- file.path(fig_path, out_file)
  ggsave(out_path, plot = p, width = width, height = height, dpi = 150)
  message("Saved: ", out_path)
  invisible(p)
}

# -----------------------------------------------------------------------------
# plot_baseline_share_binary
#
# Args:
#   df_spouse_long : spouse-long data frame (see above)
#   var            : binary outcome variable name (unquoted string)
#   out_file, fig_path, width, height : as above
# -----------------------------------------------------------------------------
plot_baseline_share_binary <- function(
    df_spouse_long,
    var,
    out_file,
    fig_path,
    width  = 14,
    height = 8,
    axis_text_size  = 13,
    axis_title_size = 14,
    strip_text_size = 13,
    title_size      = 13
) {

  df_plot <- .baseline_dist_data(df_spouse_long, var)
  if (is.null(df_plot) || nrow(df_plot) == 0) return(invisible(NULL))

  df_summary <- df_plot %>%
    dplyr::group_by(child_group_plot, spouse) %>%
    dplyr::summarise(
      share = mean(.data[[var]], na.rm = TRUE),
      n     = dplyr::n(),
      .groups = "drop"
    )

  var_label <- couple_plot_var_label(var)

  p <- ggplot(df_summary, aes(x = spouse, y = share)) +
    geom_col(fill = "#4e81bd", width = 0.55) +
    geom_text(
      aes(label = scales::percent(share, accuracy = 1)),
      vjust = -0.4,
      size  = axis_text_size / 3
    ) +
    facet_wrap(~ child_group_plot) +
    scale_y_continuous(
      labels = scales::percent_format(accuracy = 1),
      limits = c(0, 1),
      expand = expansion(mult = c(0, 0.08))
    ) +
    labs(
      title = NULL,
      x     = NULL,
      y     = "Share"
    ) +
    theme_bw(base_size = axis_text_size) +
    theme(
      axis.text    = element_text(size = axis_text_size),
      axis.title   = element_text(size = axis_title_size),
      strip.text   = element_text(size = strip_text_size),
      plot.title   = element_text(size = title_size),
      panel.grid.minor  = element_blank(),
      panel.grid.major.x = element_blank()
    )

  out_path <- file.path(fig_path, out_file)
  ggsave(out_path, plot = p, width = width, height = height, dpi = 150)
  message("Saved: ", out_path)
  invisible(p)
}

# -----------------------------------------------------------------------------
# plot_baseline_dist_for_var
#
# Dispatcher: calls the correct function based on whether var is binary.
# -----------------------------------------------------------------------------
plot_baseline_dist_for_var <- function(
    df_spouse_long,
    var,
    out_file,
    fig_path,
    ...
) {
  if (couple_plot_is_binary(var)) {
    plot_baseline_share_binary(
      df_spouse_long = df_spouse_long,
      var            = var,
      out_file       = out_file,
      fig_path       = fig_path,
      ...
    )
  } else {
    plot_baseline_dist_continuous(
      df_spouse_long      = df_spouse_long,
      var                 = var,
      out_file            = out_file,
      fig_path            = fig_path,
      zero_if_not_working = var %in% .ZERO_IF_NOT_WORKING_VARS,
      ...
    )
  }
}

# -----------------------------------------------------------------------------
# plot_baseline_work_status
#
# Single-period (baseline) stacked bar chart of work_last_week_status.
# 2x2 panel: rows = child age group, cols = spouse.
#
# Args:
#   df_spouse_long : spouse-long baseline data frame
#   out_file, fig_path, width, height : as above
# -----------------------------------------------------------------------------
plot_baseline_work_status <- function(
    df_spouse_long,
    out_file,
    fig_path,
    width  = 14,
    height = 8,
    axis_text_size  = 13,
    axis_title_size = 14,
    strip_text_size = 13,
    title_size      = 13
) {

  var <- "work_last_week_status"

  df_plot <- .baseline_dist_data(df_spouse_long, var)
  if (is.null(df_plot) || nrow(df_plot) == 0) return(invisible(NULL))

  status_levels <- c("Worked last week", "Has job, did not work", "Not employed")
  status_colours <- c(
    "Worked last week"      = "#2171b5",
    "Has job, did not work" = "#6baed6",
    "Not employed"          = "#bdd7e7"
  )

  df_summary <- df_plot %>%
    dplyr::mutate(
      status_label = work_last_week_status_labels(.data[[var]]),
      status_label = factor(status_label, levels = status_levels)
    ) %>%
    dplyr::filter(!is.na(status_label)) %>%
    dplyr::count(child_group_plot, spouse, status_label) %>%
    dplyr::group_by(child_group_plot, spouse) %>%
    dplyr::mutate(share = n / sum(n)) %>%
    dplyr::ungroup()

  p <- ggplot(df_summary, aes(x = status_label, y = share, fill = status_label)) +
    geom_col(width = 0.65) +
    geom_text(
      aes(label = scales::percent(share, accuracy = 1)),
      vjust = -0.3,
      size  = axis_text_size / 3.5
    ) +
    facet_grid(
      rows = vars(child_group_plot),
      cols = vars(spouse)
    ) +
    scale_fill_manual(values = status_colours, breaks = status_levels, guide = "none") +
    scale_y_continuous(
      labels = scales::percent_format(accuracy = 1),
      limits = c(0, 1),
      expand = expansion(mult = c(0, 0.1))
    ) +
    labs(
      title = NULL,
      x     = NULL,
      y     = "Share"
    ) +
    theme_bw(base_size = axis_text_size) +
    theme(
      axis.text.x  = element_text(angle = 20, hjust = 1, size = axis_text_size),
      axis.text.y  = element_text(size = axis_text_size),
      axis.title   = element_text(size = axis_title_size),
      strip.text   = element_text(size = strip_text_size),
      plot.title   = element_text(size = title_size),
      panel.grid.minor   = element_blank(),
      panel.grid.major.x = element_blank()
    )

  out_path <- file.path(fig_path, out_file)
  ggsave(out_path, plot = p, width = width, height = height, dpi = 150)
  message("Saved: ", out_path)
  invisible(p)
}

# -----------------------------------------------------------------------------
# plot_covid_work_status_childgrid
#
# Stacked bar chart of work_last_week_status distribution over COVID waves,
# for one spouse at a time.
#
# Layout: facet_grid(child_group_plot ~ treatment_group)
#   rows = child age group (u10 / 11-17)
#   cols = treatment group (treated / control)
#   x    = wave
#   fill = work status category (3 levels, stacked to 100%)
#
# Args:
#   df_couple      : couple-level COVID panel (long by wave)
#                    must have work_last_week_status_h and _w columns
#   spouse         : "wife" or "husband"
#   treatment_var  : name of treatment column (e.g. "treat_wife_key_notedu_husb_not_or_edu")
#   treated_label  : optional override for treated-group legend label
#   out_file, fig_path, width, height : as above
# -----------------------------------------------------------------------------
plot_covid_work_status_childgrid <- function(
    df_couple,
    spouse,
    treatment_var,
    out_file,
    fig_path,
    treated_label  = NULL,
    wave_scale     = "covid",
    width  = 14,
    height = 8,
    axis_text_size  = 13,
    axis_title_size = 14,
    strip_text_size = 13,
    legend_text_size = 13,
    title_size      = 13
) {

  stopifnot(spouse %in% c("wife", "husband"))

  suffix <- if (spouse == "wife") "_w" else "_h"
  status_col <- paste0("work_last_week_status", suffix)

  if (!status_col %in% names(df_couple)) {
    warning("Column '", status_col, "' not found; skipping.")
    return(invisible(NULL))
  }
  if (!treatment_var %in% names(df_couple)) {
    warning("Treatment variable '", treatment_var, "' not found; skipping.")
    return(invisible(NULL))
  }

  status_levels <- c("Worked last week", "Has job, did not work", "Not employed")
  status_colours <- c(
    "Worked last week"      = "#2171b5",
    "Has job, did not work" = "#6baed6",
    "Not employed"          = "#bdd7e7"
  )

  wl <- time_label_lookup(wave_scale)

  df_plot <- df_couple %>%
    filter_couples_for_child_grid() %>%
    add_treatment_group_label(treatment_var = treatment_var, treated_label = treated_label) %>%
    dplyr::filter(!is.na(.data[[status_col]]), !is.na(treatment_group)) %>%
    dplyr::mutate(
      status_label = work_last_week_status_labels(.data[[status_col]]),
      status_label = factor(status_label, levels = status_levels)
    ) %>%
    dplyr::filter(!is.na(status_label)) %>%
    dplyr::count(wave, treatment_group, child_group_plot, status_label) %>%
    dplyr::group_by(wave, treatment_group, child_group_plot) %>%
    dplyr::mutate(share = n / sum(n)) %>%
    dplyr::ungroup()

  if (nrow(df_plot) == 0) return(invisible(NULL))

  p <- ggplot(df_plot, aes(x = factor(wave, levels = wl$wave, labels = wl$wave_label_short),
                           y = share, fill = status_label)) +
    geom_col(position = "stack", width = 0.85) +
    facet_grid(
      rows = vars(child_group_plot),
      cols = vars(treatment_group)
    ) +
    scale_fill_manual(
      values = status_colours,
      breaks = status_levels,
      name   = "Work status"
    ) +
    scale_y_continuous(
      labels = scales::percent_format(accuracy = 1),
      expand = expansion(mult = c(0, 0.02))
    ) +
    labs(
      title = NULL,
      x     = NULL,
      y     = "Share"
    ) +
    theme_minimal() +
    theme_couple_facets(
      axis_text_size   = axis_text_size,
      axis_title_size  = axis_title_size,
      strip_text_size  = strip_text_size,
      legend_text_size = legend_text_size,
      title_size       = title_size
    ) +
    theme(panel.grid.major.x = element_blank())

  out_path <- file.path(fig_path, out_file)
  ggsave(out_path, plot = p, width = width, height = height, dpi = 150)
  message("Saved: ", out_path)
  invisible(p)
}
