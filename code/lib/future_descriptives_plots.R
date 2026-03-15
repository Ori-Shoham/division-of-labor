# =============================================================================
# File: code/lib/future_descriptives_plots.R
#
# Purpose:
#   Descriptive plots for future outcomes.
#   - Numeric outcomes: mean over time
#   - Binary 0/1 outcomes: share over time
#   - Categorical outcomes: distribution over time
#   - Supports grouping and faceting
# =============================================================================

suppressPackageStartupMessages({
  library(tidyverse)
  library(scales)
})

# --- labels and metadata ------------------------------------------------------

.var_label <- function(var) {
  dplyr::case_when(
    var == "workoutside" ~ "Working outside the home",
    var == "jbhrs" ~ "Weekly hours worked",
    var == "jbot" ~ "Overtime hours worked",
    var == "paygu_dv" ~ "Gross monthly pay",
    var == "fimnlabgrs_dv" ~ "Gross monthly labour income",
    var == "fimngrs_dv" ~ "Gross monthly personal income",
    var == "health_sf" ~ "Self-reported general health",
    var == "scghq1_dv" ~ "GHQ mental distress score",
    var == "scghq2_dv" ~ "GHQ caseness",
    var == "sclfsato" ~ "Life satisfaction",
    var == "nchild_dv" ~ "Number of children under 16 in the household",
    var == "ndepchl_dv" ~ "Number of dependent children in the household",
    var == "wfh_cat" ~ "Working from home status",
    var == "jbstat" ~ "Employment status",
    var == "mastat_dv" ~ "Marital status",
    var == "jbft_dv" ~ "Full-time / part-time status",
    var == "sex" ~ "Sex",
    var == "group_industry_based" ~ "Industry-based group",
    var == "group_industry_based_detailed" ~ "Detailed industry-based group",
    var == "industry" ~ "Industry",
    var == "occupation" ~ "Occupation",
    TRUE ~ stringr::str_to_sentence(gsub("_dv$|_cc$", "", gsub("_", " ", var)))
  )
}

.var_units <- function(var, is_binary = FALSE) {
  if (is_binary) return("Share of respondents")
  dplyr::case_when(
    var %in% c("jbhrs", "jbot") ~ "Hours per week",
    var %in% c("paygu_dv", "fimnlabgrs_dv", "fimngrs_dv") ~ "Pounds per month",
    var %in% c("nchild_dv", "ndepchl_dv") ~ "Count",
    var == "scghq1_dv" ~ "GHQ mental distress score\n (0-36 scale from least to greatest stress)",
    var == "scghq2_dv" ~ "GHQ mental distress caseness score\n (0-12 scale from least to greatest stress)",
    var == "sclfsato" ~ "Life satisfaction (1-7)",
    TRUE ~ .var_label(var)
  )
}

.numeric_title <- function(var, by = NULL, is_binary = FALSE) {
  metric <- if (is_binary) "Share" else "Mean"
  out <- paste(metric, .var_label(var))
  if (!is.null(by)) out <- paste0(out, ", by ", .var_label(by))
  out
}

.categorical_title <- function(var, by = NULL) {
  if (is.null(by)) {
    paste("Distribution of", .var_label(var))
  } else {
    paste(.var_label(var), ", by", .var_label(by))
  }
}

# Human-readable category labels
.label_values <- function(var, x) {
  
  x_chr <- as.character(x)
  
  label_maps <- list(
    sex = c(
      "1" = "Male",
      "2" = "Female"
    ),
    
    jbft_dv = c(
      "1" = "Full-time",
      "2" = "Part-time"
    ),
    
    jbstat = c(
      "1"  = "Self-employed",
      "2"  = "Employed",
      "3"  = "Unemployed",
      "4"  = "Retired",
      "5"  = "Maternity leave",
      "6"  = "Home / family care",
      "7"  = "Student",
      "8"  = "Sick / disabled",
      "9"  = "Training scheme",
      "10" = "Unpaid family business",
      "11" = "Apprenticeship",
      "12" = "Furlough",
      "13" = "Laid off / short-time",
      "14" = "Shared parental leave",
      "15" = "Adoption leave",
      "97" = "Other"
    ),
    
    mastat_dv = c(
      "0"  = "Child under 16",
      "1"  = "Single",
      "2"  = "Married",
      "3"  = "Civil partner",
      "4"  = "Separated",
      "5"  = "Divorced",
      "6"  = "Widowed",
      "7"  = "Separated civil partner",
      "8"  = "Former civil partner",
      "9"  = "Surviving civil partner",
      "10" = "Cohabiting"
    ),
    
    workoutside = c(
      "0" = "No",
      "1" = "Yes"
    )
  )
  
  level_maps <- list(
    sex = c("Male", "Female"),
    
    jbft_dv = c("Full-time", "Part-time"),
    
    jbstat = c(
      "Employed",
      "Self-employed",
      "Furlough",
      "Apprenticeship",
      "Training scheme",
      "Unpaid family business",
      "Unemployed",
      "Student",
      "Home / family care",
      "Maternity leave",
      "Sick / disabled",
      "Retired",
      "Laid off / short-time",
      "Shared parental leave",
      "Adoption leave",
      "Other"
    ),
    
    mastat_dv = c(
      "Child under 16",
      "Single",
      "Cohabiting",
      "Married",
      "Civil partner",
      "Separated",
      "Divorced",
      "Widowed",
      "Separated civil partner",
      "Former civil partner",
      "Surviving civil partner"
    ),
    
    workoutside = c("Yes", "No")
  )
  
  if (!var %in% names(label_maps)) {
    return(x_chr)
  }
  
  lab_map <- label_maps[[var]]
  out <- unname(lab_map[x_chr])
  
  # Keep original values if no mapping exists
  out[is.na(out)] <- x_chr[is.na(out)]
  
  if (var %in% names(level_maps)) {
    return(factor(out, levels = level_maps[[var]]))
  }
  
  out
}

# Fixed wave labels requested by user
.wave_label_map <- c(
  "2019" = "2019 baseline",
  "l" = "Wave 12",
  "m" = "Wave 13",
  "n" = "Wave 14",
  "o" = "Wave 15",
  "12" = "Wave 12",
  "13" = "Wave 13",
  "14" = "Wave 14",
  "15" = "Wave 15"
)

.apply_time_labels <- function(p, dd, agg = "wave") {
  if (agg == "wave") {
    p + scale_x_discrete(labels = function(x) {
      out <- .wave_label_map[x]
      out[is.na(out)] <- x[is.na(out)]
      unname(out)
    })
  } else if (agg == "ym") {
    p + scale_x_date(date_labels = "%b %Y", date_breaks = "3 months")
  } else {
    p + scale_x_continuous(breaks = sort(unique(dd$time)))
  }
}

# --- helpers ------------------------------------------------------------------

.safe_name <- function(x) {
  x %>%
    stringr::str_replace_all("[^A-Za-z0-9_\\-]+", "_") %>%
    stringr::str_replace_all("_+", "_") %>%
    stringr::str_replace_all("^_|_$", "")
}

.wave_order <- function(df) {
  w <- unique(df$wave)
  w <- w[!is.na(w)]
  if (length(w) == 0) return(character(0))
  if (is.factor(df$wave)) levels(df$wave) else sort(w)
}

.is_binary_01 <- function(x) {
  ux <- sort(unique(x[!is.na(x)]))
  length(ux) <= 2 && all(ux %in% c(0, 1))
}

.guess_baseline_var <- function(df, var) {
  cand <- paste0("base_", var)
  if (cand %in% names(df)) cand else NULL
}

.clean_future_numeric_for_plot <- function(x, emp = NULL, zero_if_not_working = FALSE) {
  x <- as.numeric(x)
  x[x %in% c(-9, -8, -7, -2, -1)] <- NA_real_
  
  if (zero_if_not_working) {
    if (is.null(emp)) stop("zero_if_not_working=TRUE requires emp to be supplied.")
    emp_clean <- as.numeric(emp)
    emp_clean[emp_clean %in% c(-9, -8, -7, -2, -1)] <- NA_real_
    x <- dplyr::if_else(
      !is.na(emp_clean) & !(emp_clean %in% c(1, 2)),
      0,
      x
    )
  }
  
  x
}

.add_baseline_2019_rows <- function(df, var, baseline_var, by = NULL) {
  stopifnot(var %in% names(df))
  stopifnot(!is.null(baseline_var), baseline_var %in% names(df))
  
  keep_cols <- c("pidp", baseline_var)
  if (!is.null(by)) keep_cols <- c(keep_cols, by)
  
  df %>%
    dplyr::select(dplyr::all_of(keep_cols)) %>%
    dplyr::distinct(pidp, .keep_all = TRUE) %>%
    dplyr::transmute(
      pidp = pidp,
      wave = "2019",
      ym   = as.Date("2019-01-01"),
      year = 2019L,
      value = .data[[baseline_var]],
      group = if (is.null(by)) "All" else .label_values(by, .data[[by]])
    )
}

.prepare_future_numeric_mean_data <- function(
    df,
    var,
    by = NULL,
    agg = c("wave", "ym", "year"),
    include_baseline_2019 = TRUE,
    baseline_var = NULL,
    zero_if_not_working = FALSE,
    employment_var = "jbstat"
) {
  agg <- match.arg(agg)
  
  stopifnot(var %in% names(df))
  if (!is.null(by)) stopifnot(by %in% names(df))
  if (zero_if_not_working) stopifnot(employment_var %in% names(df))
  
  if (is.null(baseline_var)) baseline_var <- .guess_baseline_var(df, var)
  
  dd_future <- df %>%
    dplyr::mutate(
      value = .clean_future_numeric_for_plot(
        x = .data[[var]],
        emp = if (zero_if_not_working) .data[[employment_var]] else NULL,
        zero_if_not_working = zero_if_not_working
      ),
      group = if (is.null(by)) "All" else .label_values(by, .data[[by]])
    ) %>%
    dplyr::filter(!is.na(group))
  
  if (include_baseline_2019) {
    if (is.null(baseline_var)) {
      stop(
        "include_baseline_2019=TRUE but no baseline variable was found for '",
        var,
        "'. Pass baseline_var explicitly or set include_baseline_2019=FALSE."
      )
    }
    
    dd_base <- .add_baseline_2019_rows(
      df = df,
      var = var,
      baseline_var = baseline_var,
      by = by
    ) %>%
      dplyr::mutate(
        value = .clean_future_numeric_for_plot(
          x = value,
          emp = NULL,
          zero_if_not_working = FALSE
        )
      )
    
    dd_future <- dd_future %>%
      dplyr::select(pidp, wave, ym, year, value, group) %>%
      dplyr::bind_rows(dd_base)
  } else {
    dd_future <- dd_future %>%
      dplyr::select(pidp, wave, ym, year, value, group)
  }
  
  is_binary <- .is_binary_01(dd_future$value)
  
  if (agg == "wave") {
    future_waves <- .wave_order(dd_future)
    wave_levels <- c("2019", setdiff(future_waves, "2019"))
    
    dd_sum <- dd_future %>%
      dplyr::filter(!is.na(wave), !is.na(value)) %>%
      dplyr::mutate(
        time = factor(wave, levels = wave_levels),
        time_lab = as.character(time)
      ) %>%
      dplyr::group_by(time, time_lab, group) %>%
      dplyr::summarise(
        mean_y = mean(value, na.rm = TRUE),
        n = dplyr::n(),
        .groups = "drop"
      )
    
  } else if (agg == "ym") {
    
    dd_sum <- dd_future %>%
      dplyr::filter(!is.na(ym), !is.na(value)) %>%
      dplyr::mutate(
        time = ym,
        time_lab = format(ym, "%Y-%m")
      ) %>%
      dplyr::group_by(time, time_lab, group) %>%
      dplyr::summarise(
        mean_y = mean(value, na.rm = TRUE),
        n = dplyr::n(),
        .groups = "drop"
      )
    
  } else {
    
    dd_sum <- dd_future %>%
      dplyr::filter(!is.na(year), !is.na(value)) %>%
      dplyr::mutate(
        time = year,
        time_lab = as.character(year)
      ) %>%
      dplyr::group_by(time, time_lab, group) %>%
      dplyr::summarise(
        mean_y = mean(value, na.rm = TRUE),
        n = dplyr::n(),
        .groups = "drop"
      )
  }
  
  list(data = dd_sum, is_binary = is_binary)
}

plot_future_numeric_mean <- function(
    df,
    var,
    by = NULL,
    agg = c("wave", "ym", "year"),
    include_baseline_2019 = TRUE,
    baseline_var = NULL,
    zero_if_not_working = FALSE,
    employment_var = "jbstat",
    out_file,
    fig_path
) {
  agg <- match.arg(agg)
  stopifnot(var %in% names(df))
  if (!is.null(by)) stopifnot(by %in% names(df))
  
  prep <- .prepare_future_numeric_mean_data(
    df = df,
    var = var,
    by = by,
    agg = agg,
    include_baseline_2019 = include_baseline_2019,
    baseline_var = baseline_var,
    zero_if_not_working = zero_if_not_working,
    employment_var = employment_var
  )
  
  dd <- prep$data
  is_binary <- prep$is_binary
  
  p <- ggplot(
    dd,
    aes(
      x = time,
      y = mean_y,
      color = group,
      shape = group,
      group = group
    )
  ) +
    geom_line(na.rm = TRUE) +
    geom_point(size = 2.4, na.rm = TRUE) +
    theme_minimal() +
    labs(
      x = NULL,
      y = .var_units(var, is_binary = is_binary),
      color = if (is.null(by)) NULL else .var_label(by),
      shape = if (is.null(by)) NULL else .var_label(by),
      title = .numeric_title(var, by = by, is_binary = is_binary)
    ) +
    theme(
      legend.position = if (is.null(by)) "none" else "bottom",
      axis.text.x = element_text(angle = 90, hjust = 1)
    )
  
  p <- .apply_time_labels(p, dd, agg = agg)
  
  if (is_binary) {
    p <- p + scale_y_continuous(labels = scales::percent_format())
  }
  
  ggsave(
    filename = out_file,
    plot = p,
    path = fig_path,
    width = 12,
    height = 7
  )
  
  p
}

# --- categorical helpers ------------------------------------------------------

.clean_future_categorical_for_plot <- function(x, include_missing = FALSE) {
  if (is.numeric(x) || is.integer(x)) {
    x <- as.numeric(x)
    x[x %in% c(-9, -8, -7, -2, -1)] <- NA_real_
  }
  
  x <- as.character(x)
  x[trimws(x) == ""] <- NA_character_
  
  if (include_missing) x[is.na(x)] <- "Missing"
  x
}

.add_baseline_2019_rows_cat <- function(df, baseline_var, by = NULL) {
  stopifnot(!is.null(baseline_var), baseline_var %in% names(df))
  
  keep_cols <- c("pidp", baseline_var)
  if (!is.null(by)) keep_cols <- c(keep_cols, by)
  
  df %>%
    dplyr::select(dplyr::all_of(keep_cols)) %>%
    dplyr::distinct(pidp, .keep_all = TRUE) %>%
    dplyr::transmute(
      pidp = pidp,
      wave = "2019",
      ym   = as.Date("2019-01-01"),
      year = 2019L,
      value = .data[[baseline_var]],
      facet_group = if (is.null(by)) "All" else .label_values(by, .data[[by]])
    )
}

.prepare_future_categorical_dist_data <- function(
    df,
    var,
    agg = c("wave", "ym", "year"),
    include_baseline_2019 = TRUE,
    baseline_var = NULL,
    include_missing = FALSE
) {
  agg <- match.arg(agg)
  stopifnot(var %in% names(df))
  
  if (is.null(baseline_var)) {
    baseline_var <- paste0("base_", var)
    if (!(baseline_var %in% names(df))) baseline_var <- NULL
  }
  
  dd_future <- df %>%
    dplyr::transmute(
      pidp,
      wave,
      ym,
      year,
      value = .label_values(
        var,
        .clean_future_categorical_for_plot(.data[[var]], include_missing = include_missing)
      )
    )
  
  if (include_baseline_2019) {
    if (is.null(baseline_var)) {
      stop(
        "include_baseline_2019=TRUE but no baseline variable was found for '",
        var,
        "'. Pass baseline_var explicitly or set include_baseline_2019=FALSE."
      )
    }
    
    dd_base <- df %>%
      dplyr::select(pidp, dplyr::all_of(baseline_var)) %>%
      dplyr::distinct(pidp, .keep_all = TRUE) %>%
      dplyr::transmute(
        pidp = pidp,
        wave = "2019",
        ym   = as.Date("2019-01-01"),
        year = 2019L,
        value = .label_values(
          var,
          .clean_future_categorical_for_plot(.data[[baseline_var]], include_missing = include_missing)
        )
      )
    
    dd_all <- dplyr::bind_rows(dd_future, dd_base)
  } else {
    dd_all <- dd_future
  }
  
  dd_all <- dd_all %>% dplyr::filter(!is.na(value))
  
  if (agg == "wave") {
    future_waves <- unique(dd_all$wave)
    future_waves <- future_waves[!is.na(future_waves)]
    future_waves <- if (is.factor(dd_all$wave)) levels(dd_all$wave) else sort(future_waves)
    wave_levels <- c("2019", setdiff(future_waves, "2019"))
    
    dd_all <- dd_all %>%
      dplyr::filter(!is.na(wave)) %>%
      dplyr::mutate(
        time = factor(wave, levels = wave_levels),
        time_lab = as.character(time)
      )
    
  } else if (agg == "ym") {
    
    dd_all <- dd_all %>%
      dplyr::filter(!is.na(ym)) %>%
      dplyr::mutate(
        time = ym,
        time_lab = format(ym, "%Y-%m")
      )
    
  } else {
    
    dd_all <- dd_all %>%
      dplyr::filter(!is.na(year)) %>%
      dplyr::mutate(
        time = year,
        time_lab = as.character(year)
      )
  }
  
  dd_all %>%
    dplyr::group_by(time, time_lab, value) %>%
    dplyr::summarise(n = dplyr::n(), .groups = "drop_last") %>%
    dplyr::mutate(share = n / sum(n)) %>%
    dplyr::ungroup()
}

plot_future_categorical_dist <- function(
    df,
    var,
    agg = c("wave", "ym", "year"),
    include_baseline_2019 = TRUE,
    baseline_var = NULL,
    include_missing = FALSE,
    out_file,
    fig_path
) {
  agg <- match.arg(agg)
  stopifnot(var %in% names(df))
  
  dd <- .prepare_future_categorical_dist_data(
    df = df,
    var = var,
    agg = agg,
    include_baseline_2019 = include_baseline_2019,
    baseline_var = baseline_var,
    include_missing = include_missing
  )
  
  p <- ggplot(dd, aes(x = time, y = share, fill = value)) +
    geom_col(position = "fill") +
    scale_y_continuous(labels = scales::percent_format()) +
    theme_minimal() +
    labs(
      x = NULL,
      y = "Share of respondents",
      fill = .var_label(var),
      title = .categorical_title(var)
    ) +
    theme(
      legend.position = "bottom",
      axis.text.x = element_text(angle = 90, hjust = 1)
    )
  
  p <- .apply_time_labels(p, dd, agg = agg)
  
  ggsave(
    filename = out_file,
    plot = p,
    path = fig_path,
    width = 12,
    height = 7
  )
  
  p
}

.prepare_future_categorical_dist_facet_data <- function(
    df,
    var,
    by,
    agg = c("wave", "ym", "year"),
    include_baseline_2019 = TRUE,
    baseline_var = NULL,
    include_missing = FALSE
) {
  agg <- match.arg(agg)
  
  stopifnot(var %in% names(df))
  stopifnot(by %in% names(df))
  
  if (is.null(baseline_var)) {
    baseline_var <- paste0("base_", var)
    if (!(baseline_var %in% names(df))) baseline_var <- NULL
  }
  
  dd_future <- df %>%
    dplyr::transmute(
      pidp,
      wave,
      ym,
      year,
      value = .label_values(
        var,
        .clean_future_categorical_for_plot(.data[[var]], include_missing = include_missing)
      ),
      facet_group = .label_values(by, .data[[by]])
    ) %>%
    dplyr::filter(!is.na(facet_group))
  
  if (include_baseline_2019) {
    if (is.null(baseline_var)) {
      stop(
        "include_baseline_2019=TRUE but no baseline variable was found for '",
        var,
        "'. Pass baseline_var explicitly or set include_baseline_2019=FALSE."
      )
    }
    
    dd_base <- .add_baseline_2019_rows_cat(
      df = df,
      baseline_var = baseline_var,
      by = by
    ) %>%
      dplyr::mutate(
        value = .label_values(
          var,
          .clean_future_categorical_for_plot(value, include_missing = include_missing)
        )
      )
    
    dd_all <- dplyr::bind_rows(dd_future, dd_base)
  } else {
    dd_all <- dd_future
  }
  
  dd_all <- dd_all %>% dplyr::filter(!is.na(value))
  
  if (agg == "wave") {
    future_waves <- unique(dd_all$wave)
    future_waves <- future_waves[!is.na(future_waves)]
    future_waves <- if (is.factor(dd_all$wave)) levels(dd_all$wave) else sort(future_waves)
    wave_levels <- c("2019", setdiff(future_waves, "2019"))
    
    dd_all <- dd_all %>%
      dplyr::filter(!is.na(wave)) %>%
      dplyr::mutate(
        time = factor(wave, levels = wave_levels),
        time_lab = as.character(time)
      )
    
  } else if (agg == "ym") {
    
    dd_all <- dd_all %>%
      dplyr::filter(!is.na(ym)) %>%
      dplyr::mutate(
        time = ym,
        time_lab = format(ym, "%Y-%m")
      )
    
  } else {
    
    dd_all <- dd_all %>%
      dplyr::filter(!is.na(year)) %>%
      dplyr::mutate(
        time = year,
        time_lab = as.character(year)
      )
  }
  
  dd_all %>%
    dplyr::group_by(time, time_lab, facet_group, value) %>%
    dplyr::summarise(n = dplyr::n(), .groups = "drop_last") %>%
    dplyr::mutate(share = n / sum(n)) %>%
    dplyr::ungroup()
}

plot_future_categorical_dist_facet <- function(
    df,
    var,
    by,
    agg = c("wave", "ym", "year"),
    include_baseline_2019 = TRUE,
    baseline_var = NULL,
    include_missing = FALSE,
    scales = "fixed",
    ncol = NULL,
    out_file,
    fig_path
) {
  agg <- match.arg(agg)
  
  stopifnot(var %in% names(df))
  stopifnot(!missing(by), by %in% names(df))
  
  dd <- .prepare_future_categorical_dist_facet_data(
    df = df,
    var = var,
    by = by,
    agg = agg,
    include_baseline_2019 = include_baseline_2019,
    baseline_var = baseline_var,
    include_missing = include_missing
  )
  
  p <- ggplot(dd, aes(x = time, y = share, fill = value)) +
    geom_col(position = "fill") +
    facet_wrap(~ facet_group, scales = scales, ncol = ncol) +
    scale_y_continuous(labels = scales::percent_format()) +
    theme_minimal() +
    labs(
      x = NULL,
      y = "Share of respondents",
      fill = .var_label(var),
      title = .categorical_title(var, by = by)
    ) +
    theme(
      legend.position = "bottom",
      axis.text.x = element_text(angle = 90, hjust = 1)
    )
  
  p <- .apply_time_labels(p, dd, agg = agg)
  
  ggsave(
    filename = out_file,
    plot = p,
    path = fig_path,
    width = 14,
    height = 9
  )
  
  p
}

# --- main driver: plot everything ---------------------------------------------

plot_all_future_outcomes <- function(
    df,
    prefix,
    fig_path,
    exclude = c(
      "pidp", "hidp", "wave", "ym", "year",
      "industry", "occupation",
      "partner_pidp", "partner_rel", "family_id",
      "base_partner_pidp", "base_partner_rel", "base_family_id",
      "source_wave"
    )
) {
  dir.create(fig_path, showWarnings = FALSE, recursive = TRUE)
  
  vars <- setdiff(names(df), exclude)
  vars <- vars[vars %in% names(df) & !vapply(df[vars], \(x) all(is.na(x)), logical(1))]
  
  message("Future outcomes to plot (n=", length(vars), "):")
  message(paste(vars, collapse = ", "))
  
  for (v in vars) {
    out_stub <- paste0(prefix, "_", .safe_name(v))
    x <- df[[v]]
    
    if (is.numeric(x)) {
      plot_future_numeric_mean(
        df = df,
        var = v,
        out_file = paste0(out_stub, "_timeplot.png"),
        fig_path = fig_path
      )
    } else {
      plot_future_categorical_dist(
        df = df,
        var = v,
        out_file = paste0(out_stub, "_dist.png"),
        fig_path = fig_path
      )
    }
  }
  
  invisible(TRUE)
}