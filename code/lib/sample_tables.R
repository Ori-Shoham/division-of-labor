# =============================================================================
# File: code/lib/sample_tables.R
#
# Purpose:
#   Helper functions for creating couple-sample composition tables and exporting
#   them as LaTeX fragments suitable for Beamer slides or coauthor sharing.
#
# Inputs expected by downstream run script:
#   - samples/s2019_baseline_couplelevel.rds        (rich baseline couple data)
#   - derived/df_sample_long_covid_couplelevel.rds  (couple x wave)
#   - derived/future_outcomes_couple_long_lmo.rds   (couple x wave)
#
# Outputs:
#   LaTeX .tex fragments saved by the run script to tables/
#
# Notes:
#   - Assumes the build pipeline has already been corrected so that the
#     couple-level datasets contain:
#         group_industry_based_h / _w
#         group_industry_based_detailed_h / _w
#         base_age_youngest_child_h / _w
#   - Therefore, unlike earlier debugging versions, this file does NOT include
#     fallback logic for missing or alternative variable names.
# =============================================================================

suppressPackageStartupMessages({
  library(tidyverse)
  library(knitr)
  library(kableExtra)
  library(scales)
})

# -----------------------------------------------------------------------------
# Standardize variables needed for sample tables
#
# Creates:
#   - wife_group_3 / husband_group_3
#   - wife_group_5 / husband_group_5
#   - youngest_child_2019
#
# Assumes final cleaned variable names are present in the input data.
# -----------------------------------------------------------------------------
prep_sample_table_vars <- function(df) {
  df %>%
    dplyr::mutate(
      wife_group_3 = factor(
        group_industry_based_w,
        levels = c(
          "shutdown sector",
          "key worker",
          "other",
          "missing industry / occupation"
        )
      ),
      husband_group_3 = factor(
        group_industry_based_h,
        levels = c(
          "shutdown sector",
          "key worker",
          "other",
          "missing industry / occupation"
        )
      ),
      wife_group_5 = factor(
        group_industry_based_detailed_w,
        levels = c(
          "shutdown sector",
          "key worker - health\n and social services",
          "key worker - education",
          "key worker - public safety\n and essential gvt. services",
          "other",
          "missing industry / occupation"
        )
      ),
      husband_group_5 = factor(
        group_industry_based_detailed_h,
        levels = c(
          "shutdown sector",
          "key worker - health\n and social services",
          "key worker - education",
          "key worker - public safety\n and essential gvt. services",
          "other",
          "missing industry / occupation"
        )
      ),
      youngest_child_2019 = base_age_youngest_child_w
    )
}

# -----------------------------------------------------------------------------
# Reduce a couple x wave dataset to one row per couple
#
# Used for sample-composition tables, where the unit is the couple rather than
# the couple-wave observation.
# -----------------------------------------------------------------------------
collapse_to_unique_couples <- function(df) {
  df %>%
    dplyr::distinct(couple_id, .keep_all = TRUE)
}

# -----------------------------------------------------------------------------
# Complete cross-tab in wide form
#
# Example:
#   wife_group_3 x husband_group_3
#
# Returns:
#   data frame in wide format with zero-filled cells
# -----------------------------------------------------------------------------
make_crosstab <- function(df, row_var, col_var) {
  row_var <- rlang::ensym(row_var)
  col_var <- rlang::ensym(col_var)

  df %>%
    dplyr::count(!!row_var, !!col_var, name = "N") %>%
    tidyr::complete(!!row_var, !!col_var, fill = list(N = 0)) %>%
    tidyr::pivot_wider(
      names_from  = !!col_var,
      values_from = N
    ) %>%
    dplyr::arrange(!!row_var)
}

# -----------------------------------------------------------------------------
# Exact-age distribution of youngest child in 2019
#
# Notes:
#   - Uses wife's baseline youngest-child variable, which should be identical
#     within the couple by construction.
#   - Missing values are interpreted as "No children".
#   - Ages 18+ are pooled into a single "18+" category.
#   - Adds a final row with the total number of couples.
# -----------------------------------------------------------------------------
make_child_age_table_exact <- function(df) {
  out <- df %>%
    dplyr::mutate(
      youngest_child_age_cat = dplyr::case_when(
        is.na(youngest_child_2019) ~ "No children",
        youngest_child_2019 >= 18 ~ "18+",
        TRUE ~ as.character(youngest_child_2019)
      )
    ) %>%
    dplyr::count(youngest_child_age_cat, name = "N") %>%
    dplyr::mutate(
      sort_num = dplyr::case_when(
        youngest_child_age_cat == "No children" ~ -1,
        youngest_child_age_cat == "18+" ~ 18,
        TRUE ~ suppressWarnings(as.numeric(youngest_child_age_cat))
      )
    ) %>%
    dplyr::arrange(sort_num) %>%
    dplyr::select(-sort_num) %>%
    dplyr::rename(`Youngest child age in 2019` = youngest_child_age_cat)

  total_row <- tibble::tibble(
    `Youngest child age in 2019` = "Total couples",
    N = nrow(df)
  )

  dplyr::bind_rows(out, total_row)
}

# -----------------------------------------------------------------------------
# Binned distribution of youngest child in 2019
#
# Bins:
#   - No children
#   - 0-6
#   - 7-10
#   - 11-15
#   - 16-17
#   - 18+
#
# Adds a final row with the total number of couples.
# -----------------------------------------------------------------------------
make_child_age_table_binned <- function(df) {
  out <- df %>%
    dplyr::mutate(
      youngest_child_age_bin = dplyr::case_when(
        is.na(youngest_child_2019) ~ "No children",
        youngest_child_2019 <= 6 ~ "0-6",
        youngest_child_2019 <= 10 ~ "7-10",
        youngest_child_2019 <= 15 ~ "11-15",
        youngest_child_2019 <= 17 ~ "16-17",
        youngest_child_2019 >= 18 ~ "18+",
        TRUE ~ NA_character_
      ),
      youngest_child_age_bin = factor(
        youngest_child_age_bin,
        levels = c("No children", "0-6", "7-10", "11-15", "16-17", "18+")
      )
    ) %>%
    dplyr::count(youngest_child_age_bin, name = "N") %>%
    tidyr::complete(youngest_child_age_bin, fill = list(N = 0)) %>%
    dplyr::rename(`Youngest child age in 2019` = youngest_child_age_bin) %>%
    dplyr::mutate(
      `Youngest child age in 2019` = as.character(`Youngest child age in 2019`)
    )

  total_row <- tibble::tibble(
    `Youngest child age in 2019` = "Total couples",
    N = nrow(df)
  )

  dplyr::bind_rows(out, total_row)
}

# -----------------------------------------------------------------------------
# Combine three one-column frequency tables into one comparison table
#
# Inputs should each have:
#   - first column = category label
#   - second column = N
#
# Output:
#   one table with the category column plus three sample columns
# -----------------------------------------------------------------------------
combine_three_count_tables <- function(df_a, df_b, df_c,
                                       sample_names = c("Baseline", "COVID", "Future")) {

  key_name <- names(df_a)[1]

  out_a <- df_a %>%
    dplyr::rename(!!sample_names[1] := N)

  out_b <- df_b %>%
    dplyr::rename(!!sample_names[2] := N)

  out_c <- df_c %>%
    dplyr::rename(!!sample_names[3] := N)

  out_a %>%
    dplyr::left_join(out_b, by = key_name) %>%
    dplyr::left_join(out_c, by = key_name) %>%
    dplyr::select(
      !!rlang::sym(key_name),
      dplyr::all_of(sample_names)
    )
}

# -----------------------------------------------------------------------------
# Internal helper: convert a data frame to a LaTeX tabular fragment
#
# Returns:
#   character string containing a LaTeX tabular environment
#
# Notes:
#   - knitr::kable() can error if digits = NULL is passed explicitly in some
#     R / knitr combinations, so we only pass the digits argument when it is
#     actually supplied.
# -----------------------------------------------------------------------------
df_to_latex_tabular <- function(df,
                                align = NULL,
                                digits = NULL,
                                escape = FALSE,
                                col_names = NULL) {
  if (is.null(align)) {
    align <- paste0("l", paste(rep("r", ncol(df) - 1), collapse = ""))
  }

  kable_args <- list(
    x         = df,
    format    = "latex",
    booktabs  = TRUE,
    longtable = FALSE,
    escape    = escape,
    align     = align,
    col.names = col_names,
    linesep   = ""
  )

  if (!is.null(digits)) {
    kable_args$digits <- digits
  }

  out <- do.call(knitr::kable, kable_args)

  as.character(out)
}

# -----------------------------------------------------------------------------
# Write a single LaTeX table fragment with header
#
# Output:
#   A .tex file containing:
#     - optional title
#     - tabular environment (no floating table)
#
# This is Beamer-friendly via \input{}.
# -----------------------------------------------------------------------------
write_latex_table <- function(df,
                              file,
                              title = NULL,
                              align = NULL,
                              escape = FALSE,
                              digits = NULL) {

  tex <- df_to_latex_tabular(
    df        = df,
    align     = align,
    digits    = digits,
    escape    = escape,
    col_names = names(df)
  )

  header <- ""

  if (!is.null(title)) {
    header <- paste0(header, "\\textbf{", title, "}\\\\\n")
  }

  out <- paste0(header, tex)

  writeLines(out, con = file)
  invisible(file)
}

# -----------------------------------------------------------------------------
# Write a three-panel LaTeX table fragment with header
#
# Output:
#   One tabular with:
#     - title
#     - stacked panels
# -----------------------------------------------------------------------------
write_three_panel_table <- function(df_a, df_b, df_c,
                                    panel_titles,
                                    file,
                                    title = NULL,
                                    align = NULL,
                                    escape = FALSE,
                                    digits = NULL) {
  
  tex_a <- df_to_latex_tabular(
    df_a,
    align = align,
    digits = digits,
    escape = escape,
    col_names = names(df_a)
  )
  tex_b <- df_to_latex_tabular(
    df_b,
    align = align,
    digits = digits,
    escape = escape,
    col_names = names(df_b)
  )
  tex_c <- df_to_latex_tabular(
    df_c,
    align = align,
    digits = digits,
    escape = escape,
    col_names = names(df_c)
  )
  
  strip_tabular <- function(x) {
    x <- gsub("\\\\begin\\{tabular\\}\\{[^}]+\\}", "", x)
    x <- gsub("\\\\end\\{tabular\\}", "", x)
    trimws(x)
  }
  
  get_spec <- function(x) {
    stringr::str_match(x, "\\\\begin\\{tabular\\}\\{([^}]+)\\}")[, 2]
  }
  
  strip_rules <- function(x) {
    x <- gsub("^\\\\toprule\\s*", "", x)
    x <- gsub("\\s*\\\\bottomrule\\s*$", "", x)
    trimws(x)
  }
  
  spec <- get_spec(tex_a)
  
  body_a <- strip_rules(strip_tabular(tex_a))
  body_b <- strip_rules(strip_tabular(tex_b))
  body_c <- strip_rules(strip_tabular(tex_c))
  
  panel_line <- function(title, ncol) {
    paste0(
      "\\addlinespace[0.5em]\n",
      "\\multicolumn{", ncol, "}{l}{\\textbf{", title, "}} \\\\\n",
      "\\addlinespace[0.25em]\n"
    )
  }
  
  ncol_out <- ncol(df_a)
  
  table_body <- paste0(
    "\\begin{tabular}{", spec, "}\n",
    "\\toprule\n",
    panel_line(panel_titles[1], ncol_out),
    body_a, "\n",
    panel_line(panel_titles[2], ncol_out),
    body_b, "\n",
    panel_line(panel_titles[3], ncol_out),
    body_c, "\n",
    "\\bottomrule\n",
    "\\end{tabular}\n"
  )
  
  header <- ""
  
  if (!is.null(title)) {
    header <- paste0(header, "\\textbf{", title, "}\\\\\n")
  }
  
  out <- paste0(header, table_body)
  
  writeLines(out, con = file)
  invisible(file)
}

# =============================================================================
# SECTION: Couple workoutside composition over time
#
# Purpose:
#   Build and plot the distribution of joint husband-wife workoutside status
#   over time for the couple-level COVID and future-outcomes panels.
#
# Key behavior:
#   - If use_shares = TRUE:
#       stacked bars of shares within each time period
#   - If use_shares = FALSE:
#       side-by-side (dodged) bars of counts within each time period
#
# Dependencies:
#   - Expects code/lib/wave_labels.R to be sourced before these helpers are used.
# =============================================================================

# -----------------------------------------------------------------------------
# Internal helper: build joint husband-wife binary status
#
# Output categories:
#   - Neither spouse ...
#   - Husband only ...
#   - Wife only ...
#   - Both spouses ...
#
# Notes:
#   - Assumes the husband and wife variables are coded 0/1
#   - Rows with missing husband or wife values are dropped
# -----------------------------------------------------------------------------
make_joint_binary_status <- function(df,
                                     husband_var,
                                     wife_var,
                                     status_var_name,
                                     label_neither,
                                     label_husband_only,
                                     label_wife_only,
                                     label_both) {
  husband_var <- rlang::ensym(husband_var)
  wife_var <- rlang::ensym(wife_var)
  status_var_name <- rlang::as_string(rlang::ensym(status_var_name))

  status_levels <- c(
    label_neither,
    label_husband_only,
    label_wife_only,
    label_both
  )

  out <- df %>%
    dplyr::filter(
      !is.na(!!husband_var),
      !is.na(!!wife_var)
    ) %>%
    dplyr::mutate(
      status_value = dplyr::case_when(
        !!husband_var == 0 & !!wife_var == 0 ~ label_neither,
        !!husband_var == 1 & !!wife_var == 0 ~ label_husband_only,
        !!husband_var == 0 & !!wife_var == 1 ~ label_wife_only,
        !!husband_var == 1 & !!wife_var == 1 ~ label_both,
        TRUE ~ NA_character_
      )
    )

  out[[status_var_name]] <- factor(out$status_value, levels = status_levels)
  out$status_value <- NULL
  out
}

# -----------------------------------------------------------------------------
# Joint husband-wife workoutside status
#
# Output categories:
#   - Neither spouse works outside
#   - Husband only works outside
#   - Wife only works outside
#   - Both spouses work outside
#
# Notes:
#   - Assumes workoutside_h and workoutside_w are coded 0/1
#   - Rows with missing husband or wife workoutside are dropped
# -----------------------------------------------------------------------------
make_joint_workoutside_status <- function(df) {
  make_joint_binary_status(
    df = df,
    husband_var = workoutside_h,
    wife_var = workoutside_w,
    status_var_name = couple_workoutside_status,
    label_neither = "Neither spouse works outside",
    label_husband_only = "Husband only works outside",
    label_wife_only = "Wife only works outside",
    label_both = "Both spouses work outside"
  )
}

# -----------------------------------------------------------------------------
# Joint husband-wife WFH-some status
#
# Output categories:
#   - Neither spouse WFH at least sometimes
#   - Husband only WFH at least sometimes
#   - Wife only WFH at least sometimes
#   - Both spouses WFH at least sometimes
#
# Notes:
#   - Assumes wfh_some_h and wfh_some_w are coded 0/1
#   - Rows with missing husband or wife wfh_some are dropped
# -----------------------------------------------------------------------------
make_joint_wfh_some_status <- function(df) {
  make_joint_binary_status(
    df = df,
    husband_var = wfh_some_h,
    wife_var = wfh_some_w,
    status_var_name = couple_wfh_some_status,
    label_neither = "Neither spouse WFH at least sometimes",
    label_husband_only = "Husband only WFH at least sometimes",
    label_wife_only = "Wife only WFH at least sometimes",
    label_both = "Both spouses WFH at least sometimes"
  )
}

# -----------------------------------------------------------------------------
# Internal helper: build labeled time axis
#
# Arguments:
#   - time_values: character vector of time values observed in the data
#   - time_scale:
#       "covid_wave"  -> use covid wave labels
#       "future_wave" -> use future wave labels
#       "year"        -> use sorted year values as labels
#
# Returns:
#   tibble with:
#     time_value
#     time_label
# -----------------------------------------------------------------------------
build_time_axis_lookup <- function(time_values,
                                   time_scale = c("covid_wave", "future_wave", "year")) {
  time_scale <- match.arg(time_scale)

  time_values <- unique(as.character(time_values))
  time_values <- time_values[!is.na(time_values)]

  if (time_scale == "covid_wave") {
    wl <- covid_wave_label_lookup() %>%
      dplyr::transmute(
        time_value = as.character(wave),
        time_label = wave_label_short
      )

    return(wl %>% dplyr::filter(time_value %in% time_values))
  }

  if (time_scale == "future_wave") {
    wl <- future_wave_label_lookup() %>%
      dplyr::transmute(
        time_value = as.character(wave),
        time_label = wave_label_short
      )

    return(wl %>% dplyr::filter(time_value %in% time_values))
  }

  tibble::tibble(
    time_value = as.character(sort(unique(as.numeric(time_values)))),
    time_label = as.character(sort(unique(as.numeric(time_values))))
  )
}

# -----------------------------------------------------------------------------
# Internal helper: build composition table for couple binary status over time
#
# Arguments:
#   - df: couple-level long dataset
#   - time_var: time aggregation variable (e.g. wave, year)
#   - time_scale:
#       "covid_wave"
#       "future_wave"
#       "year"
#   - joint_status_fn: function that adds the joint status variable
#   - status_var: name of the joint status variable
#   - status_levels: levels in the intended plotting order
#
# Returns:
#   one row per time period x joint-status with:
#     - N
#     - share
#     - time_label (for plotting)
# -----------------------------------------------------------------------------
make_binary_composition <- function(df,
                                    time_var,
                                    time_scale = c("covid_wave", "future_wave", "year"),
                                    joint_status_fn,
                                    status_var,
                                    status_levels) {
  time_scale <- match.arg(time_scale)
  time_var <- rlang::ensym(time_var)
  status_var <- rlang::ensym(status_var)

  time_values <- df %>%
    dplyr::filter(!is.na(!!time_var)) %>%
    dplyr::pull(!!time_var) %>%
    as.character()

  axis_lookup <- build_time_axis_lookup(
    time_values = time_values,
    time_scale  = time_scale
  )

  df %>%
    joint_status_fn() %>%
    dplyr::mutate(
      time_value = as.character(!!time_var)
    ) %>%
    dplyr::count(time_value, !!status_var, name = "N") %>%
    tidyr::complete(
      time_value = axis_lookup$time_value,
      !!status_var := factor(status_levels, levels = status_levels),
      fill = list(N = 0)
    ) %>%
    dplyr::group_by(time_value) %>%
    dplyr::mutate(
      share = N / sum(N)
    ) %>%
    dplyr::ungroup() %>%
    dplyr::left_join(axis_lookup, by = "time_value") %>%
    dplyr::mutate(
      time_label = factor(
        time_label,
        levels = axis_lookup$time_label
      )
    )
}

# -----------------------------------------------------------------------------
# Build composition table for couple workoutside status over time
#
# Arguments:
#   - df: couple-level long dataset
#   - time_var: time aggregation variable (e.g. wave, year)
#   - time_scale:
#       "covid_wave"
#       "future_wave"
#       "year"
#
# Returns:
#   one row per time period x joint-status with:
#     - N
#     - share
#     - time_label (for plotting)
# -----------------------------------------------------------------------------
make_workoutside_composition <- function(df,
                                         time_var,
                                         time_scale = c("covid_wave", "future_wave", "year")) {
  status_levels <- c(
    "Neither spouse works outside",
    "Husband only works outside",
    "Wife only works outside",
    "Both spouses work outside"
  )

  make_binary_composition(
    df = df,
    time_var = {{ time_var }},
    time_scale = time_scale,
    joint_status_fn = make_joint_workoutside_status,
    status_var = couple_workoutside_status,
    status_levels = status_levels
  )
}

# -----------------------------------------------------------------------------
# Build composition table for couple WFH-some status over time
# -----------------------------------------------------------------------------
make_wfh_some_composition <- function(df,
                                      time_var,
                                      time_scale = c("covid_wave", "future_wave", "year")) {
  status_levels <- c(
    "Neither spouse WFH at least sometimes",
    "Husband only WFH at least sometimes",
    "Wife only WFH at least sometimes",
    "Both spouses WFH at least sometimes"
  )

  make_binary_composition(
    df = df,
    time_var = {{ time_var }},
    time_scale = time_scale,
    joint_status_fn = make_joint_wfh_some_status,
    status_var = couple_wfh_some_status,
    status_levels = status_levels
  )
}

# -----------------------------------------------------------------------------
# Internal helper: plot couple binary composition over time
#
# Arguments:
#   - composition_fn: function returning the composition dataset
#   - fill_var: fill variable in the composition dataset
#   - fill_lab: legend title
# -----------------------------------------------------------------------------
plot_binary_composition <- function(df,
                                    time_var,
                                    time_scale = c("covid_wave", "future_wave", "year"),
                                    composition_fn,
                                    fill_var,
                                    use_shares = TRUE,
                                    x_lab = NULL,
                                    y_lab = NULL,
                                    title = NULL) {
  time_scale <- match.arg(time_scale)
  fill_var <- rlang::ensym(fill_var)

  dd <- composition_fn(
    df = df,
    time_var = {{ time_var }},
    time_scale = time_scale
  )

  y_var <- if (use_shares) "share" else "N"

  if (is.null(y_lab)) {
    y_lab <- if (use_shares) "Share of couples" else "Number of couples"
  }

  if (is.null(x_lab)) {
    x_lab <- dplyr::case_when(
      time_scale == "covid_wave"  ~ "COVID study wave",
      time_scale == "future_wave" ~ "Main study wave",
      time_scale == "year"        ~ "Calendar year"
    )
  }

  p <- ggplot2::ggplot(
    dd,
    ggplot2::aes(
      x = time_label,
      y = .data[[y_var]],
      fill = !!fill_var
    )
  ) +
    {
      if (use_shares) {
        ggplot2::geom_col(width = 0.8)
      } else {
        ggplot2::geom_col(
          position = ggplot2::position_dodge(width = 0.85),
          width = 0.75
        )
      }
    } +
    ggplot2::labs(
      x = x_lab,
      y = y_lab,
      fill = NULL,
      title = title
    ) +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(
      legend.position = "bottom",
      axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)
    )

  if (use_shares) {
    p <- p + ggplot2::scale_y_continuous(labels = scales::percent_format())
  }

  p
}

# -----------------------------------------------------------------------------
# Plot couple workoutside composition over time
#
# Arguments:
#   - df: couple-level long dataset
#   - time_var: aggregation variable (e.g. wave, year)
#   - time_scale:
#       "covid_wave"
#       "future_wave"
#       "year"
#   - use_shares:
#       TRUE  -> stacked bars of shares
#       FALSE -> side-by-side bars of counts
#
# Notes:
#   - Shares are stacked because they represent composition within each period
#   - Counts are dodged because side-by-side bars are easier to compare in levels
# -----------------------------------------------------------------------------
plot_workoutside_composition <- function(df,
                                         time_var,
                                         time_scale = c("covid_wave", "future_wave", "year"),
                                         use_shares = TRUE,
                                         x_lab = NULL,
                                         y_lab = NULL,
                                         title = NULL) {
  plot_binary_composition(
    df = df,
    time_var = {{ time_var }},
    time_scale = time_scale,
    composition_fn = make_workoutside_composition,
    fill_var = couple_workoutside_status,
    use_shares = use_shares,
    x_lab = x_lab,
    y_lab = y_lab,
    title = title
  )
}

# -----------------------------------------------------------------------------
# Plot couple WFH-some composition over time
# -----------------------------------------------------------------------------
plot_wfh_some_composition <- function(df,
                                      time_var,
                                      time_scale = c("covid_wave", "future_wave", "year"),
                                      use_shares = TRUE,
                                      x_lab = NULL,
                                      y_lab = NULL,
                                      title = NULL) {
  plot_binary_composition(
    df = df,
    time_var = {{ time_var }},
    time_scale = time_scale,
    composition_fn = make_wfh_some_composition,
    fill_var = couple_wfh_some_status,
    use_shares = use_shares,
    x_lab = x_lab,
    y_lab = y_lab,
    title = title
  )
}
