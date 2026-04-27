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

subset_couples_with_young_child_2019 <- function(df, max_age = 10) {
  df %>%
    dplyr::mutate(
      youngest_child_2019_num = haven::zap_labels(youngest_child_2019),
      youngest_child_2019_num = as.numeric(youngest_child_2019_num)
    ) %>%
    dplyr::filter(
      !is.na(youngest_child_2019_num),
      youngest_child_2019_num >= 0,
      youngest_child_2019_num <= max_age
    ) %>%
    dplyr::select(-youngest_child_2019_num)
}

collapse_to_unique_couples <- function(df) {
  df %>%
    dplyr::distinct(couple_id, .keep_all = TRUE)
}

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
    ) +
    guides(fill = guide_legend(nrow = 2, byrow = TRUE))

  if (use_shares) {
    p <- p + ggplot2::scale_y_continuous(labels = scales::percent_format())
  }

  p
}

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

# =============================================================================

# =============================================================================
# Treatment x child-age baseline/history comparison table
# =============================================================================

clean_balance_numeric <- function(x, invalid_negative = TRUE) {
  x <- haven::zap_labels(x)
  x <- suppressWarnings(as.numeric(x))
  if (invalid_negative) {
    x[x < 0] <- NA_real_
  }
  x
}

balance_col_numeric <- function(df, col, invalid_negative = TRUE) {
  if (!col %in% names(df)) {
    return(rep(NA_real_, nrow(df)))
  }
  clean_balance_numeric(df[[col]], invalid_negative = invalid_negative)
}

balance_coalesce_numeric <- function(df, cols, invalid_negative = TRUE) {
  vals <- lapply(cols, function(col) {
    balance_col_numeric(df, col, invalid_negative = invalid_negative)
  })
  Reduce(dplyr::coalesce, vals)
}

balance_keyworker_nonedu <- function(df, spouse = c("w", "h")) {
  spouse <- match.arg(spouse)

  bool_col <- if (spouse == "w") {
    "wife_is_keyworker_nonedu"
  } else {
    "husb_is_keyworker_nonedu"
  }

  if (bool_col %in% names(df)) {
    x <- df[[bool_col]]
    return(dplyr::case_when(
      is.na(x) ~ NA_real_,
      x %in% c(TRUE, 1) ~ 1,
      x %in% c(FALSE, 0) ~ 0,
      TRUE ~ NA_real_
    ))
  }

  group_col <- paste0("group_industry_based_detailed_", spouse)
  if (!group_col %in% names(df)) {
    return(rep(NA_real_, nrow(df)))
  }

  dplyr::case_when(
    is.na(df[[group_col]]) ~ NA_real_,
    df[[group_col]] %in% c(
      "key worker - health\n and social services",
      "key worker - public safety\n and essential gvt. services"
    ) ~ 1,
    TRUE ~ 0
  )
}

prep_treatment_child_balance_vars <- function(df,
                                              treatment_var = "treat_wife_key_notedu_husb_not_or_edu") {
  stopifnot(treatment_var %in% names(df))

  df <- collapse_to_unique_couples(df)

  youngest_child <- balance_coalesce_numeric(
    df,
    cols = c("youngest_child_2019", "base_age_youngest_child_w", "base_age_youngest_child_h")
  )

  first_cohabit_year <- balance_col_numeric(
    df,
    "hist_couple_first_observed_cohabit_base_partner_year",
    invalid_negative = FALSE
  )

  df$balance_treatment_status <- dplyr::case_when(
    df[[treatment_var]] == 1 ~ "Treated",
    df[[treatment_var]] == 0 ~ "Control",
    TRUE ~ NA_character_
  )

  df$balance_child_group <- dplyr::case_when(
    !is.na(youngest_child) & youngest_child >= 0 & youngest_child <= 10 ~ "Young child",
    !is.na(youngest_child) & youngest_child >= 11 & youngest_child <= 17 ~ "Older child",
    TRUE ~ NA_character_
  )

  df$balance_group <- dplyr::case_when(
    df$balance_treatment_status == "Treated" & df$balance_child_group == "Young child" ~
      "Treated, youngest child 0-10",
    df$balance_treatment_status == "Treated" & df$balance_child_group == "Older child" ~
      "Treated, youngest child 11-17",
    df$balance_treatment_status == "Control" & df$balance_child_group == "Young child" ~
      "Control, youngest child 0-10",
    df$balance_treatment_status == "Control" & df$balance_child_group == "Older child" ~
      "Control, youngest child 11-17",
    TRUE ~ NA_character_
  )

  df$balance_group <- factor(
    df$balance_group,
    levels = c(
      "Treated, youngest child 0-10",
      "Treated, youngest child 11-17",
      "Control, youngest child 0-10",
      "Control, youngest child 11-17"
    )
  )

  df$balance_wife_age <- balance_col_numeric(df, "base_age_dv_w")
  df$balance_husband_age <- balance_col_numeric(df, "base_age_dv_h")
  df$balance_children_u18 <- balance_coalesce_numeric(
    df,
    cols = c(
      "base_n_children_18_under_w",
      "base_n_children_18_under_h",
      "base_ndepchl_dv_w",
      "base_ndepchl_dv_h"
    )
  )
  df$balance_children_u10 <- balance_coalesce_numeric(
    df,
    cols = c("base_n_children_10_under_w", "base_n_children_10_under_h")
  )
  df$balance_youngest_child_age <- youngest_child
  df$balance_years_cohabit_before_2019 <- dplyr::case_when(
    is.na(first_cohabit_year) ~ NA_real_,
    first_cohabit_year < 2019 ~ 2019 - first_cohabit_year,
    TRUE ~ 0
  )
  df$balance_wife_jbhrs <- balance_col_numeric(df, "base_jbhrs_w")
  df$balance_husband_jbhrs <- balance_col_numeric(df, "base_jbhrs_h")
  df$balance_wife_keyworker_nonedu <- balance_keyworker_nonedu(df, "w")
  df$balance_husband_keyworker_nonedu <- balance_keyworker_nonedu(df, "h")
  df$balance_wife_fimnlabgrs <- balance_col_numeric(df, "base_fimnlabgrs_dv_w")
  df$balance_husband_fimnlabgrs <- balance_col_numeric(df, "base_fimnlabgrs_dv_h")

  df
}

treatment_child_balance_specs <- function() {
  tibble::tribble(
    ~var, ~label, ~type, ~digits,
    "balance_wife_age", "Wife age at baseline", "mean", 1,
    "balance_husband_age", "Husband age at baseline", "mean", 1,
    "balance_children_u18", "Number of children under 18 at baseline", "mean", 2,
    "balance_children_u10", "Number of children under 10 at baseline", "mean", 2,
    "balance_youngest_child_age", "Age of youngest child at baseline", "mean", 1,
    "balance_years_cohabit_before_2019", "Years observed cohabiting before 2019", "mean", 1,
    "balance_wife_jbhrs", "Wife weekly hours worked at baseline", "mean", 1,
    "balance_husband_jbhrs", "Husband weekly hours worked at baseline", "mean", 1,
    "balance_wife_keyworker_nonedu", "Wife keyworker non-education share", "percent", 1,
    "balance_husband_keyworker_nonedu", "Husband keyworker non-education share", "percent", 1,
    "balance_wife_fimnlabgrs", "Wife gross monthly labour income at baseline", "mean", 0,
    "balance_husband_fimnlabgrs", "Husband gross monthly labour income at baseline", "mean", 0
  )
}

format_balance_value <- function(x, type = c("mean", "percent", "n"), digits = 1) {
  type <- match.arg(type)
  if (length(x) == 0 || is.na(x)) {
    return("")
  }

  if (type == "n") {
    return(formatC(x, format = "d", big.mark = ","))
  }

  if (type == "percent") {
    return(paste0(formatC(100 * x, format = "f", digits = digits, big.mark = ","), "\\%"))
  }

  formatC(x, format = "f", digits = digits, big.mark = ",")
}

summarise_balance_cell <- function(df, var, type, digits) {
  if (type == "n") {
    n_couples <- dplyr::n_distinct(df$couple_id)
    return(list(mean = format_balance_value(n_couples, type = "n"), sd = ""))
  }

  if (!var %in% names(df)) {
    return(list(mean = "", sd = ""))
  }

  x <- df[[var]]
  x <- x[!is.na(x)]

  if (length(x) == 0) {
    return(list(mean = "", sd = ""))
  }

  mean_x <- mean(x)
  sd_x <- if (length(x) >= 2) stats::sd(x) else NA_real_

  list(
    mean = format_balance_value(mean_x, type = type, digits = digits),
    sd = ifelse(
      is.na(sd_x),
      "",
      paste0("(", format_balance_value(sd_x, type = type, digits = digits), ")")
    )
  )
}

make_treatment_child_balance_table <- function(df,
                                               treatment_var = "treat_wife_key_notedu_husb_not_or_edu",
                                               include_sd_rows = TRUE) {
  group_levels <- c(
    "Treated, youngest child 0-10",
    "Treated, youngest child 11-17",
    "Control, youngest child 0-10",
    "Control, youngest child 11-17"
  )

  dd <- prep_treatment_child_balance_vars(
    df = df,
    treatment_var = treatment_var
  ) %>%
    dplyr::filter(!is.na(balance_group))

  specs <- treatment_child_balance_specs()

  out_rows <- list()

  n_row <- tibble::tibble(Variable = "Number of couples")
  for (g in group_levels) {
    cell <- summarise_balance_cell(
      df = dd %>% dplyr::filter(balance_group == g),
      var = NULL,
      type = "n",
      digits = 0
    )
    n_row[[g]] <- cell$mean
  }
  out_rows[[length(out_rows) + 1]] <- n_row

  for (i in seq_len(nrow(specs))) {
    spec <- specs[i, ]

    mean_row <- tibble::tibble(Variable = spec$label)
    sd_row <- tibble::tibble(Variable = "")

    for (g in group_levels) {
      cell <- summarise_balance_cell(
        df = dd %>% dplyr::filter(balance_group == g),
        var = spec$var,
        type = spec$type,
        digits = spec$digits
      )
      mean_row[[g]] <- cell$mean
      sd_row[[g]] <- cell$sd
    }

    out_rows[[length(out_rows) + 1]] <- mean_row
    if (include_sd_rows) {
      out_rows[[length(out_rows) + 1]] <- sd_row
    }
  }

  dplyr::bind_rows(out_rows)
}

write_treatment_child_balance_table <- function(df,
                                                file,
                                                title = NULL) {
  write_latex_table(
    df = df,
    file = file,
    title = title,
    align = paste0("l", paste(rep("r", ncol(df) - 1), collapse = "")),
    escape = FALSE
  )
}
