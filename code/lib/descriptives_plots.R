# =============================================================================
# File: code/lib/descriptives_plots.R
#
# Purpose:
#   Parameterized descriptive plots to avoid copy/paste blocks.
#   Produces:
#     - worked at all (hours>0): counts and percent by industry or group
#     - work status (sempderived): counts and percent by industry or group
#     - furlough (furlough var): percent by industry or group (employed sample)
#     - WFH distribution: percent by industry or group (employed sample)
#     - overtime/event-style lines for worked_at_all and hours
#     - workoutside over time (overall and by groups)
#
# Notes:
#   - Expects df with columns:
#       wave, industry (string), occupation (string) from SOC/SIC joins,
#       group_industry_based, group_industry_based_detailed,
#       hours, sempderived, furlough, wah, workoutside
# =============================================================================

suppressPackageStartupMessages({
  library(tidyverse)
  library(scales)
  library(forcats)
})

# -----------------------------------------------------------------------------
# Wave label mapping (short for axis, full for titles)
# -----------------------------------------------------------------------------
wave_labels <- function() {
  wave_label_lookup() %>%
    dplyr::transmute(
      wave,
      wave_lab_short = wave_label_short,
      wave_lab_full  = wave_label_full
    )
}

wave_full_label <- function(wave_code) {
  wl <- wave_label_lookup()
  out <- wl %>%
    dplyr::filter(wave == wave_code) %>%
    dplyr::pull(wave_label_full)
  if (length(out) == 0) wave_code else out
}

# -----------------------------------------------------------------------------
# Cleaning helpers
# -----------------------------------------------------------------------------
clean_hours <- function(x) {
  # Remove common missing codes seen in COVID extracts
  x[x %in% c(-9, -2, -1)] <- NA
  x
}

# -----------------------------------------------------------------------------
# Generic label helpers
# -----------------------------------------------------------------------------
.work_status <- function(sempderived) {
  case_when(
    is.na(sempderived) ~ NA_character_,
    sempderived < 0 ~ "Missing",
    sempderived == 1 ~ "Employed",
    sempderived == 2 ~ "Self-employed",
    sempderived == 3 ~ "Both employed and self-employed",
    TRUE ~ "Not employed"
  )
}

.wfh_cat <- function(sempderived, wah) {
  case_when(
    is.na(sempderived) ~ NA_character_,
    sempderived %in% c(4) ~ "Not employed",
    wah == 1 ~ "Always",
    wah == 2 ~ "Often",
    wah == 3 ~ "Sometimes",
    wah == 4 ~ "Never",
    TRUE ~ NA_character_
  )
}

.furlough_cat <- function(sempderived, furlough) {
  case_when(
    is.na(sempderived) ~ NA_character_,
    sempderived %in% c(2, 4) ~ "Self- or not employed",
    furlough == 1 ~ "Yes",
    furlough == 2 ~ "No",
    TRUE ~ NA_character_
  )
}

# -----------------------------------------------------------------------------
# Worked at all (hours>0): bar / percent
# -----------------------------------------------------------------------------
plot_worked_at_all_bar <- function(df, wave_code, by, min_n = 25, perc = FALSE, out_file, fig_path) {
  
  stopifnot(by %in% c("industry", "group_industry_based", "group_industry_based_detailed"))
  
  dd <- df %>%
    filter(wave == wave_code, sempderived >= 0) %>%
    mutate(hours = clean_hours(hours)) %>%
    filter(!is.na(hours)) %>%
    mutate(byvar = .data[[by]]) %>%
    add_count(byvar, name = "n_by_raw") %>%
    mutate(byvar = if_else(n_by_raw < min_n, "other", as.character(byvar)))
  
  if (!perc) {
    p <- ggplot(dd, aes(x = forcats::fct_infreq(byvar),
                        fill = factor(hours > 0, levels = c(TRUE, FALSE),
                                      labels = c("Yes", "No")))) +
      geom_bar() +
      coord_flip() +
      labs(fill = "Worked at all last week", x = NULL,
           title = paste0("Worked last week, ", wave_full_label(wave_code))) +
      theme(legend.position = "bottom")
    
    ggsave(out_file, p, path = fig_path, width = 12, height = 8)
    return(p)
  }
  
  dd2 <- dd %>%
    group_by(byvar) %>%
    mutate(
      n_by = n(),
      share_yes = mean(hours > 0),
      by_lab = paste0(byvar, " (n=", n_by, ")")
    ) %>%
    ungroup()
  
  p <- ggplot(dd2, aes(x = forcats::fct_reorder(by_lab, share_yes, .desc = TRUE),
                       fill = factor(hours > 0, levels = c(TRUE, FALSE),
                                     labels = c("Yes", "No")))) +
    geom_bar(position = "fill") +
    coord_flip() +
    scale_y_continuous(labels = scales::percent_format()) +
    labs(fill = "Worked at all last week", x = NULL, y = NULL,
         title = paste0("Worked last week, ", wave_full_label(wave_code))) +
    theme(legend.position = "bottom")
  
  ggsave(out_file, p, path = fig_path, width = 12, height = 8)
  p
}

# -----------------------------------------------------------------------------
# Work status (sempderived): bar / percent
# -----------------------------------------------------------------------------
plot_work_status_bar <- function(df, wave_code, by, min_n = 25, perc = FALSE, out_file, fig_path) {
  
  stopifnot(by %in% c("industry", "group_industry_based", "group_industry_based_detailed"))
  
  dd <- df %>%
    filter(wave == wave_code, !is.na(sempderived)) %>%
    mutate(byvar = .data[[by]]) %>%
    add_count(byvar, name = "n_by_raw") %>%
    mutate(
      byvar = if_else(n_by_raw < min_n, "other", as.character(byvar)),
      status = .work_status(sempderived)
    )
  
  if (!perc) {
    dd <- dd %>%
      mutate(status = factor(status,
                             levels = c("Employed", "Self-employed",
                                        "Both employed and self-employed",
                                        "Not employed", "Missing")))
    
    p <- ggplot(dd, aes(x = forcats::fct_infreq(byvar), fill = status)) +
      geom_bar(position = position_stack(reverse = TRUE)) +
      coord_flip() +
      labs(fill = "Work status", x = NULL,
           title = paste0("Work status, ", wave_full_label(wave_code))) +
      theme(legend.position = "bottom")
    
    ggsave(out_file, p, path = fig_path, width = 14, height = 8)
    return(p)
  }
  
  dd2 <- dd %>%
    group_by(byvar) %>%
    mutate(
      n_by = n(),
      share_not = mean(status == "Not employed"),
      by_lab = paste0(byvar, " (n=", n_by, ")"),
      status = factor(status,
                      levels = c("Not employed", "Employed", "Self-employed",
                                 "Both employed and self-employed", "Missing"))
    ) %>%
    ungroup()
  
  p <- ggplot(dd2, aes(x = forcats::fct_reorder(by_lab, share_not, .desc = TRUE),
                       fill = status)) +
    geom_bar(position = position_fill(reverse = TRUE)) +
    coord_flip() +
    scale_y_continuous(labels = scales::percent_format()) +
    labs(fill = "Work status", x = NULL, y = NULL,
         title = paste0("Work status, ", wave_full_label(wave_code))) +
    theme(legend.position = "bottom")
  
  ggsave(out_file, p, path = fig_path, width = 14, height = 8)
  p
}

# -----------------------------------------------------------------------------
# Furlough: percent by industry/group (employed sample)
# -----------------------------------------------------------------------------
plot_furlough_bar <- function(df, wave_code, by, min_n = 25, out_file, fig_path) {
  
  stopifnot(by %in% c("industry", "group_industry_based", "group_industry_based_detailed"))
  
  dd <- df %>%
    filter(wave == wave_code, !is.na(sempderived), sempderived > 0) %>%
    mutate(byvar = .data[[by]]) %>%
    add_count(byvar, name = "n_by_raw") %>%
    mutate(
      byvar = if_else(n_by_raw < min_n, "other", as.character(byvar)),
      furlough_cat = .furlough_cat(sempderived, furlough)
    ) %>%
    filter(!is.na(furlough_cat)) %>%
    group_by(byvar) %>%
    mutate(
      n_by = n(),
      share_yes = mean(furlough_cat == "Yes", na.rm = TRUE),
      by_lab = paste0(byvar, " (n=", n_by, ")"),
      furlough_cat = factor(furlough_cat, levels = c("Yes", "No", "Self- or not employed"))
    ) %>%
    ungroup()
  
  p <- ggplot(dd, aes(x = forcats::fct_reorder(by_lab, share_yes, .desc = TRUE),
                      fill = furlough_cat)) +
    geom_bar(position = position_fill(reverse = TRUE)) +
    coord_flip() +
    scale_y_continuous(labels = scales::percent_format()) +
    labs(fill = "Furloughed", x = NULL, y = NULL,
         title = paste0("Furlough, ", wave_full_label(wave_code))) +
    theme(legend.position = "bottom")
  
  ggsave(out_file, p, path = fig_path, width = 12, height = 8)
  p
}

# -----------------------------------------------------------------------------
# WFH distribution: percent by industry/group (employed sample)
# -----------------------------------------------------------------------------
plot_wfh_bar <- function(df, wave_code, by, min_n = 25, out_file, fig_path) {
  
  stopifnot(by %in% c("industry", "group_industry_based", "group_industry_based_detailed"))
  
  dd <- df %>%
    filter(wave == wave_code, !is.na(sempderived), sempderived > 0) %>%
    mutate(byvar = .data[[by]]) %>%
    add_count(byvar, name = "n_by_raw") %>%
    mutate(
      byvar = if_else(n_by_raw < min_n, "other", as.character(byvar)),
      wfh_cat = .wfh_cat(sempderived, wah)
    ) %>%
    filter(!is.na(wfh_cat)) %>%
    group_by(byvar) %>%
    mutate(
      n_by = n(),
      share_always = mean(wfh_cat == "Always", na.rm = TRUE),
      by_lab = paste0(byvar, " (n=", n_by, ")"),
      wfh_cat = factor(wfh_cat, levels = c("Always", "Often", "Sometimes", "Never", "Not employed"))
    ) %>%
    ungroup()
  
  p <- ggplot(dd, aes(x = forcats::fct_reorder(by_lab, share_always, .desc = FALSE),
                      fill = wfh_cat)) +
    geom_bar(position = position_fill(reverse = TRUE)) +
    coord_flip() +
    scale_y_continuous(labels = scales::percent_format()) +
    labs(fill = "Work from home", x = NULL, y = NULL,
         title = paste0("Work from home, ", wave_full_label(wave_code))) +
    theme(legend.position = "bottom")
  
  ggsave(out_file, p, path = fig_path, width = 12, height = 8)
  p
}

# -----------------------------------------------------------------------------
# Over-time: worked at all and hours (axis labels SHORT, titles FULL)
# -----------------------------------------------------------------------------
plot_overtime_worked <- function(df, by, out_file, fig_path) {
  stopifnot(by %in% c("group_industry_based", "group_industry_based_detailed"))
  
  wl <- wave_labels()
  
  dd <- df %>%
    mutate(hours = clean_hours(hours)) %>%
    filter(!is.na(hours), sempderived >= 0, !is.na(.data[[by]])) %>%
    group_by(.data[[by]], wave) %>%
    summarise(work = mean(hours > 0), .groups = "drop") %>%
    left_join(wl, by = "wave")
  
  p <- ggplot(dd, aes(x = factor(wave, levels = wl$wave, labels = wl$wave_lab_short),
                      y = work, color = .data[[by]])) +
    geom_point() +
    theme_minimal() +
    scale_y_continuous(labels = scales::percent_format()) +
    labs(color = NULL, x = NULL, y = "Worked at all last week",
         title = "Worked last week (2019–September 2021)") +
    theme(legend.position = "bottom",
          axis.text.x = element_text(angle = 90, hjust = 1))
  
  ggsave(out_file, p, path = fig_path, width = 12, height = 8)
  p
}

plot_overtime_hours <- function(df, by, out_file, fig_path) {
  stopifnot(by %in% c("group_industry_based", "group_industry_based_detailed"))
  
  wl <- wave_labels()
  
  dd <- df %>%
    mutate(hours = clean_hours(hours)) %>%
    filter(!is.na(hours), sempderived >= 0, !is.na(.data[[by]])) %>%
    mutate(hours = if_else(hours == -8, 0, hours)) %>%
    group_by(.data[[by]], wave) %>%
    summarise(work_hours = mean(hours), .groups = "drop") %>%
    left_join(wl, by = "wave")
  
  p <- ggplot(dd, aes(x = factor(wave, levels = wl$wave, labels = wl$wave_lab_short),
                      y = work_hours, color = .data[[by]], shape = .data[[by]])) +
    geom_point() +
    theme_minimal() +
    labs(color = NULL, x = NULL, y = "Hours worked last week",
         title = "Hours worked last week (2019–September 2021)") +
    theme(legend.position = "bottom",
          axis.text.x = element_text(angle = 90, hjust = 1))
  
  ggsave(out_file, p, path = fig_path, width = 12, height = 8)
  p
}

# -----------------------------------------------------------------------------
# workoutside over time (axis short labels, title full)
# -----------------------------------------------------------------------------
plot_workoutside_overtime <- function(df, by = NULL, out_file, fig_path) {
  
  wl <- wave_labels()
  
  if (is.null(by)) {
    
    dd <- df %>%
      filter(wave != "2019") %>%
      group_by(wave) %>%
      summarise(workoutside = mean(workoutside, na.rm = TRUE), .groups = "drop") %>%
      left_join(wl, by = "wave")
    
    p <- ggplot(dd, aes(x = factor(wave, levels = wl$wave, labels = wl$wave_lab_short),
                        y = workoutside)) +
      geom_point() +
      theme_minimal() +
      scale_y_continuous(labels = scales::percent_format()) +
      labs(x = NULL, y = "% Work outside last week",
           title = "Work outside (January–February 2020 to September 2021)") +
      theme(axis.text.x = element_text(angle = 90, hjust = 1))
    
  } else {
    
    stopifnot(by %in% c("group_industry_based", "group_industry_based_detailed"))
    
    dd <- df %>%
      filter(wave != "2019") %>%
      group_by(wave, .data[[by]]) %>%
      summarise(workoutside = mean(workoutside, na.rm = TRUE), .groups = "drop") %>%
      left_join(wl, by = "wave")
    
    p <- ggplot(dd, aes(x = factor(wave, levels = wl$wave, labels = wl$wave_lab_short),
                        y = workoutside, color = .data[[by]], shape = .data[[by]])) +
      geom_point() +
      theme_minimal() +
      scale_y_continuous(labels = scales::percent_format()) +
      labs(color = NULL, shape = NULL, x = NULL, y = "% Work outside last week",
           title = "Work outside (January–February 2020 to September 2021)") +
      theme(legend.position = "bottom",
            axis.text.x = element_text(angle = 90, hjust = 1))
  }
  
  ggsave(out_file, p, path = fig_path, width = 12, height = 8)
  p
}

# -----------------------------------------------------------------------------
# Over-time: wfh - bar charts by group facets over time (axis labels SHORT, titles FULL)
# -----------------------------------------------------------------------------

plot_wfh_overtime_facets <- function(df, by, out_file, fig_path) {
  
  # by must be one of the group variables
  stopifnot(by %in% c("group_industry_based", "group_industry_based_detailed"))
  
  wl <- wave_labels()
  
  dd <- df %>%
    # Match your original: drop 2019, keep employed sample
    filter(wave != "2019", !is.na(sempderived), sempderived > 0) %>%
    mutate(
      wfh_cat = case_when(
        sempderived %in% c(4) ~ "Not employed",
        wah == 1 ~ "Always",
        wah == 2 ~ "Often",
        wah == 3 ~ "Sometimes",
        wah == 4 ~ "Never",
        TRUE ~ NA_character_
      )
    ) %>%
    filter(!is.na(wfh_cat), !is.na(.data[[by]])) %>%
    left_join(wl, by = "wave") %>%
    mutate(
      # short labels for axis
      wave_lab_short = factor(wave_lab_short, levels = wl$wave_lab_short),
      # keep consistent ordering in stacks
      wfh_cat = factor(wfh_cat, levels = c("Always", "Often", "Sometimes", "Never", "Not employed"))
    )
  
  p <- ggplot(dd, aes(x = wave_lab_short, fill = wfh_cat)) +
    geom_bar(position = position_fill(reverse = TRUE)) +
    facet_grid(cols = vars(.data[[by]])) +
    scale_y_continuous(labels = scales::percent_format()) +
    labs(
      fill = "Work from home",
      x = NULL, y = NULL,
      title = "Work from home over time"
    ) +
    theme_minimal() +
    theme(
      legend.position = "bottom",
      axis.text.x = element_text(angle = 90, hjust = 1)
    )
  
  ggsave(out_file, p, path = fig_path, width = 12, height = 8)
  p
}

# -----------------------------------------------------------------------------
# Keyworker definition comparison plots
#
# Purpose:
#   Compare self-reported COVID keyworker status / sector to:
#     - baseline industry-based groups
#     - detailed baseline industry-based groups
#     - industry
#     - occupation
#
# Why this exists:
#   These are "classification diagnostic" figures rather than general
#   descriptives. They show how the self-reported COVID keyworker measures
#   line up with the baseline SIC/SOC-based group definitions used in the
#   main analysis.
#
# Supported cases:
#   - April 2020 (wave ca): self-report from `keyworker`
#   - May 2020   (wave cb): self-report from `keyworksector`
#
# Features:
#   - For industry / occupation plots:
#       * bundle small categories into "other"
#       * sort categories by descending self-reported keyworker share
#       * use horizontal bars for readability
#   - For group-based plots:
#       * keep original vertical-bar style
# -----------------------------------------------------------------------------

plot_keyworker_definition_compare <- function(
    df,
    wave_code,
    by,
    detailed_fill = FALSE,
    min_n = 25,
    order_desc = TRUE,
    out_file,
    fig_path
) {
  
  stopifnot(wave_code %in% c("ca", "cb"))
  stopifnot(by %in% c(
    "group_industry_based",
    "group_industry_based_detailed",
    "industry",
    "occupation"
  ))
  
  # Keep all positive sempderived states, including 4 = not employed.
  # This matches the original exploratory figures.
  dd <- df %>%
    dplyr::filter(wave == wave_code, sempderived > 0) %>%
    dplyr::mutate(byvar = .data[[by]])
  
  # Bundle small industries / occupations into "other"
  if (by %in% c("industry", "occupation")) {
    dd <- dd %>%
      dplyr::add_count(byvar, name = "n_by_raw") %>%
      dplyr::mutate(
        byvar = dplyr::if_else(n_by_raw < min_n, "other", as.character(byvar))
      )
  }
  
  # Helper: whether this plot should be horizontal
  is_wide_cat <- by %in% c("industry", "occupation")
  
  # Put legend at bottom for flipped plots, right otherwise
  legend_pos <- if (is_wide_cat) "bottom" else "right"
  
  # ---------------------------------------------------------------------------
  # April 2020: binary self-reported key worker status from `keyworker`
  # ---------------------------------------------------------------------------
  if (wave_code == "ca") {
    
    dd <- dd %>%
      dplyr::mutate(
        keyworker_slf = dplyr::case_when(
          keyworker == 1 ~ "Yes",
          keyworker == 2 ~ "No",
          sempderived == 4 ~ "Not employed",
          TRUE ~ "Missing"
        ),
        keyworker_slf = factor(
          keyworker_slf,
          levels = c("Yes", "No", "Not employed", "Missing")
        )
      )
    
    # Order bars by descending self-reported keyworker share
    if (order_desc) {
      dd <- dd %>%
        dplyr::group_by(byvar) %>%
        dplyr::mutate(
          share_yes = mean(keyworker_slf == "Yes", na.rm = TRUE),
          n_by = dplyr::n(),
          by_lab = if (is_wide_cat) {
            paste0(byvar, " (n=", n_by, ")")
          } else {
            as.character(byvar)
          }
        ) %>%
        dplyr::ungroup() %>%
        dplyr::mutate(
          by_lab = forcats::fct_reorder(by_lab, share_yes, .desc = TRUE)
        )
    } else {
      dd <- dd %>%
        dplyr::group_by(byvar) %>%
        dplyr::mutate(
          n_by = dplyr::n(),
          by_lab = if (is_wide_cat) {
            paste0(byvar, " (n=", n_by, ")")
          } else {
            as.character(byvar)
          }
        ) %>%
        dplyr::ungroup()
    }
    
    p <- ggplot(dd, aes(x = by_lab, fill = keyworker_slf)) +
      geom_bar(position = position_fill(reverse = TRUE)) +
      scale_y_continuous(labels = scales::percent_format()) +
      labs(
        fill = "Self reported key worker status",
        x = NULL, y = NULL,
        title = paste0(
          dplyr::case_when(
            by == "group_industry_based" ~ "Industry based groups and keyworker status, ",
            by == "group_industry_based_detailed" ~ "Industry based detailed groups and keyworker status, ",
            by == "industry" ~ "Industries and keyworker status, ",
            by == "occupation" ~ "Occupations and keyworker status, "
          ),
          wave_full_label(wave_code)
        )
      ) +
      theme_minimal() +
      theme(
        legend.position = legend_pos,
        axis.text.x = if (is_wide_cat) element_blank() else element_text(angle = 90, hjust = 1),
        axis.text.y = if (is_wide_cat) element_text(size = 9) else element_text()
      )
    
    if (is_wide_cat) {
      p <- p + coord_flip()
    }
    
    ggsave(
      out_file, p, path = fig_path,
      width = if (is_wide_cat) 14 else 12,
      height = if (is_wide_cat) 10 else 8
    )
    return(p)
  }
  
  # ---------------------------------------------------------------------------
  # May 2020: binary self-reported key worker status from `keyworksector`
  # ---------------------------------------------------------------------------
  if (!detailed_fill) {
    
    dd <- dd %>%
      dplyr::mutate(
        keyworker_slf = dplyr::case_when(
          keyworksector %in% 1:8 ~ "Yes",
          keyworksector == 9 ~ "No",
          sempderived == 4 ~ "Not employed",
          TRUE ~ "Missing"
        ),
        keyworker_slf = factor(
          keyworker_slf,
          levels = c("Yes", "No", "Not employed", "Missing")
        )
      )
    
    if (order_desc) {
      dd <- dd %>%
        dplyr::group_by(byvar) %>%
        dplyr::mutate(
          share_yes = mean(keyworker_slf == "Yes", na.rm = TRUE),
          n_by = dplyr::n(),
          by_lab = if (is_wide_cat) {
            paste0(byvar, " (n=", n_by, ")")
          } else {
            as.character(byvar)
          }
        ) %>%
        dplyr::ungroup() %>%
        dplyr::mutate(
          by_lab = forcats::fct_reorder(by_lab, share_yes, .desc = TRUE)
        )
    } else {
      dd <- dd %>%
        dplyr::group_by(byvar) %>%
        dplyr::mutate(
          n_by = dplyr::n(),
          by_lab = if (is_wide_cat) {
            paste0(byvar, " (n=", n_by, ")")
          } else {
            as.character(byvar)
          }
        ) %>%
        dplyr::ungroup()
    }
    
    p <- ggplot(dd, aes(x = by_lab, fill = keyworker_slf)) +
      geom_bar(position = position_fill(reverse = TRUE)) +
      scale_y_continuous(labels = scales::percent_format()) +
      labs(
        fill = "Self reported key worker status",
        x = NULL, y = NULL,
        title = paste0(
          dplyr::case_when(
            by == "group_industry_based" ~ "Industry based groups and keyworker status, ",
            by == "group_industry_based_detailed" ~ "Industry based detailed groups and keyworker status, ",
            by == "industry" ~ "Industries and keyworker status, ",
            by == "occupation" ~ "Occupations and keyworker status, "
          ),
          wave_full_label(wave_code)
        )
      ) +
      theme_minimal() +
      theme(
        legend.position = legend_pos,
        axis.text.x = if (is_wide_cat) element_blank() else element_text(angle = 90,hjust = 1),
        axis.text.y = if (is_wide_cat) element_text(size = 9) else element_text()
      )
    
    if (is_wide_cat) {
      p <- p + coord_flip()
    }
    
    ggsave(
      out_file, p, path = fig_path,
      width = if (is_wide_cat) 14 else 12,
      height = if (is_wide_cat) 10 else 8
    )
    return(p)
  }
  
  # ---------------------------------------------------------------------------
  # May 2020: detailed self-reported keywork sector from `keyworksector`
  # ---------------------------------------------------------------------------
  dd <- dd %>%
    dplyr::mutate(
      keyworker_slf = dplyr::case_when(
        keyworksector == 1 ~ "Health and social care",
        keyworksector == 2 ~ "Education and childcare",
        keyworksector == 3 ~ "Key public services",
        keyworksector == 4 ~ "Local and national government",
        keyworksector == 5 ~ "Food and other necessary goods",
        keyworksector == 6 ~ "Public safety and national security",
        keyworksector == 7 ~ "Transport",
        keyworksector == 8 ~ "Utilities, communications and financial services",
        keyworksector == 9 ~ "Not key worker",
        sempderived == 4 ~ "Not employed",
        TRUE ~ "Missing"
      ),
      keyworker_slf = factor(
        keyworker_slf,
        levels = c(
          "Health and social care",
          "Education and childcare",
          "Key public services",
          "Local and national government",
          "Food and other necessary goods",
          "Public safety and national security",
          "Transport",
          "Utilities, communications and financial services",
          "Not key worker",
          "Not employed",
          "Missing"
        )
      )
    )
  
  # For the detailed fill version, order by the share who are in any
  # positive key-worker sector (rather than "Not key worker" / "Missing").
  if (order_desc) {
    dd <- dd %>%
      dplyr::group_by(byvar) %>%
      dplyr::mutate(
        share_yes = mean(!(keyworker_slf %in% c("Not key worker", "Not employed", "Missing")), na.rm = TRUE),
        n_by = dplyr::n(),
        by_lab = if (is_wide_cat) {
          paste0(byvar, " (n=", n_by, ")")
        } else {
          as.character(byvar)
        }
      ) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(
        by_lab = forcats::fct_reorder(by_lab, share_yes, .desc = TRUE)
      )
  } else {
    dd <- dd %>%
      dplyr::group_by(byvar) %>%
      dplyr::mutate(
        n_by = dplyr::n(),
        by_lab = if (is_wide_cat) {
          paste0(byvar, " (n=", n_by, ")")
        } else {
          as.character(byvar)
        }
      ) %>%
      dplyr::ungroup()
  }
  
  p <- ggplot(dd, aes(x = by_lab, fill = keyworker_slf)) +
    geom_bar(position = position_fill(reverse = TRUE)) +
    scale_y_continuous(labels = scales::percent_format()) +
    labs(
      fill = "Self reported key work sector",
      x = NULL, y = NULL,
      title = paste0(
        dplyr::case_when(
          by == "group_industry_based" ~ "Industry based groups and keywork sector, ",
          by == "group_industry_based_detailed" ~ "Industry based detailed groups and keywork sector, ",
          by == "industry" ~ "Industries and keywork sector, ",
          by == "occupation" ~ "Occupations and keywork sector, "
        ),
        wave_full_label(wave_code)
      )
    ) +
    theme_minimal() +
    theme(
      legend.position = legend_pos,
      axis.text.x = if (is_wide_cat) element_blank() else element_text(angle = 90, hjust = 1),
      axis.text.y = if (is_wide_cat) element_text(size = 9) else element_text()
    )
  
  if (is_wide_cat) {
    p <- p + coord_flip()
  }
  
  ggsave(
    out_file, p, path = fig_path,
    width = if (is_wide_cat) 14 else 12,
    height = if (is_wide_cat) 10 else 8
  )
  p
}