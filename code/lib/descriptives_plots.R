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
#   - Uses df_sample_long_covid from derived/.
#   - Expects columns:
#       wave, industry (string), occupation (string) from SOC/SIC joins,
#       group_industry_based, group_industry_based_detailed,
#       hours, sempderived, furlough, wah, workoutside
# =============================================================================

wave_labels <- function() {
  tibble(
    wave = c("2019", "baseline", "ca", "cb", "cc", "cd", "ce", "cf", "cg", "ch", "ci"),
    wave_lab = c("2019", "Jan-Feb 20", "Apr 20", "May 20", "Jun 20", "Jul 20",
                 "Sep 20", "Nov 20", "Jan 21", "Mar 21", "Sep 21")
  )
}

clean_hours <- function(x) {
  # Remove common missing codes seen in COVID extracts
  x[x %in% c(-9, -2, -1)] <- NA
  x
}

plot_worked_at_all_bar <- function(df, wave_code, by, min_n = 25, perc = FALSE, out_file, fig_path) {
  
  # by is one of: "industry", "group_industry_based", "group_industry_based_detailed"
  stopifnot(by %in% c("industry", "group_industry_based", "group_industry_based_detailed"))
  
  dd <- df %>%
    filter(wave == wave_code, sempderived >= 0) %>%
    mutate(hours = clean_hours(hours)) %>%
    filter(!is.na(hours)) %>%
    mutate(byvar = .data[[by]]) %>%
    add_count(byvar) %>%
    mutate(byvar = if_else(n < min_n, "other", as.character(byvar)))
  
  if (!perc) {
    p <- ggplot(dd, aes(x = forcats::fct_infreq(byvar),
                        fill = factor(hours > 0, levels = c(TRUE, FALSE),
                                      labels = c("Yes", "No")))) +
      geom_bar() +
      coord_flip() +
      labs(fill = "Worked at all last week", x = NULL,
           title = paste0("Worked last week, ", wave_code)) +
      theme(legend.position = "bottom")
  } else {
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
           title = paste0("Worked last week, ", wave_code)) +
      theme(legend.position = "bottom")
  }
  
  ggsave(out_file, p, path = fig_path, width = 12, height = 8)
  p
}

plot_work_status_bar <- function(df, wave_code, by, min_n = 25, perc = FALSE, out_file, fig_path) {
  
  stopifnot(by %in% c("industry", "group_industry_based", "group_industry_based_detailed"))
  
  dd <- df %>%
    filter(wave == wave_code, !is.na(sempderived)) %>%
    mutate(
      byvar = .data[[by]]
    ) %>%
    add_count(byvar) %>%
    mutate(
      byvar = if_else(n < min_n, "other", as.character(byvar)),
      status = case_when(
        sempderived < 0 ~ "Missing",
        sempderived == 1 ~ "Employed",
        sempderived == 2 ~ "Self-employed",
        sempderived == 3 ~ "Both employed and self-employed",
        TRUE ~ "Not employed"
      )
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
           title = paste0("Work status, ", wave_code)) +
      theme(legend.position = "bottom")
  } else {
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
           title = paste0("Work status, ", wave_code)) +
      theme(legend.position = "bottom")
  }
  
  ggsave(out_file, p, path = fig_path, width = 14, height = 8)
  p
}

plot_furlough_bar <- function(df, wave_code, by, min_n = 25, out_file, fig_path) {
  
  stopifnot(by %in% c("industry", "group_industry_based", "group_industry_based_detailed"))
  
  dd <- df %>%
    filter(wave == wave_code, !is.na(sempderived), sempderived > 0) %>%
    mutate(byvar = .data[[by]]) %>%
    add_count(byvar) %>%
    mutate(
      byvar = if_else(n < min_n, "other", as.character(byvar)),
      furlough_cat = case_when(
        sempderived %in% c(2, 4) ~ "Self- or not employed",
        furlough == 1 ~ "Yes",
        furlough == 2 ~ "No",
        TRUE ~ NA_character_
      )
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
         title = paste0("Furlough, ", wave_code)) +
    theme(legend.position = "bottom")
  
  ggsave(out_file, p, path = fig_path, width = 12, height = 8)
  p
}

plot_wfh_bar <- function(df, wave_code, by, min_n = 25, out_file, fig_path) {
  
  stopifnot(by %in% c("industry", "group_industry_based", "group_industry_based_detailed"))
  
  dd <- df %>%
    filter(wave == wave_code, !is.na(sempderived), sempderived > 0) %>%
    mutate(byvar = .data[[by]]) %>%
    add_count(byvar) %>%
    mutate(
      byvar = if_else(n < min_n, "other", as.character(byvar)),
      wfh_cat = case_when(
        sempderived %in% c(4) ~ "Not employed",
        wah == 1 ~ "Always",
        wah == 2 ~ "Often",
        wah == 3 ~ "Sometimes",
        wah == 4 ~ "Never",
        TRUE ~ NA_character_
      )
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
         title = paste0("Work from home, ", wave_code)) +
    theme(legend.position = "bottom")
  
  ggsave(out_file, p, path = fig_path, width = 12, height = 8)
  p
}

plot_overtime_worked <- function(df, by, out_file, fig_path) {
  stopifnot(by %in% c("group_industry_based", "group_industry_based_detailed"))
  
  wl <- wave_labels()
  
  dd <- df %>%
    mutate(hours = clean_hours(hours)) %>%
    filter(!is.na(hours), sempderived >= 0, !is.na(.data[[by]])) %>%
    group_by(.data[[by]], wave) %>%
    summarise(work = mean(hours > 0), .groups = "drop") %>%
    left_join(wl, by = "wave")
  
  p <- ggplot(dd, aes(x = factor(wave, levels = wl$wave, labels = wl$wave_lab),
                      y = work, color = .data[[by]])) +
    geom_point() +
    theme_minimal() +
    scale_y_continuous(labels = scales::percent_format()) +
    labs(color = NULL, x = NULL, y = "Worked at all last week",
         title = "Worked last week") +
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
  
  p <- ggplot(dd, aes(x = factor(wave, levels = wl$wave, labels = wl$wave_lab),
                      y = work_hours, color = .data[[by]])) +
    geom_point() +
    theme_minimal() +
    labs(color = NULL, x = NULL, y = "Hours worked last week",
         title = "Hours worked last week") +
    theme(legend.position = "bottom",
          axis.text.x = element_text(angle = 90, hjust = 1))
  
  ggsave(out_file, p, path = fig_path, width = 12, height = 8)
  p
}

plot_workoutside_overtime <- function(df, by = NULL, out_file, fig_path) {
  
  wl <- wave_labels()
  
  if (is.null(by)) {
    dd <- df %>%
      filter(wave != "2019") %>%
      group_by(wave) %>%
      summarise(workoutside = mean(workoutside, na.rm = TRUE), .groups = "drop") %>%
      left_join(wl, by = "wave")
    
    p <- ggplot(dd, aes(x = factor(wave, levels = wl$wave, labels = wl$wave_lab),
                        y = workoutside)) +
      geom_point() +
      theme_minimal() +
      scale_y_continuous(labels = scales::percent_format()) +
      labs(x = NULL, y = "% Work outside last week", title = "Work outside") +
      theme(axis.text.x = element_text(angle = 90, hjust = 1))
    
  } else {
    stopifnot(by %in% c("group_industry_based", "group_industry_based_detailed"))
    
    dd <- df %>%
      filter(wave != "2019") %>%
      group_by(wave, .data[[by]]) %>%
      summarise(workoutside = mean(workoutside, na.rm = TRUE), .groups = "drop") %>%
      left_join(wl, by = "wave")
    
    p <- ggplot(dd, aes(x = factor(wave, levels = wl$wave, labels = wl$wave_lab),
                        y = workoutside, color = .data[[by]], shape = .data[[by]])) +
      geom_point() +
      theme_minimal() +
      scale_y_continuous(labels = scales::percent_format()) +
      labs(color = NULL, shape = NULL, x = NULL, y = "% Work outside last week",
           title = "Work outside") +
      theme(legend.position = "bottom",
            axis.text.x = element_text(angle = 90, hjust = 1))
  }
  
  ggsave(out_file, p, path = fig_path, width = 12, height = 8)
  p
}