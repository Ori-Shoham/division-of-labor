# =============================================================================
# File: code/lib/future_descriptives_plots.R
#
# Purpose:
#   Automatic descriptive plots for ALL future outcomes in future_outcomes_long.
#   - Numeric outcomes: mean over time
#   - Categorical outcomes: share distribution over time
# =============================================================================

suppressPackageStartupMessages({
  library(tidyverse)
  library(scales)
})

# --- helpers ------------------------------------------------------------------

.safe_name <- function(x) {
  x %>%
    stringr::str_replace_all("[^A-Za-z0-9_\\-]+", "_") %>%
    stringr::str_replace_all("_+", "_") %>%
    stringr::str_replace_all("^_|_$", "")
}

.wave_order <- function(df) {
  # Keep wave order as it appears in config future_waves typically (l, m, n),
  # but fall back to sorted unique.
  w <- unique(df$wave)
  w <- w[!is.na(w)]
  if (length(w) == 0) return(character(0))
  # if letters, sort; if factor, preserve levels; else unique order
  if (is.factor(df$wave)) levels(df$wave) else sort(w)
}

.is_binary_01 <- function(x) {
  ux <- sort(unique(x[!is.na(x)]))
  length(ux) <= 2 && all(ux %in% c(0,1))
}

# --- plotting primitives -------------------------------------------------------

plot_future_numeric_mean <- function(df, var, out_file, fig_path) {
  stopifnot(var %in% names(df))
  w_order <- .wave_order(df)
  
  dd <- df %>%
    filter(!is.na(wave)) %>%
    mutate(wave = factor(wave, levels = w_order)) %>%
    filter(!is.na(.data[[var]])) %>%
    group_by(wave) %>%
    summarise(
      mean_y = mean(.data[[var]], na.rm = TRUE),
      n = n(),
      .groups = "drop"
    )
  
  p <- ggplot(dd, aes(x = wave, y = mean_y)) +
    geom_point() +
    theme_minimal() +
    labs(
      x = NULL,
      y = var,
      title = paste0(var, " (mean over time)")
    ) +
    theme(axis.text.x = element_text(angle = 90, hjust = 1))
  
  ggsave(out_file, p, path = fig_path, width = 10, height = 6)
  p
}

plot_future_binary_share <- function(df, var, out_file, fig_path) {
  stopifnot(var %in% names(df))
  w_order <- .wave_order(df)
  
  dd <- df %>%
    filter(!is.na(wave)) %>%
    mutate(wave = factor(wave, levels = w_order)) %>%
    filter(!is.na(.data[[var]])) %>%
    group_by(wave) %>%
    summarise(
      share_1 = mean(.data[[var]] == 1, na.rm = TRUE),
      n = n(),
      .groups = "drop"
    )
  
  p <- ggplot(dd, aes(x = wave, y = share_1)) +
    geom_point() +
    theme_minimal() +
    scale_y_continuous(labels = percent_format()) +
    labs(
      x = NULL,
      y = paste0("Share ", var, " = 1"),
      title = paste0(var, " (share==1 over time)")
    ) +
    theme(axis.text.x = element_text(angle = 90, hjust = 1))
  
  ggsave(out_file, p, path = fig_path, width = 10, height = 6)
  p
}

plot_future_categorical_dist <- function(df, var, out_file, fig_path, max_levels = 12) {
  stopifnot(var %in% names(df))
  w_order <- .wave_order(df)
  
  dd <- df %>%
    filter(!is.na(wave), !is.na(.data[[var]])) %>%
    mutate(
      wave = factor(wave, levels = w_order),
      cat = as.character(.data[[var]])
    ) %>%
    group_by(wave) %>%
    mutate(
      # lump rare categories within each wave based on global frequency
      cat = fct_lump_n(factor(cat), n = max_levels, other_level = "OTHER")
    ) %>%
    ungroup() %>%
    count(wave, cat) %>%
    group_by(wave) %>%
    mutate(share = n / sum(n)) %>%
    ungroup()
  
  p <- ggplot(dd, aes(x = wave, y = share, fill = cat)) +
    geom_col(position = "fill") +
    theme_minimal() +
    scale_y_continuous(labels = percent_format()) +
    labs(
      x = NULL,
      y = NULL,
      fill = NULL,
      title = paste0(var, " (distribution over time)")
    ) +
    theme(
      axis.text.x = element_text(angle = 90, hjust = 1),
      legend.position = "bottom"
    )
  
  ggsave(out_file, p, path = fig_path, width = 11, height = 6)
  p
}

# --- main driver: plot EVERYTHING ---------------------------------------------

plot_all_future_outcomes <- function(df, prefix, fig_path,
                                     exclude = c(
                                       "pidp", "hidp", "wave",
                                       # labels and ids that aren’t outcomes
                                       "industry", "occupation",
                                       "partner_pidp", "partner_rel", "family_id",
                                       "base_partner_pidp", "base_partner_rel", "base_family_id",
                                       "source_wave"
                                     )) {
  
  dir.create(fig_path, showWarnings = FALSE, recursive = TRUE)
  
  vars <- setdiff(names(df), exclude)
  
  # Drop columns that are all NA
  vars <- vars[vars %in% names(df) & !vapply(df[vars], \(x) all(is.na(x)), logical(1))]
  
  message("Future outcomes to plot (n=", length(vars), "):")
  message(paste(vars, collapse = ", "))
  
  for (v in vars) {
    out_stub <- paste0(prefix, "_", .safe_name(v))
    
    x <- df[[v]]
    
    # Decide plot type:
    # - binary 0/1 -> share
    # - numeric -> mean
    # - otherwise -> distribution
    if (is.numeric(x) && .is_binary_01(x)) {
      plot_future_binary_share(
        df = df, var = v,
        out_file = paste0(out_stub, "_share.png"),
        fig_path = fig_path
      )
    } else if (is.numeric(x)) {
      plot_future_numeric_mean(
        df = df, var = v,
        out_file = paste0(out_stub, "_mean.png"),
        fig_path = fig_path
      )
    } else {
      plot_future_categorical_dist(
        df = df, var = v,
        out_file = paste0(out_stub, "_dist.png"),
        fig_path = fig_path
      )
    }
  }
  
  invisible(TRUE)
}