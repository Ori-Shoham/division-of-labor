# =============================================================================
# Script: code/run/03_models_workoutside.R
#
# Purpose:
#   Replicate your OLS models predicting workoutside in April 2020 (wave ca)
#   and output a LaTeX regression table via modelsummary.
#
# Inputs:
#   derived/df_sample_long_covid.rds
#
# Output:
#   tables/workoutside_industry_comparison.tex
# =============================================================================

suppressPackageStartupMessages({
  library(tidyverse)
  library(modelsummary)
})

rm(list = ls())

source("code/lib/config.R")

df <- readRDS(file.path(der_path, "df_sample_long_covid.rds")) %>%
  mutate(group_industry_based_detailed = relevel(factor(group_industry_based_detailed), ref = "other"))

# Models (April 2020 = ca)
mod_a <- lm(workoutside ~ factor(base_jbsic07_cc) + factor(base_jbsoc10_cc),
            data = df %>% filter(wave == "ca"))

mod_b <- lm(workoutside ~ factor(group_industry_based_detailed),
            data = df %>% filter(wave == "ca"))

mod_c <- lm(workoutside ~ factor(group_industry_based_detailed) +
              base_age_dv + I(base_age_dv^2) + I(base_age_dv^3) +
              factor(base_isced11_dv),
            data = df %>% filter(wave == "ca"))

mod_d <- lm(workoutside ~ factor(group_industry_based_detailed) +
              base_age_dv + I(base_age_dv^2) + I(base_age_dv^3) +
              factor(base_isced11_dv),
            data = df %>% filter(wave == "ca" & base_sex == 1))

mod_e <- lm(workoutside ~ factor(group_industry_based_detailed) +
              base_age_dv + I(base_age_dv^2) + I(base_age_dv^3) +
              factor(base_isced11_dv),
            data = df %>% filter(wave == "ca" & base_sex == 2))

models <- list("A" = mod_a, "B" = mod_b, "C" = mod_c, "D" = mod_d, "E" = mod_e)

coef_map <- c(
  "(Intercept)" = "Intercept",
  "factor(group_industry_based_detailed)key worker - education" = "Key – education",
  "factor(group_industry_based_detailed)key worker - health and social services" = "Key – health",
  "factor(group_industry_based_detailed)key worker - public safety" = "Key – public safety",
  "factor(group_industry_based_detailed)shutdown sector" = "Shutdown sector"
)

add_rows <- tibble(
  term = c("Industry FE", "Occupation FE", "Age controls", "Education controls", "Sample"),
  A = c("✓", "✓", "", "", "All"),
  B = c("", "", "", "", "All"),
  C = c("", "", "✓", "✓", "All"),
  D = c("", "", "✓", "✓", "Males"),
  E = c("", "", "✓", "✓", "Females")
) %>%
  mutate(across(A:E, \(x) ifelse(x == "✓", "$\\checkmark$", x)))

options(modelsummary_factory_latex = "kableExtra")

modelsummary(
  models,
  vcov = "HC1",
  coef_map = coef_map,
  estimate  = "{estimate}{stars}",
  statistic = "({std.error})",
  stars = c("*" = .10, "**" = .05, "***" = .01),
  gof_map = c("nobs", "r.squared"),
  add_rows = add_rows,
  escape = FALSE,
  title = "\\textit{Workoutside} in April 2020",
  notes = c(
    "Robust standard errors in parentheses.",
    "* p < 0.1, ** p < 0.05, *** p < 0.01."
  ),
  output = file.path(tab_path, "workoutside_industry_comparison.tex")
)

cat("\nSaved tables/workoutside_industry_comparison.tex\n")