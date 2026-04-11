# =============================================================================
# Script: code/run/04_lasso_workoutside.R
#
# Purpose:
#   Lasso logistic regression to identify industries/occupations (and interactions)
#   that predict working outside in April 2020 (wave ca).
#
# Inputs:
#   derived/df_sample_long_covid.rds
#
# Outputs:
#   tables/lasso_selected_terms.csv
#   tables/lasso_industry_effects.csv
#   tables/lasso_occupation_effects.csv
#   tables/lasso_top_cells.csv
# =============================================================================

suppressPackageStartupMessages({
  library(tidyverse)
  library(glmnet)
  library(forcats)
})

rm(list = ls())

source("code/lib/config.R")

df <- readRDS(file.path(der_path, "df_sample_long_covid.rds"))

# ---- Prep data ----------------------------------------------------------------
# Coverage threshold for factor levels
min_n <- 20

df_lasso <- df %>%
  filter(wave == "ca",
         !is.na(industry), !is.na(occupation), !is.na(workoutside)) %>%
  mutate(
    industry   = fct_lump_min(factor(industry),   min = min_n, other_level = "OTHER"),
    occupation = fct_lump_min(factor(occupation), min = min_n, other_level = "OTHER"),
    y = as.integer(workoutside)   # 0/1
  )

# ---- Design matrix with interactions ------------------------------------------
X <- model.matrix(~ industry * occupation, data = df_lasso)[, -1]
y <- df_lasso$y

# ---- Cross-validated lasso logistic regression --------------------------------
set.seed(123)
cvfit <- cv.glmnet(
  x = X, y = y,
  alpha = 1,
  nfolds = 10,
  family = "binomial",
  type.measure = "deviance",
  standardize = TRUE
)

# ---- Choose lambda (tolerance rule around best CV deviance) -------------------
cvm <- cvfit$cvm
lam <- cvfit$lambda
best <- min(cvm)
tol  <- 0.02
idx <- which(cvm <= best * (1 + tol))
lam_tol <- max(lam[idx])

# ---- Extract selected variables -----------------------------------------------
coef_sel <- coef(cvfit, s = lam_tol)
nz_idx <- which(as.numeric(coef_sel) != 0)
nz_names <- rownames(coef_sel)[nz_idx]
nz_vals  <- as.numeric(coef_sel)[nz_idx]

selected <- tibble(term = nz_names, coef = nz_vals) %>%
  arrange(desc(abs(coef))) %>%
  mutate(
    term = str_replace_all(term, c("industry" = "Industry - ", "occupation" = "Occupation - "))
  )

write_csv(selected, file.path(tab_path, "lasso_selected_terms.csv"))

# ---- Predicted probabilities --------------------------------------------------
p_hat <- as.numeric(predict(cvfit, newx = X, s = lam_tol, type = "response"))

overall_rate <- mean(df_lasso$y)

industry_effects <- df_lasso %>%
  mutate(p_hat = p_hat) %>%
  group_by(industry) %>%
  summarise(
    n = n(),
    mean_y = mean(y),
    mean_p = mean(p_hat),
    .groups = "drop"
  ) %>%
  mutate(
    diff_p = mean_p - overall_rate,
    diff_y = mean_y - overall_rate,
    score_p = abs(diff_p) * sqrt(n),
    score_y = abs(diff_y) * sqrt(n)
  ) %>%
  arrange(desc(score_y))

occupation_effects <- df_lasso %>%
  mutate(p_hat = p_hat) %>%
  group_by(occupation) %>%
  summarise(
    n = n(),
    mean_y = mean(y),
    mean_p = mean(p_hat),
    .groups = "drop"
  ) %>%
  mutate(
    diff_p = mean_p - overall_rate,
    diff_y = mean_y - overall_rate,
    score_p = abs(diff_p) * sqrt(n),
    score_y = abs(diff_y) * sqrt(n)
  ) %>%
  arrange(desc(score_y))

write_csv(industry_effects,   file.path(tab_path, "lasso_industry_effects.csv"))
write_csv(occupation_effects, file.path(tab_path, "lasso_occupation_effects.csv"))

# ---- Rank cells (industry x occupation) by non-additive deviation -------------
cell_rank <- df_lasso %>%
  mutate(p_hat = p_hat) %>%
  group_by(industry) %>%
  mutate(mean_ind_y = mean(y), mean_ind_p = mean(p_hat)) %>%
  ungroup() %>%
  group_by(occupation) %>%
  mutate(mean_occ_y = mean(y), mean_occ_p = mean(p_hat)) %>%
  ungroup() %>%
  group_by(industry, occupation) %>%
  summarise(
    n_cell = n(),
    mean_y = mean(y),
    mean_p = mean(p_hat),
    mean_ind_y = mean(mean_ind_y),
    mean_ind_p = mean(mean_ind_p),
    mean_occ_y = mean(mean_occ_y),
    mean_occ_p = mean(mean_occ_p),
    .groups = "drop"
  ) %>%
  mutate(
    diff_y_ind = mean_y - mean_ind_y,
    diff_y_occ = mean_y - mean_occ_y,
    score_y_ind = abs(diff_y_ind) * sqrt(n_cell),
    score_y_occ = abs(diff_y_occ) * sqrt(n_cell),
    score_total = score_y_ind + score_y_occ
  ) %>%
  arrange(desc(score_total)) %>%
  slice_head(n = 30)

write_csv(cell_rank, file.path(tab_path, "lasso_top_cells.csv"))

cat("\nSaved LASSO outputs to tables/:\n")
cat(" - lasso_selected_terms.csv\n")
cat(" - lasso_industry_effects.csv\n")
cat(" - lasso_occupation_effects.csv\n")
cat(" - lasso_top_cells.csv\n")