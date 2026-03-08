# =============================================================================
# Script: code/run/00_master.R
#
# Purpose:
#   One-click pipeline runner:
#     1) Check inputs and folder structure
#     2) Build derived datasets (baseline, COVID panel, future L–O, samples)
#     3) Produce descriptives (figures)
#     4) Run models + export tables
#     5) Run lasso exploration outputs
#     6) Save session info
#
# How to use:
#   source("code/run/00_master.R")
#
# Notes:
#   - Assumes code/lib/*.R exist and are up to date.
#   - Will stop early if 00_check_inputs fails.
# =============================================================================

# ---- Toggle what to run -------------------------------------------------------
RUN_CHECK_INPUTS    <- TRUE
RUN_BUILD_DATA      <- TRUE
RUN_DESCRIPTIVES    <- TRUE
RUN_MODELS          <- TRUE
RUN_LASSO           <- TRUE
RUN_SESSION_INFO    <- TRUE

# Optional: if you want to stop after each stage manually, set TRUE
STOP_AFTER_EACH     <- TRUE

# =============================================================================
# Stage 0: Input checks
# =============================================================================

if (RUN_CHECK_INPUTS) {
  message("\n==================== Stage 0: Check inputs ====================")
  source("code/run/00_check_inputs.R")
  if (STOP_AFTER_EACH) stop("Stopped after Stage 0 (as requested).")
}

# =============================================================================
# Stage 1: Build derived data
# =============================================================================

if (RUN_BUILD_DATA) {
  message("\n==================== Stage 1: Build data =======================")
  source("code/run/01_build_data.R")
  if (STOP_AFTER_EACH) stop("Stopped after Stage 1 (as requested).")
}

# =============================================================================
# Stage 2: Descriptive figures
# =============================================================================

if (RUN_DESCRIPTIVES) {
  message("\n==================== Stage 2: Descriptives =====================")
  source("code/run/02_make_descriptives.R")
  if (STOP_AFTER_EACH) stop("Stopped after Stage 2 (as requested).")
}

# =============================================================================
# Stage 3: Main regression tables
# =============================================================================

if (RUN_MODELS) {
  message("\n==================== Stage 3: Models ===========================")
  source("code/run/03_models_workoutside.R")
  if (STOP_AFTER_EACH) stop("Stopped after Stage 3 (as requested).")
}

# =============================================================================
# Stage 4: Lasso exploration
# =============================================================================

if (RUN_LASSO) {
  message("\n==================== Stage 4: Lasso ============================")
  source("code/run/04_lasso_workoutside.R")
  if (STOP_AFTER_EACH) stop("Stopped after Stage 4 (as requested).")
}

# =============================================================================
# Stage 5: Reproducibility metadata
# =============================================================================

if (RUN_SESSION_INFO) {
  message("\n==================== Stage 5: Session info =====================")
  source("code/run/99_session_info.R")
  if (STOP_AFTER_EACH) stop("Stopped after Stage 5 (as requested).")
}

message("\n==================== ALL DONE ====================\n")