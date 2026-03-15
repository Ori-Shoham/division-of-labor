# =============================================================================
# Script: code/run/00_master.R
#
# Purpose:
#   Master runner that executes each stage in its own isolated environment
#   so large objects created in earlier stages do NOT remain in memory.
#
# How it works:
#   - Each stage is sourced with: local = new.env(parent = emptyenv())
#   - This prevents carry-over of large datasets between scripts.
#   - Each stage still reads/writes from disk (derived/, figures/, tables/).
#
# How to use:
#   source("code/run/00_master.R")
#
# Notes:
#   - Each stage must source code/lib/config.R (and any other libs) itself.
#   - If a stage needs outputs from previous stages, it should read them
#     from derived/ (RDS) rather than relying on in-memory objects.
# =============================================================================

# ---- Toggle what to run -------------------------------------------------------
RUN_CHECK_INPUTS    <- TRUE
RUN_BUILD_DATA      <- TRUE
RUN_DESCRIPTIVES    <- TRUE
RUN_FUTURE_DESCRIPTIVES <- TRUE
RUN_MODELS          <- TRUE
RUN_LASSO           <- TRUE
RUN_SESSION_INFO    <- TRUE

# Optional: stop after each stage for debugging
STOP_AFTER_EACH     <- TRUE

# =============================================================================
# Helper: run a stage in a fresh environment
# =============================================================================
run_stage <- function(script_path) {
  message("\n--- Running: ", script_path, " ---")
  
  # new.env with empty parent means: no accidental access to global objects
  e <- new.env(parent = globalenv())
  
  # Give the stage the minimal base packages it needs to run `source`, `library`, etc.
  # (base is always there; we add utils explicitly for safety)
  # e$`%>%` <- NULL  # avoid accidental reliance on magrittr from global env
  
  # Source in isolated env
  source(script_path, local = e)
  
  # Drop the environment (and everything created inside)
  rm(e)
  invisible(gc())
}

# =============================================================================
# Stage 0: Input checks
# =============================================================================
if (RUN_CHECK_INPUTS) {
  message("\n==================== Stage 0: Check inputs ====================")
  run_stage("code/run/00_check_inputs.R")
  if (STOP_AFTER_EACH) stop("Stopped after Stage 0 (as requested).")
}

# =============================================================================
# Stage 1: Build derived data
# =============================================================================
if (RUN_BUILD_DATA) {
  message("\n==================== Stage 1: Build data =======================")
  run_stage("code/run/01_build_data.R")
  if (STOP_AFTER_EACH) stop("Stopped after Stage 1 (as requested).")
}

# =============================================================================
# Stage 2: Descriptive figures
# =============================================================================
if (RUN_DESCRIPTIVES) {
  message("\n==================== Stage 2: Descriptives =====================")
  run_stage("code/run/02_make_descriptives.R")
  if (STOP_AFTER_EACH) stop("Stopped after Stage 2 (as requested).")
}

if (RUN_FUTURE_DESCRIPTIVES) {
  message("\n==================== Stage 2b: Future descriptives =============")
  run_stage("code/run/02b_make_future_descriptives.R")
  if (STOP_AFTER_EACH) stop("Stopped after Stage 2b (as requested).")
}

# =============================================================================
# Stage 3: Regression tables
# =============================================================================
if (RUN_MODELS) {
  message("\n==================== Stage 3: Models ===========================")
  run_stage("code/run/03_models_workoutside.R")
  if (STOP_AFTER_EACH) stop("Stopped after Stage 3 (as requested).")
}

# =============================================================================
# Stage 4: Lasso exploration
# =============================================================================
if (RUN_LASSO) {
  message("\n==================== Stage 4: Lasso ============================")
  run_stage("code/run/04_lasso_workoutside.R")
  if (STOP_AFTER_EACH) stop("Stopped after Stage 4 (as requested).")
}

# =============================================================================
# Stage 5: Reproducibility metadata
# =============================================================================
if (RUN_SESSION_INFO) {
  message("\n==================== Stage 5: Session info =====================")
  run_stage("code/run/99_session_info.R")
  if (STOP_AFTER_EACH) stop("Stopped after Stage 5 (as requested).")
}

message("\n==================== ALL DONE ====================\n")