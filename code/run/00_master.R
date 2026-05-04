# =============================================================================
# Script: code/run/00_master.R
#
# Purpose:
#   Master runner that executes each stage in its own isolated environment
#   so large objects created in earlier stages do NOT remain in memory.
#
# How it works:
#   - Each stage is sourced with: local = new.env(parent = globalenv())
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
RUN_CHECK_INPUTS        <- TRUE
RUN_BUILD_DATA          <- TRUE
RUN_DESCRIPTIVES        <- TRUE
RUN_FUTURE_DESCRIPTIVES <- TRUE
RUN_SAMPLE_TABLES       <- TRUE
RUN_COUPLE_TREATMENT_DESCRIPTIVES <- TRUE
RUN_EVENT_STUDIES       <- TRUE
RUN_MODELS              <- TRUE
RUN_LASSO               <- TRUE
RUN_SESSION_INFO        <- TRUE

# Optional: stop after each stage for debugging
STOP_AFTER_EACH         <- FALSE

# =============================================================================
# Helper: run a stage in a fresh environment
# =============================================================================
run_stage <- function(script_path) {
  message("\n--- Running: ", script_path, " ---")
  
  # new.env with global parent keeps package/base functions available while
  # preventing accidental reuse of objects created by previous stages.
  e <- new.env(parent = globalenv())
  
  # Source in isolated env
  source(script_path, local = e)
  
  # Drop the environment and clean memory
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
# Stage 2: Descriptive figures and tables
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

if (RUN_SAMPLE_TABLES) {
  message("\n==================== Stage 2c: Sample tables ===================")
  run_stage("code/run/02c_sample_tables.R")
  if (STOP_AFTER_EACH) stop("Stopped after Stage 2c (as requested).")
}

if (RUN_COUPLE_TREATMENT_DESCRIPTIVES) {
  message("\n==================== Stage 2d: Couple treatment descriptives ====")
  run_stage("code/run/02d_make_couple_treatment_descriptives.R")
  if (STOP_AFTER_EACH) stop("Stopped after Stage 2d (as requested).")
}

# =============================================================================
# Stage 3a: Event-study regression analogs of treatment descriptives
# =============================================================================
if (RUN_EVENT_STUDIES) {
  message("\n==================== Stage 3a: Event studies ===================")
  run_stage("code/run/03a_make_couple_treatment_event_studies.R")
  if (STOP_AFTER_EACH) stop("Stopped after Stage 3a (as requested).")
}

# =============================================================================
# Stage 3b: Regression tables
# =============================================================================
if (RUN_MODELS) {
  message("\n==================== Stage 3b: Models ==========================")
  run_stage("code/run/03_models_workoutside.R")
  if (STOP_AFTER_EACH) stop("Stopped after Stage 3b (as requested).")
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

