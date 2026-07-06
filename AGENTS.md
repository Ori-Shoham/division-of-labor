# AGENTS.md

> **Note:** This file is the canonical source of project instructions for Codex and other `AGENTS.md`-reading tools. `CLAUDE.md` in this repo is a duplicate maintained for Claude specifically — Codex should ignore `CLAUDE.md` and rely on this file instead. Claude should ignore this file and treat `CLAUDE.md` as canonical. When updating project instructions, edit both files together to keep them in sync.

This repository studies how COVID-era work arrangements changed the division of labor within households, especially for couples with children. The current implemented pipeline is centered on UKHLS data and produces descriptive figures, sample tables, and couple-treatment event-study outputs used in slides and proposals. Some project documents also describe related or future extensions using Israeli administrative data and German survey data, but the code in this repo is primarily the UKHLS workflow.

## What this repo does

- Builds a pre-COVID baseline from UKHLS main-study waves I/J/K, prioritizing 2019 interviews.
- Loads UKHLS COVID-study waves `ca`-`ci` and future main-study follow-up waves `j`-`o`.
- Constructs person-level and couple-level analytic panels.
- Defines treatment and comparison groups using baseline SIC/SOC logic.
- Generates descriptive figures (including baseline distributions and categorical work-status figures), sample-composition tables, couple-treatment plots, and event-study analogs.
- Feeds generated figures and `tables/*.tex` fragments into Beamer slide decks and proposal documents.

## Repo layout

- `code/run/`: staged pipeline entrypoints.
- `code/lib/`: reusable loaders, harmonizers, sample builders, plotting helpers, sample-table helpers, and event-study helpers.
- `figures/`: generated shareable figures consumed by TeX.
- `tables/`: generated LaTeX table fragments and a few other report artifacts.
- `policies/`: in-repo policy and crosswalk inputs such as SOC/SIC lookups and the key-worker reference workbook.
- `grants/`: proposal documents for broader project framing.
- Top-level `.tex` files: active slide decks, notes, and draft manuscript scaffolding.

## Execution order

The intended run order is:

1. `code/run/00_check_inputs.R`
   - Verifies local scripts, policy files, external raw-data paths, and output roots.
2. `code/run/00_master.R`
   - Orchestrates the pipeline stage by stage in fresh environments.
3. `code/run/01_build_data.R`
   - Builds the baseline composite, pre-baseline history, COVID long panel, future outcomes, couple-level files, and analytic samples.
4. `code/run/02_make_descriptives.R`
   - Produces COVID descriptive figures.
5. `code/run/02b_make_future_descriptives.R`
   - Produces future/main-wave descriptives when enabled in config.
6. `code/run/02c_sample_tables.R`
   - Produces sample-composition tables and couple composition figures.
7. `code/run/02d_make_couple_treatment_descriptives.R`
   - Produces couple-treatment descriptive figures, including the new `work_last_week_status` stacked-bar over-time figures.
8. `code/run/02e_make_couple_baseline_descriptives.R`
   - Produces baseline distribution figures (2019 or COVID wave 1) for `couples_graphs_short.tex`. Saves to `figures/couple_treatment/baseline_distributions/`.
9. `code/run/03a_make_couple_treatment_event_studies.R`
   - Produces regression/event-study analogs, figures, and saved results.
10. `code/run/03_models_workoutside.R`
    - Produces the April 2020 regression table `tables/workoutside_industry_comparison.tex`.
11. `code/run/04_lasso_workoutside.R`
    - Produces exploratory lasso outputs.
12. `code/run/99_session_info.R`
    - Saves reproducibility metadata.

## Data locations and privacy constraints

- Raw UKHLS inputs do **not** live in this repo. They are configured in `code/lib/config.R`.
- On this machine, the licensed raw/derived data are intentionally **not present**. This is deliberate and is meant to prevent local code execution that would inspect restricted data in ways that could violate the end-user license.
- Most derived analytic outputs, especially `.rds` files, also live outside the repo under the external output root configured in `config.R`.
- Repo-safe artifacts are mainly:
  - scripts,
  - generated figures under `figures/`,
  - generated LaTeX table fragments under `tables/`,
  - TeX source files.
- Keep path logic centralized in `code/lib/config.R`.
- Do not introduce new hardcoded machine-specific paths in run scripts or helper modules unless there is a strong reason and the pattern is updated centrally.
- Large confidential outputs belong outside git even if they are being synced locally by Dropbox.
- Do **not** try to run the full pipeline, probe external data paths, or execute scripts for the purpose of inspecting restricted data on this machine.
- When working here, rely on code inspection, existing generated artifacts checked into the repo, and the documented pipeline structure rather than attempting to access data.

## Git and Dropbox notes

- This repository lives inside Dropbox, so expect sync artifacts, timestamp churn, and occasional transient file noise.
- Make small, focused commits and check `git status` often before editing, staging, or committing.
- Be cautious with generated-file diffs. Inspect changes under `figures/`, `tables/`, and TeX auxiliary outputs before treating them as intentional.
- Avoid simultaneous conflicting edits across multiple machines when possible, especially for generated outputs and `.tex` files.
- Do not assume a missing generated file in git means the pipeline should regenerate everything; first check whether Dropbox or a local clean-up step caused the change.

## Important concepts and naming conventions

- **Baseline construction**
  - Built from main-study waves I/J/K using the most recent 2019 interview available.
- **Wave families**
  - COVID waves: `ca`-`ci`
  - Future/main follow-up waves: `j`-`o`
- **Group definitions**
  - Baseline industry/occupation group logic lives in `code/lib/work_groups.R`.
  - Grouping is based on baseline SIC/SOC, not post-treatment status.
- **Main couple-treatment definitions**
  - Wife key worker (non-education), husband not key worker or education only.
  - Husband shutdown sector, wife not.
- **Data shapes**
  - Person-level and couple-level panels are both active, and many downstream outputs depend on couple-level files with baseline group variables carried through.

## Document and TeX outputs

- `meeting_slides_descriptive_analysis.tex`
  - Main descriptive slide deck.
  - Designed to tolerate missing generated assets with placeholders.
- `couples_graphs.tex`
  - Main couple-treatment slide deck with event-study figures and balance tables. Covers both treatments.
- `couples_graphs_short.tex`
  - Shortened couple-treatment deck: wife-key treatment only, prefixes each outcome with a baseline distribution slide. Requires figures from `02e_make_couple_baseline_descriptives.R`.
- `meeting_slides.tex`
  - Older, lighter slide deck for earlier descriptive updates.
- `main.tex`
  - Currently a stub, not the main writing target.
- `method.tex`
  - Rough methodology and project notes, more brainstorming than polished manuscript text.
- `grants/Falk/falk_proposal_division_of_labor.tex`
  - Broader proposal document with cross-country framing.
- `grants/Sapir/sapir_household_division_proposal.tex`
  - Related proposal document with COVID and reserve-duty framing.
- `tables/*.tex`
  - Generated LaTeX fragments intended for `\input{}` into slide decks or documents.
  - Treat these as outputs, not hand-maintained source files.

## Editing guidelines for agents

- Read `code/lib/config.R`, `code/run/00_master.R`, and the relevant run script before making changes.
- Prefer shared fixes in `code/lib/` when multiple run scripts depend on the same logic.
- Preserve established `.rds` names, figure stems, and TeX asset paths unless you are intentionally migrating the pipeline.
- If a TeX file references a missing or incorrect figure, trace the referenced filename back to the generating run script and helper module before editing the TeX.
- Avoid manual edits to generated `tables/*.tex` fragments except for unusual debugging or one-off emergency inspection.
- Be conservative around output naming because slide decks and proposals often reference files by exact path.
- **Do not add design choices that are not in the approved plan.** Any deviation — even seemingly minor ones like trimming data, filtering observations, or changing plot defaults — must be raised with the user before implementing. Implement exactly what was agreed, nothing more.

## Common starting points

- If the problem is about paths, flags, or where outputs live: start with `code/lib/config.R`.
- If the problem is about execution flow: start with `code/run/00_master.R`.
- If the problem is about baseline construction or family structure: start with `code/lib/family_baseline.R`.
- If the problem is about COVID long panels: start with `code/lib/covid_loader.R` and `code/lib/covid_panel.R`.
- If the problem is about future outcomes or history/follow-up panels: start with `code/lib/future_outcomes.R` and `code/lib/history.R`.
- If the problem is about treatment definitions or work-outside logic: start with `code/lib/work_groups.R`.
- If the problem is about couple sample tables or composition figures: start with `code/lib/sample_tables.R` and `code/run/02c_sample_tables.R`.
- If the problem is about event-study outputs: start with `code/lib/event_study_regressions.R` and `code/run/03a_make_couple_treatment_event_studies.R`.
- If the problem is about baseline distribution or work-status figures in `couples_graphs_short.tex`: start with `code/lib/couple_baseline_dist_plots.R` and `code/run/02e_make_couple_baseline_descriptives.R`.

## Known rough edges and caveats

- The repo mixes active code, generated outputs, exploratory notes, and proposal documents.
- `main.tex` is not the main downstream consumer of outputs; the slide decks are more representative of current usage.
- Some TeX files reflect broader research ambitions than the currently implemented UKHLS codebase.
- Because outputs are partly generated and the repo sits inside Dropbox, `git status` may show noisy changes that are not substantive code edits.
- This workspace is documentation/code-facing only: assume restricted data are unavailable locally and should remain unavailable.
