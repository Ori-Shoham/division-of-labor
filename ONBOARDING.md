# Onboarding & Handover Guide

> **Purpose.** This is the single document a new research assistant should read to take
> over all work on this project. It covers the research question and design, the data and
> its licensing, the code and conventions, the current outputs, and the working setups
> (git, Overleaf, the remote desktop where licensed data is run, and Claude Code).
>
> **Companion files.** `CLAUDE.md` and `AGENTS.md` (repo root) are the agent-facing
> versions of much of this — they tell Claude Code / Codex how to behave in this repo.
> This guide is the human-facing version and is broader. Where they overlap, all three
> should agree; if you change the pipeline, update them together.
>
> **Sections marked _[verify with Ori]_** were drafted from project documents and may be
> slightly out of date — confirm them during the handover.

---

## 0. Read this first (the 30-minute path)

If you do nothing else, read these in order:

1. This section and **§1 (research design)** — so you know *what* we are doing and *why*.
2. **§4 (data & licensing)** and **§5 (paperwork)** — so you understand what you may and
   may not do with the data, and what you must sign **before** touching it.
3. **§3 (repo tour)** + **§6 (how to run the code)** — so you can orient in the codebase.
4. Skim **§7 (conventions)**, **§9–10 (git/Overleaf)**, **§11 (Claude Code)**.

**Do this before touching any licensed data** (details in §5):
- Get added to the Special Licence as an additional researcher and **wait for UK Data
  Service approval**.
- Sign the Special Licence User Agreement; read the data-handling/security guide.
- Only then request access to the Special Licence (SL) secure machine.

**The two rules that matter most** — note the two licence tiers are governed differently
(see §4.5 for the full version):
- **Special Licence (SL) data** — the detailed SIC/SOC and Local Authority data — may
  **only** be stored and processed on the approved secure machine (§6). It never touches a
  personal laptop, Dropbox, email, or an AI tool.
- **End User Licence (EUL) data** may live in your normal Dropbox-synced project folder and
  on a personal laptop — that's allowed by the licence. The constraint on EUL data is
  narrower but still absolute: **it must never be readable by an AI coding tool** (Claude
  Code, Codex, ChatGPT, etc.). In practice that means keeping the EUL raw/derived data
  folder **out of the sync scope of whatever machine you run AI tools on** — see §11 for how
  this is actually set up.

---

## 1. Project overview — research question & design  _[verify with Ori]_

*Drafted from the Special Licence project application
(`data_agreements/submit/SpecialLicenceProjectApplication_revised.pdf`), `CLAUDE.md`,
`method.tex`, and the two grant proposals under `grants/`.*

**Administrative identity of the project**
- UK Data Service project number **282609**, title **"Work from home in the UK"**.
- Project lead: **Itay Saporta Eksten** (University of Manchester / Tel Aviv University,
  Eitan Berglas School of Economics).
- Research team member: **Ori Nahshon Shoham** (the role being handed over).
- Approved project window: **2026-05-07 → 2029-12-31**.

**The question.** Gender gaps in earnings and careers are tightly linked to how couples
divide *paid* and *unpaid* work, especially after children arrive. This project asks
whether a **temporary, externally-imposed shock to household specialization** — caused by
COVID-19 — **durably changed the division of labour within couples**, and whether that fed
through to gender gaps in labour-market outcomes and career trajectories. The focus is on
**co-resident couples, especially couples with dependent children** observed before the
pandemic.

**Why COVID is the experiment.** The pandemic forced sharp, externally-driven variation
across households depending on each partner's **pre-pandemic industry and occupation**:
some parents kept working outside the home (key workers), some shifted to home working,
others were shut down or furloughed. Because exposure depended on pre-pandemic SIC/SOC
codes and on **local** restrictions, it gives credible quasi-experimental variation in how
much time each partner had to spend at home.

**Treatment is built only from pre-pandemic information** (so it is not contaminated by
post-shock choices). Using each partner's pre-COVID **SIC** (industry) and **SOC**
(occupation) codes, plus the ONS key-worker classification and the IFS shutdown-sector
classification (Joyce & Xu 2020), jobs are sorted into exposure groups (key-worker sectors,
shutdown sectors, high/low capacity to work from home). The two headline couple-level
treatments are:
1. **Wife is a key worker in a non-education sector**, while the **husband is not a key
   worker (or is education-only)**.
2. **Husband is in a shutdown sector**, while the **wife is not** (with symmetric
   husband/wife variants).

**Two empirical steps.**
1. **Descriptive + event-study (the part the code currently implements most fully).** A
   difference-in-differences event study traces how outcomes evolve around the pandemic,
   by treatment group and by spouse. Time fixed effects; pre-pandemic controls (both
   partners' age, education, number/age of children, region, baseline employment); optional
   couple or couple-by-spouse fixed effects; omitted category = the wave just before COVID.
   Pre-period coefficients double as a pre-trend diagnostic. Outcomes include employment,
   work hours, wages, work location, housework hours, childcare hours/responsibility, and
   selected health/well-being, plus family events (divorces, childbirths). Heterogeneity by
   **age of youngest child** at onset is a key cut.
2. **Instrumental-variables design (planned / partially scaffolded).** Treats the couple's
   *realised* work-location configuration during COVID (wife home/out × husband home/out)
   as endogenous, and instruments it with pre-COVID SIC/SOC exposure interacted with
   **local** COVID restrictions (school closures, workplace restrictions) at Local Authority
   District level.

**Data linkage.** Local Authority District identifiers (Special Licence only) link
households to the **IFS COVID-19 Restrictions Dataset** (school closures, workplace/social
restrictions over time) — see `policies/` for the in-repo copies and crosswalks.

**Key references** (from the application): Goldin (2014); Kleven, Landais, Posch, Steinhauer
& Zweimüller (2019); Hupkau & Petrongolo (2020); Sevilla & Smith (2020); ONS key-worker
reference tables (2020); Joyce & Xu, IFS BN278 (2020).

**Scope note.** Some project documents (`method.tex`, the Falk/Sapir proposals) sketch a
broader **cross-country** ambition (Israeli administrative data, German survey data). Those
are framing/future work — **the code in this repo is the UKHLS workflow only.**

---

## 2. The codebase in one paragraph

An R pipeline turns raw Understanding Society (UKHLS) survey files into a pre-COVID
**baseline**, then person-level and couple-level **panels** spanning pre-COVID history, the
COVID waves, and later main-study waves. It assigns **baseline-SIC/SOC treatment groups**,
and emits **figures** (`figures/`) and **LaTeX table fragments** (`tables/`) that are
`\input{}` into Beamer slide decks and proposal documents. Raw data and most derived `.rds`
live **outside** the repo; the repo holds scripts, generated figures/tables, and TeX.

---

## 3. Repository tour

```
code/
  run/      staged pipeline entrypoints (00_… → 99_…); run these
  lib/      reusable loaders, harmonizers, sample builders, plot/table/event-study helpers
  tests/    a few R test scripts
figures/    generated, shareable figures consumed by TeX (SL run nests under figures/SL/)
tables/     generated LaTeX table fragments + a few report artifacts (SL run → tables/SL/)
policies/   in-repo policy inputs: SOC/SIC lookups, ONS key-worker workbook,
            lockdown/restriction datasets, UK price index
grants/     proposal documents (Falk, Sapir) for broader framing
data_agreements/  UK Data Service licence paperwork (application, user agreements, forms)
*.tex       active slide decks, notes, draft manuscript scaffolding (repo root)
CLAUDE.md, AGENTS.md   agent guidance; ONBOARDING.md   this file
```

**"Which file do I open for problem X?"** (mirrors `AGENTS.md`):

| If the problem is about… | Start with… |
|---|---|
| paths, flags, where outputs live | `code/lib/config.R` |
| execution flow / what runs when | `code/run/00_master.R` |
| baseline construction / family structure | `code/lib/family_baseline.R` |
| COVID long panels | `code/lib/covid_loader.R`, `code/lib/covid_panel.R` |
| future outcomes / history panels | `code/lib/future_outcomes.R`, `code/lib/history.R` |
| treatment definitions / work-outside logic | `code/lib/work_groups.R` |
| key-worker classification | `code/lib/policies_keyworkers.R` |
| couple sample tables / composition figures | `code/lib/sample_tables.R`, `code/run/02c_sample_tables.R` |
| event-study outputs | `code/lib/event_study_regressions.R`, `code/run/03a_…R` |
| baseline-distribution figures for `couples_graphs_short.tex` | `code/lib/couple_baseline_dist_plots.R`, `code/run/02e_…R` |
| real (deflated) pay | `code/lib/real_pay.R` |

---

## 4. Data & licensing (full walkthrough)

### 4.1 The datasets
This project uses three UK Data Service (UKDS) study families. The pipeline currently
switches between the **EUL** and **SL** editions of the main study via one flag (§4.3).

| SN | What it is | Role here |
|---|---|---|
| **SN 6614** | Understanding Society, **End User Licence (EUL)** main study | Default edition; condensed (`*_cc`) industry/occupation only |
| **SN 6931** | Understanding Society, **Special Licence (SL)** main study | Same files **plus** detailed 4/5-digit SIC (`jbsic07`) and SOC (`jbsoc10`) |
| **SN 6666** | Understanding Society **Special Licence, Local Authority District** | LAD identifiers for linking to local restrictions (IV design; see §1) |
| **SN 8644** | Understanding Society **COVID-19 Study** | The `ca`–`ci` COVID waves; same under either licence |

> Note: `code/lib/config.R` currently wires up SN 6614, SN 6931, and SN 8644. **SN 6666
> (LAD)** is part of the approved Special Licence application but is not yet wired into the
> config — adding it is part of building the IV design. _[verify with Ori]_

### 4.2 Getting the data (UKDS)
1. Create a **UK Data Service account** (https://ukdataservice.ac.uk) using the email that
   is/will be registered on the project.
2. You must be **listed on project 282609** and **approved** before you can download SL
   data (see §5 — this is gated on paperwork).
3. Download the relevant studies as **Stata (.dta)** files. The pipeline reads `.dta` via
   the `haven` package. Expected raw files per wave:
   - Main waves: `{w}_indresp.dta`, `{w}_egoalt.dta`, `{w}_indall.dta`
     (e.g. `j_indresp.dta`). EUL and SL share identical file names and layout — SL just
     adds variables.
   - COVID waves: `{cw}_indresp_w.dta` (e.g. `cd_indresp_w.dta`).
4. Place the unpacked study folders **outside the git repo** (see §4.4).
   - **EUL** (SN 6614, SN 8644): can go in your normal Dropbox project data folder, as long
     as that folder is excluded from sync on any machine that also runs an AI coding tool
     (§11).
   - **SL** (SN 6931, SN 6666): must go **only** inside the approved encrypted location on
     the SL secure machine (§6) — never in Dropbox, never anywhere else.

### 4.3 EUL vs SL — the `DATA_LICENSE` switch
A single flag in `code/lib/config.R` selects the edition and rewires everything:

```r
DATA_LICENSE <- "EUL"   # or "SL"
```

- `"EUL"` (SN 6614): condensed industry/occupation; uses `jbsic07_cc` / `jbsoc10_cc`.
- `"SL"` (SN 6931): detailed codes; uses `jbsic07` / `jbsoc10`; key-worker and
  shutdown-sector groups are redefined from the detailed codes; **outputs are written to
  separate locations** so an SL run never overwrites EUL outputs.

The config derives the right variable names automatically:
```r
VAR_SIC <- if (DATA_LICENSE == "SL") "jbsic07" else "jbsic07_cc"
VAR_SOC <- if (DATA_LICENSE == "SL") "jbsoc10" else "jbsoc10_cc"
```
Most other variables share names across editions (SL incomes such as `fimngrs_dv` keep the
same name but are non-top-coded). See the project memory note on EUL vs SL for the detailed
mapping, and **verify the stored width of `jbsic07` on real SL data** when you first get it.

### 4.4 Where things live on disk (paths)
All path logic is centralized in `code/lib/config.R`. **Do not hardcode machine-specific
paths anywhere else.** The config resolves paths per machine using the OS username
(`Sys.info()[["user"]]`), so each researcher's machine gets a branch:

- **Inputs (raw, confidential, outside repo):** `path_main_eul` (SN 6614),
  `path_main_sl` (SN 6931), `path_covid` (SN 8644). `path_main` follows `DATA_LICENSE`.
- **Outputs (derived, confidential, outside repo):** everything under `data_out_root`:
  - `der_path` → `derived/` (or `derived_SL/`): derived datasets (`.rds`)
  - `samples_path` → `samples/` (or `samples_SL/`): analytic samples (`.rds`)
  - `cache_path` → `cache/` (or `cache_SL/`): cached intermediates
- **Repo-safe outputs (inside repo, OK to sync):** `figures/` and `tables/`
  (SL run nests under `figures/SL/`, `tables/SL/`); policy inputs in `policies/`.

> **To onboard a new machine:** add a username branch in `config.R` setting `path_main_*`,
> `path_covid`, and `data_out_root` for that machine — following the existing pattern. Keep
> `data_out_root` **outside** the git/Dropbox-tracked repo.

### 4.5 Privacy constraints (non-negotiable) — the two tiers are different

The EUL and SL editions carry **different** handling obligations. Don't conflate them:

- **SL data (SN 6931, SN 6666 — detailed SIC/SOC, Local Authority District)** is the more
  restricted tier: it may **only** be stored and processed on the approved secure machine
  (§6.1 — currently the TAU-managed desktop named in the licence application). It never goes
  onto a personal laptop, USB drive, personal cloud, or Dropbox folder, and never into an AI
  tool. This is a hard requirement of the Special Licence User Agreement.
- **EUL data (SN 6614, SN 8644)** is less restricted: the licence permits storing and
  processing it on an ordinary researcher machine, and in practice it lives in a Dropbox
  folder like any other project file. The binding constraint on EUL data is narrower but
  still absolute: **no AI tool may ever have read access to it.** See §11 for how this is
  enforced in practice (the short version: the folder holding EUL raw/derived data is kept
  out of the Dropbox sync scope on whichever machine is used for AI-assisted coding).
- Raw micro-data and derived `.rds` files (either tier) **never go in the git repo** — they
  live outside the repo per §4.4, regardless of which machine holds them.
- On **this** checkout (the one Claude Code/Codex work in) the licensed data are
  intentionally **absent for both tiers** — that's the point of the sync-exclusion setup in
  §11. Do **not** try to run the full pipeline or probe data paths from here; this checkout
  is for code/docs only.
- Only **repo-safe artifacts** may leave the secure/EUL environment into shared or public
  places: scripts, `figures/`, `tables/` fragments, `.tex`. Anything you export must be
  **aggregated and disclosure-safe** (no small cells, no raw SIC/SOC or fine geography) per
  the application's §2.7 and the UKDS Research Data Handling guide.

---

## 5. Onboarding paperwork (do this before any data access)

All forms are in `data_agreements/` (signed/submitted copies in `data_agreements/submit/`).
Order of operations:

1. **Be invited to the project** (project 282609) on the UK Data Service website by the
   project lead.
2. **Complete the Special Licence *additional researcher* form** —
   `data_agreements/SpecialLicenceAdditionalResearcher.docx` (see Ori's filed example
   `SpecialLicenceAdditionalResearcher_Ori.docx`).
3. **Sign the Special Licence User Agreement** —
   `data_agreements/SpecialLicenceUserAgreement.pdf` (filed example:
   `submit/SpecialLicenceUserAgreement_Ori.pdf`). If home-working access is needed, complete
   the agreement's home-working **Appendix**.
4. **Read** the obligations you are agreeing to:
   - `data_agreements/cd171-researchdatahandling.pdf` (Research Data Handling & Security)
   - `data_agreements/cd137-enduserlicence.pdf` (End User Licence terms)
5. **Email everything to UKDS** (`help@ukdataservice.ac.uk`) and **wait for written
   approval**. Do not download or handle SL data until approved.
6. **At the end of the engagement:** complete `data_agreements/Data_Destruction_Form.docx`
   and securely delete the data per UKDS requirements.

The approved project application itself
(`data_agreements/submit/SpecialLicenceProjectApplication_revised.pdf`) is the canonical
description of what the project is permitted to do — read §2.1–2.7 of it.

---

## 6. Running the code (the secure remote desktop for SL work)

This section covers the **Special Licence (SL)** secure machine. EUL-only work does not
require this — it can run on a normal Dropbox-synced machine (see §4.5 and §11 for the one
constraint that still applies: no AI tool may read the data folder).

### 6.1 What the environment is
Per the approved application, the SL data are **hosted on a Tel Aviv University-managed
desktop in a locked office** and accessed **remotely via TAU's secure VPN** (two-factor
auth, personal accounts). The machine is BitLocker-encrypted, auto-locks on idle, and runs
anti-malware. No data may be copied off it. This is the "remote desktop" referred to
throughout this guide.

### 6.2 Connection & access  _[secrets: obtain from Ori / TAU IT]_
- You will need: a TAU account with VPN access + 2FA, and credentials/permission to the
  specific desktop that hosts the data.
- The VPN client, the desktop's host/address, and login details are **deliberately not
  written here** (this file is committed to GitHub). **Obtain them from Ori or TAU IT.**
- Connect to the VPN first, then open a Remote Desktop session to the host. Close the
  session when you are done.

### 6.3 Environment setup on the secure machine
- **R + RStudio** (RStudio recommended; there is a `.Rproj` for the project).
- **R packages used by the pipeline** (install once):
  `tidyverse`, `haven`, `readxl`, `scales`, `forcats`, `modelsummary`, `glmnet`,
  `knitr`, `kableExtra`. (`data.table` appears only in exploratory scripts.)
  ```r
  install.packages(c("tidyverse","haven","readxl","scales","forcats",
                     "modelsummary","glmnet","knitr","kableExtra"))
  ```
- **Get the code onto the machine:** clone from GitHub and `git pull` to update (see §9).
  The *code* may live on the secure machine; the *data* lives only in the approved location.
- **Set paths:** add/confirm your machine's branch in `code/lib/config.R` (§4.4) so
  `path_main_*`, `path_covid`, and `data_out_root` point at the right places.

### 6.4 Run workflow
Always start from the repo root (so relative paths like `code/lib/config.R` resolve).

1. **Pre-flight check** — confirms scripts, policy files, raw data, and output folders
   exist (and creates output folders):
   ```r
   source("code/run/00_check_inputs.R")
   ```
   It prints a per-wave completeness report and stops with a checklist if anything is
   missing. Fix issues, re-run until it prints `--- CHECK PASSED ---`.

2. **Run the whole pipeline** via the master script:
   ```r
   source("code/run/00_master.R")
   ```
   - Each stage runs in a **fresh environment** (`run_stage()` sources it with
     `local = new.env(parent = globalenv())`), so large datasets don't carry over in
     memory — every stage reads/writes from disk instead.
   - Toggle stages with the `RUN_*` flags at the top of `00_master.R`
     (e.g. `RUN_BUILD_DATA`, `RUN_DESCRIPTIVES`, `RUN_EVENT_STUDIES`, …).
   - Set `STOP_AFTER_EACH <- TRUE` to halt after each stage while debugging.

3. **Stage order** (also runnable individually, in this order):

   | Stage | Script | Produces |
   |---|---|---|
   | 0 | `00_check_inputs.R` | input validation |
   | 1 | `01_build_data.R` | baseline, history, COVID panel, future outcomes, couple files, samples (`.rds`) |
   | 2 | `02_make_descriptives.R` | COVID descriptive figures |
   | 2b | `02b_make_future_descriptives.R` | future/main-wave descriptives (flag-gated) |
   | 2c | `02c_sample_tables.R` | sample-composition tables + couple composition figures |
   | 2d | `02d_make_couple_treatment_descriptives.R` | couple-treatment figures (incl. work-status stacked bars) |
   | 2e | `02e_make_couple_baseline_descriptives.R` | baseline-distribution figures for `couples_graphs_short.tex` |
   | 3a | `03a_make_couple_treatment_event_studies.R` | event-study figures + saved results |
   | 3b | `03_models_workoutside.R` | `tables/workoutside_industry_comparison.tex` |
   | 4 | `04_lasso_workoutside.R` | exploratory lasso outputs |
   | 5 | `99_session_info.R` | reproducibility metadata |

4. **Getting outputs out (egress).** Only commit/export the repo-safe, disclosure-checked
   artifacts: scripts, `figures/`, `tables/`, `.tex`. The `.rds` files under `data_out_root`
   and all raw data **stay on the secure machine**. Push code/figures/tables changes to
   GitHub from the secure machine (or copy only those files), never the data.

---

## 7. Code & coding conventions

- **Staged pipeline, isolated environments.** Stages communicate **through disk**, not
  memory. If a stage needs a previous stage's output, it reads the `.rds` from
  `der_path`/`samples_path` — it does not assume in-memory objects exist.
- **Centralize configuration.** All paths, flags, wave lists, and the licence switch live in
  `code/lib/config.R`. Every stage `source()`s it. Don't scatter machine-specific paths.
- **Shared logic in `code/lib/`.** If several run scripts need the same behaviour, fix it in
  the relevant `lib/` helper, not in each run script.
- **Treat `figures/*` and `tables/*.tex` as generated outputs**, not hand-edited source.
  Slide decks reference them by exact path, so **preserve `.rds` names, figure stems, and
  TeX asset paths** unless you are deliberately migrating the pipeline (and then update the
  consuming `.tex`).
- **Naming / wave conventions.**
  - COVID waves: `ca`–`ci`. Future/main follow-up waves: `j`–`o`. History candidate waves:
    `a`–`k`.
  - **Baseline** = most recent pre-COVID interview from main waves **I/J/K**, prioritizing
    2019 interviews.
  - **Grouping is by *baseline* SIC/SOC**, not post-treatment status (`code/lib/work_groups.R`).
- **Stick to the approved plan.** Do **not** add design choices that aren't agreed —
  including seemingly minor ones (trimming/filtering observations, changing plot defaults).
  Raise any deviation with the project lead **before** implementing. (This is also encoded in
  `CLAUDE.md` and the project memory.)
- **R style.** Match the surrounding code: tidyverse idioms, `suppressPackageStartupMessages`
  around `library()` blocks, comment density similar to existing files.

---

## 8. Current outputs & where they're consumed

**Figures** are written under `figures/` in organized subfolders defined by the `fig_path_*`
variables in `config.R` (e.g. `figures/descriptives/covid/`, `figures/sample_composition/`,
`figures/couple_treatment/…`, and `figures/couple_treatment/baseline_distributions/`). SL
runs nest the whole tree under `figures/SL/`.

**LaTeX table fragments** are written under `tables/` (SL → `tables/SL/`) — e.g. the
`sample_table_*` family and `workoutside_industry_comparison.tex`. These are `\input{}` into
the decks; treat them as generated.

**TeX consumers** (which deck uses which outputs):

| TeX file | What it is | Fed by |
|---|---|---|
| `meeting_slides_descriptive_analysis.tex` | main descriptive deck (tolerates missing assets via placeholders) | 02, 02b, 02c |
| `couples_graphs.tex` | main couple-treatment deck: both treatments, event studies + balance tables | 02c, 02d, 03a |
| `couples_graphs_short.tex` | shortened deck: wife-key treatment only; each outcome prefixed by a baseline-distribution slide | 02d, 02e, 03a |
| `meeting_slides.tex` | older, lighter descriptive deck | earlier descriptives |
| `method.tex` | rough methodology / brainstorming notes | — |
| `main.tex` | stub, not the current writing target | — |
| `grants/Falk/…tex`, `grants/Sapir/…tex` | proposals (broader framing) | selected figures |

---

## 9. Git workflow (from scratch)

This repo has **two remotes**: `origin` (GitHub) and `overleaf` (Overleaf — see §10).

**One-time setup**
```bash
git clone https://github.com/Ori-Shoham/division-of-labor.git
cd division-of-labor
git config user.name  "Your Name"
git config user.email "you@example.com"
```

**Everyday loop**
```bash
git status                 # see what changed (do this often)
git pull origin main       # get latest before you start
# … edit …
git add <specific files>   # stage deliberately, not "git add ."
git commit -m "message"
git push origin main
```

**Dropbox caveat (important here).** The repo lives inside Dropbox, so `git status` often
shows **noise**: timestamp churn, sync conflict files (`*~$*`, `* (1).*`), and re-touched
generated outputs under `figures/`/`tables/`. **Inspect generated-file diffs before staging**
— a changed figure is only meaningful if you intended to regenerate it. Make **small,
focused commits**. A missing generated file in `git status` does **not** mean "regenerate
everything" — check whether Dropbox or a cleanup moved it first.

**Branches.** `main` is the working branch. There is an `overleaf-backup` branch and dated
`overleaf-YYYY-MM-DD-…` branches that exist only for Overleaf syncing (§10) — don't develop
on those.

---

## 10. Overleaf workflow (writing ↔ git)

The `.tex` writing is mirrored to **Overleaf** via Overleaf's **git bridge**. The Overleaf
project is wired in as the `overleaf` remote:

```
overleaf  https://git.overleaf.com/<project-id>   (fetch/push)
```
(`origin` = GitHub; `overleaf` = the Overleaf project's git endpoint. Overleaf's `master`
branch ↔ our content.)

**How the two remotes relate.** GitHub (`origin`) is the source of truth for **code +
TeX**. Overleaf is where co-authors **edit the TeX in the browser**. Changes therefore flow
both ways and must be reconciled by hand. The `overleaf-backup` branch and the dated
`overleaf-YYYY-MM-DD-HHMM` branches in the history are snapshots taken at each sync so a bad
merge can be recovered.

**Typical sync ritual** _[confirm exact steps with Ori — this is the general pattern]_:

- **Pull co-authors' Overleaf edits into git:**
  ```bash
  git fetch overleaf
  git checkout -b overleaf-$(date +%F-%H%M) overleaf/master   # snapshot the Overleaf state
  git checkout main
  git merge overleaf-<that-snapshot>                          # bring edits into main
  # resolve conflicts in the .tex files, then:
  git push origin main
  ```
- **Push your git-side TeX changes back to Overleaf:**
  ```bash
  git push overleaf main:master
  ```
- Keep `overleaf-backup` updated as a safety snapshot before risky merges.

Because figures/tables are generated on the (data) side and TeX is edited on the Overleaf
side, the safest habit is: **regenerate assets → commit to GitHub → push to Overleaf**, and
**pull Overleaf text edits back** before regenerating, to avoid clobbering co-author edits.

---

## 11. AI coding tools

### Claude Code (primary)
- **Install / auth:** Claude Code is Anthropic's CLI (also available as desktop/IDE/web).
  Install per the official docs and sign in with the project's Anthropic account.
- **How this repo guides the agent:**
  - `CLAUDE.md` and `AGENTS.md` (repo root) are loaded as project instructions — they encode
    the repo layout, run order, data-privacy rules, and conventions. Keep them in sync with
    this guide when the pipeline changes.
  - **Persistent memory** lives under
    `~/.claude/projects/<this-project-slug>/memory/` with an index `MEMORY.md`. Useful facts
    already saved there include the EUL-vs-SL variable mapping and the "stick to the approved
    plan" rule. The agent reads/writes these across sessions.
  - **Plan mode** lets the agent research and propose a plan before editing — use it for
    anything non-trivial; approve the plan before it writes code.
- **Project guardrails for any AI tool here (critical) — how this is actually set up:**
  - **SL data** never touches a machine that runs AI tools at all — it only exists on the
    TAU secure desktop (§6), which has no Claude Code / Codex on it. That tier is enforced
    by physical/network isolation, not by discipline.
  - **EUL data** *is* allowed on an ordinary laptop, and that laptop may also run AI coding
    tools — but the two must not have access to each other. The way this project does that:
    the EUL raw/derived data folder is **excluded from the Dropbox sync** on the machine
    used for AI-assisted coding. Concretely: the project Dropbox folder syncs normally, but
    the specific "understanding society uk all data" subfolder (the `path_main_*` /
    `path_covid` / `data_out_root` targets from §4.4) is set to **not sync** to this
    machine, so it simply doesn't exist on disk here — an AI tool has no path to read even
    if asked. That's why this checkout has no data present, and why `00_check_inputs.R`
    will report the data roots missing if you run it here (expected — don't "fix" it).
  - If you set up a new machine for AI-assisted coding, replicate this: let the repo folder
    sync, but exclude/unsync the external data folder(s) before installing or running any AI
    tool there. Do this *before* pointing an agent at the repo, not after.
  - **Never paste licensed data (SL or EUL), raw variable extracts, or file contents from the
    data folders into an AI tool's chat**, even from a machine that does have access (e.g.
    while debugging on the SL desktop, don't copy data into a browser-based AI tool).
  - On this checkout, since the data is absent, **do not ask the agent to run the pipeline or
    probe data paths** to "see" the data — it can't, and shouldn't be made able to. Use code
    inspection and the committed figures/tables instead.
  - The agent must follow §7 ("stick to the approved plan") — it should not introduce
    unrequested design changes.

### Codex (secondary)  _[verify with Ori]_
Codex / OpenAI tooling is also used on occasion as an alternative coding assistant. It reads
the same `AGENTS.md` for project context. The **same data-privacy guardrails apply without
exception**: no licensed data, no raw extracts, no running against restricted data. Pick
whichever assistant you prefer for code edits; keep both pointed at the committed code only.

---

## 12. Where things stand & first tasks  _[draft — Ori to edit]_

*Inferred from recent git history and the current outputs; Ori should correct/replace this.*

- The **EUL pipeline is the mature path**: baseline (2019-priority), couple panels, sample
  tables, couple-treatment descriptives, baseline-distribution figures, and event-study
  analogs all run and feed the decks (`couples_graphs.tex`, `couples_graphs_short.tex`,
  `meeting_slides_descriptive_analysis.tex`).
- Recent work (see `git log`) has been on **baseline distribution figures** (2018/2019
  baselines), **winsorizing**, **event-study updates**, and **slide/Overleaf cleanup**.
- **Likely next steps:**
  1. Move from EUL to **SL** once the Special Licence data is in hand: set `DATA_LICENSE
     <- "SL"`, verify `jbsic07`/`jbsoc10` widths, re-derive key-worker/shutdown groups from
     detailed codes, and re-run the pipeline into the `*_SL` output trees.
  2. Wire in **SN 6666 (LAD)** and the **IFS restrictions linkage**, then build the
     **IV design** (realised work-location states instrumented by pre-COVID exposure ×
     local restrictions) described in §1.
  3. Expand event-study outcomes (housework/childcare, well-being, family events) and the
     youngest-child heterogeneity cut.

---

## 13. Contacts, resources & glossary

**People**
- Project lead: **Itay Saporta Eksten** — `itay.saportaeksten@manchester.ac.uk`.
- Outgoing RA (handover): **Ori Shoham** — `orishoham@mail.tau.ac.il` / GitHub `Ori-Shoham`.

**Resources**
- UK Data Service: https://ukdataservice.ac.uk · `help@ukdataservice.ac.uk`
- Understanding Society (study docs/variable search): https://www.understandingsociety.ac.uk
- GitHub repo: https://github.com/Ori-Shoham/division-of-labor
- Overleaf: the `overleaf` git remote's project (ask Ori for the share link).
- ONS key-worker reference tables; IFS BN278 shutdown sectors (Joyce & Xu 2020) — copies and
  crosswalks in `policies/`.

**Glossary**
- **UKHLS / Understanding Society** — the UK Household Longitudinal Study; a large, partner-
  linked household panel survey.
- **Wave** — one round of data collection. Main waves are letters (`a`, `b`, …); COVID waves
  are `ca`–`ci`.
- **EUL / SL** — End User Licence (coarser, SN 6614) vs Special Licence (detailed SIC/SOC and
  LAD, SN 6931/6666). Selected by `DATA_LICENSE`.
- **SIC / SOC** — Standard Industrial / Occupational Classification codes (industry / job).
- **Key worker** — ONS-defined essential-sector worker expected to work outside the home.
- **Shutdown sector** — IFS-defined sector forced to close in early COVID.
- **Baseline** — each person's most recent pre-COVID interview (main waves I/J/K, 2019-first).
- **Event study** — DiD specification tracing outcome differences by event time relative to
  COVID onset, treated vs comparison couples.

---

*Maintenance: when you change the pipeline, update this file together with `CLAUDE.md` and
`AGENTS.md`. Keep secrets (hosts, credentials, share links) out of this committed file —
point to "obtain from Ori / TAU IT" instead.*
