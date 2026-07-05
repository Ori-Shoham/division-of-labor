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

1. This section, then **§1 (project overview)** in full — research question, data, and
   design, in that order — so you know *what* we are doing, on *what data*, and *why*.
2. **§5 (data & licensing)** and **§6 (paperwork)** — so you understand what you may and
   may not do with the data, and what you must sign **before** touching it.
3. **§4 (repo tour)** + **§7 (how to run the code)** — so you can orient in the codebase.
4. Skim **§8 (conventions)**, **§10–11 (git/Overleaf)**, **§12 (Claude Code)**.

**Do this before touching any licensed data** (details in §6):

- Get added to the Special Licence as an additional researcher and **wait for UK Data
  Service approval**.
- Sign the Special Licence User Agreement; read the data-handling/security guide.
- Only then request access to the Special Licence (SL) secure machine.

**The two rules that matter most** — note the two licence tiers are governed differently
(see §5.5 for the full version):

- **Special Licence (SL) data** — the detailed SIC/SOC and Local Authority data — may
  **only** be stored and processed on the approved secure machine (§7). It never touches a
  personal laptop, Dropbox, email, or an AI tool.
- **End User Licence (EUL) data** may live in your normal Dropbox-synced project folder and
  on a personal laptop — that's allowed by the licence. The constraint on EUL data is
  narrower but still absolute: **it must never be readable by an AI coding tool** (Claude
  Code, Codex, ChatGPT, etc.). In practice that means keeping the EUL raw/derived data
  folder **out of the sync scope of whatever machine you run AI tools on** — see §12 for how
  this is actually set up.

---

## 1. Project overview — research question, data, and design  _[verify with Ori]_

*Drafted from the Special Licence project application
(`data_agreements/submit/SpecialLicenceProjectApplication_revised.pdf`), the event-study
equation and reading guide in `couples_graphs.tex` / `couples_graphs_short.tex`, `CLAUDE.md`,
`method.tex`, and the two grant proposals under `grants/`.*

### 1.1 Research question and the COVID natural experiment

**The question.** Gender gaps in earnings and careers are tightly linked to how couples
divide *paid* and *unpaid* work, especially after children arrive. This project asks
whether a **temporary, externally-imposed shock to household work allocation** — caused by
COVID-19 — **durably changed the division of labour within couples**, and whether that fed
through to gender gaps in labour-market outcomes and career trajectories. The focus is on
**co-resident couples, especially couples with dependent children** observed before the
pandemic.

**Why COVID is the experiment.** The pandemic forced sharp, externally-driven variation
across households depending on each partner's **pre-pandemic industry and occupation**:
some parents kept working outside the home (key workers), some shifted to home working,
others were shut down or furloughed. Think of a couple where the mother is a nurse who
keeps working at the hospital while her gym-instructor husband stays home. The unforeseen
nature of the pandemic, its differential effect on people depending on their
pre-determined work characteristics, combined with the fact that **local** restrictions
varied over time and place, gives credible quasi-experimental variation in how much time
each partner had to spend at home.

### 1.2 Two legs of the project: Israel administrative data and UKHLS

The project currently has **two legs** using the same underlying research
question, run on different data:

- **An Israel administrative-data leg.** Uses Israeli administrative data form the
Central Bureau of Statistics (למ"ס).
- **The UKHLS leg — what this repo implements.** Uses UK Understanding Society survey data with the same underlying logic (pre-pandemic industry/occupation
  exposure → COVID-era division-of-labour outcomes). While smaller in sample it
  allows more detailed analysis of time use which is critical to understanding
  changes in household divison of labor and home production. **Everything else in this guide, and
  essentially all the code in `code/`, is this leg.**

Some project documents (`method.tex`, the Falk/Sapir proposals under `grants/`) also sketch a
broader **cross-country** ambition that additionally mentions German survey data. That is
framing/future work, not implemented anywhere in this repo.

### 1.3 Data: Understanding Society (UKHLS)

This project uses the UK Household Longitudinal Study (**Understanding Society**, UKHLS), a
large, nationally representative panel that has followed the same UK households annually
since 2009, collecting detailed employment, income, education, health, and family
information for every household member.

We draw on **two linked UKHLS instruments**: the regular **main survey** — the core, ongoing
annual interview — and a short, separate **COVID-19 Study** fielded to the same panel
members at a much higher frequency specifically during the pandemic (2020–2021). Because
both instruments follow the same individuals, and link partners within a household to one
another, we can observe the same couples before, during, and after COVID.

Concretely, we use the person/household identifiers that link spouses to each other to
construct a **panel of couples**: for each couple we require both partners to be observed,
employed, and assigned a known **pre-pandemic industry and occupation** in the main survey
before COVID, and we then follow that couple through the COVID survey and into later
main-survey waves. This pre-pandemic industry/occupation pairing is exactly what feeds the
exposure classification and treatment/instrument construction in §1.4 below.

The operational detail behind this — wave letters, calendar mapping, file types, and how the
identifiers and variable naming actually work in the code — is covered separately in §2
("UKHLS data in detail"). You don't need it to follow the design in §1.4, but read it before
touching the data pipeline.

---

### 1.4 Empirical design: treatment, instrument, and equations

**Pre-pandemic exposure classification (the shared building block for both designs
below).** Using each partner's pre-COVID **SIC** (industry) and **SOC** (occupation)
codes, plus the Office of National Statistics (ONS) key-worker classification and the
IFS shutdown-sector classification (Joyce & Xu 2020), jobs are sorted into exposure groups —
key-worker sectors, shutdown sectors, and later we may consider classifying high/low capacity to work
from home. This classification is built only from pre-pandemic information, so it is not
contaminated by post-shock choices. Importantly, it plays **two different roles** depending
on the empirical design (§ "Two empirical steps"
below): it defines the **treatment groups** in the event-study design, and it is part of
the **excluded instrument** in the IV design. The two headline couple-level
exposure contrasts are:

1. **Wife is a key worker in a non-education sector**, while the **husband is not a key
   worker (or is education-only)**.
2. **Husband is in a shutdown sector**, while the **wife is not** (with symmetric
   husband/wife variants).

**Two empirical steps.**

#### Step 1 — Descriptive + event study (the part the code currently implements most fully)

Here the exposure contrasts above are used directly **as the treatment**. Event studies are
estimated separately by study sample (COVID waves / main-study waves), treatment definition,
spouse, outcome, and baseline child-age group (youngest child 0–10 vs. 11–17). This is
exactly the specification in the slide decks' "Event-study methodology" frame
(`couples_graphs.tex`, `couples_graphs_short.tex`) and in
`code/lib/event_study_regressions.R` (`build_event_study_formula()`,
estimated with `fixest::feols`):

$$
Y_{ict} \;=\; \alpha_t \;+\; \delta T_c \;+\;
\sum_{\tau \neq \tau_0} \beta_{\tau}\Big[\mathbf{1}\{t=\tau\}\times T_c\Big]
\;+\; X_c'\gamma \;+\; \varepsilon_{ict}
$$

where:
- $i$ indexes the person (wife or husband, estimated in **separate regressions by spouse**),
  $c$ the couple, $t$ calendar time (event time in the code).
- $T_c$ is the **couple-level treatment indicator** (one of the two exposure contrasts above,
  e.g. wife-key-worker/husband-not).
- $\alpha_t$ are period fixed effects; $\tau_0$ is the **omitted reference period** — 2019 for
  main-study event studies, 2019 where available for COVID-study event studies (else
  Jan–Feb 2020 for WFH/work-outside outcomes).
- $\beta_\tau$ are the coefficients of interest: the treatment-vs-comparison gap in event
  time $\tau$, relative to $\tau_0$. Pre-period $\beta_\tau$'s near zero support the
  pre-trends/parallel-trends assumption.
- $X_c'\gamma$ are baseline covariates: wife/husband age, wife/husband education category,
  number of children under 18, number of children under 10, and region.
- $\delta$ (the level term on $T_c$) is dropped and replaced by **couple fixed effects** in
  the couple-FE specification (`MAKE_EVENT_STUDIES_COUPLE_FE` in `config.R`); standard errors
  are clustered at the person level in all specifications.

Outcomes include employment, work hours, wages, work location, housework hours, childcare
hours/responsibility, selected health/well-being, and family events (divorces, childbirths).
Heterogeneity by **age of youngest child** at onset is a key cut (run as separate regressions
by child-age group, not an interaction term, in the current code).

#### Step 2 — Instrumental-variables design (planned for future work)

Here the couple's ***realised*** work-location configuration during COVID — the joint state
of wife's and husband's at-home/working-outside status, e.g. (wife home, husband home),
(wife home, husband out), (wife out, husband home), (wife out, husband out) — is treated as
**endogenous** (parents may select into WFH, reduce hours, or leave employment in response to
household needs). It is instrumented using the **same pre-pandemic exposure classification**
from Step 1 — not as a treatment here, but as (part of) the **excluded instrument** —
interacted with **local** COVID restrictions at Local Authority District level. Per the
Special Licence application (§2.2), the two-stage design is:

**First stage** (one regression per realised state $s$):

$$
D_{c,s} \;=\; \pi_s' Z_c \;+\; \rho_s R_{\ell(c)} \;+\; \theta_s'\big(Z_c \times R_{\ell(c)}\big)
\;+\; X_c'\Gamma_s \;+\; \eta_{c,s}
$$

**Second stage:**

$$
Y_{ict} \;=\; \sum_{s} \lambda_{s,i}\, \widehat{D}_{c,s} \;+\; \psi_t \;+\; X_c'\kappa
\;+\; \varepsilon_{ict}
$$

where:
- $D_{c,s}$ is an indicator that couple $c$ is in realised state $s$ during the COVID period.
- $Z_c$ is the vector of **pre-pandemic predicted exposure** for both partners — predicted
  key-worker status, predicted shutdown-sector exposure, and predicted capacity to work from
  home, for wife and husband separately. This is the same underlying SIC/SOC-based
  classification as Step 1, just used here as instrument components rather than as treatment.
- $R_{\ell(c)}$ is the local-restriction intensity (school closures, workplace/social
  restrictions) in couple $c$'s Local Authority District $\ell(c)$ at the time of interview —
  requires the Special-Licence LAD identifiers (§5.1) and the IFS restrictions linkage
  (see "Data linkage" below).
- $Z_c \times R_{\ell(c)}$ lets the same pre-pandemic job imply different realised
  work-location outcomes depending on how binding local restrictions were at the time.
- $\lambda_{s,i}$ (second stage) is the causal effect of interest: how each realised
  couple work-location configuration affects spouse $i$'s outcomes.
- Couple fixed effects can be added to both stages where not collinear with the excluded
  instruments; $X_c$ denotes the corresponding controls vector in each stage.

This design is **not yet built** in `code/`; the SL Local Authority data (SN 6666) and the
IFS restrictions linkage need to be wired in first (see §13).

**Data linkage.** Local Authority District identifiers (Special Licence only) link
households to the **IFS COVID-19 Restrictions Dataset** (school closures, workplace/social
restrictions over time) — see `policies/` for the in-repo copies and crosswalks.

**Key references** (from the application): Goldin (2014); Kleven, Landais, Posch, Steinhauer
& Zweimüller (2019); Hupkau & Petrongolo (2020); Sevilla & Smith (2020); ONS key-worker
reference tables (2020); Joyce & Xu, IFS BN278 (2020).

---

## 2. UKHLS data in detail

This section covers the operational details behind the two UKHLS instruments introduced in
§1.3 — waves, files, identifiers, and how the analytic panel is actually threaded together
in the code. None of this is required to follow the research design in §1.4; come back to it
once you start working with the data pipeline.

### 2.1 The two data collections, in detail

This project draws on **two separate UKHLS instruments** that happen to share the same
underlying panel members but run on completely different schedules and questionnaires:

| | **Main survey ("main study")** | **COVID-19 Study ("COVID survey")** |
|---|---|---|
| What it is | The regular, ongoing UKHLS annual interview — the core survey | A short, separate survey fielded rapidly to the *same* panel members specifically because of the pandemic, mainly online but with a telephone-mode top-up in some waves (see §2.3) |
| Wave naming | Lettered waves, one fieldwork round per letter: `a, b, c, … o` (each round's fieldwork spans roughly two calendar years, so a wave isn't a single instant) | Lettered `ca`–`ci` (nine rounds) |
| Cadence | Roughly annual, ongoing since 2009 | Monthly at first (April–July 2020), then roughly every 2 months through September 2021 |
| Content | Comprehensive: employment, income, education, health, housing, relationships, time use, etc. | Narrow and COVID-specific: work status, working from home, furlough, childcare, health/well-being during the pandemic |
| Analysis time unit | Primarily **calendar year of interview** (`study = "main"`); a **monthly** version also exists (`study = "main_monthly"`) but suffers from power issues — relatively few people are interviewed in any given month, so monthly event-time estimates are noisy | **Wave** (`study = "covid"`) — each COVID wave is treated as its own period, since waves are irregularly spaced calendar months rather than a regular monthly grid |
| Study number | SN 6614 (EUL) / SN 6931 (SL) | SN 8644 (same under either licence, see §5.1) |
| Relevant code | `code/lib/family_baseline.R`, `code/lib/history.R`, `code/lib/future_outcomes.R` | `code/lib/covid_loader.R`, `code/lib/covid_panel.R` |

The **wave-letter ↔ calendar mapping actually used in this repo** (from
`code/lib/wave_labels.R`, the single source of truth):

- **Main survey:** `a`=Wave 1 … `h`=Wave 8, **`i`=Wave 9, `j`=Wave 10, `k`=Wave 11** (the
  three candidate **baseline** waves — see §2.2), `l`=Wave 12, `m`=Wave 13, `n`=Wave 14,
  `o`=Wave 15.
- **COVID survey:** `ca`=Apr 2020, `cb`=May 2020, `cc`=Jun 2020, `cd`=Jul 2020,
  `ce`=Sep 2020, `cf`=Nov 2020, `cg`=Jan 2021, `ch`=Mar 2021, `ci`=Sep 2021.

Because these are two different questionnaires on two different schedules, the code always
tracks which "study" a panel came from (`study = "main"` vs. `"covid"` vs. `"main_monthly"`
in `code/lib/event_study_regressions.R`) — event time is measured in **calendar years** for
the main survey but in **irregular calendar months** for the COVID survey, and the two are
never silently pooled.

### 2.2 How this project threads the two together

- **Baseline (pre-COVID) information** — both partners' industry/occupation, age,
  education, children, region — is built from the **main survey**, using whichever of
  waves **I/J/K (Waves 9/10/11)** gives each person their most recent interview, prioritizing
  a **2019** interview date (people are interviewed at different points across a wave's
  ~2-year fieldwork window, so "2019" isn't the same wave letter for everyone).
- **Pre-baseline history** (waves `a`–`k`) is used for pre-trend checks and longer-run
  controls.
- **What happened *during* the pandemic** — work status, WFH, furlough, childcare, in
  April 2020 through September 2021 — comes from the **COVID survey** (`ca`–`ci`).
- **What happened *after* the pandemic** — later employment, pay, family outcomes — comes
  from **main-survey follow-up waves `j`–`o`**. The main version currently restricts to
  people/couples also observed in the COVID sample (`code/lib/future_outcomes.R`). We had
  some troubles with things looking different in the main survey vs. the COVID sample
  results in the periods where they overlap.


### 2.3 File types per wave and identifiers

Each main-survey wave ships as (at least) three Stata files, and the COVID survey ships one
required file (`00_check_inputs.R` looks for exactly these):

- `{w}_indresp.dta` — **individual response**: one row per person interviewed that wave;
  most substantive variables (job, income, health, attitudes) live here.
- `{w}_egoalt.dta` — **ego–alter file**: relationship links between household members (who
  is whose spouse/partner/child) — this is what lets the pipeline pair up wife/husband rows
  into couple-level records.
- `{w}_indall.dta` — **individual all**: a fuller household roster (includes people who
  weren't interviewed that wave but are still household members), used to keep track of
  household composition.
- `{cw}_indresp_w.dta` — the COVID survey's individual response file for wave `cw`,
  **web-mode** respondents. This is the **only** COVID file `00_check_inputs.R` requires and
  the only one `code/lib/covid_loader.R` loads.
- `{cw}_indresp_t.dta` — a **telephone-mode** counterpart that also exists on disk for at
  least some COVID waves (e.g. `ca_indresp_t.dta`). It is a smaller, differently-fielded
  top-up (respondents interviewed by phone rather than the web questionnaire) and was **not
  run in every wave**, so it isn't a consistent panel on its own. **`00_check_inputs.R` does
  not check for it, and the pipeline does not load or merge it** — it is not part of the
  required-inputs checklist at all, so its absence never blocks the pre-flight check. If you
  need to check exactly which waves have a `_t` file and whether it's worth incorporating
  (e.g. for sample size), check the raw data folder directly; this hasn't been
  systematically verified. _[verify with Ori]_

Person-level records are keyed by `pidp` (a person identifier stable across waves and
across the main/COVID surveys — the same `pidp` links a person's main-survey and
COVID-survey rows). Couple-level files are built by joining each person's row to their
partner's row (via the `egoalt` links) and keeping both spouses' variables side by side,
usually suffixed `_w` (wife) / `_h` (husband) — you'll see this suffix convention throughout
`code/lib/`.

### 2.4 Datasets and samples built by the pipeline

`code/run/01_build_data.R` is the single script that turns the raw wave files into
everything downstream. It writes to two separate output roots (§5.4), and the distinction
between them matters:

- **`derived/`** (`der_path`) — every intermediate and full-population dataset the pipeline
  produces along the way. Not restricted to any particular analytic sample; other scripts
  read from here when they need a fuller population than the curated samples provide.
- **`samples/`** (`samples_path`) — the curated, ready-to-analyze **analytic samples**
  (prefixed `s2019_*`) that downstream figures/tables/regressions actually use. These are
  built *from* the `derived/` files by applying the sample restrictions described below.

#### The baseline restriction that underlies (almost) everything

`build_baseline()` (`code/lib/family_baseline.R`) builds one row per person from the I/J/K
main-survey waves, preferring wave K restricted to 2019 and back-filling from J then I for
anyone missing at K — this is `baseline.rds` in `derived/`. Every downstream dataset then
starts from the **analytic baseline restriction**: baseline `jbstat` (labour-market status)
in `{1, 2}`, i.e. **employed or self-employed at baseline**. This restriction is applied
once (`df_baseline_analytic` in the build script) and threaded through nearly every dataset
described below — it is *not* re-derived independently in each one. Baseline SIC/SOC are
**not** required to be non-missing at this stage; missing/invalid codes are flagged by
`add_baseline_work_groups()` and only excluded where a specific analysis needs it.

#### Person-level datasets and samples

- `prebaseline_history_long.rds` / `_summary.rds` — each baseline-analytic person's
  observations in **main-survey waves before their own baseline wave** (long panel + a
  compact per-person summary), used for pre-trend checks and history-based controls.
- `df_sample_long_covid.rds` — the full **person × COVID-wave** panel (`ca`–`ci`) for
  baseline-analytic people, built from the merged COVID waves (`covid_all_wide.rds`).
- `future_outcomes_long_lmo.rds` (+ `_monthly` variant) — the **person × main-survey
  follow-up wave** panel (waves `l`–`o`), restricted to the baseline-analytic sample. The
  default version starts March 2020; the `_monthly` variant additionally keeps Jan–Feb 2020
  for monthly-resolution plots/event studies (see the power caveat in §2.1).
- `person_history_future_long.rds` — a single **ready-to-plot stacked panel** per person:
  pre-baseline history → baseline row → COVID-study rows → main-survey follow-up. A
  `_mainonly` variant exists that drops the COVID-study rows, for comparisons using only
  the regular main-survey cadence.
- The four **analytic samples** built by `build_samples_2019()` (`code/lib/samples.R`), each
  nested inside the previous one:
  1. `s2019_all` — all baseline-analytic workers.
  2. `s2019_couples` — (1) restricted to people with a valid baseline partner link
     (`base_partner_rel` indicating spouse/cohabiting/civil partner).
  3. `s2019_covid` — (1) restricted to people also observed in the COVID study.
  4. `s2019_covid_couples` — (2) ∩ (3): baseline couples also observed in the COVID study.
  - `_plus_lmo` suffixed versions of all four (e.g. `s2019_all_plus_lmo.rds`) have the wide
    future-outcomes file merged in.

#### Couple-level datasets

- **Couple roster** (`baseline_couple_roster.rds`, `code/lib/samples.R:build_baseline_couple_roster()`)
  — one row per couple, built by self-joining baseline-analytic people to their baseline
  partner, keeping only **reciprocal** partner links where one partner is male and the
  other female (the heterosexual-couple restriction used throughout). A
  `..._both_in_covid.rds` variant keeps only couples where both spouses are observed in the
  COVID study.
- **Baseline couple-level dataset** (`s2019_baseline_couplelevel.rds`,
  `build_baseline_couple_dataset()`) — one row per couple, both partners' baseline variables
  side by side (`_h` / `_w` suffixes), plus couple-level **treatment and child-age
  variables** added by `add_couple_baseline_treatments()`: `treat_wife_key_notedu_husb_not_or_edu`,
  `treat_wife_key_notedu_any`, `treat_husb_shutdown_wife_not`, `sample_husb_notkey_or_edu`,
  and `child_age_group_2019` / `has_child_u10_2019` / `has_child_11_17_2019`. A
  `_both_in_covid` variant restricts to the COVID-observed roster. Pre-baseline couple
  history is merged in afterward.
- **COVID couple-wave panel** (`df_sample_long_covid_couplelevel.rds`,
  `build_covid_couple_long()`) — one row per **couple × COVID wave**, keeping only waves
  where *both* spouses are observed that wave, with couple treatment/child-group variables
  attached.
- **Future-outcomes couple panels** (`future_outcomes_couple_long_lmo.rds` /
  `_wide_lmo.rds`, `build_future_couple_long()` / `build_future_couple_wide()`) — the
  couple-level analogue of the person-level future-outcomes files, again in both `_long`
  (couple × wave) and `_wide` (one row per couple) shapes, each with `_both_in_covid` and
  `_monthly` variants.
- **Couple-level stacked panel** (`couple_history_future_long.rds`,
  `build_couple_history_future_long()`) — the couple-level analogue of
  `person_history_future_long.rds`: pre-baseline couple history → baseline couple row →
  COVID couple-wave rows → future couple-wave rows, one ready-to-plot long file. A
  `_mainonly` variant (and its `_both_in_covid` / `_monthly` combinations) drops the
  COVID-study rows.

#### Naming conventions worth memorizing

| Pattern | Meaning |
|---|---|
| `s2019_*` | A curated analytic **sample** (lives in `samples/`, not `derived/`) |
| `_couplelevel` / `_couple_*` | One row per **couple**, spouse variables suffixed `_h` / `_w` |
| `_long` | Panel shape: one row per person/couple **× time period** |
| `_wide` | One row per person/couple, outcomes spread across columns by period |
| `_both_in_covid` | Restricted to couples where **both** spouses are observed in the COVID study |
| `_plus_lmo` | The dataset with wide future outcomes (waves **L/M/N/O**) merged in |
| `_mainonly` | Excludes COVID-study rows; regular main-survey cadence only |
| `_monthly` | Keeps Jan–Feb 2020 for monthly-resolution use (see the power caveat in §2.1) |

If you need to trace exactly how a given `.rds` is built, `code/run/01_build_data.R` is
staged and commented step-by-step in this same order — read it alongside
`code/lib/samples.R`, `code/lib/family_baseline.R`, `code/lib/history.R`,
`code/lib/covid_panel.R`, and `code/lib/future_outcomes.R`.

---

## 3. The codebase in one paragraph

An R pipeline turns raw Understanding Society (UKHLS) survey files into a pre-COVID
**baseline**, then person-level and couple-level **panels** spanning pre-COVID history, the
COVID waves, and later main-study waves. It assigns **baseline-SIC/SOC treatment groups**,
and emits **figures** (`figures/`) and **LaTeX table fragments** (`tables/`) that are
`\input{}` into Beamer slide decks and proposal documents. Raw data and most derived `.rds`
live **outside** the repo; the repo holds scripts, generated figures/tables, and TeX.

---

## 4. Repository tour

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

## 5. Data & licensing (full walkthrough)

### 5.1 The datasets
This project uses three UK Data Service (UKDS) study families. The pipeline currently
switches between the **EUL** and **SL** editions of the main study via one flag (§5.3).

| SN | What it is | Role here |
|---|---|---|
| **SN 6614** | Understanding Society, **End User Licence (EUL)** main study | Default edition; condensed (`*_cc`) industry/occupation only |
| **SN 6931** | Understanding Society, **Special Licence (SL)** main study | Same files **plus** detailed 4/5-digit SIC (`jbsic07`) and SOC (`jbsoc10`) |
| **SN 6666** | Understanding Society **Special Licence, Local Authority District** | LAD identifiers for linking to local restrictions (IV design; see §1.4) |
| **SN 8644** | Understanding Society **COVID-19 Study** | The `ca`–`ci` COVID waves; same under either licence |

> Note: `code/lib/config.R` currently wires up SN 6614, SN 6931, and SN 8644. **SN 6666
> (LAD)** is part of the approved Special Licence application but is not yet wired into the
> config — adding it is part of building the IV design. _[verify with Ori]_

### 5.2 Getting the data (UKDS)

1. Create your UKDS account and get invited/approved on the project — see §6 for the full
   account, invitation, and paperwork sequence. **Do this before downloading anything.**
2. Download the relevant studies as **Stata (.dta)** files. The pipeline reads `.dta` via
   the `haven` package. Expected raw files per wave:
   - Main waves: `{w}_indresp.dta`, `{w}_egoalt.dta`, `{w}_indall.dta`
     (e.g. `j_indresp.dta`). EUL and SL share identical file names and layout — SL just
     adds variables.
   - COVID waves: `{cw}_indresp_w.dta` (e.g. `cd_indresp_w.dta`).
3. Place the unpacked study folders **outside the git repo** (see §5.4).
   - **EUL** (SN 6614, SN 8644): can go in your normal Dropbox project data folder, as long
     as that folder is excluded from sync on any machine that also runs an AI coding tool
     (§12).
   - **SL** (SN 6931, SN 6666): must go **only** inside the approved encrypted location on
     the SL secure machine (§7) — never in Dropbox, never anywhere else.

### 5.3 EUL vs SL — the `DATA_LICENSE` switch
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

### 5.4 Where things live on disk (paths)
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

### 5.5 Privacy constraints (non-negotiable) — the two tiers are different

The EUL and SL editions carry **different** handling obligations. Don't conflate them:

- **SL data (SN 6931, SN 6666 — detailed SIC/SOC, Local Authority District)** is the more
  restricted tier: it may **only** be stored and processed on the approved secure machine
  (§7.1 — currently the TAU-managed desktop named in the licence application). It never goes
  onto a personal laptop, USB drive, personal cloud, or Dropbox folder, and never into an AI
  tool. This is a hard requirement of the Special Licence User Agreement.
- **EUL data (SN 6614, SN 8644)** is less restricted: the licence permits storing and
  processing it on an ordinary researcher machine, and in practice it lives in a Dropbox
  folder like any other project file. The binding constraint on EUL data is narrower but
  still absolute: **no AI tool may ever have read access to it.** See §12 for how this is
  enforced in practice (the short version: the folder holding EUL raw/derived data is kept
  out of the Dropbox sync scope on whichever machine is used for AI-assisted coding).
- Raw micro-data and derived `.rds` files (either tier) **never go in the git repo** — they
  live outside the repo per §5.4, regardless of which machine holds them.
- On **this** checkout (the one Claude Code/Codex work in) the licensed data are
  intentionally **absent for both tiers** — that's the point of the sync-exclusion setup in
  §12. Do **not** try to run the full pipeline or probe data paths from here; this checkout
  is for code/docs only.
- Only **repo-safe artifacts** may leave the secure/EUL environment into shared or public
  places: scripts, `figures/`, `tables/` fragments, `.tex`. Anything you export must be
  **aggregated and disclosure-safe** (no small cells, no raw SIC/SOC or fine geography) per
  the application's §2.7 and the UKDS Research Data Handling guide.

---

## 6. Project administration & onboarding paperwork (do this before any data access)

### 6.1 Project identity  _[verify with Ori]_

- UK Data Service **project number 282609**, title **"Work from home in the UK"**.
- **Project lead:** Itay Saporta Eksten (University of Manchester / Tel Aviv University,
  Eitan Berglas School of Economics) — the project lead is the person who must invite you
  and who submits any changes to the project or team on the UKDS side.
- Outgoing team member: Ori Nahshon Shoham (the role being handed over).
- Approved project window: **2026-05-07 → 2029-12-31**.

### 6.2 Get a UK Data Service account and be added to the project

This has to happen **before** any of the paperwork in §6.3, and well before you can
download or see any data:

1. **Create your own UK Data Service account** at
   https://ukdataservice.ac.uk/register — use the email you want permanently associated
   with this project (institutional email is usually expected).
2. **Send that registered email to the current project lead** (Itay, or whoever holds the
   role at handover time) and ask them to add you to project **282609**.
3. **The project lead invites you** as a team member on the project through their UK Data
   Service account (Section 1.2 of the project application — "Research team" — lists
   everyone who is allowed to see the raw data; the lead adds new members there and on the
   UKDS website).
4. **Accept the invitation** when it arrives at your registered email, then complete the
   paperwork in §6.3. You are not approved to access SL data until that paperwork is
   submitted and UKDS confirms approval in writing — don't download anything before that.

### 6.3 Required forms

All forms are in `data_agreements/` (signed/submitted copies in `data_agreements/submit/`).
Order of operations, once you've been invited (§6.2):

1. **Complete the Special Licence *additional researcher* form** —
   `data_agreements/SpecialLicenceAdditionalResearcher.docx` (see Ori's filed example
   `SpecialLicenceAdditionalResearcher_Ori.docx`).
2. **Sign the Special Licence User Agreement** —
   `data_agreements/SpecialLicenceUserAgreement.pdf` (filed example:
   `submit/SpecialLicenceUserAgreement_Ori.pdf`). If home-working access is needed, complete
   the agreement's home-working **Appendix**.
3. **Read** the obligations you are agreeing to:
   - `data_agreements/cd171-researchdatahandling.pdf` (Research Data Handling & Security)
   - `data_agreements/cd137-enduserlicence.pdf` (End User Licence terms)
4. **Email everything to UKDS** (`help@ukdataservice.ac.uk`) and **wait for written
   approval**. Do not download or handle SL data until approved.
5. **At the end of the engagement:** complete `data_agreements/Data_Destruction_Form.docx`
   and securely delete the data per UKDS requirements.

The approved project application itself
(`data_agreements/submit/SpecialLicenceProjectApplication_revised.pdf`) is the canonical
description of what the project is permitted to do — read §2.1–2.7 of it.

---

## 7. Running the code (the secure remote desktop for SL work)

This section covers the **Special Licence (SL)** secure machine. EUL-only work does not
require this — it can run on a normal Dropbox-synced machine (see §5.5 and §12 for the one
constraint that still applies: no AI tool may read the data folder).

### 7.1 What the environment is
Per the approved application, the SL data are **hosted on a Tel Aviv University-managed
desktop in a locked office** and accessed **remotely via TAU's secure VPN** (two-factor
auth, personal accounts). The machine is BitLocker-encrypted, auto-locks on idle, and runs
anti-malware. No data may be copied off it. This is the "remote desktop" referred to
throughout this guide.

### 7.2 Connection & access  _[secrets: obtain from Ori / TAU IT]_

- You will need: a TAU account with VPN access + 2FA, and credentials/permission to the
  specific desktop that hosts the data.
- The VPN client, the desktop's host/address, and login details are **deliberately not
  written here** (this file is committed to GitHub). **Obtain them from Ori or TAU IT.**
- Connect to the VPN first, then open a Remote Desktop session to the host. Close the
  session when you are done.

### 7.3 Environment setup on the secure machine

- **R + RStudio** (RStudio recommended; there is a `.Rproj` for the project).
- **R packages used by the pipeline** (install once):
  `tidyverse`, `haven`, `readxl`, `scales`, `forcats`, `modelsummary`, `glmnet`,
  `knitr`, `kableExtra`. (`data.table` appears only in exploratory scripts.)
  ```r
  install.packages(c("tidyverse","haven","readxl","scales","forcats",
                     "modelsummary","glmnet","knitr","kableExtra"))
  ```
- **Get the code onto the machine:** clone from GitHub and `git pull` to update (see §10).
  The *code* may live on the secure machine; the *data* lives only in the approved location.
- **Set paths:** add/confirm your machine's branch in `code/lib/config.R` (§5.4) so
  `path_main_*`, `path_covid`, and `data_out_root` point at the right places.

### 7.4 Run workflow
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

## 8. Code & coding conventions

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

## 9. Current outputs & where they're consumed

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

## 10. Git workflow (from scratch)

This repo has **two remotes**: `origin` (GitHub) and `overleaf` (Overleaf — see §11).

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
`overleaf-YYYY-MM-DD-…` branches that exist only for Overleaf syncing (§11) — don't develop
on those.

---

## 11. Overleaf workflow (writing ↔ git)

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

## 12. AI coding tools

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
    TAU secure desktop (§7), which has no Claude Code / Codex on it. That tier is enforced
    by physical/network isolation, not by discipline.
  - **EUL data** *is* allowed on an ordinary laptop, and that laptop may also run AI coding
    tools — but the two must not have access to each other. The way this project does that:
    the EUL raw/derived data folder is **excluded from the Dropbox sync** on the machine
    used for AI-assisted coding. Concretely: the project Dropbox folder syncs normally, but
    the specific "understanding society uk all data" subfolder (the `path_main_*` /
    `path_covid` / `data_out_root` targets from §5.4) is set to **not sync** to this
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
  - The agent must follow §8 ("stick to the approved plan") — it should not introduce
    unrequested design changes.

### Codex (secondary)  _[verify with Ori]_
Codex / OpenAI tooling is also used on occasion as an alternative coding assistant. It reads
the same `AGENTS.md` for project context. The **same data-privacy guardrails apply without
exception**: no licensed data, no raw extracts, no running against restricted data. Pick
whichever assistant you prefer for code edits; keep both pointed at the committed code only.

---

## 13. Where things stand & first tasks  _[draft — Ori to edit]_

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
     local restrictions) described in §1.4.
  3. Expand event-study outcomes (housework/childcare, well-being, family events) and the
     youngest-child heterogeneity cut.

---

## 14. Contacts, resources & glossary

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
