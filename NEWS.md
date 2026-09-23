# routineqc (development version)

## Breaking changes

- A missing `tested` or `positive` count is no longer a logical error.
  `flag_missing_tested_or_positive` is replaced by `core_count_status`
  (`complete`, `missing_positive`, `missing_tested`, `missing_both`), which is
  descriptive and feeds no flag. Such rows no longer set `flag_core_invalid` or
  `flag_invalid_logical`, and are no longer assigned `exclude_core_invalid`.
  A missing `tested` alongside a reported `positive` is treated the same way,
  since its prevalence is undefined either way.
- The action policy is now version 2, and new runs can only use version 2.
  Requesting version 1, which excluded rows missing either count, raises an
  error naming the change. Runs persisted under version 1 remain readable by
  `read_qc_run()` and the Run Explorer, with their original flags.
- Rows missing a count are ineligible for the prevalence model with the new
  `prediction_status` level `ineligible_missing_counts`, distinct from
  `ineligible_core_counts`. The fit previously relied on the missing-count flag
  to drop rows with `tested` but no `positive`; it now excludes them explicitly.
- The residual rule is dispersion-adjusted. `add_prevalence_predictions()`
  estimates the fit's dispersion (Pearson chi-square over residual df, floored
  at 1) as `resid_dispersion` and adds `pearson_resid_scaled`;
  `add_prevalence_qc_flags()` applies the residual cutoffs to the scaled
  residual when present. Overdispersed data will raise far fewer residual
  flags, and no longer disproportionately at high tested counts. The manifest
  records `prevalence_dispersion`.
- The all-negative rule is expectation-based. A month with at least
  `all_negative_min_tested` tested and no positives is flagged only when the
  model makes zero positives improbable: `(1 - p_hat)^(tested / dispersion)`
  below the new `prevalence$all_negative_max_probability`, default 0.001. The
  count-only rule flagged ordinary months in low-prevalence areas; on 452,755
  ANC facility-months 77% of its flags had a model probability of zero above 1%.
  Rows the model did not assess are not evaluated. The configuration schema is
  now 5; stored schema 4 configurations remain readable.
- Summary prevalence (`prevalence_before_qc`,
  `prevalence_after_authorized_exclusions`) is computed over rows carrying both
  counts. It previously divided all positives by all tested, so a row with
  `tested` but no `positive` pulled prevalence down. Summaries gain
  `incomplete_count_rows`.

## New features

- `summarise_qc_count_completeness()` measures how often `tested` and `positive`
  are reported together, per facility or pooled per district, council, or
  region, with a pass/fail against `max_prop_incomplete_counts`.
  `summarise_qc_reporting_consistency()` includes the same measures and now
  returns `passes_reporting` and `passes_completeness` alongside
  `passes_strictness`. `qc_consistency_levels()` gains the
  `max_prop_incomplete_counts` criterion, and the Run Explorer's Reporting
  consistency tab shows and filters on it.
- Added an Attendance tab to the Run Explorer: rows where tested exceeds the
  attendance denominator, as a proportion of rows reporting both, overall, by
  month, and by district, region, or facility.
- Added facility reporting-consistency measurement:
  `summarise_qc_reporting_consistency()` reports, per facility, the months
  without testing split into absent, zero, and missing components, the longest
  gap without testing, when data collection starts and stops, and coverage of
  both the facility reporting window and the dataset-wide month span.
  `run_routine_qc()` now also returns `summaries$by_facility_reporting`.
- Added named reporting-consistency strictness levels via
  `qc_consistency_levels()` and cohort selection via `filter_qc_facilities()`,
  with every threshold overridable. Strictness selects facilities for review and
  never authorizes exclusion; see `docs/REPORTING_CONSISTENCY.md`.
- Added a Reporting consistency tab to the Run Explorer, a reporting-consistency
  cohort filter to the review queue, a district total time series above the
  district facets, and a toggle to hide facets for facilities with no testing
  data. The consistency views are computed from the flagged data, so runs saved
  by earlier versions still open.
- Added `plot_reporting_consistency()` and optional `zero_tested_fraction` and
  `absent_month_fraction` arguments to `simulate_qc_data()`, which inject nothing
  and draw no random numbers at their defaults, so existing seeded output is
  unchanged.

# routineqc 0.1.0

This is the first experimental release candidate. The QC engine, orchestration
API, provenance model, generic adapter, and read-only Run Explorer are
complete for synthetic data; no upstream production pipeline has yet been
integrated (see `MIGRATION.md`). Anomaly flags remain evidence for review, not
automatic proof of a data-entry error.

- Added a GitHub Actions `R CMD check` workflow (`r-lib/actions`) covering
  Linux (release and oldrel-1), Windows, and macOS, using only synthetic
  fixtures.
- Documented the experimental status, principal limitations, and privacy
  guidance for saved runs in the README, and added installation instructions
  for exercising a tagged release candidate.
- Added `CITATION.cff` and `URL`/`BugReports` fields in `DESCRIPTION`; put the
  package `Title` in title case.
- Excluded the generated `README.html` and `CITATION.cff` from the built
  package via `.Rbuildignore`.
- Redesigned the read-only Run Explorer with a QC-action donut, an
  individual-reason frequency chart, configurable facility and district burden
  summaries, an always-visible detailed QC decision flow, Plotly tooltips,
  observed testing-volume time series, and district faceted time-series views.
  Replaced the fragile review
  queue header JavaScript with server-generated tooltips and explicit empty
  states.
- Expanded the synthetic smoke fixture to three districts with eight facilities
  each and staggered facility histories of 12 to 24 months.
- Hardened the exported logical-flag helper so non-missing attendance is
  rejected unless accompanied by a non-empty attendance definition; absent or
  entirely missing attendance remains optional.
- Initial package scaffold using usethis/devtools conventions.
- Added modular QC pipeline for routine facility-month surveillance data.
- Added prevalence GAM modeling, tested volume anomaly flags, temporal checks,
	and hierarchical QC action assignment.
- Added wrappers for end-to-end execution and region/council/district/month/
	facility summaries.
- Added simulation and truth-evaluation helpers.
- Added plotting helpers and a Quarto report template.
- Added unit tests for logical flags, prevalence calculation, tested rolling QC,
	action priority, simulated errors, row-retention behavior, and wrapper output.
- Added validated `recommended` and `permissive_sensitivity` configuration profiles;
  `run_routine_qc()` now returns the exact configuration snapshot used.
- Reconciled optional attendance, prevalence thresholds, adjacent-month change
  rules, and temporal-context flags with synthetic boundary tests.
- Added calendar-aware trailing operational and centered retrospective
  tested-volume baselines, with explicit future-data metadata and documented
  limitations for both modes.
- Added versioned `conservative_review` and `flags_only` action policies.
  Automatic exclusion is authorized only for impossible core counts under the
  conservative policy; attendance and anomaly signals remain review-only.
- Added versioned `routineqc_run` objects with deterministic input and analysis
  identities, unique execution IDs, safe provenance, manifests, integrity
  validation, and guarded RDS/JSON persistence.
- Reconciled the prevalence GAM with the prototype region fixed effect and added
  safe row-level prediction eligibility, status, coverage warnings, and manifest
  coverage metadata. Unavailable predictions remain `NA` and review-only.
- Added a generic, versioned adapter result with declarative column mapping,
  structured row-level translation diagnostics, safe provenance handoff, and a
  guarded end-to-end runner. Added a synthetic adapter vignette.
- Added a read-only, localhost-bound Shiny Run Explorer with metadata-first
  artifact discovery, validated selection, review filters, diagnostic plots,
  and configuration/provenance views.
- Added a synthetic command-line smoke test covering adapter mapping, QC,
  run validation, persistence, discovery, and optional app launch.
- Fixed facility diagnostic filtering, added observed-versus-model prevalence
  time series, and made the review queue manageable with focused column sets,
  fixed identifiers, and header-definition tooltips.
- Made review dates opt-in, reset categorical filters on run selection, added a
  visible queue count and reset button, and removed hidden-tab fixed-column
  initialization that could leave the table blank in a browser.
