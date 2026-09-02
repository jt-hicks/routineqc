# routineqc 0.0.0.9000

- Redesigned the read-only Run Explorer with a nested action/reason-combination
  status view, configurable facility and district burden summaries, a detailed
  QC decision flow, Plotly tooltips, observed testing-volume time series, and
  district heatmap and faceted time-series views. Replaced the fragile review
  queue header JavaScript with server-generated tooltips and explicit empty
  states.
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
