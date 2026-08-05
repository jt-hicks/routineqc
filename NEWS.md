# routineqc 0.0.0.9000

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
