# Analytical scenarios, imputation and synthetic validation

This describes the *Filter impact* view and the functions behind it. None of it
changes a stored run, an observation or an action. A scenario is a hypothetical
analytical choice applied to a copy of the data.

## Scenarios

`qc_scenario()` states which facilities enter the cohort and which flagged
counts are left out of calculations. `qc_scenario_presets()` gives four
(`relaxed`, `moderate`, `strict`, `very_strict`); `describe_qc_scenarios()`
prints the exact rules, and the dashboard shows them.

- All four presets include facilities regardless of their reporting history.
  At each facility-month, observation rules omit only the counts targeted by
  applicable flags and authorized exclusions. Other months remain unchanged,
  pre-existing missing counts remain missing, and invalid counts are never used.
  Positivity requires both tested and positive, so omitting one also makes the
  pair unavailable without removing the other count.
- Explicit custom `qc_scenario()` facility criteria remain available for callers
  who deliberately request them, but are not used in dashboard presets.
- These scenarios are separate from the *reporting-consistency* strictness
  cohorts, which serve a separate review purpose.

`apply_qc_scenario()` adds scenario columns to a copy. `qc_aggregate_metrics()`
aggregates at national, region, council, district or facility level and by
month: positivity is total positives over total tested for records where both
counts are usable (with a cluster-robust standard error), and coverage is tested
over attendance over records where both are usable. Facility percentages are
never averaged. Genuine zeros, missing, absent, invalid and inactive months are
kept distinct. `compare_qc_scenario()` returns the reported and filtered series.

"Positivity" is the term used throughout: the data are facility tested counts,
not a population prevalence.

## Landing page: QC baseline replacement

The national Moderate comparison has a third line, **QC baseline replacement**,
computed from expectations already saved in the run. No model is fitted when the
landing page loads:

| Targeted counts in a facility-month | Replacement |
|---|---|
| Positive only | Saved GAM probability (`p_hat`) times observed tested |
| Tested and positive | Saved rolling tested median, then `p_hat` times that replacement |
| Tested only | Rolling tested median, keeping observed positives |

If the tested-only median is below observed positives, tested remains missing and
the pair is unresolved; observed positives are not changed. The saved rolling
median must be finite, non-negative and, when recorded, have
`tested_baseline_ready = TRUE`. GAM probabilities must be finite, in `[0, 1]`
and, when recorded, have `prediction_status = "assessed"`. Missing columns in
older runs leave targets unsupported rather than triggering a new fit.

Pre-existing missing counts and absent reports are not filled. Untargeted usable
counts, raw observations, flags and actions are unchanged. Replacements retain
the exact median/probability products (possibly fractional expected counts);
they are not rounded or treated as newly reported observations. Attendance is
not replaced, and coverage-specific omissions still apply. National positivity
and coverage continue to use matched pairs.

The page displays how many targeted counts were filled and a table identifying
unresolved counts and their reasons. An unresolved count cannot contribute to a
matched pair, but does not suppress valid contributions from other facilities;
months without usable contributions remain gaps. The original GAM and rolling
baseline may have been influenced by suspect observations. Replacing anomalies
with expected values also smooths the series. This is a sensitivity comparison,
not new-model imputation or an uncertainty interval.

## Imputation (exploratory, optional in Filter impact)

By default excluded values are left missing. `qc_impute_scenario()` optionally
replaces them, and `summarise_qc_imputation()` / `qc_scenario_results()` report
them as a third version beside *reported* and *filtered*.

- Tested counts use a negative-binomial additive model; positives use a binomial
  model given the tested count, so completed values satisfy `positive <= tested`.
  The model structure falls back from full to simpler forms when data are thin.
- `method = "multiple"` draws repeated predictive completions and pools
  positivity with Rubin's rules (Barnard-Rubin degrees of freedom).
  `method = "expected"` fills expected values and has no interval; it is
  labelled exploratory.
- `mode = "retrospective"` fits once to all retained data. `"operational"` refits
  per target month using only data up to that month.
- The seed, method, mode, number of imputations, model structure, facility
  windows and counts left unsupported are recorded in `provenance`.
- Facility windows default to first to last report. A facility that stopped
  reporting or started late cannot be seen without a supplied `schedule`.

In Filter impact, imputation sits under *Advanced*, is opt-in and runs only for
the selected scenario. This is separate from the landing page's saved-baseline
replacement.

## Reading the results

Differences between scenarios are scenario sensitivity. The interval on an
imputed series is imputation uncertainty within one scenario. Neither says which
scenario is closest to the truth.

## Synthetic truth simulator

`simulate_qc_truth()` generates a hierarchy with known true counts, then
injects missingness (isolated, consecutive, informative) and errors, and returns
the observed data with the truth. `evaluate_qc_recovery()` runs the pipeline and
scenarios and compares recovered aggregates with the truth, including the share
of injected errors caught and interval coverage; `evaluate_qc_recovery_grid()`
repeats this over settings.

This checks that the machinery recovers a *known* answer under *assumed*
mechanisms. It does not establish that the methods are valid for real
surveillance data. In the bundled examples council-level interval coverage fell
below the nominal level, and informative missingness is not fully corrected.

## Compatibility

No run schema changed. The new functions read existing runs; runs saved before
this work are valid and the guide falls back to current defaults for sections
they lack. Preset results recomputed with this version may differ from earlier
versions because facilities are no longer excluded for reporting completeness.
