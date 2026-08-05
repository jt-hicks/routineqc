# Prototype behavior comparison

This audit compares the reusable QC engine in `routineqc` with the working
prototype in `anc_data_curation/src/anc_qc_protocol.R`. It is a behavioral
comparison, not a request to preserve pipeline-specific loading or output code.

Audit date: 4 August 2026.

## Outcome

The package contains counterparts for every reusable prototype layer, but it is
not behaviorally equivalent. Some differences are deliberate package
improvements; others can materially change which rows are flagged or assigned
to primary removal. They must be reconciled and tested before the package is
treated as a replacement for the prototype.

## Decision matrix

| Area | Prototype | Current package | Decision |
|---|---|---|---|
| Input preparation | Requires attendance and pipeline-specific column mapping | Uses generic mapping and does not require attendance | Keep package design; adapters own source mapping and attendance remains optional |
| Structural validation | Mixed into preparation and downstream flags | Explicit canonical gate rejects malformed keys and duplicates | Keep package design |
| Logical counts | Missing attendance is invalid; checks `tested > attending` | Core validity depends only on tested and positive | Keep tested/positive as core; add attendance checks only when an approved attendance definition is supplied |
| Invalid rows | Preserved and flagged | Preserved and flagged | Equivalent and required |
| GAM formula | Region fixed effect, region-specific cyclic seasonality, time trend, facility random effect | Region fixed effect, region-specific cyclic seasonality, time trend, facility random effect | Reconciled and protected by a regression test |
| Small-data GAM | Dynamically caps basis dimensions; errors with no fit rows | Returns `NULL` below 50 rows and retries smaller bases | Keep package resilience; document the minimum-data behavior |
| Prediction eligibility | Explicitly excludes unseen model factor levels | Preserves all rows and records assessed, ineligible, unseen-level, unavailable, error, or non-finite status | Reconciled with a stricter row-level status contract |
| Residual rule | `abs(residual) > 4` for tested >= 20 and `> 5` below 20 | One configurable cutoff, default `>= 4` | Restore the size-dependent prototype rule as the compatibility default; retain configuration |
| All-negative rule | Tested >= 50 | Tested >= 30 | Restore prototype default; expose configuration |
| All-positive rule | Tested >= 30 | Tested >= 30 | Equivalent |
| Large prevalence change | Change > 0.5, both months tested >= 20, and months contiguous | Change >= 0.35 without denominator or adjacency guards | Restore prototype guards/default; expose configuration |
| Raw prevalence bounds | No separate near-zero/near-one rule | <= 0.001 or >= 0.999 contributes to prevalence extreme | Do not silently use as a primary compatibility rule; retain only as an optional sensitivity rule |
| Isolated extreme | Based on statistical residual extreme; optional adjacency requirement | Based on broad prevalence extreme and ignores calendar gaps | Use the statistical basis and require adjacent calendar months by default; expose insufficient temporal context |
| Tested rolling baseline | Centered 13-month window excluding current observation; at least six valid neighbors | Previous six observations only | Implemented calendar-aware trailing operational and centered retrospective modes |
| Tested high/low rule | Requires robust-z and ratio conditions plus minimum-volume guards | Robust-z or ratio condition, without equivalent guards | Implemented corroborating evidence and safeguards by default; OR behavior is sensitivity-only |
| Tested jump/drop | Strict thresholds with minimum current/previous counts | Inclusive thresholds with only previous count > 0 | Implemented strict thresholds and minimum-volume guards |
| Action policy | Statistical combinations could recommend primary removal | Statistical anomalies could recommend primary removal | Replaced with versioned policy: only core-invalid counts may be excluded by the conservative default; all anomalies remain review-only |
| Sensitivity flag | Any non-retain action | Excluded some review categories | Retained as an anomaly-analysis marker; exclusion and review now have explicit separate fields |
| Summaries | Region/month/facility counts | Generic grouped before/after summaries and more geographic levels | Keep package implementation, after flag-name reconciliation |
| Plots | Multi-plot diagnostic bundle | Small composable plotting functions | Keep package design; add missing diagnostics only when the app needs them |
| Simulation/evaluation | Absent | Synthetic injection, truth evaluation, threshold grid | Keep package additions |
| Orchestration result | List of data, model, summaries, and plots | List of data, model, summaries, and plots | Rework later into a versioned QC run object with configuration and provenance |

## Explicitly excluded from migration

- SharePoint paths, file discovery, and OPD/supplement loaders.
- The fallback that replaced missing attendance with tested.
- Tanzania-specific hierarchy and filename assumptions.
- Hard-coded artifact and dashboard paths.
- Real facility records or known source contradictions as test fixtures.

## Reconciliation sequence

1. Add synthetic characterization tests for logical and attendance behavior.
2. Reconcile prevalence and temporal facts without changing action policy in the
   same commit.
3. Introduce named compatibility configuration for prototype thresholds.
4. Reconcile tested-volume modes and document retrospective versus prospective
   use.
5. Reconcile action policy after all underlying facts have stable tests.
6. Run a separate upstream adapter comparison on non-committed data before any
   production integration.

## Resolved domain decisions

- Isolated-extreme classification requires actual adjacent calendar months by
  default. A residual extreme without both adjacent months is marked as having
  insufficient temporal context rather than isolated.
- Operational runs use a trailing calendar baseline and retrospective curation
  may explicitly select a centered baseline. Centered runs record that they use
  future information. Caveats for both modes are documented in
  `docs/TESTED_VOLUME_BASELINES.md`.
- The default conservative action policy authorizes exclusion only for
  impossible or missing core tested/positive counts. Attendance contradictions
  and all anomaly flags are review-only. `flags_only` authorizes no exclusions.
- The authoritative prevalence GAM includes the prototype region fixed effect.
  Prediction failures and unseen factor levels remain explicit non-assessments;
  they never become anomaly evidence or authorize exclusion.

## Open domain decisions

- Any future policy that authorizes broader exclusions requires reviewed data,
  domain sign-off, a new policy name/version, and explicit tests.
