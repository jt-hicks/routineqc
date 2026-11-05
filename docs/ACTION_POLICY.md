# QC action policy

Quality-control facts and action policy are separate layers. Logical,
prevalence, temporal, and tested-volume functions describe observations; the
action policy decides whether those facts imply retention, review, or an
authorized exclusion. No package function deletes source rows.

## Conservative review policy

`conservative_review` is the default policy. It authorizes exclusion only when
core tested/positive data are impossible as reported:

- tested or positive is negative;
- positive is greater than tested;
- positive is greater than zero when tested is zero.

A missing `tested` or `positive` count is not grounds for exclusion or review.
Missingness describes a facility's reporting, not an error in a record, so it is
recorded in `core_count_status` and summarised per facility and area by
`summarise_qc_reporting_consistency()` and `summarise_qc_count_completeness()`.
A row missing a count receives `retain` unless another rule applies to what it
does report: tested-volume checks still run on a row with `tested` but no
`positive`. Its prevalence is `NA`, and prevalence summaries use only rows
carrying both counts.

This is policy version 3, the only version accepted for new runs. Version 3
sets `review_priority` per flag from the configurable `config$priority` (a
configuration schema 6 setting) rather than per domain, and adds the testing
coverage actions below. Version 1 authorized exclusion of rows missing either
count and version 2 set priority by domain with no coverage rules. Neither is
supported for new runs, but stored version 1 and 2 runs remain readable and keep
their original flags; a run is reproduced by the routineqc release recorded in
its manifest.

A count that is supplied but is not a valid whole number (malformed text, `NaN`,
`Inf`, or a fractional value) is not missing and is not impossible as reported
either. It is kept unrepaired, with the original in `tested_source` /
`positive_source`, and `flag_count_invalid_value` routes it to
`review_core_invalid` at critical priority. It never authorizes exclusion.

Such a row is left out of every calculation that needs a count: the prevalence
model fit, prevalence, the prevalence rules, the tested-volume baseline and
rules, the impossible-count and attendance checks, and the summary totals. It
is counted in the summary column `invalid_count_rows` and in no other count
column. `tested` and `positive` themselves keep the supplied numeric value (or
`NA` for malformed and non-finite text); the original stays in `*_source`.

### Versioning decision

Policy version 3 and configuration schema 6 are unchanged by this release. The
reason is that nothing a stored run depends on was reinterpreted: version 3 had
no rule for these values, the new flag and its action only add a path for rows
that were previously unflagged or treated as missing, and every earlier flag,
priority and coverage action keeps its meaning. Runs saved before the change
carry no `flag_count_invalid_value` column and still validate. The consequence
is that two builds of version 3 can act differently on invalid counts. They are
told apart by the `implementation_hash` in the manifest, which is part of
`analysis_id`, not by the policy version. A later change that reinterprets an
existing flag, priority or action must bump the policy version.

Attendance contradictions are review-only, even with an approved attendance
definition. They can reflect a problem with the external attendance denominator
rather than with the tested/positive record.

Statistical, temporal, plausibility, and tested-volume anomalies are also
review-only. Unusual values are evidence for investigation, not proof of error.

## Flags-only policy

`flags_only` never authorizes exclusion. A core-invalid record receives the
critical `review_core_invalid` action. This policy is intended for threshold
validation, comparison work, and settings where exclusion authority remains
entirely outside the package.

## Actions and priorities

Each row receives one primary `qc_action`, while `qc_reason` accumulates all
underlying evidence.

| Action | Priority | Meaning |
|---|---|---|
| `exclude_core_invalid` | critical | Conservative policy authorizes exclusion of an unusable core record |
| `review_core_invalid` | critical | Flags-only policy routes an unusable core record to review; also a reported count that is not a valid whole number under either policy |
| `review_multiple_signals` | high | At least two independent domains among attendance, prevalence, tested volume, and coverage are active |
| `review_attendance` | medium | Attendance contradiction without another active domain |
| `review_coverage` | medium | Low or very low testing coverage against attendance, without another active domain |
| `review_prevalence` | medium | Prevalence/statistical issue without another active domain |
| `review_tested_volume` | medium | Tested-volume issue without another active domain |
| `review_temporal_context` | low | Legacy label shown by the app for stored runs; version 3 never assigns it (temporal context only qualifies `qc_reason`) |
| `retain` | none | This policy recommends neither review nor exclusion |

`retain` does not certify correctness. It only means that the configured rules
and policy did not recommend action.

## Output safety fields

- `flag_exclude_authorized` is the sole package field authorizing exclusion.
- `flag_review_recommended` identifies review actions.
- `action_policy` and `action_policy_version` are written to every row.
- `review_priority` supports queue ordering but does not alter data. The level
  shown is the highest priority among the active flags, using the defaults or
  `config$priority`.
- `qc_reason` records all evidence even when one primary action is selected.

Reporting-consistency strictness (`docs/REPORTING_CONSISTENCY.md`) selects a
cohort of facilities for review. It is not an action policy: it assigns no
`qc_action`, sets no flag, and never authorizes exclusion. A facility outside the
selected cohort was not reviewed under that cohort, which is not a finding about
its records.

Summaries described as “after” QC use only `flag_exclude_authorized`. Review
rows remain included. Hypothetical exclusions based on unresolved reviews must
be labeled as scenarios and must not be presented as the package-recommended
cleaned dataset.
