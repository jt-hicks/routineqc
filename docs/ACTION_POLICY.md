# QC action policy

Quality-control facts and action policy are separate layers. Logical,
prevalence, temporal, and tested-volume functions describe observations; the
action policy decides whether those facts imply retention, review, or an
authorized exclusion. No package function deletes source rows.

## Conservative review policy

`conservative_review` is the default policy. It authorizes exclusion only when
core tested/positive data are structurally unusable:

- tested or positive is missing;
- tested or positive is negative;
- positive is greater than tested;
- positive is greater than zero when tested is zero.

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
| `review_core_invalid` | critical | Flags-only policy routes an unusable core record to review |
| `review_multiple_signals` | high | At least two independent domains among attendance, prevalence, and tested volume are active |
| `review_attendance` | medium | Attendance contradiction without another active domain |
| `review_prevalence` | medium | Prevalence/statistical issue without another active domain |
| `review_tested_volume` | medium | Tested-volume issue without another active domain |
| `review_temporal_context` | low | Temporal context is insufficient without another active issue |
| `retain` | none | This policy recommends neither review nor exclusion |

`retain` does not certify correctness. It only means that the configured rules
and policy did not recommend action.

## Output safety fields

- `flag_exclude_authorized` is the sole package field authorizing exclusion.
- `flag_review_recommended` identifies review actions.
- `action_policy` and `action_policy_version` are written to every row.
- `review_priority` supports queue ordering but does not alter data.
- `qc_reason` records all evidence even when one primary action is selected.

Summaries described as “after” QC use only `flag_exclude_authorized`. Review
rows remain included. Hypothetical exclusions based on unresolved reviews must
be labeled as scenarios and must not be presented as the package-recommended
cleaned dataset.
