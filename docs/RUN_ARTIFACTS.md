# Reproducible QC run artifacts

`run_routine_qc()` returns a validated `routineqc_run`. Run schema version 2
contains flagged data, the fitted model (or `NULL`), summaries, the exact
configuration, and a metadata manifest. Version 2 adds prevalence-model
assessment coverage and status counts to the integrity-checked manifest. The
configuration inside it is schema 6 and the action policy version 3; stored
runs with configuration schema 4 or 5 or policy version 1 or 2 stay readable.

## Identities

- `input_fingerprint` is a deterministic SHA-256 digest of normalized canonical
  fields. Row order does not affect it; changes to relevant values do. From
  fingerprint schema 2 (recorded as `input_fingerprint_schema_version`) it also
  covers `tested_source`, `positive_source` and the two `*_parse_status`
  columns, so a missing count and a malformed one, both `NA` in `tested`, give
  different fingerprints. Runs without that field are verified with schema 1.
- `analysis_id` deterministically combines the input fingerprint, configuration
  fingerprint, package version, run schema, and `build$implementation_hash`.
  Runs saved before the build identity was recorded omit that last part, so
  their identities are unchanged.
- `execution_id` is unique to one execution, even when analysis identity is the
  same.

A fingerprint is an identifier, not encryption or anonymization. It does not
make confidential source data safe to share.

## Build identity

The package version alone cannot tell a development tree from a release, so the
manifest's `build` records `implementation_hash` (a digest of every function in
the package namespace and of its constant data objects, such as the statuses
counted as invalid, so a change to either alters it), `source_revision` (the
commit in the installed DESCRIPTION, `NA` for a plain local install), the R
version, and the versions of mgcv, dplyr, tidyr, lubridate, rlang, and digest.
Two implementations therefore cannot share an `analysis_id` silently. The
implementation hash identifies code, not behaviour: it changes with any code
edit, including behaviour-neutral refactors (not comments), and does not capture
platform numerics.

`count_translation` records, for tested and positive, how many values were `ok`,
`missing`, `malformed`, `non_finite`, or `fractional` (see `prepare_qc_data()`).

## Manifest contents

The manifest records schemas, identities, UTC creation time, package version,
input dimensions and month coverage, optional-field availability, full
configuration, policy and tested-baseline selections, and caller-provided safe
provenance. It intentionally excludes flagged rows, model contents, source
paths, and credentials.

Provenance must be a flat named list of scalar identifiers. Names indicating
passwords, secrets, tokens, credentials, API keys, or local paths are rejected.
Callers remain responsible for ensuring identifier values are safe to disclose.

## Persistence

```r
write_qc_run(run, 'qc-runs/example.rds')
restored <- read_qc_run('qc-runs/example.rds')
```

Writing creates:

- `example.rds`: the complete R object;
- `example-manifest.json`: human-readable metadata only.

Existing files are not overwritten unless `overwrite = TRUE` is explicit.
Reading validates object schemas, row count, canonical-data fingerprint,
configuration fingerprint, deterministic analysis identity, and agreement with
the adjacent JSON manifest.

## What validation guarantees

`validate_qc_run()` (also run when reading) checks that the pieces of a run
agree with one another:

- the configuration, row count, input fingerprint, model-assessment counts, and
  analysis identity match the manifest;
- the flagged data carry `qc_action`, `flag_exclude_authorized`,
  `flag_review_recommended` and `flag_any_qc_issue` (a run missing any of them
  is rejected, not skipped), and `flag_exclude_authorized` is true exactly for `qc_action == "exclude_core_invalid"`,
  `flag_review_recommended` exactly for `review_*` actions, `flag_any_qc_issue`
  exactly for any action but `retain`, and no exclusion is authorized for a row
  without `flag_core_invalid`;
- for runs that record `build$implementation_hash`, the `by_region`, `by_month`,
  `by_facility`, `by_council`, and `by_district` summaries equal a recomputation
  from the flagged data. Older runs skip this because their summaries were
  computed by earlier rules.

It does not re-run the model or the rules, so it cannot show that the flags are
what the stated configuration would produce, only that the stored pieces are
mutually consistent and unaltered. A self-consistent run altered together with
its manifest is not detected: the manifest is not signed.

RDS files contain the complete flagged dataset and may therefore be
confidential. They must be stored under the same access controls as the input
data and must not be committed to the package repository.
