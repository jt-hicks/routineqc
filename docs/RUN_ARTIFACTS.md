# Reproducible QC run artifacts

`run_routine_qc()` returns a validated `routineqc_run`. Run schema version 1
contains flagged data, the fitted model (or `NULL`), summaries, the exact
configuration, and a metadata manifest.

## Identities

- `input_fingerprint` is a deterministic SHA-256 digest of normalized canonical
  fields. Row order does not affect it; changes to relevant values do.
- `analysis_id` deterministically combines the input fingerprint, configuration
  fingerprint, package version, and run schema.
- `execution_id` is unique to one execution, even when analysis identity is the
  same.

A fingerprint is an identifier, not encryption or anonymization. It does not
make confidential source data safe to share.

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

RDS files contain the complete flagged dataset and may therefore be
confidential. They must be stored under the same access controls as the input
data and must not be committed to the package repository.
