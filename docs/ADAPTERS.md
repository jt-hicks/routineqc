# Pipeline adapter contract

Adapters are the boundary between pipeline-specific data and the reusable QC
engine:

```text
source pipeline -> adapter -> canonical data -> routineqc
```

Source paths, file discovery, worksheet layouts, country-specific hierarchy,
and pipeline cleaning rules do not belong in the core QC functions.

## Required canonical mapping

Every adapter maps source columns to `facility_id`, `month_date`, `region`,
`tested`, and `positive`. It may additionally map `district`, `council`,
`facility_name`, `attending`, and `source_record_id`. Attendance requires one
explicit approved definition.

The facility identity strategy must be documented. Prefer a durable source
system ID, then a reviewed crosswalk ID, then a documented composite key.
Normalized names are a last resort and should be disclosed in
`identity_strategy`.

## Adapter result

`adapt_qc_data()` returns a versioned `routineqc_adapter_result` containing:

- `data`: canonical rows, with the source row count preserved;
- `diagnostics`: source-row references, severity, issue code, and field;
- `provenance`: safe dataset and adapter identifiers;
- `mapping`: the exact canonical-to-source mapping;
- `ready`: whether the result can enter QC.

Errors block `run_qc_adapter()`. They include missing facility identity or
region, missing or unparseable month, and duplicate facility-month keys.
Adapters must not silently aggregate duplicates.

Warnings do not block handoff. Counts are translated without repair by the same
parser as `prepare_qc_data()`. Each tested/positive value carries a status
(`tested_parse_status`, `positive_parse_status`) and its original in
`tested_source` / `positive_source`; factors are read by label, never by integer
code. Diagnostics separate `missing_count` (not reported, not an error),
`unparseable_count` (malformed), `non_finite_count`, and `fractional_count`.
Malformed and non-finite values become `NA`; a fractional value is kept as
supplied. The logical QC protocol flags a malformed, non-finite, or fractional
count as `flag_count_invalid_value` (review only). A missing count is recorded in
`core_count_status`, not flagged.

## Prohibited adapter behavior

An adapter must not:

- delete or aggregate source rows;
- replace missing attendance with tested;
- cap positive at tested;
- impute missing counts;
- turn anomalies into actions or exclusions;
- include credentials, paths, or record-level contents in provenance.

Any upstream correction or aggregation must happen under a separate documented
curation rule before the adapter is called.

## Source-specific adapters

Thin source-specific adapter functions may live with their upstream pipelines
or, after review, in optional package modules. They should call
`adapt_qc_data()` and supply a fixed mapping, semantic definitions, identity
strategy, adapter version, and safe dataset identifier. The generic contract
must not acquire SharePoint, ANC filename, or country-specific assumptions.
