# Read-only QC Run Explorer

The Run Explorer is a local Shiny application for understanding persisted QC
runs. It does not ingest raw source data, rerun QC, edit observations, resolve
review items, or change action policy.

## Launch

```r
list_qc_runs('qc-runs')
launch_qc_app('qc-runs')
```

`list_qc_runs()` reads JSON metadata only. It reports complete pairs, missing
RDS files, missing manifests, invalid manifests, and unsupported schemas. The
full RDS is loaded only after selection and must pass `read_qc_run()` integrity
validation.

The launcher always binds to `127.0.0.1`. It performs no writes, makes no network
requests, and includes no download control. Run directories and browser sessions
must still be treated according to the confidentiality of the source data.

## Views

- **Overview:** dataset identity, row/facility counts, exclusion and review
  counts, prediction coverage, profile, policy, and run identities.
- **Review queue:** display-only filters for action, priority, geography,
  facility, reason, prediction status, and reporting dates.
- **Diagnostics:** facility time series, prevalence residuals, tested-volume
  robust z-scores, regional burden, and before/after prevalence.
- **Configuration and provenance:** the exact configuration snapshot and safe
  caller-supplied provenance.

Filtering never modifies the stored run or authorizes a new action. Invalid or
tampered RDS/manifest pairs cannot be opened silently.

## Deliberately deferred

The first application version has no reviewer decisions, annotations, edits,
authentication, remote deployment, run comparison, raw-data upload, or report
download. Editable review requires a separate versioned decision schema,
reviewer identity, timestamps, audit history, and clear separation from source
observations before it can be added safely.
