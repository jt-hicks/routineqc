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

- **Overview:** a compact run summary, a QC-action donut across all rows, and a
  separate bar chart counting each individual QC reason among affected rows.
  Rows with multiple reasons contribute to each applicable reason bar; the
  reason chart therefore describes evidence frequency rather than a mutually
  exclusive partition. Facility burden groups facilities by affected-row
  count. District
  burden ranks districts by the number or proportion of facilities exceeding
  an adjustable affected-row threshold. Both burden views can switch among
  review-or-exclusion, review only, and authorized exclusion only. An always-visible
  flow diagram explains the tested/positive validity, attendance, prevalence,
  temporal, and tested-volume evidence used by the action policy.
- **Review queue:** display-only filters for action, priority, geography,
  facility, reason, prediction status, and reporting dates. Focused column
  views cover review essentials, counts, model assessment, and tested-volume
  assessment; an all-fields view remains available. Hovering over a heading
  shows its definition. Date filtering is opt-in, the displayed row count is
  explicit, and one button resets all queue filters.
- **Diagnostics:** interactive selected-facility prevalence and tested-count
  time series, prevalence residuals, tested-volume robust z-scores, and
  before/after prevalence. Facility plots compare observations with the
  already-stored GAM expectation or tested-volume rolling baseline; they do not
  refit either model. Hover text exposes the available facility, geography,
  count, model, action, priority, and reason fields for each point.
- **District:** a vertically scrollable set of interactive, faceted facility
  time series. The measure can switch between prevalence and number tested.
  Red marks identify rows recommended for review
  or authorized exclusion. Runs without a district field display an explanatory
  message instead of failing.
- **Configuration and provenance:** the exact configuration snapshot and safe
  caller-supplied provenance.

Filtering never modifies the stored run or authorizes a new action. Invalid or
tampered RDS/manifest pairs cannot be opened silently.

“Affected” in the facility and district burden views means either
`flag_review_recommended` or `flag_exclude_authorized`. The controls expose
each component separately. Review recommendations remain distinct from
authorized exclusions throughout the application.

## Deliberately deferred

The first application version has no reviewer decisions, annotations, edits,
authentication, remote deployment, run comparison, raw-data upload, or report
download. Editable review requires a separate versioned decision schema,
reviewer identity, timestamps, audit history, and clear separation from source
observations before it can be added safely.
