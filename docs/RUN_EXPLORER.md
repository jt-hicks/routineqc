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

The dashboard has four views and a run-details page. The original panels are
unchanged and now sit inside them.

- **Overview** opens on the *Landing page*, with record shares by review
  priority, regional and monthly flag rates, and
  explanations of the QC process and priority levels. The national chart uses
  the Moderate observation scenario and compares reported aggregates, aggregates
  with targeted counts left missing, and aggregates with those counts imputed.
  Switch between positivity, tested, attendance and testing coverage. Imputation
  fits retrospective expected values once per selected run and shows how many
  targeted counts were filled. Use *Filter impact* to compare other scenarios
  or run repeated predictive imputation with uncertainty intervals.
- **Data usability** (under *Overview*): how much of
  the run can be assessed, and where review effort concentrates. Each check
  reports records assessed and not flagged, assessed and flagged, and not
  assessed (missing or unusable inputs, insufficient history, prediction
  unavailable). A record a check could not assess is never shown as passing, and
  there is no composite quality score. Expected facility-months are inferred
  from each facility's first to last report unless a schedule is supplied; the
  page says so.
- **Explore data** (*Explorer*, then *Review queue*, *Diagnostics*, *District
  grid*): cascading National, Region, Council or District, Facility navigation
  with a breadcrumb and reset. Four measures (positivity, tested, attendance,
  coverage) are computed as totals over matched records, never as averages of
  facility percentages. Months without a valid value are gaps, not zeros. A
  facility selection shows its reporting states (reported, zero tested, missing
  count, invalid count, absent) and marks flagged records; clicking a mark, or a
  row of the review table, explains the flags with the thresholds of the run.
  The table of units one level down drills down when a row is selected.
- **Filter impact:** how hypothetical observation-level QC choices change aggregate
  estimates. All preset scenarios include facilities regardless of reporting
  completeness; flags omit only affected counts in each facility-month. See
  `docs/ANALYTICAL_SCENARIOS.md`. The earlier *Reporting
  consistency* panel sits here too.
- **QC guide:** every flag, by issue family, searchable, with the thresholds
  actually used by the selected run (runs saved without some settings show the
  current defaults and say so), the context modifiers, and the flag, priority and
  action distinction.
- **Run details:** configuration, provenance and the *Attendance* panel.

The earlier panels are described below.

- **Action summary:** a compact run summary, a QC-action donut across all rows, and a
  separate bar chart counting each individual QC reason among affected rows.
  Rows with multiple reasons contribute to each applicable reason bar; the
  reason chart therefore describes evidence frequency rather than a mutually
  exclusive partition. Facility burden groups facilities by affected-row
  count. District
  burden ranks districts by the number or proportion of facilities exceeding
  an adjustable affected-row threshold. Both burden views can switch among
  review-or-exclusion, review only, and authorized exclusion only. An always-visible
  flow diagram explains the tested/positive validity, attendance, coverage,
  prevalence, temporal, and tested-volume evidence used by action policy
  version 3, including the configurable per-flag priority.
- **Reporting consistency:** facility-level reporting behavior measured from the
  flagged data, covering months without testing, their absent, zero, and missing
  components, the longest gap without testing, and coverage of both the facility
  reporting window and the dataset-wide month span (descriptive only, not a
  filtering criterion). A strictness selector with
  optional threshold overrides reports how many facilities fall inside the chosen
  cohort, and a chart groups facilities by their longest gap. Strictness selects a
  review cohort; it never authorizes exclusion and never changes the stored run.
  Count completeness, the share of rows missing exactly one of tested and
  positive, is shown and filtered alongside. See `docs/REPORTING_CONSISTENCY.md`.
- **Attendance:** where tested exceeds the attendance denominator. Proportions are
  over rows reporting both counts, never all rows. Shows the attendance
  definition the run carries, an overall summary, the monthly proportion, and a
  table by district, region, or facility ranked by that proportion. Runs without
  attendance data say so.
- **Review queue:** display-only filters for action, priority, geography,
  facility, reason, prediction status, reporting dates, and reporting-consistency
  cohort. Focused column
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
- **District:** a district total time series summed across all reporting
  facilities, above a vertically scrollable set of interactive, faceted facility
  time series. One measure is shown at a time, and the same prevalence or
  number-tested selection drives both the district total and the facets. The
  district total is computed from the source data before authorized exclusions.
  An optional toggle hides facets for facilities with no testing data in any
  month. Red marks identify rows recommended for review
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
