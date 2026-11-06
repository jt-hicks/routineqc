
<!-- README.md is generated from README.Rmd. Please edit that file -->

# routineqc

<!-- badges: start -->

<!-- badges: end -->

The goal of routineqc is to provide reusable quality-control tools for
routine health facility-month surveillance data. It is designed to help
identify likely data-entry anomalies. Rows are never dropped and no
value is overwritten: QC flags are appended, and a count as originally
supplied stays available in `tested_source` / `positive_source` (it is
translated to numbers beside it, see below).

## Status: experimental (0.1.0)

`routineqc` 0.1.0 is an **experimental release candidate**. APIs, QC
thresholds, and the action policy may still change before a stable
release. Treat every output as an aid to review, not a validated
production tool.

- **Anomaly flags are evidence for review, not automatic proof of a
  data-entry error.** Only impossible core tested/positive counts are
  auto-excluded under the default `conservative_review` policy (action
  policy version 3, configuration schema 6); attendance, coverage,
  statistical, temporal, and tested-volume signals remain review-only. A
  missing count is reporting, not error, and is neither flagged nor
  excluded.
- **“Operational” means a trailing tested-volume baseline only.** The
  prevalence GAM is fitted to all rows including later months, and the
  temporal isolation check reads the following month, so prevalence
  flags can change when later data arrive. The dispersion-adjusted
  all-negative rule is a heuristic that awaits calibration.
- The prevalence model, tested-volume baselines, and action policy have
  been validated against synthetic fixtures only; no upstream production
  pipeline has yet been integrated (see `MIGRATION.md`).
- The Run Explorer is read-only: there is no reviewer decision,
  annotation, or audit-trail data model yet.
- Saved QC runs (the `.rds` file and its manifest) contain the complete
  flagged dataset, including facility identifiers, and must be treated
  as confidential, equivalent to source data. The fingerprint recorded
  in provenance is an identifier, not anonymization.

See `docs/ACTION_POLICY.md`, `docs/TESTED_VOLUME_BASELINES.md`,
`docs/PREVALENCE_MODEL.md`, and `docs/RUN_EXPLORER.md` for full policy,
model, and privacy detail.

## Quick start

From the package directory, run this in an R terminal:

``` r
# install.packages('pak')
pak::pkg_install('.')
system2('Rscript', c('scripts/smoke_test_routineqc.R', '--launch'))
```

The smoke test builds a synthetic QC run, checks that the package can
complete the main workflow, and opens the local QC Run Explorer when
`--launch` is included. The app listens on a localhost URL; keep the R
terminal open while you review it in the browser, then stop the process
when finished.

## Installation

Install from source in this repository:

``` r
# install.packages("pak")
pak::pkg_install("path/to/routineqc")
```

### Installing this exact release candidate

External testers exercising a specific tagged release (for example, a
reviewer following up on the 0.1.0 release candidate) should install
from that tag rather than an untagged checkout, so results are
reproducible:

``` r
# git clone https://github.com/jt-hicks/routineqc.git
# cd routineqc && git checkout v0.1.0

# install.packages('pak')
pak::pkg_install('.')
```

Then confirm the install and exercise the package end to end with the
non-interactive smoke test, adding `--launch` to also open the local Run
Explorer:

``` r
system2('Rscript', c('scripts/smoke_test_routineqc.R', '--launch'))
```

Report any install or smoke-test failure against the exact commit or tag
checked out.

## Minimal simulation example

``` r
library(routineqc)

sim <- simulate_qc_data(n_facilities = 20, n_months = 24, seed = 42)
head(sim)
#> # A tibble: 6 × 8
#>   facility region   district   council month      tested positive injected_error
#>   <chr>    <chr>    <chr>      <chr>   <date>      <dbl>    <dbl> <lgl>         
#> 1 F001     Region_A District_2 Counci… 2022-01-01    119       36 FALSE         
#> 2 F001     Region_A District_2 Counci… 2022-02-01    154       46 FALSE         
#> 3 F001     Region_A District_4 Counci… 2022-03-01    173       43 FALSE         
#> 4 F001     Region_A District_1 Counci… 2022-04-01    150       31 FALSE         
#> 5 F001     Region_B District_2 Counci… 2022-05-01    102       26 FALSE         
#> 6 F001     Region_D District_5 Counci… 2022-06-01    162       29 FALSE
```

## Run routine QC

``` r
qc <- suppressWarnings(run_routine_qc(
  raw_data = sim,
  facility_var = "facility",
  region_var = "region",
  council_var = "council",
  district_var = "district",
  month_var = "month",
  tested_var = "tested",
  positive_var = "positive",
  nthreads = 1
))

names(qc$summaries)
#> [1] "by_region"                      "by_council"                    
#> [3] "by_district"                    "by_month"                      
#> [5] "by_facility"                    "by_facility_reporting"         
#> [7] "count_completeness_by_district" "count_completeness_by_council"
qc$summaries$by_region
#> # A tibble: 4 × 21
#>   region   total_rows incomplete_count_rows n_any_qc_issue pct_any_qc_issue
#>   <chr>         <int>                 <int>          <int>            <dbl>
#> 1 Region_A        130                     0              6             4.62
#> 2 Region_B        141                     0              2             1.42
#> 3 Region_C        103                     0              6             5.83
#> 4 Region_D        106                     0              4             3.77
#> # ℹ 16 more variables: core_invalid_rows <int>, attendance_issue_rows <int>,
#> #   prevalence_extreme_rows <int>, tested_volume_extreme_rows <int>,
#> #   tested_only_issues <int>, prevalence_only_issues <int>,
#> #   combined_tested_prevalence_issues <int>, authorized_exclusion_rows <int>,
#> #   review_recommended_rows <int>, sensitivity_flags <int>,
#> #   total_tested_before_qc <dbl>, total_positive_before_qc <dbl>,
#> #   total_tested_after_authorized_exclusions <dbl>, …
qc$manifest$model_prediction_coverage
#> [1] 1
table(qc$data_flagged$prediction_status)
#> 
#>               assessed ineligible_core_counts 
#>                    475                      5
```

## Assess reporting consistency

Record-level QC asks whether a value is plausible. Reporting consistency
asks whether a facility reports at all, and how continuously. Absent
months, months reported as zero, and months with a missing tested value
are counted separately and combined into months without testing:

``` r
gappy <- simulate_qc_data(
  n_facilities = 8, n_months = 24, seed = 42,
  zero_tested_fraction = 0.05, absent_month_fraction = 0.05
)
prepared <- prepare_qc_data(
  gappy, 'facility', 'region', 'month', 'tested', 'positive'
)

consistency <- summarise_qc_reporting_consistency(prepared)
consistency[c(
  'facility_id', 'months_with_testing', 'months_absent', 'months_zero_tested',
  'months_without_testing', 'longest_gap_months', 'never_tested'
)]
#> # A tibble: 8 × 7
#>   facility_id months_with_testing months_absent months_zero_tested
#>   <chr>                     <int>         <int>              <int>
#> 1 F001                         20             1                  3
#> 2 F002                         21             2                  1
#> 3 F003                         23             0                  1
#> 4 F004                         22             0                  2
#> 5 F005                         20             3                  1
#> 6 F006                         24             0                  0
#> 7 F007                         23             1                  0
#> 8 F008                         19             3                  2
#> # ℹ 3 more variables: months_without_testing <int>, longest_gap_months <int>,
#> #   never_tested <lgl>
```

Gap length uses each facility’s own reporting window, so a late joiner
is not penalised for months before it existed. Dataset-wide proportions are also
reported as context, but they are not used to filter facilities:

``` r
consistency[c(
  'facility_id', 'first_month_tested', 'last_month_tested',
  'prop_without_testing_facility_window', 'prop_with_testing_dataset_window'
)]
#> # A tibble: 8 × 5
#>   facility_id first_month_tested last_month_tested prop_without_testing_facili…¹
#>   <chr>       <date>             <date>                                    <dbl>
#> 1 F001        2022-01-01         2023-12-01                               0.167 
#> 2 F002        2022-01-01         2023-12-01                               0.125 
#> 3 F003        2022-01-01         2023-12-01                               0.0417
#> 4 F004        2022-01-01         2023-12-01                               0.0833
#> 5 F005        2022-01-01         2023-12-01                               0.167 
#> 6 F006        2022-01-01         2023-12-01                               0     
#> 7 F007        2022-01-01         2023-12-01                               0.0417
#> 8 F008        2022-01-01         2023-12-01                               0.208 
#> # ℹ abbreviated name: ¹​prop_without_testing_facility_window
#> # ℹ 1 more variable: prop_with_testing_dataset_window <dbl>
```

Named strictness levels select a review cohort, from `any_testing` (any
facility with at least one month of testing) to `complete` (testing in every
month of the facility's own observed window). `complete_panel` remains a legacy
alias for `complete`. Other thresholds remain overridable:

``` r
qc_consistency_levels()
#> # A tibble: 7 × 6
#>   level          max_prop_without_testing max_gap_months min_months_with_testing
#>   <chr>                             <dbl>          <dbl>                   <dbl>
#> 1 all                               NA                NA                      NA
#> 2 any_testing                       NA                NA                       1
#> 3 lenient                            0.5               3                       1
#> 4 moderate                           0.2               2                       1
#> 5 near_complete                      0.05              1                       1
#> 6 complete                           0                 0                       1
#> 7 complete_panel                     0                 0                       1
#> # ℹ 2 more variables: min_prop_with_testing_dataset_window <dbl>,
#> #   max_prop_incomplete_counts <dbl>

vapply(
  qc_consistency_levels()$level,
  function(level) {
    cohort <- summarise_qc_reporting_consistency(prepared, strictness = level)
    sum(cohort$passes_strictness)
  },
  integer(1)
)
#>            all    any_testing        lenient       moderate  near_complete 
#>              8              8              8              7              3 
#>       complete complete_panel 
#>              1              1
```

Cohort selection is not exclusion. A facility outside the cohort was not
reviewed under it, which is not a finding that its records are wrong,
and `flag_exclude_authorized` is unaffected. See
`docs/REPORTING_CONSISTENCY.md`.

A missing `tested` or `positive` count is not flagged on the row either.
It leaves prevalence undefined, so it is measured as count completeness
per facility or area instead:

``` r
prepared$positive[c(3, 17, 40)] <- NA
summarise_qc_count_completeness(prepared, strictness = 'near_complete')
#> # A tibble: 8 × 7
#>   facility_id rows_with_any_count rows_complete_counts rows_missing_positive
#>   <chr>                     <int>                <int>                 <int>
#> 1 F001                         23                   21                     2
#> 2 F002                         22                   21                     1
#> 3 F003                         24                   24                     0
#> 4 F004                         24                   24                     0
#> 5 F005                         21                   21                     0
#> 6 F006                         24                   24                     0
#> 7 F007                         23                   23                     0
#> 8 F008                         21                   21                     0
#> # ℹ 3 more variables: rows_missing_tested <int>, prop_incomplete_counts <dbl>,
#> #   passes_completeness <lgl>
```

## Adapt pipeline data

Pipeline-specific names and conventions belong in an adapter, outside
the QC engine. A generic declarative adapter is included:

``` r
adapted <- adapt_qc_data(
  pipeline_data,
  mapping = list(
    facility_id = 'org_unit_id', month_date = 'reporting_month',
    region = 'province', tested = 'malaria_tested',
    positive = 'malaria_positive'
  ),
  identity_strategy = 'DHIS2 organisation unit ID',
  adapter = 'example_pipeline', adapter_version = '0.1.0',
  dataset_id = 'curated-2026-08', source_system = 'DHIS2'
)

adapted$diagnostics
qc <- run_qc_adapter(adapted)
```

Adapter errors block QC without deleting records. Count-conversion
problems do not block QC and no value is repaired. Each count gets a
status (`tested_parse_status`, `positive_parse_status`): `missing` (not
reported) is distinguished from `malformed`, `non_finite`, and
`fractional` values. The original is kept in `tested_source` /
`positive_source`, factors are read by label, and a value that is
reported but not a valid whole number is flagged
`flag_count_invalid_value` for review.

The default `conservative_review` policy authorizes exclusion only for
impossible core tested/positive records. Attendance contradictions and
statistical, temporal, or tested-volume anomalies remain review-only.

Use a flags-only run when all exclusion decisions must remain external:

``` r
flags_only <- qc_config(action_policy = list(name = 'flags_only'))
```

Operational tested-volume baselines use only previous calendar months.
For a substantially complete historical dataset, explicitly select the
retrospective profile, which records that future observations were used:

``` r
historical <- qc_config('retrospective')
```

Each result is a validated, versioned run object with deterministic
input and analysis identities and a unique execution identity. Persist
complete runs with a metadata-only JSON manifest:

``` r
write_qc_run(qc, 'qc-runs/example.rds')
restored <- read_qc_run('qc-runs/example.rds')
```

The RDS contains the complete flagged dataset and must be protected like
the source data. The fingerprint is an identifier, not anonymization.

## Explore saved runs locally

The read-only Run Explorer discovers paired artifacts, validates a
selected run, and provides overview, review-queue, diagnostic,
configuration, and provenance views:

``` r
list_qc_runs('qc-runs')
launch_qc_app('qc-runs')
```

The app binds to localhost, performs no writes, and provides no record
download. Treat the local browser session and run directory as
confidential. The review queue offers focused column views with hover
definitions. Facility diagnostics compare observed prevalence with
stored GAM expectations without rerunning the model.

The Overview combines a mutually exclusive QC-action donut, an
individual-reason bar chart, and facility and district burden summaries.
A row with multiple QC reasons contributes once to each applicable
reason bar, so reason counts need not sum to the number of rows. The
always-visible flow diagram explains how evidence is mapped to retain,
review, or authorized exclusion actions.

Interactive diagnostics expose row details on hover and compare observed
prevalence and testing volume with their stored expectations. The
District tab opens with a district total time series summed across all
reporting facilities, above faceted facility time series. One measure is
shown at a time, and the same prevalence or number-tested selection
drives both, when the upstream adapter supplies district. Facets for
facilities with no testing data can be hidden.

The Reporting consistency tab measures how continuously each facility
reports and narrows the review queue to a chosen strictness cohort. Both
are computed from the flagged data, so runs saved by earlier versions
still open.

To exercise the complete package with synthetic data before connecting a
real pipeline, run from the package repository:

``` sh
Rscript scripts/smoke_test_routineqc.R
```

The smoke fixture contains three districts with eight facilities each
and 12 to 24 months per facility. Pass an output directory as the first
argument and add `--launch` to open the generated run immediately.

routineqc appends QC outputs as additional columns rather than deleting
records, and keeps each tested and positive value as supplied in a
`*_source` column. The numeric `tested` / `positive` are translations of
those, not the originals.

Anomaly flags are evidence for review, not automatic proof of error. See
`docs/ACTION_POLICY.md` and `docs/TESTED_VOLUME_BASELINES.md` for policy
and baseline limitations, `docs/PREVALENCE_MODEL.md` for model
assessment semantics, `docs/ADAPTERS.md` for the pipeline boundary, and
`docs/RUN_ARTIFACTS.md` for persistence guidance. See
`docs/RUN_EXPLORER.md` for application scope and privacy constraints.

## Notes

You can regenerate README.md from this file with:

``` r
devtools::build_readme()
```
