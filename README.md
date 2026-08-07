
<!-- README.md is generated from README.Rmd. Please edit that file -->

# routineqc

<!-- badges: start -->

<!-- badges: end -->

The goal of routineqc is to provide reusable quality-control tools for
routine health facility-month surveillance data. It is designed to help
identify likely data-entry anomalies while preserving raw records and
appending QC flags.

## Installation

Install from source in this repository:

``` r
# install.packages("pak")
pak::pkg_install("path/to/routineqc")
```

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
qc <- run_routine_qc(
  raw_data = sim,
  facility_var = "facility",
  region_var = "region",
  council_var = "council",
  district_var = "district",
  month_var = "month",
  tested_var = "tested",
  positive_var = "positive",
  nthreads = 1
)
#> Warning: Default GAM basis dimensions were too large for the available data;
#> refitting with k_month=11 and k_time=23.

names(qc$summaries)
#> [1] "by_region"   "by_council"  "by_district" "by_month"    "by_facility"
qc$summaries$by_region
#> # A tibble: 4 × 20
#>   region   total_rows n_any_qc_issue pct_any_qc_issue core_invalid_rows
#>   <chr>         <int>          <int>            <dbl>             <int>
#> 1 Region_A        130             10             7.69                 2
#> 2 Region_B        141              3             2.13                 1
#> 3 Region_C        103              7             6.80                 2
#> 4 Region_D        106              5             4.72                 0
#> # ℹ 15 more variables: attendance_issue_rows <int>,
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
warnings do not block QC: the converted `NA` values remain available to
the logical checks.

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
confidential.

To exercise the complete package with synthetic data before connecting a
real pipeline, run from the package repository:

``` sh
Rscript scripts/smoke_test_routineqc.R
```

Pass an output directory as the first argument and add `--launch` to
open the generated run immediately.

routineqc preserves raw data and appends QC outputs as additional
columns rather than deleting records.

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
