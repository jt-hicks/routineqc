
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
#> # A tibble: 4 × 18
#>   region   total_rows n_any_qc_issue pct_any_qc_issue invalid_logical_rows
#>   <chr>         <int>          <int>            <dbl>                <int>
#> 1 Region_A        130             19            14.6                     2
#> 2 Region_B        141              9             6.38                    1
#> 3 Region_C        103             14            13.6                     2
#> 4 Region_D        106              6             5.66                    0
#> # ℹ 13 more variables: prevalence_extreme_rows <int>,
#> #   tested_volume_extreme_rows <int>, tested_only_issues <int>,
#> #   prevalence_only_issues <int>, combined_tested_prevalence_issues <int>,
#> #   recommended_primary_removals <int>, sensitivity_flags <int>,
#> #   total_tested_before_qc <dbl>, total_positive_before_qc <dbl>,
#> #   total_tested_after_primary_qc <dbl>, total_positive_after_primary_qc <dbl>,
#> #   prevalence_before_qc <dbl>, prevalence_after_primary_qc <dbl>
```

routineqc preserves raw data and appends QC outputs as additional
columns rather than deleting records.

Flagged records should be interpreted as records for review and
sensitivity analysis, not automatic proof of error.

## Notes

You can regenerate README.md from this file with:

``` r
devtools::build_readme()
```
