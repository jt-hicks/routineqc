# Facility reporting consistency

Record-level QC asks whether a reported value is plausible. Reporting-consistency
measurement asks a different question: whether a facility reports at all, and how
continuously. A facility that submits a handful of months, or stops reporting for a
year, is a data-collection problem rather than a data-entry anomaly, and the two must
not be confused.

`summarise_qc_reporting_consistency()` measures reporting behavior.
`filter_qc_facilities()` selects a review cohort from it. Neither assigns a QC action,
sets a row-level flag, drops a record, or authorizes exclusion.

## What counts as a month with testing

A month has testing when a row exists for that facility-month **and** `tested` is
greater than zero. Three distinct situations count as a month *without* testing, and
each is also reported separately so the underlying evidence stays visible:

- `months_absent`: no row exists for that facility-month at all;
- `months_zero_tested`: a row exists reporting zero, or an unusual non-positive count;
- `months_missing_tested`: a row exists but `tested` is missing.

These partition the reporting window exactly:

```text
months_reported          = months_with_testing + months_zero_tested + months_missing_tested
months_in_facility_window = months_reported + months_absent
months_without_testing    = months_in_facility_window - months_with_testing
```

A month reported as zero and a month never reported are different failures of data
collection. They are counted separately for that reason, and combined only in
`months_without_testing`.

## Two windows, deliberately

Gap length and the within-window proportion are measured against each facility's
**own reporting window**, from its first to its last reported month. A facility that
joined the programme late is not penalised for months before it existed.

That alone would hide a facility with three internally complete months, so
proportions are also reported against the **dataset-wide month span**:

| Column | Denominator | Answers |
|---|---|---|
| `prop_without_testing_facility_window` | facility window | How reliable is this facility while active? |
| `prop_with_testing_dataset_window` | dataset span | How much of the study period did it actually contribute? |
| `prop_reported_dataset_window` | dataset span | How much of the study period did it report at all? |

`first_month_reported`/`last_month_reported` bound the window; `first_month_tested`/
`last_month_tested` say when data collection actually started and stopped, which can
differ when a facility reports zeros before its first real test or after its last.

## Gaps

`longest_gap_months` is the longest run of consecutive months without testing inside
the facility window, counting absent, zero, and missing months alike.
`longest_gap_start` and `longest_gap_end` locate it, and `n_gaps` counts how many
separate runs exist. A facility that never tested has one gap spanning its whole
window and `never_tested` is true.

Gaps are measured on a complete calendar-month grid, so a missing row and a reported
zero produce the same gap length. This is intentional: from a data-availability
standpoint they are the same loss.

## Strictness levels

`qc_consistency_levels()` is the single source of truth for the presets, shared by the
functions, the Run Explorer control, and this document. `NA` means a criterion is not
applied. A facility passes when every applied criterion holds.

| Level | Max proportion without testing | Max gap | Min months with testing | Min dataset-span coverage |
|---|---|---|---|---|
| `all` | - | - | - | - |
| `any_testing` | - | - | 1 | - |
| `lenient` | 0.50 | 3 | 1 | 0.25 |
| `moderate` | 0.20 | 2 | 1 | 0.50 |
| `near_complete` | 0.05 | 1 | 1 | 0.75 |
| `complete` | 0 | 0 | 1 | - |
| `complete_panel` | 0 | 0 | 1 | 1.00 |

`any_testing` is the most lenient useful level: it admits any facility with at least
one month of testing, so it excludes only facilities that never tested. `complete`
means no month without testing inside the facility's own window. `complete_panel`
additionally requires testing in every month of the dataset span and is the most
severe.

Every threshold is overridable, so the presets are a starting point rather than
policy:

```r
summarise_qc_reporting_consistency(qc$data_flagged, strictness = 'moderate')
filter_qc_facilities(qc$data_flagged, strictness = 'all', max_gap_months = 2)
```

**The levels are not strictly nested.** A facility with a short but internally
complete history passes `complete` while failing `moderate`, because `complete`
judges only its own window and `moderate` also requires dataset-span coverage. Choose
the level that matches the question being asked, rather than assuming a single
severity ordering.

## Cohort selection is not exclusion

A facility outside the selected cohort was not reviewed under that cohort. That is not
a finding that its records are wrong, and it does not authorize removing them.
`flag_exclude_authorized` remains the only field in the package that authorizes
exclusion, and it is unaffected by any strictness setting. See
`docs/ACTION_POLICY.md`.

Cohort choice is therefore an analysis decision that must be reported alongside any
result derived from it. A prevalence estimate computed over a `near_complete` cohort
is a different quantity from one computed over all facilities, and describing either as
"the QC'd data" is misleading.

## Limitations

- A facility that never reported at all is invisible: it has no rows, so it cannot
  appear in these measurements. Only the upstream facility list can reveal it.
- A reported zero may be correct. Some facilities genuinely test nobody in a month.
- The dataset-span denominator depends on which months the extract covers, so
  coverage proportions are not comparable across extracts with different spans.
- Because gaps count absent and zero months identically, a facility that reports
  diligent zeros scores the same as one that submits nothing. The component counts
  distinguish them; the combined gap does not.
- Reporting consistency says nothing about whether reported values are accurate. A
  facility can report every month, consistently, and still report wrong numbers.
