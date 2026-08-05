# Prevalence model contract

The prevalence model provides statistical evidence for review. It does not
authorize record removal. Deterministic logical QC continues even when the
model cannot be fitted or a row cannot be predicted.

## Authoritative formula

The package fits a binomial GAM to logically valid rows with `tested > 0`:

```r
cbind(positive, tested - positive) ~
  region +
  s(month_num, by = region, bs = 'cc') +
  s(time_index) +
  s(facility_id, bs = 're')
```

This includes a region fixed effect, region-specific cyclic seasonality, a
long-term time smooth, and a facility random effect. Basis dimensions may be
reduced when the data do not support the defaults. Fewer than 50 valid rows or
fewer than two facilities produces no model rather than an unstable fit.

## Row-level assessment status

Every input row is preserved. `model_assessment_eligible` identifies rows with
valid core counts, positive tested count, and complete model predictors.
`model_assessed` is true only when a finite prediction was obtained.
`prediction_status` records one of:

- `assessed`
- `ineligible_core_counts`
- `ineligible_nonpositive_tested`
- `ineligible_missing_predictor`
- `model_unavailable`
- `unseen_region`
- `unseen_facility`
- `prediction_error`
- `non_finite_prediction`

For every non-assessed row, `p_hat`, `expected_positive`, and `pearson_resid`
remain `NA`; model-derived residual flags therefore remain false.

## Coverage

Coverage is assessed rows divided by model-eligible rows. Valid rows with unseen
model levels count against coverage; logically ineligible rows do not. The
default configuration warns below 80%, and
`qc_config(model = list(min_prediction_coverage = ...))` changes that threshold.
The run manifest records model availability, eligible and assessed counts,
coverage, and counts by prediction status.

## Limitations

- A GAM expectation is conditional on the available reporting history and may
  reproduce systematic biases in that history.
- Sparse facilities, regions, or seasons can make smooth estimates unstable.
- Basis fallback improves fit resilience but means effective model complexity
  can differ between datasets.
- Predictions near zero and one are bounded for residual calculation; this is a
  numerical safeguard, not evidence that the underlying expectation is certain.
- Low coverage means residual-based QC represents only part of the dataset and
  should be interpreted alongside the recorded status distribution.
