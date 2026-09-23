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
valid core counts, both `tested` and `positive` present, positive tested count,
and complete model predictors.
`model_assessed` is true only when a finite prediction was obtained.
`prediction_status` records one of:

- `assessed`
- `ineligible_core_counts`
- `ineligible_missing_counts`: `tested` or `positive` is missing. This is not a
  QC finding; see `core_count_status` and `summarise_qc_count_completeness()`.
- `ineligible_nonpositive_tested`
- `ineligible_missing_predictor`
- `model_unavailable`
- `unseen_region`
- `unseen_facility`
- `prediction_error`
- `non_finite_prediction`

For every non-assessed row, `p_hat`, `expected_positive`, and `pearson_resid`
remain `NA`; model-derived residual flags therefore remain false.

## Dispersion

The binomial family fixes the scale at 1. Routine facility-months are usually
more variable than that, and every Pearson residual is then inflated by the
square root of the dispersion. The inflation compounds with volume: extra-binomial
variation grows with the tested count, so an unadjusted cutoff flags mostly the
largest facilities.

`resid_dispersion` is the Pearson chi-square over residual degrees of freedom of
the fit, floored at 1 so that an underdispersed fit never enlarges a residual.
`pearson_resid_scaled` is `pearson_resid / sqrt(resid_dispersion)`, and the
residual rule applies its cutoffs to it. The raw residual is kept beside it. The
manifest records the value as `prevalence_dispersion`.

On 452,755 ANC facility-months the dispersion was 3.47, and the unscaled rule
flagged 3.3% of assessed rows at tested >= 20 against a nominal 0.006%, rising to
14% at 200 or more tested.

The all-negative rule uses the same dispersion. A month with no positives is
flagged only when `(1 - p_hat)^(tested / resid_dispersion)`, the chance of zero
positives with the tested count deflated by the dispersion, is below
`all_negative_max_probability`. Fifty negative tests are unremarkable where the
model expects 2% positivity and implausible where it expects 20%.

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
