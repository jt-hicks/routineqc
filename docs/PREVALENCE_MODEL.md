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

The structure is simplified before the fit is abandoned. With a single region the
region term and `by = region` are dropped. The seasonal smooth is kept only when
every calendar month has at least five valid rows, and the time smooth only with
at least six distinct months; a warning names what was dropped. If no structure
can be fitted, `fit_prevalence_gam()` returns `NULL` with a warning and
`run_routine_qc()` carries on: every row is `model_unavailable`, model-derived
flags stay false, and the logical, tested-volume, attendance, and count-only
rules still run.

## Statistical scope: what "operational" covers

"Operational" describes the trailing tested-volume baseline only: it uses
earlier months, so those flags reflect what was knowable at the reporting date.
It is not a property of the whole run. The GAM is fitted to every valid row,
including months after the one being assessed, and the temporal isolation check
(`flag_isolated_extreme`) reads the following month. A row's
prevalence flags can therefore change when later data are appended, and a run
must not be described as using only information available at the time.

## Row-level assessment status

Every input row is preserved. `model_assessment_eligible` identifies rows with
valid core counts, both `tested` and `positive` present, positive tested count,
and complete model predictors.
`model_assessed` is true only when a finite prediction was obtained.
`prediction_status` records one of:

- `assessed`
- `ineligible_core_counts`
- `ineligible_invalid_count`: `tested` or `positive` was supplied but is
  malformed, non-finite or fractional. Review-only; see
  `tested_parse_status` / `positive_parse_status`.
- `ineligible_missing_counts`: `tested` or `positive` is missing. This is not a
  QC finding; see `core_count_status` and `summarise_qc_count_completeness()`.
- `ineligible_nonpositive_tested`
- `ineligible_missing_predictor`
- `model_unavailable`: no model could be fitted. Given only to rows that would
  otherwise be eligible; every other row keeps its own ineligibility status.
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

The extra variation is not the same for every month: it grows with the tested
count. `resid_rho` describes it as a beta-binomial intra-month correlation, so
that a month of `n` tested has overdispersion `1 + (n - 1) * rho`. It is a
moment estimate, `sum(r^2 - 1) / sum(tested - 1)` over rows with more than one
tested and `|r| < 5`, and the manifest records it as
`prevalence_overdispersion_rho`. On first-ANC data from one zone, rho was 0.037
against a run-wide dispersion of 2.8: about 1.15 at 5 tested, 1.7 at 20, 2.8
at 50 and 4.6 at 100.

The all-negative and all-positive rules use it. A month with no positives (or
only positives) is flagged only when its beta-binomial chance under the model,
with that rho, is below `all_negative_max_probability` (or
`all_positive_max_probability`). Fifty negative tests are unremarkable where the
model expects 2% positivity and implausible where it expects 20%. Runs made
before `resid_rho` used `(1 - p_hat)^(tested / resid_dispersion)`, which
deflates every month by the run-wide dispersion and so was about 2.4 times too
lenient for months of 5 to 9 tested. The probability is model-based, not
calibrated against labelled data, and the default threshold of 0.001 should not
be read as a false-positive rate.

The residual rule still divides by the run-wide dispersion, so it is likely too
lenient for small months and too strict for large ones. A size-aware residual
is a known limitation, not yet adopted, because it would change every
prevalence flag.

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
