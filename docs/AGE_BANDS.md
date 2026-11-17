# Age-disaggregated data

routineqc accepts data supplied one row per facility, month and age band, such
as first antenatal-care (ANC) attendance by maternal age (`<20` and `>=20`).
Pass the band column as `age_group_var` to `prepare_qc_data()` or
`run_routine_qc()`, or map it as `age_group` in `adapt_qc_data()`.

```r
run <- run_routine_qc(
  data, facility_var = 'facility_id', region_var = 'region', month_var = 'month_date',
  tested_var = 'tested', positive_var = 'positive', attending_var = 'attending',
  attendance_definition = 'first ANC attendance', district_var = 'district',
  age_group_var = 'age_group', config = qc_config('anc_first_visit')
)
```

`simulate_qc_age_data()` produces synthetic data in this shape.

## What changes

- **The row key** is facility, month and age band. A duplicate key is an
  error. The supplied label stays in `age_group`; an ASCII key is added in
  `age_group_code` (`<20` becomes `lt20`, `>=20` or the single-character sign
  becomes `ge20`).
- **Time-series rules run per series**, recorded in `qc_series`: one
  facility's band. Rolling tested baselines, month-to-month changes and
  temporal context compare a band only with its own history.
- **The prevalence model** gains an age-band term, so each band is judged
  against its own expected positivity. The facility effect, season and trend
  are shared across bands.
- **Scenarios and imputation** omit and fill counts band by band; all-band
  estimates are sums across bands. Imputation models each facility's band as
  its own unit.

Data without `age_group_var` are unchanged: a series is a facility.

## Optional columns

These are recognised by name when present (and can be mapped in the adapter).

| Column | Meaning |
|---|---|
| `anc_before_12wk`, `anc_after_12wk` | First-ANC bookings before 12 weeks of gestation, and at 12 weeks or later |
| `positive_zero_filled` | `TRUE` when the positive count was read as zero because every positive band was blank |
| `bands_expected` | Finer source bands making up this row's band: 1 when the source supplies the band directly |
| `tested_bands_reported`, `positive_bands_reported`, `anc_before_12wk_bands_reported`, `anc_after_12wk_bands_reported` | How many of those bands are known: reported, or zero by the source's convention (see below) |
| `source_id`, `facility_name` | Carried through |

## Sources that do not store zeros

DHIS2 aggregate data normally store no zeros: a zero is saved as a blank, so a
single blank cell cannot say whether the value was zero or not reported. For
such a source, decide at the level of the month, not the cell:

- A facility-month is **reported** when any ANC value is non-blank in any band.
  In a reported month every blank is a zero: a band whose cells are all blank
  is 0, not missing, and every finer band counts as known
  (`<count>_bands_reported = bands_expected`).
- A facility-month with no ANC value at all stays **missing**.
- `positive_zero_filled` is `TRUE` only when the band's positive count came
  entirely from blanks, never because one of several finer bands was blank.

With this convention the partial-sum and band-count flags do not fire on such
a source; they remain for sources where a blank does mean not reported. A form
submitted with a section left blank is caught by the ordinary rules instead:
zero tested at a busy facility, very low testing coverage, a large drop in
tested, or no positives where the model expects some.

## Change of source

When `bands_expected` changes between 1 (supplied directly) and more than 1
(summed from finer bands) within a facility's band, the series restarts there.
Rolling baselines and month-to-month comparisons do not reach across the
change, so the switch itself is not flagged. The cost is that each band needs
`tested_volume$min_roll_n` months after the change before its tested-volume
baseline is ready again.

## Row-level checks

`add_age_band_qc_flags()` adds:

- `flag_attendance_sum_mismatch`: `attending` is not
  `anc_before_12wk + anc_after_12wk`.
- `flag_band_sum_partial_tested`, `flag_band_sum_partial_positive`,
  `flag_band_sum_partial_attending`: only some of the finer bands are known,
  so the sum is probably an undercount.
- `flag_band_metadata_inconsistent`: the band counts contradict the value (a
  value with no band known and no zero-fill, a missing value although bands
  are known, or more bands known than exist).
- `flag_positive_assumed_zero`: the positive count was read as zero. This is
  context only: no priority, no action, and the row is used like any other.

Rows with a partial tested or positive sum, or contradictory band counts, are
left out of the prevalence model fit; they are still assessed against it.
Zero-filled positives are kept in the fit: in a source without stored zeros
they are the zeros, and leaving them out would push expected positivity up.
Priorities and actions are in `docs/ACTION_POLICY.md`.

## First-ANC profile

`qc_config('anc_first_visit')` is the recommended profile with coverage
cutoffs suited to near-universal testing at first ANC: low below 85% and very
low below 70%. Coverage is judged only where at least 20 attended, because age
bands make denominators small: with 95% true coverage, fewer than 2% of such
rows fall below 85% by chance. Coverage above 100% is the existing
tested-above-attendance flag.

The profile also judges every test being positive from 5 tested instead of 30,
and only where the model makes it less likely than 1 in 1,000
(`p_hat^(tested / dispersion)`), as the all-negative rule already does. On real
first-ANC data, nearly every all-positive month from 20 tested was already a
positivity outlier, while hundreds from 5 to 19 tested were not flagged at
all; at expected positivity near 5%, five of five positive has a chance of
about 1 in 3 million. Months with 1 to 4 tested are left to the residual rule.

## Checks across bands

`add_age_structure_qc_flags()` compares the bands of one facility-month with
each other and with the facility's own history. Settings are in
`qc_config(age_structure = list(...))`; the bands compared are `lt20` and
`ge20` by default.

| Flag | What it compares | Default priority |
|---|---|---|
| `flag_age_band_missing` | A month with rows for some bands but not all | medium |
| `flag_age_mix_shift` | Share of bookings in the younger band | medium |
| `flag_age_bands_swapped` | An age-mix shift that fits if the bands are exchanged | high |
| `flag_booking_timing_shift` | Share of bookings before 12 weeks | low |
| `flag_age_positivity_contrast` | Younger-band positivity relative to the older (log odds ratio) | medium |

Every check is count-based, because ratios at band level are unstable (many
months have no young bookings or no positives). The current month is compared
with the facility's pooled counts over a baseline window: 12 months before it
for an operational run, and 12 months either side for a retrospective run,
restarting where the band construction changes. At least 6 usable baseline
months are needed. The z-score is binomial, inflated by the month-to-month
overdispersion seen in the baseline. A flag needs both:

- a z-score beyond the threshold (4 by default), and
- a shift at least as large as the minimum: 15 percentage points for age mix,
  20 for booking timing, and 1 on the log odds ratio scale for positivity
  (about a 2.7-fold change in the odds ratio),

and the current month must have at least 20 bookings (age mix, timing) or 20
tested in each band (positivity). A swap is an age-mix shift where the
exchanged bands would have a z-score of at most 2. No direction is assumed for
positivity.

A booking-timing flag carries one of two context flags when it fits:
`flag_booking_after_zero` when no booking after 12 weeks was reported (in a
source without stored zeros this is often a blank cell), otherwise
`flag_booking_columns_swapped` when the share would fit the baseline with the
before and after counts exchanged. They have no priority or action; they
choose the reason shown for review.

These checks form the evidence domain "age structure", which counts towards
`review_multiple_signals`; alone they give `review_age_structure`. A missing
band is a completeness issue (`review_count_completeness`).

**They are review-only.** A value entered in the wrong band leaves the
facility's totals intact, so no count is omitted from all-band estimates. They
will matter for age-specific estimates, which are a later phase.

The measurements are kept on both band rows (`age_mix_share`,
`age_mix_baseline`, `age_mix_z`, `booking_early_*`, `age_positivity_*`).
`summarise_age_band_deviations()` summarises them as aggregate quantiles and
exceedance shares, with no identifiers, for choosing thresholds from real data.

## One age band or both?

An extreme positivity residual in one band (`flag_resid_extreme`) is compared
with the other band of the same facility-month, using its dispersion-scaled
residual (kept in `resid_other_band`):

- `flag_resid_bands_agree`: the other band is off in the same direction by at
  least 2. Both bands are affected.
- `flag_resid_other_band_normal`: the other band is within 2 of expectation,
  and it was large enough to show the same move: only this band is affected.
- `flag_resid_other_band_inconclusive`: the other band is within 2 but too
  small to tell. Had its positivity risen by the same factor as this band's,
  its residual would still have been below 2.

These are context only: they add a reason for review but do not change the
priority or what any scenario omits. `summarise_residual_agreement()` gives
aggregate evidence by class: how often the outlier persists into the
neighbouring month, and how often other facilities in the same area move the
same way that month.

On a first full run of real first-ANC data, outliers were almost all upward,
facility-specific, single-month spikes, with median observed positivity of
45-100% against an expected 4-7%. Persistence and shared movement were the
same whether or not both bands were affected, and testing volume and coverage
were normal. Both bands being affected therefore most often marks an error
repeated across the form (for example tested counts copied into positives, or
positives from another indicator), not a real change, and agreeing outliers
keep their priority.

## Not yet included

Age-specific
estimates (where the age-structure flags would omit counts), and age filters
in the dashboard are later phases. In the dashboard, a flagged record is still
one band's row.
