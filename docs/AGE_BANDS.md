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

## Not yet included

Checks across the bands of one facility-month (age mix, timing of booking,
the positivity ratio between bands, and whether a model residual appears in
one band or both) and age filters in the dashboard are later phases. In the
dashboard, a flagged record is still one band's row.
