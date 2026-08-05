# Tested-volume baseline modes

Tested-volume QC compares each facility-month tested count with a local
facility-specific baseline. The selected baseline mode is part of the run
configuration and is also written to every flagged row.

## Operational: trailing baseline

The `operational` profile uses only non-missing observations from the preceding
six calendar months. `recommended` is currently an alias for this profile.

Use this mode for routine monitoring or whenever the result must reflect only
information available at the reporting date.

Limitations:

- A facility needs the configured minimum number of earlier observations before
  rolling high/low and unusual-zero rules can run. This creates a warm-up period.
- Missing months reduce baseline evidence; the window does not stretch farther
  back to replace them.
- A genuine permanent shift can produce alerts until the trailing baseline
  adapts.
- Several erroneous preceding reports can contaminate the baseline for later
  months.
- Early anomalies cannot be assessed by rolling high/low rules, although
  logically impossible counts and eligible jump/drop rules can still be flagged.

## Retrospective: centered baseline

The `retrospective` profile uses non-missing observations from the six calendar
months before and six calendar months after the current row, excluding the
current row itself.

Use this mode to review a substantially complete historical dataset when
look-ahead is acceptable and explicitly disclosed.

Limitations:

- It uses future information and must not be described as an alert that was
  available at the reporting date.
- A row's baseline and flags can change when later reports are appended or
  corrected.
- Genuine abrupt structural changes can make observations near the transition
  look anomalous relative to data from both regimes.
- Missing months and dataset boundaries still reduce baseline evidence.
- Results from a centered run are not directly comparable with an operational
  trailing run unless the differing baseline mode is accounted for.

## Shared safeguards and interpretation

Both modes use calendar months rather than a fixed number of available rows.
The default rolling high/low rules require:

- at least six non-missing neighboring observations;
- both an extreme robust z-score and an extreme ratio to the rolling median;
- minimum-volume safeguards for high, low, zero, jump, and drop rules.

The output retains baseline count, median, MAD, adjusted MAD, robust z-score,
ratio to baseline, ratio to the previous report, component evidence flags,
`tested_baseline_mode`, and `baseline_uses_future`. A combined flag therefore
does not hide the evidence used to create it.

Jump and drop rules compare with the previous available facility observation.
If reports are missing between the two observations, the ratio spans that gap;
it should be interpreted as a review signal rather than proof of an error.

Tested-volume flags identify unusual reporting patterns. They do not establish
that an observation is wrong, and they do not by themselves authorize removal.
