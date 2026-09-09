# routineqc migration roadmap

This package is the independent home for the reusable QC engine originally prototyped alongside `anc_data_curation`. It must consume documented canonical inputs and must not depend on that repository's SharePoint paths, raw-file conventions, or release process.

## Migration phases

| Phase | Deliverable | Status |
|---|---|---|
| 1. Preserve | Existing package scaffold and QC prototype preserved in Git | Done |
| 2. Charter | Package scope, ownership boundary, licence, and input contract | Done |
| 3. Scaffold | Installable R package passing `R CMD check` | Done |
| 4. Extract | Reconcile reusable logical, temporal, statistical, volume, summary, and plotting behavior | Done |
| 5. Test | Synthetic tests cover critical rules and edge cases | Done |
| 6. Orchestrate | Stable `run_routine_qc()` API and QC run object | Done |
| 7. Provenance | Configuration, manifests, input identifiers, and persistence | Done |
| 8. Application | Shiny app consumes selectable QC run objects | Done |
| 9. Demonstrate | Generic adapter and synthetic end-to-end vignette | Done |
| 10. Release | CI, documentation, checks, and experimental `0.1.0` | Done, tagged `v0.1.0` |
| 11. Integrate | Optional adapters for upstream data-curation pipelines | Not started |

Migration is complete. Phase 11 remains deliberately unstarted so the generic
input contract can stabilize before any upstream pipeline depends on it.

## Immediate next steps

1. Pilot one thin source-specific adapter on non-committed upstream data.
2. Define an audited reviewer-decision data model before making the app editable.
3. Decide whether the reporting-consistency strictness level should be recorded
   in the run configuration. It is currently a display and analysis argument, so
   a saved run does not record which cohort a reviewer used. Recording it would
   require a configuration schema bump to version 5 and new identity fingerprints.

## Current position

The reusable QC engine, synthetic rule coverage, orchestration, provenance,
generic adapter demonstration, and read-only run application are complete.
Experimental release `0.1.0` has been committed, tagged `v0.1.0`, and published
to the GitHub repository, and a GitHub Actions `R CMD check` workflow covering
Linux release and oldrel-1, Windows, and macOS runs on every push to `main`.

The full test suite passes with 354 tests, 0 failures, and four expected model
fallback or insufficient-data warnings. `R CMD check` reports OK with no notes;
`R CMD check --as-cran` adds only the standard new-submission note, which is
expected for a package that has not been submitted to CRAN.

No upstream pipeline has yet been integrated, preserving the package boundary
while the generic contract stabilizes. Every flag, threshold, and policy remains
validated against synthetic fixtures only.

## Development since 0.1.0

Work after the release candidate is new capability rather than migration, so it
has no counterpart in `docs/PROTOTYPE_COMPARISON.md`:

- **Facility reporting consistency.** Measures whether a facility reports at all
  and how continuously, separately from whether its reported values are
  plausible: months without testing split into absent, zero, and missing
  components, the longest gap without testing, when data collection starts and
  stops, and coverage of both the facility reporting window and the dataset-wide
  month span. Named strictness levels select a review cohort. Cohort selection is
  not exclusion authority; `flag_exclude_authorized` remains the only field that
  authorizes exclusion. See `docs/REPORTING_CONSISTENCY.md`.
- **Run Explorer additions.** A Reporting consistency tab, a cohort filter on the
  review queue, a district total time series above the district facets, and a
  toggle hiding facets for facilities with no testing data. These compute from
  the flagged data rather than stored summaries, so runs written by earlier
  versions still open unchanged.

Neither change altered the configuration, run, or manifest schema, so persisted
run identities are unaffected.

## Repository and licence

- Maintainer: Joseph T Hicks (`jthicks@imperial.ac.uk`).
- Repository: <https://github.com/jt-hicks/routineqc>.
- Visibility: public. Everything committed here is world-readable, so the
  prohibition on committing real or confidential facility data is a hard
  constraint rather than a convention. Run artifacts are excluded by both
  `.gitignore` and `.Rbuildignore`.
- Licence: MIT.

## Canonical minimum input

Required fields after adapter mapping are `facility_id`, `month_date`, `region`, `tested`, and `positive`. `month_date` must be a first-of-month `Date`, facility-month keys must be unique, and count fields must be numeric. Illogical count values remain admissible at this structural gate so the QC rules can flag them rather than silently remove them.

Attendance is optional and may only enable logical rules when accompanied by an explicit, approved `attendance_definition`. Missing attendance must never be replaced with `tested`.

Real or confidential facility data must not be committed; tests and examples use synthetic fixtures.
