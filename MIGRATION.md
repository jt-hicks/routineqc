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
| 8. Application | Shiny app consumes selectable QC run objects | Not started |
| 9. Demonstrate | Generic adapter and synthetic end-to-end vignette | Done |
| 10. Release | CI, documentation, checks, and experimental `0.1.0` | In progress |
| 11. Integrate | Optional adapters for upstream data-curation pipelines | Not started |

## Immediate next steps

1. Build the first Shiny application increment around selectable persisted QC runs.
2. Add continuous integration and prepare the experimental `0.1.0` release checklist.
3. Pilot one thin source-specific adapter on non-committed upstream data.

## Current position

The reusable QC engine, synthetic rule coverage, orchestration, provenance, and
generic adapter demonstration are complete. Application work has not started.
Release preparation has begun through generated documentation and clean package
checks, but CI and the `0.1.0` release remain outstanding. No upstream pipeline
has yet been integrated, preserving the package boundary while the generic
contract stabilizes.

## Repository and licence

- Maintainer: Joseph T Hicks (`jthicks@imperial.ac.uk`).
- Visibility: private during development.
- Licence: MIT.

## Canonical minimum input

Required fields after adapter mapping are `facility_id`, `month_date`, `region`, `tested`, and `positive`. `month_date` must be a first-of-month `Date`, facility-month keys must be unique, and count fields must be numeric. Illogical count values remain admissible at this structural gate so the QC rules can flag them rather than silently remove them.

Attendance is optional and may only enable logical rules when accompanied by an explicit, approved `attendance_definition`. Missing attendance must never be replaced with `tested`.

Real or confidential facility data must not be committed; tests and examples use synthetic fixtures.
