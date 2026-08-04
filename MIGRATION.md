# routineqc migration roadmap

This package is the independent home for the reusable QC engine originally prototyped alongside `anc_data_curation`. It must consume documented canonical inputs and must not depend on that repository's SharePoint paths, raw-file conventions, or release process.

## Migration phases

| Phase | Deliverable | Status |
|---|---|---|
| 1. Preserve | Existing package scaffold and QC prototype preserved in Git | Done |
| 2. Charter | Package scope, ownership boundary, licence, and input contract | In progress |
| 3. Scaffold | Installable R package passing `R CMD check` | Done |
| 4. Extract | Reconcile reusable logical, temporal, statistical, volume, summary, and plotting behavior | Not started |
| 5. Test | Synthetic tests cover critical rules and edge cases | In progress |
| 6. Orchestrate | Stable `run_routine_qc()` API and QC run object | Prototype |
| 7. Provenance | Configuration, manifests, input identifiers, and persistence | Not started |
| 8. Application | Shiny app consumes selectable QC run objects | Not started |
| 9. Demonstrate | Generic adapter and synthetic end-to-end vignette | Not started |
| 10. Release | CI, documentation, checks, and experimental `0.1.0` | Not started |
| 11. Integrate | Optional adapters for upstream data-curation pipelines | Not started |

## Immediate next steps

1. Formalise and test the canonical input contract, including missingness and duplicate keys.
2. Compare this package's existing QC behavior with the earlier prototype and migrate only justified differences.
3. Add reproducible configuration and QC run manifests.

## Repository and licence

- Maintainer: Joseph T Hicks (`jthicks@imperial.ac.uk`).
- Visibility: private during development.
- Licence: MIT.

## Canonical minimum input

Required fields after adapter mapping are `facility_id`, `month_date`, `tested`, and `positive`. Geographic labels are optional. Attendance is optional and may only enable logical rules when accompanied by an explicit, approved `attendance_definition`. Missing attendance must never be replaced with `tested`.

Real or confidential facility data must not be committed; tests and examples use synthetic fixtures.
