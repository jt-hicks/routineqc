canonical_run_input <- function() {
  tibble::tibble(
    facility_id = c('SECRET_F1', 'SECRET_F1', 'SECRET_F2'),
    region = c('R1', 'R1', 'R2'),
    month_date = as.Date(c('2024-01-01', '2024-02-01', '2024-01-01')),
    tested = c(10, 12, 8),
    positive = c(2, 3, 1)
  )
}

small_qc_run <- function(config = routineqc::qc_config(), provenance = list()) {
  raw <- canonical_run_input()
  suppressWarnings(routineqc::run_routine_qc(
    raw,
    facility_var = 'facility_id', region_var = 'region', month_var = 'month_date',
    tested_var = 'tested', positive_var = 'positive',
    config = config, provenance = provenance, nthreads = 1
  ))
}

testthat::test_that('input fingerprint ignores row order but detects data changes', {
  dat <- canonical_run_input()
  reordered <- dat[c(3, 1, 2), ]
  changed <- dat
  changed$tested[1] <- changed$tested[1] + 1
  testthat::expect_identical(
    routineqc::fingerprint_qc_input(dat),
    routineqc::fingerprint_qc_input(reordered)
  )
  testthat::expect_false(identical(
    routineqc::fingerprint_qc_input(dat),
    routineqc::fingerprint_qc_input(changed)
  ))
})

testthat::test_that('analysis identity is deterministic and execution identity is unique', {
  first <- small_qc_run()
  second <- small_qc_run()
  changed_config <- small_qc_run(
    routineqc::qc_config(action_policy = list(name = 'flags_only'))
  )
  testthat::expect_s3_class(first, 'routineqc_run')
  testthat::expect_invisible(routineqc::validate_qc_run(first))
  testthat::expect_identical(first$manifest$analysis_id, second$manifest$analysis_id)
  testthat::expect_false(identical(first$manifest$execution_id, second$manifest$execution_id))
  testthat::expect_false(identical(first$manifest$analysis_id, changed_config$manifest$analysis_id))
  testthat::expect_identical(first$manifest$prevalence_model_available, FALSE)
  testthat::expect_identical(first$manifest$model_assessed_rows, 0L)
  testthat::expect_identical(first$manifest$model_eligible_rows, 3L)
  testthat::expect_identical(first$manifest$model_prediction_coverage, 0)
})

testthat::test_that('safe provenance is preserved and unsafe metadata is rejected', {
  run <- small_qc_run(provenance = list(
    dataset_id = 'anc-curated-2026-08', source_system = 'DHIS2', adapter_version = '1'
  ))
  testthat::expect_identical(run$manifest$provenance$dataset_id, 'anc-curated-2026-08')
  testthat::expect_error(small_qc_run(provenance = list(api_token = 'secret')), 'must not contain')
  testthat::expect_error(small_qc_run(provenance = list(source_path = 'C:/private')), 'must not contain')
  testthat::expect_error(small_qc_run(provenance = list(c('unnamed'))), 'flat named list')
  duplicate_names <- structure(list('one', 'two'), names = c('batch', 'batch'))
  testthat::expect_error(small_qc_run(provenance = duplicate_names), 'unique')
})

testthat::test_that('write and read round trip is guarded and metadata-only', {
  run <- small_qc_run(provenance = list(dataset_id = 'safe-dataset-id'))
  directory <- tempfile('routineqc-roundtrip-')
  dir.create(directory)
  on.exit(unlink(directory, recursive = TRUE), add = TRUE)
  path <- file.path(directory, 'example.rds')

  written <- routineqc::write_qc_run(run, path)
  testthat::expect_true(all(file.exists(unlist(written))))
  testthat::expect_error(routineqc::write_qc_run(run, path), 'already exists')
  restored <- routineqc::read_qc_run(path)
  testthat::expect_identical(restored$manifest$analysis_id, run$manifest$analysis_id)
  testthat::expect_identical(restored$data_flagged, run$data_flagged)

  json_text <- paste(readLines(written$manifest, warn = FALSE), collapse = '\n')
  testthat::expect_false(grepl('SECRET_F1|SECRET_F2', json_text, fixed = FALSE))
  testthat::expect_false(grepl('data_flagged', json_text, fixed = TRUE))
  testthat::expect_true(grepl('safe-dataset-id', json_text, fixed = TRUE))

  disk_manifest <- jsonlite::read_json(written$manifest, simplifyVector = TRUE)
  disk_manifest$analysis_id <- paste0('tampered-', disk_manifest$analysis_id)
  jsonlite::write_json(disk_manifest, written$manifest, auto_unbox = TRUE, pretty = TRUE)
  testthat::expect_error(routineqc::read_qc_run(path), 'disagrees')
})

testthat::test_that('unsupported and inconsistent run schemas are rejected', {
  run <- small_qc_run()
  unsupported <- run
  unsupported$manifest$run_schema_version <- 99L
  testthat::expect_error(routineqc::validate_qc_run(unsupported), 'Unsupported')
  inconsistent <- run
  inconsistent$manifest$input_rows <- inconsistent$manifest$input_rows + 1L
  testthat::expect_error(routineqc::validate_qc_run(inconsistent), 'row count')
  changed_data <- run
  changed_data$data_flagged$tested[1] <- changed_data$data_flagged$tested[1] + 1
  testthat::expect_error(routineqc::validate_qc_run(changed_data), 'input fingerprint')
  changed_coverage <- run
  changed_coverage$manifest$model_assessed_rows <- 1L
  testthat::expect_error(
    routineqc::validate_qc_run(changed_coverage),
    'model-assessment results'
  )
})
