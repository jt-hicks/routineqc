small_qc_run <- function() {
  raw <- tibble::tibble(
    facility_id = c('F1', 'F1', 'F2'),
    region = c('R1', 'R1', 'R2'),
    month_date = as.Date(c('2024-01-01', '2024-02-01', '2024-01-01')),
    tested = c(10, 12, 8),
    positive = c(2, 3, 1)
  )
  suppressWarnings(routineqc::run_routine_qc(
    raw,
    facility_var = 'facility_id', region_var = 'region', month_var = 'month_date',
    tested_var = 'tested', positive_var = 'positive', nthreads = 1
  ))
}

testthat::test_that('a run records its implementation and dependency versions', {
  run <- small_qc_run()
  build <- run$manifest$build
  testthat::expect_match(build$implementation_hash, '^[0-9a-f]{64}$')
  testthat::expect_true('mgcv' %in% names(build$dependency_versions))
  testthat::expect_true(all(c('source_revision', 'r_version') %in% names(build)))
  testthat::expect_true(all(c('tested', 'positive') %in% names(run$manifest$count_translation)))
})

testthat::test_that('different implementations cannot share an analysis identity', {
  run <- small_qc_run()
  manifest <- run$manifest
  other <- routineqc:::.analysis_id(
    manifest$run_schema_version, manifest$package_version, manifest$input_fingerprint,
    manifest$config_fingerprint, implementation_hash = strrep('0', 64)
  )
  testthat::expect_false(identical(other, manifest$analysis_id))
  run$manifest$build$implementation_hash <- strrep('0', 64)
  testthat::expect_error(routineqc::validate_qc_run(run), 'analysis identity')
})

testthat::test_that('runs saved without build identity still validate', {
  run <- small_qc_run()
  run$manifest$build <- NULL
  run$manifest$count_translation <- NULL
  run$manifest$analysis_id <- routineqc:::.analysis_id(
    run$manifest$run_schema_version, run$manifest$package_version,
    run$manifest$input_fingerprint, run$manifest$config_fingerprint
  )
  testthat::expect_invisible(routineqc::validate_qc_run(run))
})

testthat::test_that('validation rejects inconsistent actions and exclusion flags', {
  run <- small_qc_run()
  bad <- run
  bad$data_flagged$flag_exclude_authorized[1] <- TRUE
  testthat::expect_error(routineqc::validate_qc_run(bad), 'inconsistent')
  bad <- run
  bad$data_flagged$qc_action[1] <- 'review_prevalence'
  testthat::expect_error(routineqc::validate_qc_run(bad), 'inconsistent')
})

testthat::test_that('validation rejects summaries that do not match the data', {
  run <- small_qc_run()
  run$summaries$by_region$total_rows[1] <- run$summaries$by_region$total_rows[1] + 1L
  testthat::expect_error(routineqc::validate_qc_run(run), 'summary `by_region`')
})
