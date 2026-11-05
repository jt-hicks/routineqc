review_raw <- function(tested) {
  tibble::tibble(
    facility_id = c('F1', 'F1', 'F2'),
    region = c('R1', 'R1', 'R2'),
    month_date = as.Date(c('2024-01-01', '2024-02-01', '2024-01-01')),
    tested = tested,
    positive = c(2, 3, 1)
  )
}

review_run <- function(raw) {
  suppressWarnings(routineqc::run_routine_qc(
    raw,
    facility_var = 'facility_id', region_var = 'region', month_var = 'month_date',
    tested_var = 'tested', positive_var = 'positive', nthreads = 1
  ))
}

testthat::test_that('a missing and a malformed count have different identities', {
  missing_run <- review_run(review_raw(c(10, NA, 8)))
  malformed_run <- review_run(review_raw(c('10', 'oops', '8')))
  testthat::expect_identical(missing_run$data_flagged$qc_action[2], 'retain')
  testthat::expect_identical(malformed_run$data_flagged$qc_action[2], 'review_core_invalid')
  testthat::expect_false(identical(
    missing_run$manifest$input_fingerprint, malformed_run$manifest$input_fingerprint
  ))
  testthat::expect_false(identical(
    missing_run$manifest$analysis_id, malformed_run$manifest$analysis_id
  ))
  testthat::expect_invisible(routineqc::validate_qc_run(malformed_run))
})

testthat::test_that('runs with the earlier fingerprint schema still validate', {
  run <- review_run(review_raw(c(10, 12, 8)))
  run$manifest$input_fingerprint_schema_version <- NULL
  run$manifest$input_fingerprint <- routineqc::fingerprint_qc_input(run$data_flagged, 1L)
  run$manifest$analysis_id <- routineqc:::.analysis_id(
    run$manifest$run_schema_version, run$manifest$package_version,
    run$manifest$input_fingerprint, run$manifest$config_fingerprint,
    run$manifest$build$implementation_hash
  )
  testthat::expect_invisible(routineqc::validate_qc_run(run))
})

testthat::test_that('an invalid count takes no part in coverage', {
  raw <- review_raw(c(0.5, 12, 8))
  raw$attending <- c(100, 100, 100)
  data <- suppressWarnings(routineqc::prepare_qc_data(
    raw, facility_var = 'facility_id', region_var = 'region', month_var = 'month_date',
    tested_var = 'tested', positive_var = 'positive', attending_var = 'attending',
    attendance_definition = 'test'
  ))
  out <- routineqc::add_attendance_qc_flags(routineqc::flag_logical_errors(data))
  testthat::expect_true(out$flag_count_invalid_value[1])
  testthat::expect_true(is.na(out$testing_coverage[1]))
  testthat::expect_false(out$flag_coverage_very_low[1])
  testthat::expect_false(out$flag_coverage_low[1])
  testthat::expect_equal(out$testing_coverage[2], 0.12)
  testthat::expect_true(is.na(routineqc:::.district_metric_value(out, 'coverage')[1]))
  comparable <- routineqc:::.attendance_comparable(out)
  testthat::expect_false(comparable$.comparable[1])
})

testthat::test_that('a change to a rule constant changes the implementation identity', {
  current <- routineqc:::.build_identity()$implementation_hash
  changed <- testthat::with_mocked_bindings(
    routineqc:::.build_identity()$implementation_hash,
    .count_invalid_statuses = c('malformed', 'non_finite'),
    .package = 'routineqc'
  )
  testthat::expect_false(identical(current, changed))
  testthat::expect_identical(current, routineqc:::.build_identity()$implementation_hash)
})

testthat::test_that('edited prepared data are judged again, not trusted', {
  prepared <- suppressWarnings(routineqc::prepare_qc_data(
    review_raw(c(100, 120, 80)),
    facility_var = 'facility_id', region_var = 'region', month_var = 'month_date',
    tested_var = 'tested', positive_var = 'positive'
  ))
  prepared$tested[1] <- 100.5
  again <- suppressWarnings(routineqc::prepare_qc_data(
    prepared,
    facility_var = 'facility_id', region_var = 'region', month_var = 'month_date',
    tested_var = 'tested', positive_var = 'positive'
  ))
  testthat::expect_identical(again$tested_parse_status[1], 'fractional')
  testthat::expect_identical(again$tested_source[1], 100)
  run <- review_run(prepared)
  testthat::expect_true(run$data_flagged$flag_count_invalid_value[1])
  testthat::expect_identical(run$data_flagged$qc_action[1], 'review_core_invalid')

  contradictory <- prepared
  contradictory$tested[2] <- NA
  testthat::expect_error(
    routineqc::prepare_qc_data(
      contradictory,
      facility_var = 'facility_id', region_var = 'region', month_var = 'month_date',
      tested_var = 'tested', positive_var = 'positive'
    ),
    'disagree'
  )
})

testthat::test_that('validation requires the action columns, not only their consistency', {
  run <- review_run(review_raw(c(10, 12, 8)))
  for (col in c('qc_action', 'flag_exclude_authorized', 'flag_review_recommended', 'flag_any_qc_issue')) {
    bad <- run
    bad$data_flagged[[col]] <- NULL
    testthat::expect_error(routineqc::validate_qc_run(bad), 'required action fields')
  }
})
