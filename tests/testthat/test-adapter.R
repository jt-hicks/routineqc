adapter_mapping <- function() {
  list(
    facility_id = 'site_code', month_date = 'period', region = 'area',
    tested = 'tests_done', positive = 'tests_positive',
    source_record_id = 'record_id'
  )
}

adapter_source <- function() {
  tibble::tibble(
    site_code = c('F1', 'F1', 'F2'),
    period = c('2024-01', '2024-02', '2024-01'),
    area = c('R1', 'R1', 'R2'),
    tests_done = c('10', '12', '8'),
    tests_positive = c('2', '3', '1'),
    record_id = c('row-a', 'row-b', 'row-c')
  )
}

adapt_fixture <- function(data = adapter_source(), mapping = adapter_mapping(), ...) {
  routineqc::adapt_qc_data(
    data = data,
    mapping = mapping,
    identity_strategy = 'stable source facility code',
    adapter = 'synthetic_example',
    adapter_version = '0.1.0',
    dataset_id = 'synthetic-2024-01',
    source_system = 'synthetic',
    ...
  )
}

testthat::test_that('generic adapter maps canonical data and safe provenance', {
  result <- adapt_fixture()
  testthat::expect_s3_class(result, 'routineqc_adapter_result')
  testthat::expect_true(result$ready)
  testthat::expect_equal(nrow(result$data), nrow(adapter_source()))
  testthat::expect_identical(
    names(result$data),
    c('facility_id', 'month_date', 'region', 'tested', 'positive', 'source_record_id')
  )
  testthat::expect_s3_class(result$data$month_date, 'Date')
  testthat::expect_true(is.numeric(result$data$tested))
  testthat::expect_equal(nrow(result$diagnostics), 0L)
  testthat::expect_identical(result$provenance$identity_strategy, 'stable source facility code')
  testthat::expect_invisible(routineqc::validate_qc_adapter_result(result))
})

testthat::test_that('malformed counts are diagnosed without dropping rows or blocking QC', {
  source <- adapter_source()
  source$tests_done[2] <- 'not-a-number'
  result <- adapt_fixture(source)
  testthat::expect_true(result$ready)
  testthat::expect_equal(nrow(result$data), nrow(source))
  testthat::expect_true(is.na(result$data$tested[2]))
  testthat::expect_identical(result$diagnostics$issue_code, 'unparseable_count')
  testthat::expect_identical(result$diagnostics$source_record_id, 'row-b')

  run <- suppressWarnings(routineqc::run_qc_adapter(result, nthreads = 1))
  testthat::expect_s3_class(run, 'routineqc_run')
  testthat::expect_equal(nrow(run$data_flagged), nrow(source))
  testthat::expect_true(run$data_flagged$flag_core_invalid[2])
  testthat::expect_identical(run$manifest$provenance$adapter, 'synthetic_example')
})

testthat::test_that('ambiguous structural records block handoff and remain diagnosed', {
  source <- adapter_source()
  source$period[2] <- source$period[1]
  source$site_code[3] <- ''
  result <- adapt_fixture(source)
  testthat::expect_false(result$ready)
  testthat::expect_equal(nrow(result$data), nrow(source))
  testthat::expect_true(all(
    c('duplicate_facility_month', 'missing_facility_id') %in% result$diagnostics$issue_code
  ))
  testthat::expect_error(
    routineqc::run_qc_adapter(result),
    'not ready for QC'
  )
})

testthat::test_that('unparseable months are blocking diagnostics', {
  source <- adapter_source()
  source$period[1] <- 'not-a-month'
  result <- adapt_fixture(source)
  testthat::expect_false(result$ready)
  testthat::expect_identical(result$diagnostics$issue_code, 'unparseable_month')
  testthat::expect_true(is.na(result$data$month_date[1]))
})

testthat::test_that('adapter mappings and provenance fail explicitly', {
  missing_mapping <- adapter_mapping()
  missing_mapping$positive <- NULL
  testthat::expect_error(adapt_fixture(mapping = missing_mapping), 'Missing required')

  absent_column <- adapter_mapping()
  absent_column$tested <- 'unknown_column'
  testthat::expect_error(adapt_fixture(mapping = absent_column), 'not found')

  testthat::expect_error(
    adapt_fixture(provenance = list(api_token = 'unsafe')),
    'must not contain'
  )
})

testthat::test_that('attendance mapping requires and preserves its definition', {
  source <- dplyr::mutate(adapter_source(), attended = c(15, 15, 10))
  mapping <- c(adapter_mapping(), list(attending = 'attended'))
  testthat::expect_error(
    adapt_fixture(source, mapping),
    'attendance_definition'
  )
  result <- adapt_fixture(
    source, mapping,
    attendance_definition = 'First ANC attendees eligible for testing'
  )
  testthat::expect_true(result$ready)
  testthat::expect_identical(
    unique(result$data$attendance_definition),
    'First ANC attendees eligible for testing'
  )
})

testthat::test_that('adapter readiness cannot disagree with diagnostics', {
  result <- adapt_fixture()
  result$diagnostics <- tibble::tibble(
    source_row = 1L, source_record_id = 'row-a', severity = 'error',
    issue_code = 'test_error', field = 'facility_id'
  )
  testthat::expect_error(
    routineqc::validate_qc_adapter_result(result),
    'readiness does not agree'
  )
})
