testthat::test_that('valid canonical input passes', {
  dat <- tibble::tibble(
    facility_id = c('F1', 'F1'), region = 'R1',
    month_date = as.Date(c('2024-01-01', '2024-02-01')),
    tested = c(10, 20), positive = c(2, 3)
  )
  testthat::expect_invisible(routineqc::validate_qc_input(dat))
})

testthat::test_that('structural key problems fail before QC', {
  base <- tibble::tibble(
    facility_id = c('F1', 'F1'), region = 'R1',
    month_date = as.Date(c('2024-01-01', '2024-01-01')),
    tested = c(10, 20), positive = c(2, 3)
  )
  testthat::expect_error(routineqc::validate_qc_input(base), 'duplicate')
  testthat::expect_error(routineqc::validate_qc_input(dplyr::mutate(base, facility_id = NA_character_)), 'facility_id')
  testthat::expect_error(routineqc::validate_qc_input(dplyr::mutate(base, month_date = as.Date(NA))), 'month_date')
})

testthat::test_that('logical count errors remain available for QC flags', {
  dat <- tibble::tibble(
    facility_id = c('F1', 'F2'), region = 'R1',
    month_date = as.Date(c('2024-01-01', '2024-01-01')),
    tested = c(-1, 2), positive = c(0, 5)
  )
  testthat::expect_invisible(routineqc::validate_qc_input(dat))
  flagged <- routineqc::flag_logical_errors(dat)
  testthat::expect_true(all(flagged$flag_invalid_logical))
})

testthat::test_that('attendance requires an explicit definition', {
  dat <- tibble::tibble(
    facility_id = 'F1', region = 'R1', month_date = as.Date('2024-01-01'),
    tested = 10, positive = 2, attending = 12
  )
  testthat::expect_error(routineqc::validate_qc_input(dat), 'attendance_definition')
  dat$attendance_definition <- 'First ANC attendees eligible for malaria testing'
  testthat::expect_invisible(routineqc::validate_qc_input(dat))
})
