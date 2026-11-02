testthat::test_that("logical flags catch impossible values", {
  dat <- tibble::tibble(
    facility_id = "F1",
    region = "R1",
    month_date = as.Date("2024-01-01") + 0:4,
    tested = c(10, -1, 0, 5, NA),
    positive = c(11, 0, 2, -1, 1),
    prevalence = c(NA, NA, NA, NA, NA),
    month_num = 1:5,
    time_index = 1:5
  )

  out <- routineqc::flag_logical_errors(dat)

  testthat::expect_true(out$flag_positive_gt_tested[1])
  testthat::expect_true(out$flag_tested_negative[2])
  testthat::expect_true(out$flag_zero_tested_positive[3])
  testthat::expect_true(out$flag_positive_negative[4])
  testthat::expect_equal(sum(out$flag_invalid_logical), 4)
})

testthat::test_that('missing counts are recorded as status, not as logical errors', {
  dat <- tibble::tibble(
    tested = c(10, 10, NA, NA),
    positive = c(2, NA, 1, NA)
  )
  out <- routineqc::flag_logical_errors(dat)
  testthat::expect_identical(
    out$core_count_status,
    c('complete', 'missing_positive', 'missing_tested', 'missing_both')
  )
  testthat::expect_false(any(out$flag_core_invalid))
  testthat::expect_false(any(out$flag_invalid_logical))
  testthat::expect_false('flag_missing_tested_or_positive' %in% names(out))
})

testthat::test_that('summary prevalence uses only rows carrying both counts', {
  dat <- tibble::tibble(
    region = 'R1',
    tested = c(100, 100, NA),
    positive = c(10, NA, 5),
    flag_exclude_authorized = FALSE
  )
  s <- routineqc::summarise_before_after_qc(dat)
  testthat::expect_equal(s$prevalence_before_qc, 0.1)
  testthat::expect_equal(s$prevalence_after_authorized_exclusions, 0.1)
  testthat::expect_equal(s$incomplete_count_rows, 2)
  testthat::expect_equal(s$total_tested_before_qc, 200)
})

testthat::test_that('logical helper allows absent attendance', {
  core <- tibble::tibble(tested = 10, positive = 2)
  out <- routineqc::flag_logical_errors(core)
  testthat::expect_false(out$flag_attendance_issue)
})

testthat::test_that('logical helper allows entirely missing attendance', {
  dat <- tibble::tibble(tested = 10, positive = 2, attending = NA_real_)
  out <- routineqc::flag_logical_errors(dat)
  testthat::expect_false(out$flag_attendance_issue)
})

testthat::test_that('logical helper rejects undefined non-missing attendance', {
  missing_definition <- tibble::tibble(tested = 10, positive = 2, attending = 8)
  testthat::expect_error(
    routineqc::flag_logical_errors(missing_definition),
    'Non-missing `attending` values require a non-missing `attendance_definition`',
    fixed = TRUE
  )

  blank_definition <- dplyr::mutate(
    missing_definition, attendance_definition = '   '
  )
  testthat::expect_error(
    routineqc::flag_logical_errors(blank_definition),
    'Non-missing `attending` values require a non-missing `attendance_definition`',
    fixed = TRUE
  )
})

testthat::test_that('logical helper accepts defined attendance', {
  dat <- tibble::tibble(
    tested = 10,
    positive = 2,
    attending = 8,
    attendance_definition = 'Eligible ANC attendees from the adapter source'
  )
  out <- routineqc::flag_logical_errors(dat)
  testthat::expect_true(out$flag_tested_gt_attending)
  testthat::expect_true(out$flag_attendance_issue)
})

testthat::test_that("prepare_qc_data computes prevalence correctly", {
  raw <- tibble::tibble(
    fac = c("A", "A"),
    reg = c("R", "R"),
    council = c("C1", "C1"),
    district = c("D1", "D1"),
    month = c("2024-01-01", "2024-02-01"),
    t = c(20, 0),
    p = c(5, 0)
  )

  out <- routineqc::prepare_qc_data(
    raw,
    facility_var = "fac",
    region_var = "reg",
    month_var = "month",
    tested_var = "t",
    positive_var = "p",
    council_var = "council",
    district_var = "district"
  )

  testthat::expect_equal(out$prevalence[1], 0.25)
  testthat::expect_true(is.na(out$prevalence[2]))
  testthat::expect_true(all(c("council", "district") %in% names(out)))
})

testthat::test_that('approved attendance enables attendance logical rules', {
  raw <- tibble::tibble(
    fac = c('A', 'B', 'C'), reg = 'R', month = '2024-01-01',
    attending = c(8, -1, NA), tested = c(10, 1, 4), positive = c(2, 0, 1)
  )
  prepared <- routineqc::prepare_qc_data(
    raw, facility_var = 'fac', region_var = 'reg', month_var = 'month',
    tested_var = 'tested', positive_var = 'positive',
    attending_var = 'attending',
    attendance_definition = 'Eligible ANC attendees from the adapter source'
  )
  testthat::expect_invisible(routineqc::validate_qc_input(prepared))
  out <- routineqc::flag_logical_errors(prepared)
  testthat::expect_true(out$flag_tested_gt_attending[1])
  testthat::expect_true(out$flag_attending_negative[2])
  testthat::expect_false(out$flag_core_invalid[1])
  testthat::expect_false(out$flag_core_invalid[2])
  testthat::expect_false(out$flag_invalid_logical[3])
})

testthat::test_that('attendance mapping requires an explicit definition', {
  raw <- tibble::tibble(
    fac = 'A', reg = 'R', month = '2024-01-01',
    attending = 10, tested = 8, positive = 1
  )
  testthat::expect_error(
    routineqc::prepare_qc_data(
      raw, facility_var = 'fac', region_var = 'reg', month_var = 'month',
      tested_var = 'tested', positive_var = 'positive', attending_var = 'attending'
    ),
    'attendance_definition'
  )
})

testthat::test_that('attendance flags grade the excess and judge coverage', {
  dat <- tibble::tibble(
    month_date = as.Date(c('2014-06-01', rep('2015-06-01', 6))),
    tested    = c(2,  40, 12, 30, 9,  50, NA),
    positive  = 0,
    attending = c(20, 20, 11, 50, 15, 60, 30),
    attendance_definition = 'first visits'
  )
  out <- routineqc::add_attendance_qc_flags(routineqc::flag_logical_errors(dat), coverage_from = '2015-01-01')
  testthat::expect_equal(out$testing_coverage[1:2], c(0.1, 2))
  # 40 of 20 is 2x and 20 over; 12 of 11 is a slight excess.
  testthat::expect_identical(out$flag_tested_gt_attending, c(FALSE, TRUE, TRUE, FALSE, FALSE, FALSE, FALSE))
  testthat::expect_identical(out$flag_tested_gt_attending_major, c(FALSE, TRUE, FALSE, FALSE, FALSE, FALSE, FALSE))
  # 2014 is before coverage_from; 30/50 is low; 9/15 is low; 50/60 passes; missing tested is not judged.
  testthat::expect_identical(out$flag_coverage_low, c(FALSE, FALSE, FALSE, TRUE, TRUE, FALSE, FALSE))
  testthat::expect_identical(out$flag_coverage_very_low, rep(FALSE, 7))
  low <- routineqc::add_attendance_qc_flags(routineqc::flag_logical_errors(dat), coverage_min_attending = 20)
  testthat::expect_identical(low$flag_coverage_very_low, c(TRUE, rep(FALSE, 6)))
  testthat::expect_false(low$flag_coverage_low[5])
  testthat::expect_error(
    routineqc::add_attendance_qc_flags(dat, coverage_low = 0.4, coverage_very_low = 0.5),
    'Coverage cutoffs'
  )
})
