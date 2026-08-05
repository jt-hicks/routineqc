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
  testthat::expect_true(out$flag_missing_tested_or_positive[5])
  testthat::expect_equal(sum(out$flag_invalid_logical), 5)
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
