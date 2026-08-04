prevalence_fixture <- function(tested, positive, residual, dates = seq(as.Date('2024-01-01'), by = 'month', length.out = length(tested)), facility = 'F1') {
  tibble::tibble(
    facility_id = facility,
    month_date = dates,
    tested = tested,
    positive = positive,
    prevalence = dplyr::if_else(tested > 0, positive / tested, NA_real_),
    pearson_resid = residual
  )
}

testthat::test_that('residual thresholds depend on tested count and are strict', {
  dat <- prevalence_fixture(
    tested = c(19, 19, 20, 20), positive = c(2, 2, 2, 2),
    residual = c(4.5, 5, 4.5, 4)
  )
  out <- routineqc::add_prevalence_qc_flags(dat)
  testthat::expect_identical(out$flag_resid_extreme, c(FALSE, FALSE, TRUE, FALSE))
  sensitivity <- routineqc::add_prevalence_qc_flags(dat, resid_threshold = 4)
  testthat::expect_identical(sensitivity$flag_resid_extreme, c(TRUE, TRUE, TRUE, FALSE))
})

testthat::test_that('raw prevalence bounds are opt-in sensitivity flags', {
  dat <- prevalence_fixture(10000, 1, NA_real_)
  default <- routineqc::add_prevalence_qc_flags(dat)
  sensitivity <- routineqc::add_prevalence_qc_flags(dat, use_prevalence_bounds = TRUE)
  testthat::expect_false(default$flag_raw_prevalence_bound)
  testthat::expect_true(sensitivity$flag_raw_prevalence_bound)
})

testthat::test_that('all-negative and all-positive defaults use separate boundaries', {
  dat <- prevalence_fixture(
    tested = c(49, 50, 29, 30), positive = c(0, 0, 29, 30),
    residual = rep(NA_real_, 4)
  )
  out <- routineqc::add_prevalence_qc_flags(dat)
  testthat::expect_identical(out$flag_all_negative_large_n, c(FALSE, TRUE, FALSE, FALSE))
  testthat::expect_identical(out$flag_all_positive_large_n, c(FALSE, FALSE, FALSE, TRUE))
})

testthat::test_that('large prevalence changes require volume and adjacent months', {
  dat <- dplyr::bind_rows(
    prevalence_fixture(c(20, 20), c(2, 14), c(0, 0), facility = 'A'),
    prevalence_fixture(c(19, 20), c(2, 14), c(0, 0), facility = 'B'),
    prevalence_fixture(c(20, 20), c(2, 14), c(0, 0),
                       dates = as.Date(c('2024-01-01', '2024-03-01')), facility = 'C'),
    prevalence_fixture(c(20, 20), c(2, 12), c(0, 0), facility = 'D')
  )
  out <- routineqc::add_prevalence_qc_flags(dat)
  flagged <- out$facility_id[out$flag_large_monthly_prevalence_change]
  testthat::expect_identical(flagged, 'A')
})

testthat::test_that('isolated extremes require complete adjacent-month context', {
  dat <- tibble::tibble(
    facility_id = c(rep('A', 3), rep('B', 3)),
    month_date = as.Date(c('2024-01-01', '2024-02-01', '2024-03-01',
                           '2024-01-01', '2024-03-01', '2024-04-01')),
    flag_resid_extreme = c(FALSE, TRUE, FALSE, FALSE, TRUE, FALSE)
  )
  out <- routineqc::add_temporal_qc_flags(dat)
  testthat::expect_true(out$flag_isolated_extreme[out$facility_id == 'A' & out$flag_resid_extreme])
  testthat::expect_false(out$flag_isolated_extreme[out$facility_id == 'B' & out$flag_resid_extreme])
  testthat::expect_true(out$flag_temporal_context_insufficient[out$facility_id == 'B' & out$flag_resid_extreme])
})
