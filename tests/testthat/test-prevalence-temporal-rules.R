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

testthat::test_that('the residual rule uses the dispersion-scaled residual when present', {
  dat <- prevalence_fixture(
    tested = c(50, 50, 50), positive = c(5, 5, 5), residual = c(6, 9, -9)
  )
  dat$resid_dispersion <- 4
  dat$pearson_resid_scaled <- dat$pearson_resid / 2
  out <- routineqc::add_prevalence_qc_flags(dat)
  # Scaled residuals are 3, 4.5 and -4.5 against a cutoff of 4.
  testthat::expect_identical(out$flag_resid_extreme, c(FALSE, TRUE, TRUE))
  testthat::expect_false('.rule_resid' %in% names(out))
  unscaled <- routineqc::add_prevalence_qc_flags(dplyr::select(dat, -pearson_resid_scaled))
  testthat::expect_identical(unscaled$flag_resid_extreme, c(TRUE, TRUE, TRUE))
})

testthat::test_that('model dispersion is estimated from the fit and floored at 1', {
  set.seed(4)
  n <- 400
  tested <- rep(60, n)
  over <- stats::rbinom(n, tested, stats::rbeta(n, 2, 12))
  fit_over <- stats::glm(cbind(over, tested - over) ~ 1, family = stats::binomial())
  testthat::expect_gt(routineqc:::.model_dispersion(fit_over), 2)
  # Identical proportions everywhere leave no residual variation at all.
  flat <- rep(6, n)
  fit_flat <- stats::glm(cbind(flat, tested - flat) ~ 1, family = stats::binomial())
  testthat::expect_identical(routineqc:::.model_dispersion(fit_flat), 1)
  testthat::expect_true(is.na(routineqc:::.model_dispersion(NULL)))
})

testthat::test_that('an all-negative month is flagged only when the model finds it improbable', {
  dat <- prevalence_fixture(
    tested = c(60, 60, 60, 60, 40), positive = c(0, 0, 0, 3, 0),
    residual = 0
  )
  # Zero of 60 at 1% expected is ordinary; at 20% it is not; unassessed is
  # not evaluated; a non-zero month and one below the tested minimum never flag.
  dat$p_hat <- c(0.01, 0.2, NA, 0.2, 0.5)
  out <- routineqc::add_prevalence_qc_flags(dat)
  testthat::expect_identical(out$flag_all_negative_large_n, c(FALSE, TRUE, NA, FALSE, FALSE))
  testthat::expect_identical(out$flag_prevalence_extreme[3], FALSE)

  # Dispersion deflates the tested count: (1 - 0.15)^(60 / 4) is about 0.087,
  # against 6e-5 at dispersion 1.
  dat$p_hat <- 0.15
  dat$resid_dispersion <- 4
  testthat::expect_false(routineqc::add_prevalence_qc_flags(dat)$flag_all_negative_large_n[1])
  dat$resid_dispersion <- 1
  testthat::expect_true(routineqc::add_prevalence_qc_flags(dat)$flag_all_negative_large_n[1])

  # Without a model the count-only rule applies.
  plain <- routineqc::add_prevalence_qc_flags(dplyr::select(dat, -p_hat, -resid_dispersion))
  testthat::expect_identical(plain$flag_all_negative_large_n, c(TRUE, TRUE, TRUE, FALSE, FALSE))
})
