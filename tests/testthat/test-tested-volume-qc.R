testthat::test_that("tested volume QC creates rolling columns and flags", {
  dat <- tibble::tibble(
    facility_id = rep("F1", 10),
    region = "R1",
    month_date = seq.Date(as.Date("2023-01-01"), by = "month", length.out = 10),
    tested = c(100, 102, 98, 101, 99, 100, 501, 95, 20, 0),
    positive = c(10, 10, 9, 10, 9, 10, 45, 8, 2, 0),
    prevalence = positive / tested,
    month_num = lubridate::month(month_date),
    time_index = seq_along(month_date),
    pearson_resid = 0
  )

  out <- routineqc::add_tested_volume_qc(dat, min_roll_n = 6)

  testthat::expect_true(all(c(
    'tested_baseline_n', 'tested_baseline_mode', 'baseline_uses_future',
    "tested_roll_median", "tested_roll_mad", "tested_robust_z",
    "flag_tested_extreme_high", "flag_tested_extreme_low",
    "flag_tested_large_jump", "flag_tested_large_drop",
    "flag_tested_volume_extreme"
  ) %in% names(out)))

  testthat::expect_true(any(out$flag_tested_large_jump, na.rm = TRUE))
  testthat::expect_true(any(out$flag_tested_volume_extreme, na.rm = TRUE))
  testthat::expect_true(all(out$tested_baseline_mode == 'trailing'))
  testthat::expect_false(any(out$baseline_uses_future))
})

testthat::test_that('centered baseline can assess early historical rows', {
  dat <- tibble::tibble(
    facility_id = 'F1',
    month_date = seq.Date(as.Date('2023-01-01'), by = 'month', length.out = 7),
    tested = c(500, 100, 101, 99, 100, 102, 98)
  )
  trailing <- routineqc::add_tested_volume_qc(dat)
  centered <- routineqc::add_tested_volume_qc(
    dat, baseline_mode = 'centered', baseline_uses_future = TRUE
  )
  testthat::expect_equal(trailing$tested_baseline_n[1], 0)
  testthat::expect_false(trailing$flag_tested_extreme_high[1])
  testthat::expect_equal(centered$tested_baseline_n[1], 6)
  testthat::expect_true(centered$flag_tested_extreme_high[1])
  testthat::expect_true(all(centered$baseline_uses_future))
})

testthat::test_that('calendar gaps reduce baseline evidence', {
  dat <- tibble::tibble(
    facility_id = 'F1',
    month_date = as.Date(c('2023-01-01', '2023-08-01')),
    tested = c(100, 500)
  )
  out <- routineqc::add_tested_volume_qc(dat, min_roll_n = 1)
  testthat::expect_equal(out$tested_baseline_n, c(0L, 0L))
  testthat::expect_false(any(out$tested_baseline_ready))
})

testthat::test_that('high and low flags require corroborating evidence by default', {
  dat <- tibble::tibble(
    facility_id = 'F1',
    month_date = seq.Date(as.Date('2023-01-01'), by = 'month', length.out = 7),
    tested = c(20, 40, 80, 120, 160, 180, 310)
  )
  strict <- routineqc::add_tested_volume_qc(dat)
  permissive <- routineqc::add_tested_volume_qc(dat, require_both_extreme = FALSE)
  testthat::expect_true(strict$flag_tested_high_ratio[7])
  testthat::expect_false(strict$flag_tested_high_z[7])
  testthat::expect_false(strict$flag_tested_extreme_high[7])
  testthat::expect_true(permissive$flag_tested_extreme_high[7])
})

testthat::test_that('minimum volumes suppress dramatic ratios among tiny counts', {
  dat <- tibble::tibble(
    facility_id = 'F1',
    month_date = as.Date(c('2023-01-01', '2023-02-01')),
    tested = c(1, 6)
  )
  out <- routineqc::add_tested_volume_qc(dat, min_roll_n = 1)
  testthat::expect_true(out$tested_to_prev_ratio[2] > 5)
  testthat::expect_false(out$flag_tested_large_jump[2])
})
