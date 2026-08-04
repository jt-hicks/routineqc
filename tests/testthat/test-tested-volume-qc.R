testthat::test_that("tested volume QC creates rolling columns and flags", {
  dat <- tibble::tibble(
    facility_id = rep("F1", 10),
    region = "R1",
    month_date = seq.Date(as.Date("2023-01-01"), by = "month", length.out = 10),
    tested = c(100, 102, 98, 101, 99, 100, 500, 95, 20, 0),
    positive = c(10, 10, 9, 10, 9, 10, 45, 8, 2, 0),
    prevalence = positive / tested,
    month_num = lubridate::month(month_date),
    time_index = seq_along(month_date),
    pearson_resid = 0
  )

  out <- routineqc::add_tested_volume_qc(dat, min_roll_n = 6)

  testthat::expect_true(all(c(
    "tested_roll_median", "tested_roll_mad", "tested_robust_z",
    "flag_tested_extreme_high", "flag_tested_extreme_low",
    "flag_tested_large_jump", "flag_tested_large_drop",
    "flag_tested_volume_extreme"
  ) %in% names(out)))

  testthat::expect_true(any(out$flag_tested_large_jump, na.rm = TRUE))
  testthat::expect_true(any(out$flag_tested_volume_extreme, na.rm = TRUE))
})