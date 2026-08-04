testthat::test_that("qc_action priority order is respected", {
  dat <- tibble::tibble(
    tested = c(10, 10, 0, 10),
    positive = c(1, 1, 0, 1),
    flag_invalid_logical = c(TRUE, FALSE, FALSE, FALSE),
    flag_resid_extreme = c(FALSE, TRUE, FALSE, FALSE),
    flag_all_negative_large_n = c(FALSE, FALSE, FALSE, FALSE),
    flag_all_positive_large_n = c(FALSE, FALSE, FALSE, FALSE),
    flag_large_monthly_prevalence_change = c(FALSE, FALSE, TRUE, FALSE),
    flag_prevalence_extreme = c(FALSE, FALSE, TRUE, FALSE),
    flag_tested_volume_extreme = c(FALSE, FALSE, FALSE, TRUE),
    flag_isolated_extreme = c(FALSE, FALSE, FALSE, FALSE)
  )

  out <- routineqc::assign_qc_action(dat)

  testthat::expect_identical(out$qc_action[1], "remove_invalid_logical")
  testthat::expect_identical(out$qc_action[2], "review_or_remove_high_confidence")
  testthat::expect_identical(out$qc_action[3], "review_prevalence_extreme")
  testthat::expect_identical(out$qc_action[4], "review_tested_volume")
})

testthat::test_that("wrapper returns expected objects and preserves row count", {
  sim <- routineqc::simulate_qc_data(n_facilities = 8, n_months = 12, seed = 1)

  out <- routineqc::run_routine_qc(
    sim,
    facility_var = "facility",
    region_var = "region",
    month_var = "month",
    tested_var = "tested",
    positive_var = "positive",
    council_var = "council",
    district_var = "district",
    nthreads = 1
  )

  testthat::expect_true(is.list(out))
  testthat::expect_true(all(c("data_flagged", "model", "summaries") %in% names(out)))
  testthat::expect_equal(nrow(out$data_flagged), nrow(sim))
  testthat::expect_true(all(c("by_region", "by_month", "by_facility", "by_council", "by_district") %in% names(out$summaries)))
})
