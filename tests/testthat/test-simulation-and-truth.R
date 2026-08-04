testthat::test_that("simulated injected errors are detectable", {
  sim <- routineqc::simulate_qc_data(n_facilities = 12, n_months = 12, seed = 2)

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

  eval <- routineqc::evaluate_qc_against_truth(out$data_flagged, truth_var = "injected_error")

  testthat::expect_true(eval$recall >= 0.2)
  testthat::expect_true(eval$precision > 0)
})

testthat::test_that("no QC step silently drops rows", {
  sim <- routineqc::simulate_qc_data(n_facilities = 4, n_months = 8, seed = 3)

  x1 <- routineqc::prepare_qc_data(sim, "facility", "region", "month", "tested", "positive")
  x2 <- routineqc::flag_logical_errors(x1)
  mod <- routineqc::fit_prevalence_gam(x2, nthreads = 1)
  x3 <- routineqc::add_prevalence_predictions(x2, mod)
  x4 <- routineqc::add_prevalence_qc_flags(x3)
  x5 <- routineqc::add_tested_volume_qc(x4)
  x6 <- routineqc::add_temporal_qc_flags(x5)
  x7 <- routineqc::assign_qc_action(x6)

  testthat::expect_equal(nrow(sim), nrow(x1))
  testthat::expect_equal(nrow(sim), nrow(x2))
  testthat::expect_equal(nrow(sim), nrow(x3))
  testthat::expect_equal(nrow(sim), nrow(x4))
  testthat::expect_equal(nrow(sim), nrow(x5))
  testthat::expect_equal(nrow(sim), nrow(x6))
  testthat::expect_equal(nrow(sim), nrow(x7))
})