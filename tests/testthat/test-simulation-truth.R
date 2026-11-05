truth_sim <- function(...) {
  routineqc::simulate_qc_truth(n_regions = 2, councils_per_region = 2, facilities_per_council = 4,
                               n_months = 30, seed = 5, ...)
}

testthat::test_that('truth simulator has a consistent hierarchy and valid truth', {
  sim <- truth_sim(missing = 'none', error_rate = 0)
  fac <- unique(sim$truth[c('facility_id', 'council', 'region')])
  testthat::expect_equal(anyDuplicated(fac$facility_id), 0L)
  council <- unique(sim$truth[c('council', 'region')])
  testthat::expect_equal(anyDuplicated(council$council), 0L)
  testthat::expect_true(all(sim$truth$positive <= sim$truth$tested))
  testthat::expect_true(all(sim$truth$attending >= sim$truth$tested))
  testthat::expect_identical(sim$observed$tested, as.numeric(sim$truth$tested))
  testthat::expect_true(all(is.na(sim$injected$error_type)))
  testthat::expect_true(any(sim$truth$genuine_change))
})

testthat::test_that('the simulator is reproducible and each mechanism produces its own pattern', {
  a <- truth_sim(missing = 'consecutive')
  b <- truth_sim(missing = 'consecutive')
  testthat::expect_identical(a$observed, b$observed)
  for (mech in c('isolated', 'consecutive', 'whole_facility', 'volume_related')) {
    s <- truth_sim(missing = mech, missing_rate = 0.1)
    lost <- s$injected$missing_mechanism
    testthat::expect_true(any(lost == mech, na.rm = TRUE), info = mech)
    testthat::expect_true(all(is.na(s$observed$tested[!is.na(lost)])), info = mech)
  }
  whole <- truth_sim(missing = 'whole_facility', missing_rate = 0.15)
  by_fac <- tapply(is.na(whole$observed$tested), whole$observed$facility, mean)
  testthat::expect_true(any(by_fac == 1))
  vol <- truth_sim(missing = 'volume_related', missing_rate = 0.15)
  lost <- !is.na(vol$injected$missing_mechanism)
  testthat::expect_gt(mean(vol$truth$true_positivity[lost]), mean(vol$truth$true_positivity[!lost]))
})

testthat::test_that('injected errors are recorded and only on reported rows', {
  sim <- truth_sim(missing = 'isolated', error_rate = 0.1)
  err <- !is.na(sim$injected$error_type)
  testthat::expect_true(any(err))
  testthat::expect_true(all(is.na(sim$injected$missing_mechanism[err])))
  testthat::expect_true(all(sim$observed$tested[err] != sim$truth$tested[err] |
                              sim$observed$positive[err] != sim$truth$positive[err]))
})

testthat::test_that('simulator validates its arguments', {
  testthat::expect_error(routineqc::simulate_qc_truth(missing_rate = 0.9), 'missing_rate')
  testthat::expect_error(routineqc::simulate_qc_truth(missing = 'nope'))
})

testthat::test_that('recovery evaluation reports error, coverage, detection and change survival', {
  sim <- truth_sim(missing = c('isolated', 'consecutive'), error_rate = 0.05)
  presets <- routineqc::qc_scenario_presets()[c('relaxed', 'strict')]
  ev <- routineqc::evaluate_qc_recovery(sim, presets, n_imputations = 4)
  testthat::expect_setequal(unique(ev$summary$version), c('reported', 'filtered_missing', 'filtered_imputed'))
  testthat::expect_setequal(unique(ev$summary$level), c('council', 'region', 'national'))
  testthat::expect_true(all(c('bias', 'rmse', 'interval_coverage') %in% names(ev$summary)))
  testthat::expect_equal(sum(ev$units$level == 'national' & ev$units$scenario == 'strict'), 3L)
  det <- ev$detection
  testthat::expect_true(all(det$errors_omitted >= 0 & det$errors_omitted <= 1))
  testthat::expect_true(det$tested_retained[det$scenario == 'strict'] <=
                          det$tested_retained[det$scenario == 'relaxed'])
  testthat::expect_true(det$errors_omitted[det$scenario == 'strict'] >=
                          det$errors_omitted[det$scenario == 'relaxed'])
  chg <- ev$change
  testthat::expect_true(all(c('true_change', 'estimated_change', 'change_rows_omitted') %in% names(chg)))
  testthat::expect_gt(chg$true_change[1], 0)
  testthat::expect_match(ev$settings$note, 'not evidence')
  by_unit <- ev$units[ev$units$level == 'national' & ev$units$version == 'reported', ]
  testthat::expect_equal(by_unit$error, by_unit$positivity - by_unit$truth)
})

testthat::test_that('a clean, complete data set is recovered exactly with no omissions', {
  sim <- truth_sim(missing = 'none', error_rate = 0, abrupt_change = FALSE)
  ev <- routineqc::evaluate_qc_recovery(sim, routineqc::qc_scenario_presets()['relaxed'], impute = FALSE)
  nat <- ev$units[ev$units$level == 'national' & ev$units$version == 'reported', ]
  testthat::expect_equal(nat$positivity, nat$truth)
  testthat::expect_equal(ev$detection$clean_omitted, 0)
  testthat::expect_equal(nrow(ev$change), 0L)
})

testthat::test_that('the mechanism grid stacks results with a mechanism label', {
  grid <- routineqc::evaluate_qc_recovery_grid(
    c('isolated', 'volume_related'),
    sim_args = list(n_regions = 2, councils_per_region = 2, facilities_per_council = 4,
                    n_months = 30, seed = 5),
    scenarios = routineqc::qc_scenario_presets()['moderate'], impute = FALSE, levels = 'national'
  )
  testthat::expect_setequal(unique(grid$summary$mechanism), c('isolated', 'volume_related'))
  testthat::expect_true(all(c('detection', 'change') %in% names(grid)))
})
