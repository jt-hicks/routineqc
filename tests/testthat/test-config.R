testthat::test_that('recommended configuration records agreed defaults', {
  config <- routineqc::qc_config()
  testthat::expect_s3_class(config, 'routineqc_config')
  testthat::expect_identical(config$profile, 'recommended')
  testthat::expect_identical(config$schema_version, 2L)
  testthat::expect_equal(config$prevalence$resid_threshold_large, 4)
  testthat::expect_equal(config$prevalence$resid_threshold_small, 5)
  testthat::expect_equal(config$prevalence$all_negative_min_tested, 50)
  testthat::expect_true(config$temporal$require_adjacent_months)
  testthat::expect_identical(config$tested_volume$baseline_mode, 'trailing')
  testthat::expect_false(config$tested_volume$baseline_uses_future)
  testthat::expect_invisible(routineqc::validate_qc_config(config))
})

testthat::test_that('purpose-specific profiles record future-data use', {
  operational <- routineqc::qc_config('operational')
  retrospective <- routineqc::qc_config('retrospective')
  testthat::expect_identical(operational$tested_volume$baseline_mode, 'trailing')
  testthat::expect_false(operational$tested_volume$baseline_uses_future)
  testthat::expect_identical(retrospective$tested_volume$baseline_mode, 'centered')
  testthat::expect_true(retrospective$tested_volume$baseline_uses_future)
})

testthat::test_that('permissive sensitivity profile is explicit', {
  config <- routineqc::qc_config('permissive_sensitivity')
  testthat::expect_equal(config$prevalence$resid_threshold, 4)
  testthat::expect_equal(config$prevalence$all_negative_min_tested, 30)
  testthat::expect_equal(config$prevalence$large_change_threshold, 0.35)
  testthat::expect_false(config$prevalence$large_change_require_adjacent)
  testthat::expect_false(config$temporal$require_adjacent_months)
  testthat::expect_false(config$tested_volume$require_both_extreme)
})

testthat::test_that('configuration overrides are validated', {
  config <- routineqc::qc_config(
    prevalence = list(resid_threshold_small = 6),
    tested_volume = list(tested_z_threshold = 6)
  )
  testthat::expect_equal(config$prevalence$resid_threshold_small, 6)
  testthat::expect_equal(config$tested_volume$tested_z_threshold, 6)
  testthat::expect_error(
    routineqc::qc_config(prevalence = list(unknown_rule = 1)),
    'Unknown prevalence setting'
  )
  testthat::expect_error(
    routineqc::qc_config(prevalence = list(resid_threshold_large = -1)),
    'non-negative'
  )
  testthat::expect_error(
    routineqc::qc_config(tested_volume = list(min_roll_n = 2.5)),
    'positive whole number'
  )
  testthat::expect_error(
    routineqc::qc_config(tested_volume = list(baseline_mode = 'centered')),
    'must agree'
  )
  broken <- config
  broken$temporal$require_adjacent_months <- 'yes'
  testthat::expect_error(routineqc::validate_qc_config(broken), 'TRUE or FALSE')
})
