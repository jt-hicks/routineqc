action_fixture <- function() {
  tibble::tibble(
    tested = c(10, 10, 10, 10, 10, 10, 10),
    positive = c(11, 1, 1, 1, 1, 1, 1),
    flag_core_invalid = c(TRUE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE),
    flag_attendance_issue = c(FALSE, TRUE, FALSE, FALSE, FALSE, FALSE, FALSE),
    flag_resid_extreme = c(FALSE, FALSE, TRUE, TRUE, FALSE, FALSE, FALSE),
    flag_prevalence_extreme = c(FALSE, FALSE, TRUE, TRUE, FALSE, FALSE, FALSE),
    flag_tested_volume_extreme = c(FALSE, FALSE, TRUE, FALSE, TRUE, FALSE, FALSE),
    flag_temporal_context_insufficient = c(FALSE, FALSE, FALSE, FALSE, FALSE, TRUE, FALSE)
  )
}

testthat::test_that('conservative policy excludes only core-invalid rows', {
  out <- routineqc::assign_qc_action(action_fixture())
  testthat::expect_identical(
    out$qc_action,
    c('exclude_core_invalid', 'review_attendance', 'review_multiple_signals',
      'review_prevalence', 'review_tested_volume',
      'review_temporal_context', 'retain')
  )
  testthat::expect_identical(
    out$review_priority,
    c('critical', 'medium', 'high', 'medium', 'medium', 'low', 'none')
  )
  testthat::expect_identical(which(out$flag_exclude_authorized), 1L)
  testthat::expect_false(any(out$flag_exclude_authorized[-1]))
  testthat::expect_true(grepl('prevalence residual extreme', out$qc_reason[3]))
  testthat::expect_true(grepl('tested-volume anomaly', out$qc_reason[3]))
})

testthat::test_that('flags-only policy never authorizes exclusion', {
  out <- routineqc::assign_qc_action(action_fixture(), policy = 'flags_only')
  testthat::expect_identical(out$qc_action[1], 'review_core_invalid')
  testthat::expect_true(out$flag_review_recommended[1])
  testthat::expect_false(any(out$flag_exclude_authorized))
  testthat::expect_true(all(out$action_policy == 'flags_only'))
})

testthat::test_that('authorized-exclusion summaries do not remove review rows', {
  out <- routineqc::assign_qc_action(action_fixture())
  summary <- routineqc::summarise_before_after_qc(out, by = 'overall')
  testthat::expect_equal(summary$authorized_exclusion_rows, 1)
  testthat::expect_equal(summary$review_recommended_rows, 5)
  testthat::expect_equal(summary$total_tested_before_qc, 70)
  testthat::expect_equal(summary$total_tested_after_authorized_exclusions, 60)
})

testthat::test_that('wrapper captures and applies its action policy', {
  sim <- routineqc::simulate_qc_data(n_facilities = 8, n_months = 12, seed = 1)
  config <- routineqc::qc_config(action_policy = list(name = 'flags_only'))
  out <- routineqc::run_routine_qc(
    sim,
    facility_var = 'facility', region_var = 'region', month_var = 'month',
    tested_var = 'tested', positive_var = 'positive',
    council_var = 'council', district_var = 'district',
    config = config, nthreads = 1
  )
  testthat::expect_true(all(c('data_flagged', 'model', 'summaries', 'config') %in% names(out)))
  testthat::expect_s3_class(out, 'routineqc_run')
  testthat::expect_true('manifest' %in% names(out))
  testthat::expect_s3_class(out$config, 'routineqc_config')
  testthat::expect_identical(out$config$action_policy$name, 'flags_only')
  testthat::expect_false(any(out$data_flagged$flag_exclude_authorized))
  testthat::expect_equal(nrow(out$data_flagged), nrow(sim))
  testthat::expect_true(all(c('by_region', 'by_month', 'by_facility', 'by_council', 'by_district') %in% names(out$summaries)))
})
