action_fixture <- function() {
  tibble::tibble(
    tested = c(10, 10, 10, 10, 10, 10, 10),
    positive = c(11, 1, 1, 1, 1, 1, 1),
    flag_core_invalid = c(TRUE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE),
    flag_attendance_issue = c(FALSE, TRUE, FALSE, FALSE, FALSE, FALSE, FALSE),
    flag_resid_extreme = c(FALSE, FALSE, TRUE, TRUE, FALSE, FALSE, FALSE),
    flag_prevalence_extreme = c(FALSE, FALSE, TRUE, TRUE, FALSE, FALSE, FALSE),
    flag_tested_volume_extreme = c(FALSE, FALSE, TRUE, FALSE, TRUE, FALSE, FALSE),
    flag_temporal_context_insufficient = c(FALSE, FALSE, FALSE, FALSE, FALSE, TRUE, FALSE),
    flag_positive_gt_tested = c(TRUE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE),
    flag_tested_gt_attending = c(FALSE, TRUE, FALSE, FALSE, FALSE, FALSE, FALSE),
    flag_tested_extreme_high = c(FALSE, FALSE, TRUE, FALSE, FALSE, FALSE, FALSE),
    flag_tested_large_drop = c(FALSE, FALSE, FALSE, FALSE, TRUE, FALSE, FALSE)
  )
}

testthat::test_that('conservative policy excludes only core-invalid rows', {
  out <- routineqc::assign_qc_action(action_fixture())
  testthat::expect_identical(
    out$qc_action,
    c('exclude_core_invalid', 'review_attendance', 'review_multiple_signals',
      'review_prevalence', 'review_tested_volume', 'retain', 'retain')
  )
  # Priority follows the per-flag ranking: a slight attendance excess is low,
  # prevalence and tested-volume outliers high. Insufficient temporal context on
  # its own is a qualifier of a residual flag, not a finding.
  testthat::expect_identical(
    out$review_priority,
    c('critical', 'low', 'high', 'high', 'high', 'none', 'none')
  )
  testthat::expect_identical(which(out$flag_exclude_authorized), 1L)
  testthat::expect_false(any(out$flag_exclude_authorized[-1]))
  testthat::expect_true(grepl('prevalence outlier', out$qc_reason[3]))
  testthat::expect_true(grepl('tested far above baseline', out$qc_reason[3]))
  testthat::expect_identical(out$qc_reason[2], 'tested slightly above attendance')
})

testthat::test_that('priority comes from the per-flag ranking and can be overridden', {
  dat <- tibble::tibble(
    tested = 10, positive = 1,
    flag_all_positive_large_n = c(TRUE, FALSE, FALSE, FALSE, FALSE),
    flag_prevalence_extreme = c(TRUE, FALSE, FALSE, FALSE, FALSE),
    flag_coverage_low = c(FALSE, TRUE, TRUE, FALSE, FALSE),
    flag_coverage_very_low = c(FALSE, FALSE, TRUE, FALSE, FALSE),
    flag_tested_gt_attending = c(FALSE, FALSE, FALSE, TRUE, FALSE),
    flag_tested_gt_attending_major = c(FALSE, FALSE, FALSE, TRUE, FALSE),
    flag_attendance_issue = c(FALSE, FALSE, FALSE, TRUE, FALSE),
    flag_tested_zero_unusual = c(FALSE, FALSE, FALSE, FALSE, TRUE),
    flag_tested_volume_extreme = c(FALSE, FALSE, FALSE, FALSE, TRUE)
  )
  out <- routineqc::assign_qc_action(dat)
  testthat::expect_identical(out$review_priority, c('critical', 'low', 'medium', 'medium', 'low'))
  testthat::expect_identical(
    out$qc_action,
    c('review_prevalence', 'review_coverage', 'review_coverage', 'review_attendance', 'review_tested_volume')
  )
  # Critical for review never means exclusion.
  testthat::expect_false(any(out$flag_exclude_authorized))

  bumped <- routineqc::assign_qc_action(dat, priority = list(coverage_low = 'high'))
  testthat::expect_identical(bumped$review_priority[2:3], c('high', 'high'))
  testthat::expect_error(routineqc::qc_config(priority = list(coverage_low = 'urgent')), 'Priority must be one of')
  testthat::expect_error(routineqc::qc_config(priority = list(no_such_flag = 'low')), 'Unknown priority')
})

testthat::test_that('a domain summary flag alone still gets a review priority', {
  out <- routineqc::assign_qc_action(tibble::tibble(flag_attendance_issue = TRUE))
  testthat::expect_identical(out$qc_action, 'review_attendance')
  testthat::expect_identical(out$review_priority, 'medium')
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
  testthat::expect_equal(summary$review_recommended_rows, 4)
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
  testthat::expect_true(all(c('count_completeness_by_council', 'count_completeness_by_district') %in% names(out$summaries)))
})

testthat::test_that('a missing positive count is retained and still assessed for volume', {
  sim <- routineqc::simulate_qc_data(n_facilities = 8, n_months = 24, seed = 2)
  sim$positive[c(5, 30)] <- NA
  sim$tested[40] <- NA
  out <- suppressWarnings(routineqc::run_routine_qc(
    sim,
    facility_var = 'facility', region_var = 'region', month_var = 'month',
    tested_var = 'tested', positive_var = 'positive', nthreads = 1
  ))
  d <- out$data_flagged
  rows <- c(5, 30, 40)
  testthat::expect_identical(d$core_count_status[rows], c('missing_positive', 'missing_positive', 'missing_tested'))
  testthat::expect_false(any(d$flag_core_invalid[rows]))
  testthat::expect_false(any(d$flag_exclude_authorized[rows]))
  testthat::expect_true(all(d$prediction_status[rows] == 'ineligible_missing_counts'))
  testthat::expect_false(any(grepl('core', d$qc_reason[rows])))
  # Tested volume depends only on tested, so it is still evaluated there.
  testthat::expect_true(all(!is.na(d$tested_robust_z[c(5, 30)]) | !d$tested_baseline_ready[c(5, 30)]))
  testthat::expect_true(is.na(d$tested_robust_z[40]))
})
