count_frame <- function(tested, positive = rep(1, length(tested))) {
  data.frame(
    facility = rep('f1', length(tested)),
    region = 'r1',
    month = seq(as.Date('2024-01-01'), by = 'month', length.out = length(tested)),
    tested = tested,
    positive = positive,
    stringsAsFactors = FALSE
  )
}

prep_counts <- function(df) {
  suppressWarnings(routineqc::prepare_qc_data(
    df, facility_var = 'facility', region_var = 'region', month_var = 'month',
    tested_var = 'tested', positive_var = 'positive'
  ))
}

testthat::test_that('factor counts are read by label, not integer code', {
  out <- prep_counts(count_frame(factor(c('100', '200')), factor(c('10', '20'))))
  testthat::expect_equal(out$tested, c(100, 200))
  testthat::expect_equal(out$positive, c(10, 20))
  testthat::expect_identical(out$tested_parse_status, c('ok', 'ok'))
  testthat::expect_identical(out$tested_source, c('100', '200'))
})

testthat::test_that('malformed, missing, non-finite and fractional counts are told apart', {
  parsed <- routineqc:::.parse_counts(c('5', ' 7 ', '', NA, 'n/a', '1,000', 'Inf', '2.5'))
  testthat::expect_identical(
    parsed$status,
    c('ok', 'ok', 'missing', 'missing', 'malformed', 'malformed', 'non_finite', 'fractional')
  )
  testthat::expect_equal(parsed$value, c(5, 7, NA, NA, NA, NA, NA, 2.5))
  numeric <- routineqc:::.parse_counts(c(1, NA, NaN, Inf, -Inf, 3.25))
  testthat::expect_identical(
    numeric$status, c('ok', 'missing', 'non_finite', 'non_finite', 'non_finite', 'fractional')
  )
  testthat::expect_identical(routineqc:::.parse_counts(c(TRUE, NA))$status, c('malformed', 'missing'))
})

testthat::test_that('prepare_qc_data keeps original values, warns, and flags invalid counts', {
  df <- count_frame(c('100', 'n/a', '50.5', 'Inf', NA))
  testthat::expect_warning(
    out <- routineqc::prepare_qc_data(
      df, facility_var = 'facility', region_var = 'region', month_var = 'month',
      tested_var = 'tested', positive_var = 'positive'
    ),
    'not valid counts'
  )
  testthat::expect_equal(nrow(out), 5L)
  testthat::expect_identical(out$tested_source, df$tested)
  testthat::expect_equal(out$tested[3], 50.5)
  flagged <- routineqc::flag_logical_errors(out)
  testthat::expect_identical(flagged$flag_count_invalid_value, c(FALSE, TRUE, TRUE, TRUE, FALSE))
  testthat::expect_false(any(flagged$flag_core_invalid))
})

testthat::test_that('invalid counts are reviewed, not excluded, and never dropped', {
  df <- count_frame(c(100, 100, 100.5), c(5, 5, 5))
  out <- routineqc::assign_qc_action(routineqc::flag_logical_errors(prep_counts(df)))
  testthat::expect_equal(nrow(out), 3L)
  testthat::expect_identical(out$qc_action[3], 'review_core_invalid')
  testthat::expect_false(out$flag_exclude_authorized[3])
  testthat::expect_match(out$qc_reason[3], 'not a valid whole number')
})

testthat::test_that('adapter diagnostics separate missing, malformed, non-finite and fractional', {
  source <- data.frame(
    id = paste0('r', 1:5), fac = 'f1', reg = 'r1',
    mon = seq(as.Date('2024-01-01'), by = 'month', length.out = 5),
    t = c('10', '', 'x', 'Inf', '4.5'), p = '1',
    stringsAsFactors = FALSE
  )
  result <- routineqc::adapt_qc_data(
    source,
    mapping = list(facility_id = 'fac', month_date = 'mon', region = 'reg', tested = 't',
                   positive = 'p', source_record_id = 'id'),
    adapter = 'test', adapter_version = '0', dataset_id = 'd', source_system = 's',
    identity_strategy = 'stable source facility code'
  )
  testthat::expect_equal(nrow(result$data), 5L)
  codes <- stats::setNames(result$diagnostics$issue_code, result$diagnostics$source_record_id)
  testthat::expect_identical(
    codes[c('r2', 'r3', 'r4', 'r5')],
    c(r2 = 'missing_count', r3 = 'unparseable_count', r4 = 'non_finite_count', r5 = 'fractional_count')
  )
  testthat::expect_identical(result$data$tested_source, source$t)
  testthat::expect_identical(
    result$data$tested_parse_status, c('ok', 'missing', 'malformed', 'non_finite', 'fractional')
  )
})

testthat::test_that('adapter translation survives prepare_qc_data without overwriting the source', {
  canonical <- data.frame(
    facility_id = 'f1', region = 'r1',
    month_date = seq(as.Date('2024-01-01'), by = 'month', length.out = 2),
    tested = c(NA, 10), positive = c(1, 1),
    tested_source = c('n/a', '10'), positive_source = c('1', '1'),
    tested_parse_status = c('malformed', 'ok'), positive_parse_status = c('ok', 'ok'),
    stringsAsFactors = FALSE
  )
  out <- testthat::expect_no_warning(routineqc::prepare_qc_data(
    canonical, facility_var = 'facility_id', region_var = 'region', month_var = 'month_date',
    tested_var = 'tested', positive_var = 'positive'
  ))
  testthat::expect_identical(out$tested_source, c('n/a', '10'))
  testthat::expect_identical(out$tested_parse_status, c('malformed', 'ok'))
})

invalid_count_run <- function(config = routineqc::qc_config()) {
  sim <- routineqc::simulate_qc_data(n_facilities = 8, n_months = 24, seed = 11)
  sim$tested <- as.character(sim$tested)
  sim$positive <- as.character(sim$positive)
  sim$tested[5] <- '50.5'
  sim$tested[6] <- 'n/a'
  sim$positive[7] <- 'Inf'
  sim$tested[8] <- NA
  sim$positive[9] <- '9999'
  sim$tested[9] <- '10'
  suppressWarnings(routineqc::run_routine_qc(
    sim, facility_var = 'facility', region_var = 'region', month_var = 'month',
    tested_var = 'tested', positive_var = 'positive', config = config, nthreads = 1
  ))
}

testthat::test_that('invalid counts stay traceable and review-only, with distinct statuses', {
  run <- invalid_count_run()
  d <- run$data_flagged
  testthat::expect_equal(nrow(d), 8 * 24)
  testthat::expect_identical(d$tested_source[5:6], c('50.5', 'n/a'))
  testthat::expect_identical(d$positive_source[7], 'Inf')
  testthat::expect_equal(d$tested[5], 50.5)
  testthat::expect_identical(d$tested_parse_status[5:6], c('fractional', 'malformed'))
  testthat::expect_true(all(d$flag_count_invalid_value[5:7]))
  testthat::expect_false(any(d$flag_count_invalid_value[8]))
  testthat::expect_identical(d$prediction_status[5:7], rep('ineligible_invalid_count', 3))
  testthat::expect_identical(d$prediction_status[8], 'ineligible_missing_counts')
  testthat::expect_identical(d$prediction_status[9], 'ineligible_core_counts')
  testthat::expect_true(all(d$qc_action[5:7] == 'review_core_invalid'))
  testthat::expect_false(any(d$flag_exclude_authorized[5:7]))
  testthat::expect_identical(d$qc_action[9], 'exclude_core_invalid')
  testthat::expect_false(any(d$model_assessment_eligible[5:9]))
})

testthat::test_that('invalid counts are left out of every calculation', {
  run <- invalid_count_run()
  d <- run$data_flagged
  usable <- !d$flag_count_invalid_value & !is.na(d$tested) & !is.na(d$positive) & !d$flag_exclude_authorized
  expected_before <- sum(d$positive[!d$flag_count_invalid_value & !is.na(d$tested) & !is.na(d$positive)]) /
    sum(d$tested[!d$flag_count_invalid_value & !is.na(d$tested) & !is.na(d$positive)])
  before <- routineqc:::.before_after_prevalence(d)$before
  testthat::expect_equal(before, expected_before)
  testthat::expect_equal(
    sum(run$summaries$by_region$invalid_count_rows), sum(d$flag_count_invalid_value)
  )
  testthat::expect_equal(
    sum(run$summaries$by_region$total_tested_before_qc),
    sum(d$tested[!d$flag_count_invalid_value], na.rm = TRUE)
  )
  # The fractional value neither enters the model nor the tested-volume baseline.
  refit <- routineqc::fit_prevalence_gam(d, nthreads = 1)
  testthat::expect_equal(
    nrow(refit$model),
    sum(!d$flag_core_invalid & !d$flag_count_invalid_value & !is.na(d$positive) & d$tested > 0,
        na.rm = TRUE)
  )
  facility <- d$facility_id[5]
  later <- d[d$facility_id == facility & d$month_date > d$month_date[5], ]
  trailing_n <- later$tested_baseline_n[1]
  clean <- d
  clean$tested[5] <- NA
  clean <- routineqc::add_tested_volume_qc(clean)
  after_clean <- clean[clean$facility_id == facility & clean$month_date > d$month_date[5], ]
  testthat::expect_identical(after_clean$tested_baseline_n[1], trailing_n)
})

testthat::test_that('model_unavailable is only given to otherwise eligible rows', {
  sim <- routineqc::simulate_qc_data(n_facilities = 8, n_months = 24, seed = 11)
  sim$tested <- as.character(sim$tested)
  sim$tested[1] <- '50.5'
  sim$tested[2] <- NA
  sim$tested[3] <- '0'
  sim$positive[3] <- '4'
  prepared <- suppressWarnings(routineqc::prepare_qc_data(
    sim, facility_var = 'facility', region_var = 'region', month_var = 'month',
    tested_var = 'tested', positive_var = 'positive'
  ))
  out <- routineqc::add_prevalence_predictions(routineqc::flag_logical_errors(prepared), NULL)
  testthat::expect_identical(
    out$prediction_status[1:3],
    c('ineligible_invalid_count', 'ineligible_missing_counts', 'ineligible_core_counts')
  )
  rest <- out[-(1:3), ]
  testthat::expect_identical(
    rest$prediction_status,
    ifelse(rest$flag_core_invalid, 'ineligible_core_counts', 'model_unavailable')
  )
  testthat::expect_identical(out$model_assessment_eligible, out$prediction_status == 'model_unavailable')
})
