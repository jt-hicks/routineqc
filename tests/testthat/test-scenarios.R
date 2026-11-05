scenario_data <- function() {
  months <- seq(as.Date('2024-01-01'), by = 'month', length.out = 12)
  d <- tidyr::crossing(facility_id = paste0('F', 1:4), month_date = months)
  d$region <- ifelse(d$facility_id %in% c('F1', 'F2'), 'R1', 'R2')
  d$council <- ifelse(d$facility_id %in% c('F1', 'F3'), 'C1', 'C2')
  d$tested <- 100
  d$positive <- 10
  d$attending <- 200
  d$attendance_definition <- 'all outpatient attendances'
  d$tested_parse_status <- 'ok'
  d$positive_parse_status <- 'ok'
  d$flag_exclude_authorized <- FALSE
  d
}

testthat::test_that('positivity uses matched tested/positive pairs only', {
  d <- scenario_data()[1:2, ]
  d$tested <- c(100, 100)
  d$positive <- c(10, NA)
  d$attending <- NA_real_
  out <- routineqc::qc_aggregate_metrics(d, 'national', monthly = FALSE)
  testthat::expect_equal(out$positivity, 0.1)
  testthat::expect_equal(out$tested_matched, 100)
  testthat::expect_equal(out$tested_total, 200)
  testthat::expect_equal(out$pair_rows, 1L)
})

testthat::test_that('aggregate percentages are ratios of sums, not means of facility percentages', {
  d <- scenario_data()
  d <- d[d$month_date == d$month_date[1] & d$facility_id %in% c('F1', 'F2'), ]
  d$tested <- c(10, 1000)
  d$positive <- c(10, 100)
  out <- routineqc::qc_aggregate_metrics(d, 'national', monthly = FALSE)
  testthat::expect_equal(out$positivity, 110 / 1010)
})

testthat::test_that('genuine zeros are valid contributions and missing counts are not', {
  d <- scenario_data()[1:3, ]
  d$tested <- c(0, 50, NA)
  d$positive <- c(0, 5, 3)
  out <- routineqc::qc_aggregate_metrics(d, 'national', monthly = FALSE)
  testthat::expect_equal(out$pair_rows, 2L)
  testthat::expect_equal(out$positivity, 0.1)
  testthat::expect_equal(out$tested_total, 50)
})

testthat::test_that('coverage uses comparable contributions with an approved definition', {
  d <- scenario_data()[1:3, ]
  d$attending <- c(200, 0, NA)
  counts <- routineqc:::.baseline_counts(d)
  testthat::expect_equal(counts$attending, c(200, 0, NA))
  d$attendance_definition[1] <- NA
  testthat::expect_true(is.na(routineqc:::.baseline_counts(d)$attending[1]))
  d$attendance_definition[1] <- 'defined'
  frame <- routineqc:::.scenario_frame(
    routineqc::apply_qc_scenario(d, routineqc::qc_scenario('none', omit_through = 'none')), 'reported'
  )
  out <- routineqc::qc_aggregate_metrics(frame, 'national', monthly = FALSE)
  testthat::expect_equal(out$coverage, 100 / 200)
  testthat::expect_equal(out$coverage_rows, 1L)
})

testthat::test_that('a problem confined to attendance does not discard tested or positive', {
  d <- scenario_data()
  d$flag_attending_negative <- FALSE
  d$flag_coverage_very_low <- FALSE
  d$flag_coverage_very_low[1] <- TRUE
  d$flag_attending_negative[2] <- TRUE
  scn <- routineqc::apply_qc_scenario(d, routineqc::qc_scenario('s', omit_through = 'low'))
  testthat::expect_false(any(scn$scn_omit_tested | scn$scn_omit_positive))
  testthat::expect_true(scn$scn_omit_coverage[1])
  testthat::expect_true(scn$scn_omit_attending[2])
  filtered <- routineqc:::.scenario_frame(scn, 'filtered')
  testthat::expect_false(anyNA(filtered$tested[1:2]))
  testthat::expect_false(anyNA(filtered$positive[1:2]))
  testthat::expect_true(is.na(filtered$attending[2]))
  out <- routineqc::compare_qc_scenario(d, routineqc::qc_scenario('s', omit_through = 'low'))
  testthat::expect_identical(
    out$positivity[out$version == 'reported'], out$positivity[out$version == 'filtered_missing']
  )
})

testthat::test_that('a tested-volume flag omits tested and leaves the positivity pair unmatched', {
  d <- scenario_data()
  d$flag_tested_extreme_high <- FALSE
  d$flag_tested_extreme_high[3] <- TRUE
  scn <- routineqc::apply_qc_scenario(d, routineqc::qc_scenario('s', omit_through = 'high'))
  testthat::expect_true(scn$scn_omit_tested[3])
  testthat::expect_false(scn$scn_omit_positive[3])
  testthat::expect_identical(scn$scn_tested_state[3], 'qc_omitted')
  testthat::expect_identical(scn$scn_positive_state[3], 'observed')
  out <- routineqc::compare_qc_scenario(d, routineqc::qc_scenario('s', omit_through = 'high'),
                                        monthly = FALSE)
  filtered <- out[out$version == 'filtered_missing', ]
  testthat::expect_equal(filtered$pair_rows, nrow(d) - 1L)
  testthat::expect_equal(filtered$tested_rows, nrow(d) - 1L)
})

testthat::test_that('pre-existing missingness is kept apart from QC-created missingness', {
  d <- scenario_data()
  d$tested[1] <- NA
  d$flag_tested_extreme_high <- FALSE
  d$flag_tested_extreme_high[2] <- TRUE
  d$tested_parse_status[3] <- 'fractional'
  d$tested[3] <- 10.5
  d$flag_count_invalid_value <- d$tested_parse_status == 'fractional'
  scn <- routineqc::apply_qc_scenario(d, routineqc::qc_scenario('s', omit_through = 'high'))
  testthat::expect_identical(scn$scn_tested_state[1:3], c('missing', 'qc_omitted', 'invalid'))
})

testthat::test_that('scenario presets give nested cohorts and nested omissions', {
  d <- scenario_data()
  set.seed(4)
  d$tested[sample(nrow(d), 3)] <- 0
  d$tested[d$facility_id == 'F4' & d$month_date > as.Date('2024-04-01')] <- 0
  d$tested[d$facility_id == 'F3' & d$month_date > as.Date('2024-10-01')] <- 0
  for (flag in c('flag_resid_extreme', 'flag_tested_extreme_high', 'flag_coverage_low',
                 'flag_tested_zero_unusual', 'flag_large_monthly_prevalence_change')) {
    d[[flag]] <- runif(nrow(d)) < 0.1
  }
  d$flag_exclude_authorized <- runif(nrow(d)) < 0.03
  presets <- routineqc::qc_scenario_presets()
  scn <- lapply(presets, function(s) routineqc::apply_qc_scenario(d, s))
  ordered <- names(presets)
  for (i in seq_len(length(ordered) - 1L)) {
    a <- scn[[ordered[i]]]
    b <- scn[[ordered[i + 1L]]]
    testthat::expect_true(all(b$scn_facility_eligible <= a$scn_facility_eligible))
    for (m in c('tested', 'positive', 'attending', 'coverage')) {
      col <- paste0('scn_omit_', m)
      testthat::expect_true(all(b[[col]] >= a[[col]]), info = paste(ordered[i], m))
    }
    ra <- sum(routineqc:::.scenario_frame(a, 'filtered')$tested, na.rm = TRUE)
    rb <- sum(routineqc:::.scenario_frame(b, 'filtered')$tested, na.rm = TRUE)
    testthat::expect_true(rb <= ra)
  }
  testthat::expect_true(sum(!scn$very_strict$scn_facility_eligible) >
                          sum(!scn$relaxed$scn_facility_eligible))
  rules <- routineqc::describe_qc_scenarios(presets)
  testthat::expect_identical(rules$scenario, ordered)
})

testthat::test_that('applying a scenario mutates neither observations nor stored actions', {
  d <- scenario_data()
  d$qc_action <- 'retain'
  d$flag_resid_extreme <- c(TRUE, rep(FALSE, nrow(d) - 1L))
  d$flag_exclude_authorized[2] <- TRUE
  before <- d
  scn <- routineqc::apply_qc_scenario(d, routineqc::qc_scenario('s', omit_through = 'low'))
  testthat::expect_identical(d, before)
  testthat::expect_identical(scn$tested, before$tested)
  testthat::expect_identical(scn$qc_action, before$qc_action)
  testthat::expect_identical(scn$flag_exclude_authorized, before$flag_exclude_authorized)
})

testthat::test_that('scenario definitions are validated', {
  testthat::expect_error(routineqc::qc_scenario(omit_through = 'everything'), 'omit_through')
  testthat::expect_error(routineqc::qc_scenario(max_gap_months = -1), 'non-negative')
  testthat::expect_error(routineqc::apply_qc_scenario(scenario_data(), list()), 'qc_scenario')
})

testthat::test_that('council aggregation needs a populated council field', {
  d <- scenario_data()
  d$council <- NULL
  scn <- routineqc::apply_qc_scenario(d, routineqc::qc_scenario('s'))
  testthat::expect_error(
    routineqc::qc_aggregate_metrics(routineqc:::.scenario_frame(scn, 'reported'), 'council'),
    'council'
  )
})
