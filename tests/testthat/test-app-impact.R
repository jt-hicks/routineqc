test_that("impact summary compares scenarios without altering data", {
  fx <- dashboard_fixture()
  data <- fx$data
  before <- data
  applied <- .impact_apply(data, .impact_scenarios())
  expect_identical(data, before)
  geo <- .impact_geography(list(), data)
  expect_equal(geo$level, 'national')
  s <- .impact_summary(applied, geo)
  expect_equal(s$scenario, c('relaxed', 'moderate', 'strict', 'very_strict'))
  expect_equal(s$change_pp, 100 * (s$positivity_filtered - s$positivity_reported))
  expect_true(all(s$tested_retained <= s$tested_reported))
  expect_true(all(s$share_tested_retained <= 1))
})

test_that("impact summary restricts to a selected geography", {
  data <- dashboard_fixture()$data
  applied <- .impact_apply(data, .impact_scenarios())
  region <- sort(unique(data$region))[1]
  geo <- .impact_geography(list(region = region), data)
  s <- .impact_summary(applied, geo)
  national <- .impact_summary(applied, .impact_geography(list(), data))
  expect_lt(s$tested_reported[1], national$tested_reported[1])
})

test_that("unit changes rank by absolute change and plots build", {
  data <- dashboard_fixture()$data
  applied <- .impact_apply(data, .impact_scenarios())
  ch <- .impact_unit_changes(applied, 'strict')
  expect_false(is.unsorted(rev(abs(dplyr::coalesce(ch$change_pp, 0)))))
  s <- .impact_summary(applied, .impact_geography(list(), data))
  expect_s3_class(.plot_impact_strictness(s), 'plotly')
  expect_s3_class(.plot_impact_change(s), 'plotly')
  expect_s3_class(.plot_impact_retention(s), 'plotly')
  expect_s3_class(.plot_impact_units(ch), 'plotly')
  res <- .impact_compute(applied, .impact_geography(list(), data))
  expect_s3_class(.plot_impact_series(res, 'strict'), 'plotly')
  rules <- lapply(.impact_scenarios(), .describe_scenario_rules)
  for (rule in rules) {
    expect_match(rule, 'no reporting-completeness filter')
    expect_match(rule, 'Facility-month observations')
    expect_match(rule, 'only the count\\(s\\) it puts in doubt')
    expect_false(grepl('at least [0-9]+ months', rule))
  }
})

testthat::test_that('the count flow accounts for every reported count under a scenario', {
  run <- dashboard_fixture()$qc
  data <- run$data_flagged
  applied <- routineqc:::.impact_apply(data, routineqc:::.impact_scenarios(), run$config$priority)
  geo <- list(level = 'national', unit = NULL)
  for (field in c('tested', 'positive')) {
    flow <- routineqc:::.impact_count_flow(applied, geo, 'moderate', field, run$config$priority)
    testthat::expect_equal(sum(flow$records), nrow(data))
    state <- applied$moderate[[paste0('scn_', field, '_state')]]
    testthat::expect_equal(flow$records[flow$outcome == 'Kept'], sum(state == 'observed'))
    omitted <- sum(flow$records[startsWith(flow$outcome, 'Omitted')])
    testthat::expect_equal(omitted, sum(state == 'qc_omitted'))
    testthat::expect_s3_class(routineqc:::.plot_impact_flow(flow, 'moderate'), 'plotly')
  }
  # Stricter scenarios omit at least as many counts.
  strict <- routineqc:::.impact_count_flow(applied, geo, 'very_strict', 'tested', run$config$priority)
  moderate <- routineqc:::.impact_count_flow(applied, geo, 'moderate', 'tested', run$config$priority)
  testthat::expect_lte(strict$records[strict$outcome == 'Kept'], moderate$records[moderate$outcome == 'Kept'])
  table <- routineqc:::.impact_summary_table(routineqc:::.impact_summary(applied, geo))
  testthat::expect_true(all(c('Scenario', 'Positivity, filtered', 'Share of tested kept') %in% names(table)))
})

testthat::test_that('low testing coverage is review only and does not raise coverage by selection', {
  data <- tibble::tibble(
    facility_id = 'F1', month_date = as.Date('2024-01-01') + 0:3 * 31, tested = c(50, 50, 10, 50),
    positive = 5, attending = 50, attendance_definition = 'first ANC', flag_coverage_low = c(FALSE, FALSE, TRUE, FALSE),
    flag_coverage_very_low = c(FALSE, FALSE, TRUE, FALSE)
  )
  scn <- routineqc::apply_qc_scenario(data, routineqc::qc_scenario('all', omit_through = 'low'))
  testthat::expect_false(any(scn$scn_omit_coverage))
  testthat::expect_identical(routineqc:::.qc_flag_targets$coverage_low, character())
})
