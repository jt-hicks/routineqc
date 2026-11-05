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
  expect_match(.describe_scenario_rules(.impact_scenarios()$strict), 'at least 12 months')
})
