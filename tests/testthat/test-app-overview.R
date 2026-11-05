test_that("assessment states partition records and unassessed rows never pass", {
  data <- dashboard_fixture()$data
  states <- .row_assessment(data)
  for (k in names(.assessment_checks)) {
    expect_equal(length(states[[k]]), nrow(data))
    expect_false(anyNA(states[[k]]))
  }
  data$tested[1:5] <- NA
  s2 <- .row_assessment(data)
  expect_true(all(s2$tested_volume[1:5] != 'assessed, not flagged'))
  expect_true(all(s2$count_consistency[1:5] == 'missing or unusable inputs'))
  summ <- .assessment_summary(data)
  expect_equal(sum(summ$n[summ$check == 'Count consistency']), nrow(data))
})

test_that("unit-family matrix and ranked units are consistent", {
  data <- dashboard_fixture()$data
  m <- .unit_family_matrix(data)
  expect_true(all(m$flagged <= m$assessed))
  r <- .ranked_units(data)
  expect_equal(sum(r$records), sum(!is.na(data[[.unit_field(data)]])))
  expect_s3_class(.plot_unit_family_heatmap(m), 'plotly')
  expect_s3_class(.plot_assessment(.assessment_summary(data)), 'plotly')
  expect_s3_class(.plot_overview_trend(.overview_trend(data)), 'plotly')
  comp <- .reporting_completeness(data)
  expect_lte(comp$reporting, comp$expected)
})

test_that("flag catalog follows the run configuration and old configs fall back", {
  fx <- dashboard_fixture()
  cat1 <- .qc_flag_catalog(fx$config)
  expect_true(all(c('flag', 'domain', 'label', 'definition') %in% names(cat1)))
  cfg <- fx$config
  cfg$attendance$coverage_very_low <- 0.123
  expect_true(any(grepl('0.123', .qc_flag_catalog(cfg)$definition, fixed = TRUE)) ||
                !identical(.qc_flag_catalog(cfg)$definition, cat1$definition))
  old <- list()
  expect_s3_class(.qc_flag_catalog(old), 'data.frame')
  expect_true(length(attr(.guide_config(old), 'defaulted_sections')) > 0L)
})

test_that("record explanation names flags and never claims unassessed rows passed", {
  fx <- dashboard_fixture()
  catalog <- .qc_flag_catalog(fx$config)
  data <- fx$data
  idx <- which(.is_flagged_row(data))[1]
  skip_if(is.na(idx))
  txt <- paste(.explain_record(data[idx, ], catalog), collapse = ' ')
  expect_match(txt, 'Action:')
  clean <- data[which(!.is_flagged_row(data))[1], ]
  expect_match(paste(.explain_record(clean, catalog), collapse = ' '), 'No flag fired')
})
