baseline_replacement_fixture <- function() {
  data <- tibble::tibble(
    facility_id = 'F1', region = 'R1',
    month_date = seq.Date(as.Date('2024-01-01'), by = 'month', length.out = 8),
    tested = c(100, 1000, 1000, 1000, 100, 1000, NA, 100),
    positive = c(90, 900, 20, 80, 90, 900, NA, 10),
    attending = 200, attendance_definition = 'all outpatient attendances',
    p_hat = c(0.2, 0.25, 0.3, 0.3, NA, 0.25, 0.4, 0.1),
    tested_roll_median = c(80, 120, 50, 50, 80, NA, 100, 100),
    tested_baseline_ready = TRUE, prediction_status = 'assessed',
    flag_resid_extreme = c(TRUE, TRUE, FALSE, FALSE, TRUE, TRUE, TRUE, FALSE),
    flag_tested_extreme_high = c(FALSE, TRUE, TRUE, TRUE, FALSE, TRUE, TRUE, FALSE),
    flag_exclude_authorized = FALSE, qc_action = 'retain'
  )
  data$expected_positive <- data$tested * data$p_hat
  data$review_priority <- ifelse(data$flag_resid_extreme | data$flag_tested_extreme_high, 'high', 'none')
  data
}

testthat::test_that('QC baseline replacement uses the correct denominator and preserves other counts', {
  data <- baseline_replacement_fixture()
  original <- data
  scn <- apply_qc_scenario(data, qc_scenario_presets()$moderate)
  result <- .qc_baseline_replacement(scn)
  testthat::expect_equal(result$frame$tested, c(100, 120, 50, NA, 100, NA, NA, 100))
  testthat::expect_equal(result$frame$positive, c(20, 30, 20, 80, NA, NA, NA, 10))
  testthat::expect_equal(result$frame$attending, rep(200, 8))
  testthat::expect_identical(data, original)
  for (field in names(data)) testthat::expect_identical(scn[[field]], original[[field]])
  testthat::expect_false(any(result$targets$month_date == data$month_date[7]))
  testthat::expect_equal(sum(result$targets$status == 'replaced'), 4)
  testthat::expect_setequal(result$targets$status, c(
    'replaced', 'baseline_below_observed_positive', 'prediction_unavailable',
    'baseline_unavailable', 'tested_unavailable'
  ))
})

testthat::test_that('QC baseline replacements require usable saved support and keep exact expectations', {
  data <- baseline_replacement_fixture()
  replace <- function(data) .qc_baseline_replacement(apply_qc_scenario(data, qc_scenario_presets()$moderate))
  for (p in c(NA_real_, NaN, Inf, -0.1, 1.1)) {
    data$p_hat[1] <- p
    result <- replace(data)
    testthat::expect_true(is.na(result$frame$positive[1]))
  }
  data$p_hat[1] <- 0.12345
  testthat::expect_equal(replace(data)$frame$positive[1], 12.345)
  data$prediction_status[1] <- 'model_unavailable'
  testthat::expect_true(is.na(replace(data)$frame$positive[1]))
  for (median in c(NA_real_, Inf, -1)) {
    data$tested_roll_median[3] <- median
    testthat::expect_true(is.na(replace(data)$frame$tested[3]))
  }
  data$tested_roll_median[3] <- 50
  data$tested_baseline_ready[3] <- FALSE
  testthat::expect_true(is.na(replace(data)$frame$tested[3]))
  data$tested_baseline_ready[3] <- TRUE
  data$tested_roll_median[3] <- 20
  testthat::expect_equal(replace(data)$frame$tested[3], 20)
  data$tested_roll_median[3] <- 0
  data$positive[3] <- 0
  testthat::expect_equal(replace(data)$frame$tested[3], 0)
})

testthat::test_that('older runs without saved expectations show unresolved targets without fitting', {
  data <- baseline_replacement_fixture()
  data$p_hat <- data$tested_roll_median <- data$tested_baseline_ready <- data$prediction_status <- NULL
  scn <- apply_qc_scenario(data, qc_scenario_presets()$moderate)
  result <- .qc_baseline_replacement(scn)
  testthat::expect_identical(result$frame, .scenario_frame(scn, 'filtered'))
  testthat::expect_false(any(result$targets$status == 'replaced'))
})

testthat::test_that('baseline replacement respects authorized exclusions and handles no targets', {
  data <- baseline_replacement_fixture()[8, ]
  scn <- apply_qc_scenario(data, qc_scenario_presets()$moderate)
  result <- .qc_baseline_replacement(scn)
  testthat::expect_equal(nrow(result$targets), 0)
  testthat::expect_identical(result$frame, .scenario_frame(scn, 'filtered'))
  data$flag_exclude_authorized <- TRUE
  data$tested_roll_median <- 50
  result <- .qc_baseline_replacement(apply_qc_scenario(data, qc_scenario_presets()$moderate))
  testthat::expect_equal(result$frame$tested, 50)
  testthat::expect_equal(result$frame$positive, 5)
  testthat::expect_equal(result$targets$status, c('replaced', 'replaced'))
})

testthat::test_that('all landing indicators plot actual QC baseline values with gaps and no intervals', {
  data <- baseline_replacement_fixture()
  applied <- .impact_apply(data, list(moderate = qc_scenario_presets()$moderate))
  result <- .qc_baseline_replacement(applied$moderate)
  series <- qc_aggregate_metrics(result$frame, 'national', monthly = TRUE)
  series$scenario <- 'moderate'
  series$version <- 'qc_baseline'
  combined <- dplyr::bind_rows(.impact_compute(applied, list(level = 'national', unit = NULL)), series)
  for (metric in names(.explore_metrics)) {
    built <- plotly::plotly_build(.plot_impact_series(combined, 'moderate', metric))
    line <- Filter(function(x) identical(x$name, 'QC baseline replacement'), built$x$data)
    testthat::expect_length(line, 1)
    y <- as.numeric(line[[1]]$y)
    expected <- series[[.explore_metrics[[metric]]$column]]
    if (metric == 'tested') expected[series$tested_rows == 0] <- NA_real_
    testthat::expect_equal(y[is.finite(y)], expected[is.finite(expected)], info = metric)
    testthat::expect_identical(anyNA(y), anyNA(expected), info = metric)
    testthat::expect_false(isTRUE(line[[1]]$connectgaps))
    testthat::expect_false(any(vapply(built$x$data, function(x) identical(x$fill, 'tonexty'), logical(1))))
  }
})

testthat::test_that('landing page never calls imputation and explains unresolved baseline replacements', {
  data <- baseline_replacement_fixture()
  calls <- 0L
  testthat::local_mocked_bindings(
    qc_impute_scenario = function(...) { calls <<- calls + 1L; stop('Unexpected model fitting') },
    .package = 'routineqc'
  )
  server <- function(input, output, session) {
    run <- shiny::reactive(list(data_flagged = data, config = qc_config()))
    .landing_server(input, output, run, shiny::reactive(data), shiny::reactive(.qc_flag_catalog(qc_config())))
  }
  shiny::testServer(server, {
    testthat::expect_match(output$landing_series, 'QC baseline replacement')
    testthat::expect_match(output$landing_replacement_note$html, '4 of 8')
    testthat::expect_match(output$landing_replacement_unresolved, 'Reason')
    for (metric in c('tested', 'attendance', 'coverage')) {
      session$setInputs(landing_metric = metric)
      testthat::expect_match(output$landing_series, 'QC baseline replacement')
    }
    testthat::expect_equal(calls, 0L)
  })
})
