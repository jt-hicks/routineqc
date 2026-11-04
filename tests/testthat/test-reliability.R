reliability_sim <- function(n_facilities = 12, n_months = 24, seed = 5) {
  routineqc::simulate_qc_data(n_facilities = n_facilities, n_months = n_months, seed = seed)
}

reliability_run <- function(data, ...) {
  routineqc::run_routine_qc(
    data, facility_var = 'facility', region_var = 'region', month_var = 'month',
    tested_var = 'tested', positive_var = 'positive', ...
  )
}

flag_columns <- function(data) data[grepl('^flag_', names(data))]

testthat::test_that('single-region data fit a model without a region effect', {
  sim <- reliability_sim()
  sim$region <- 'Only'
  run <- suppressWarnings(reliability_run(sim))

  testthat::expect_false(is.null(run$model))
  testthat::expect_false('region' %in% attr(stats::terms(run$model), 'term.labels'))
  testthat::expect_true(all(run$data_flagged$prediction_status %in%
    c('assessed', 'ineligible_core_counts')))
  testthat::expect_gt(sum(run$data_flagged$model_assessed), 0L)
  testthat::expect_true(run$manifest$prevalence_model_available)
})

testthat::test_that('limited temporal support reduces the model instead of failing', {
  sim <- reliability_sim(n_facilities = 30)
  months <- sort(unique(sim$month))
  short <- sim[sim$month <= months[3], ]

  testthat::expect_warning(
    run <- reliability_run(short), 'limited temporal support'
  )
  testthat::expect_false(is.null(run$model))
  testthat::expect_identical(nrow(run$data_flagged), nrow(short))
  testthat::expect_gt(sum(run$data_flagged$model_assessed), 0L)

  one_region <- short
  one_region$region <- 'Only'
  testthat::expect_warning(
    run <- reliability_run(one_region), 'limited temporal support'
  )
  testthat::expect_false(is.null(run$model))
})

testthat::test_that('an unfittable model gives NULL, a warning and a complete deterministic run', {
  sim <- reliability_sim()
  testthat::local_mocked_bindings(
    bam = function(...) stop('synthetic failure'),
    .package = 'mgcv'
  )

  train <- routineqc::flag_logical_errors(
    routineqc::prepare_qc_data(sim, 'facility', 'region', 'month', 'tested', 'positive')
  )
  testthat::expect_warning(
    model <- routineqc::fit_prevalence_gam(train),
    'could not be fitted \\(synthetic failure\\).*model_unavailable'
  )
  testthat::expect_null(model)

  warnings <- character()
  run <- withCallingHandlers(
    reliability_run(sim),
    warning = function(w) {
      warnings <<- c(warnings, conditionMessage(w))
      invokeRestart('muffleWarning')
    }
  )
  testthat::expect_true(any(grepl('could not be fitted', warnings)))
  dat <- run$data_flagged
  testthat::expect_identical(nrow(dat), nrow(sim))
  testthat::expect_false(run$manifest$prevalence_model_available)
  testthat::expect_true(all(dat$prediction_status %in%
    c('model_unavailable', 'ineligible_core_counts')))
  testthat::expect_false(any(dat$model_assessed))
  testthat::expect_true(all(is.na(dat$p_hat)))
  testthat::expect_false(anyNA(flag_columns(dat)))
  testthat::expect_false(any(dat$flag_resid_extreme))
  testthat::expect_false(any(dat$flag_all_negative_large_n))
  testthat::expect_true('qc_action' %in% names(dat))
  testthat::expect_silent(routineqc::validate_qc_run(run))
})

testthat::test_that('rules that need no model still apply when the model is unavailable', {
  dat <- tibble::tibble(
    facility_id = c('A', 'A', 'B'),
    region = 'R',
    month_date = as.Date(c('2024-01-01', '2024-02-01', '2024-01-01')),
    month_num = c(1L, 2L, 1L), time_index = c(1L, 2L, 1L),
    tested = c(100, 40, 100), positive = c(0, 40, 5)
  )
  dat$prevalence <- dat$positive / dat$tested
  dat <- routineqc::flag_logical_errors(dat)
  out <- suppressWarnings(routineqc::add_prevalence_predictions(dat, NULL)) |>
    routineqc::add_prevalence_qc_flags()

  testthat::expect_false(anyNA(out$flag_all_negative_large_n))
  testthat::expect_identical(out$flag_all_negative_large_n, c(FALSE, FALSE, FALSE))
  testthat::expect_identical(out$flag_all_positive_large_n, c(FALSE, TRUE, FALSE))
})

testthat::test_that('threshold sensitivity recomputes flagged data and keeps the run configuration', {
  sim <- reliability_sim()
  config <- routineqc::qc_config(
    'retrospective',
    action_policy = list(name = 'flags_only'),
    tested_volume = list(baseline_window_months = 3)
  )
  run <- suppressWarnings(reliability_run(sim, config = config))

  out <- routineqc::run_threshold_sensitivity(
    run, resid_thresholds = c(3, 5), tested_z_thresholds = c(4, 6), by = 'region'
  )
  testthat::expect_identical(nrow(out), 4L * dplyr::n_distinct(sim$region))
  testthat::expect_setequal(out$resid_threshold, c(3, 5))
  testthat::expect_setequal(out$tested_z_threshold, c(4, 6))
  # flags_only never authorizes exclusion, whatever the grid says
  testthat::expect_true(all(out$authorized_exclusion_rows == 0L))

  # the configured thresholds reproduce the run itself
  same <- routineqc::run_threshold_sensitivity(
    run, resid_thresholds = 4, tested_z_thresholds = 5, by = 'region'
  )
  rerun <- routineqc::summarise_before_after_qc(run$data_flagged, by = 'region')
  testthat::expect_equal(same$n_any_qc_issue, rerun$n_any_qc_issue)
})

testthat::test_that('threshold sensitivity uses the configured baseline, not the defaults', {
  sim <- reliability_sim()
  retro <- suppressWarnings(reliability_run(sim, config = routineqc::qc_config('retrospective')))

  fresh <- routineqc:::.apply_qc_rules(retro$data_flagged, retro$config)
  testthat::expect_identical(fresh$tested_baseline_mode, rep('centered', nrow(fresh)))
  testthat::expect_equal(fresh$flag_tested_volume_extreme, retro$data_flagged$flag_tested_volume_extreme)
  testthat::expect_equal(fresh$qc_action, retro$data_flagged$qc_action)
})

testthat::test_that('threshold sensitivity accepts flagged data and says what it assumed', {
  run <- suppressWarnings(reliability_run(reliability_sim()))

  testthat::expect_message(
    out <- routineqc::run_threshold_sensitivity(run$data_flagged),
    'No `config` supplied'
  )
  testthat::expect_gt(nrow(out), 0L)
  testthat::expect_silent(
    routineqc::run_threshold_sensitivity(run$data_flagged, config = run$config)
  )
  testthat::expect_error(
    routineqc::run_threshold_sensitivity(run, config = run$config),
    'carries its own configuration'
  )
})

testthat::test_that('recomputing tested-volume QC over flagged data does not collide', {
  run <- suppressWarnings(reliability_run(reliability_sim()))
  again <- routineqc::add_tested_volume_qc(run$data_flagged)
  testthat::expect_identical(nrow(again), nrow(run$data_flagged))
  testthat::expect_false(any(duplicated(names(again))))
  testthat::expect_equal(again$flag_tested_volume_extreme, run$data_flagged$flag_tested_volume_extreme)
})

testthat::test_that('summaries and both before/after charts use complete tested/positive pairs', {
  dat <- tibble::tibble(
    tested = c(100, 100), positive = c(10, NA),
    flag_exclude_authorized = c(FALSE, FALSE)
  )
  summary <- routineqc::summarise_before_after_qc(dat)
  testthat::expect_equal(summary$prevalence_before_qc, 0.1)
  testthat::expect_equal(summary$prevalence_after_authorized_exclusions, 0.1)
  testthat::expect_identical(summary$incomplete_count_rows, 1L)

  static <- ggplot2::ggplot_build(routineqc::plot_before_after_prevalence(dat))
  testthat::expect_equal(static$data[[1]]$y, c(0.1, 0.1))

  interactive <- plotly::plotly_build(routineqc:::.plot_before_after_interactive(dat))
  testthat::expect_equal(as.numeric(interactive$x$data[[1]]$y), c(0.1, 0.1))
})

testthat::test_that('before/after charts drop authorized exclusions like the summary does', {
  dat <- tibble::tibble(
    tested = c(100, 100, 50), positive = c(10, 30, 50),
    flag_exclude_authorized = c(FALSE, FALSE, TRUE)
  )
  summary <- routineqc::summarise_before_after_qc(dat)
  static <- ggplot2::ggplot_build(routineqc::plot_before_after_prevalence(dat))$data[[1]]$y
  testthat::expect_equal(static, c(summary$prevalence_before_qc, summary$prevalence_after_authorized_exclusions))
  testthat::expect_equal(static, c(90 / 250, 40 / 200))
})

testthat::test_that('static residual diagnostics plot the scale and cutoffs the rules used', {
  dat <- tibble::tibble(
    month_date = as.Date('2024-01-01') + 0:2 * 31,
    tested = c(100, 10, 100),
    pearson_resid = c(9, 3, -9),
    resid_dispersion = 4,
    pearson_resid_scaled = c(4.5, 1.5, -4.5)
  )
  config <- routineqc::qc_config(prevalence = list(resid_threshold_large = 3.5, resid_threshold_small = 6))
  built <- ggplot2::ggplot_build(routineqc::plot_prevalence_residuals(dat, config))

  point_layer <- Filter(function(d) 'y' %in% names(d) && nrow(d) == 3L, built$data)[[1]]
  testthat::expect_equal(point_layer$y, dat$pearson_resid_scaled)
  hlines <- unlist(lapply(built$data, function(d) d$yintercept))
  testthat::expect_setequal(hlines, c(-6, -3.5, 3.5, 6))
  testthat::expect_match(routineqc::plot_prevalence_residuals(dat, config)$labels$y, 'sqrt\\(dispersion 4.00\\)')

  # the same cutoff and scale drive the flags
  flagged <- routineqc::add_prevalence_qc_flags(
    dplyr::mutate(dat, facility_id = 'F', positive = 1, prevalence = 0.1),
    resid_threshold_large = 3.5, resid_threshold_small = 6
  )
  testthat::expect_identical(flagged$flag_resid_extreme, c(TRUE, FALSE, TRUE))

  # a single configured threshold replaces both
  single <- routineqc::qc_config(prevalence = list(resid_threshold = 2))
  built <- ggplot2::ggplot_build(routineqc::plot_prevalence_residuals(dat, single))
  testthat::expect_setequal(unlist(lapply(built$data, function(d) d$yintercept)), c(-2, 2))
})

testthat::test_that('residual and tested-z plots take their cutoffs from a run', {
  run <- suppressWarnings(reliability_run(
    reliability_sim(),
    config = routineqc::qc_config(
      prevalence = list(resid_threshold_large = 3, resid_threshold_small = 4),
      tested_volume = list(tested_z_threshold = 7)
    )
  ))
  resid <- ggplot2::ggplot_build(routineqc::plot_prevalence_residuals(run))
  testthat::expect_setequal(unlist(lapply(resid$data, function(d) d$yintercept)), c(-4, -3, 3, 4))
  z <- ggplot2::ggplot_build(routineqc::plot_tested_robust_z(run))
  testthat::expect_setequal(unlist(lapply(z$data, function(d) d$yintercept)), c(-7, 7))
})