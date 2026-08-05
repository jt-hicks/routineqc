prevalence_model_fixture <- function() {
  dates <- seq(as.Date('2022-01-01'), by = 'month', length.out = 30)
  tibble::tibble(
    facility_id = rep(c('F1', 'F2', 'F3', 'F4'), each = length(dates)),
    region = rep(c('R1', 'R1', 'R2', 'R2'), each = length(dates)),
    month_date = rep(dates, 4),
    month_num = lubridate::month(rep(dates, 4)),
    time_index = rep(seq_along(dates), 4),
    tested = 100,
    positive = rep(c(8, 12, 18, 22), each = length(dates))
  ) %>%
    routineqc::flag_logical_errors()
}

testthat::test_that('prevalence GAM includes the authoritative region fixed effect', {
  model <- suppressWarnings(routineqc::fit_prevalence_gam(prevalence_model_fixture(), nthreads = 1))
  testthat::expect_s3_class(model, 'gam')
  testthat::expect_true('region' %in% attr(stats::terms(model), 'term.labels'))
})

testthat::test_that('prediction preserves rows and isolates unseen model levels', {
  train <- prevalence_model_fixture()
  model <- suppressWarnings(routineqc::fit_prevalence_gam(train, nthreads = 1))
  assess <- dplyr::bind_rows(
    train[1:2, ],
    dplyr::mutate(train[3, ], facility_id = 'NEW_FACILITY'),
    dplyr::mutate(train[4, ], region = 'NEW_REGION'),
    dplyr::mutate(train[5, ], tested = 0, positive = 0)
  )

  testthat::expect_warning(
    out <- routineqc::add_prevalence_predictions(
      assess, model, min_prediction_coverage = 0.75
    ),
    'assessment coverage'
  )
  testthat::expect_identical(nrow(out), nrow(assess))
  testthat::expect_identical(
    out$prediction_status,
    c('assessed', 'assessed', 'unseen_facility', 'unseen_region',
      'ineligible_nonpositive_tested')
  )
  testthat::expect_identical(out$model_assessed, c(TRUE, TRUE, FALSE, FALSE, FALSE))
  testthat::expect_true(all(is.na(out$p_hat[!out$model_assessed])))
})

testthat::test_that('unavailable models produce explicit non-assessment reasons', {
  dat <- prevalence_model_fixture()[1:2, ]
  testthat::expect_warning(
    out <- routineqc::add_prevalence_predictions(dat, NULL),
    'assessment coverage'
  )
  testthat::expect_identical(out$prediction_status, rep('model_unavailable', 2))
  testthat::expect_false(any(out$model_assessed))
  testthat::expect_true(all(is.na(out$pearson_resid)))
})
