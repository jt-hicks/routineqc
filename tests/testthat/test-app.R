app_test_run <- function() {
  raw <- tibble::tibble(
    facility = c('F1', 'F1', 'F2', 'F2'),
    area = c('R1', 'R1', 'R2', 'R2'),
    district = c('D1', 'D1', 'D2', 'D2'),
    period = as.Date(c('2024-01-01', '2024-02-01', '2024-01-01', '2024-02-01')),
    tests = c(10, 12, 8, 9),
    cases = c(2, 3, 1, 12)
  )
  suppressWarnings(routineqc::run_routine_qc(
    raw,
    facility_var = 'facility', region_var = 'area', month_var = 'period',
    tested_var = 'tests', positive_var = 'cases', district_var = 'district',
    provenance = list(dataset_id = 'app-synthetic'), nthreads = 1
  ))
}

write_app_test_run <- function(directory, name = 'run-one') {
  path <- file.path(directory, paste0(name, '.rds'))
  routineqc::write_qc_run(app_test_run(), path)
  path
}

testthat::test_that('run discovery reads safe metadata without loading run contents', {
  directory <- tempfile('routineqc-app-')
  dir.create(directory)
  on.exit(unlink(directory, recursive = TRUE), add = TRUE)
  path <- write_app_test_run(directory)
  saveRDS(list(not_a_run = TRUE), file.path(directory, 'orphan.rds'))
  writeLines('{broken json', file.path(directory, 'broken-manifest.json'))

  index <- routineqc::list_qc_runs(directory)
  testthat::expect_equal(nrow(index), 3L)
  testthat::expect_identical(
    index$artifact_status[match(c('run-one', 'orphan', 'broken'), index$run_name)],
    c('available', 'missing_manifest', 'missing_rds')
  )
  selected <- index[index$path == normalizePath(path, winslash = '/'), ]
  testthat::expect_true(selected$selectable)
  testthat::expect_identical(selected$dataset_id, 'app-synthetic')
  testthat::expect_false(any(grepl('F1|F2', unlist(selected), fixed = FALSE)))
})

testthat::test_that('discovery reports invalid and unsupported manifests', {
  directory <- tempfile('routineqc-app-invalid-')
  dir.create(directory)
  on.exit(unlink(directory, recursive = TRUE), add = TRUE)
  path <- write_app_test_run(directory, 'invalid')
  manifest_path <- sub('\\.rds$', '-manifest.json', path)
  writeLines('{broken json', manifest_path)
  file.copy(path, file.path(directory, 'unsupported.rds'))
  unsupported <- app_test_run()$manifest
  unsupported$run_schema_version <- 999L
  jsonlite::write_json(
    unsupported, file.path(directory, 'unsupported-manifest.json'), auto_unbox = TRUE
  )

  index <- routineqc::list_qc_runs(directory)
  testthat::expect_identical(
    index$artifact_status[match(c('invalid', 'unsupported'), index$run_name)],
    c('invalid_manifest', 'unsupported_schema')
  )
  testthat::expect_false(any(index$selectable))
})

testthat::test_that('review filters are display-only and composable', {
  data <- app_test_run()$data_flagged
  original <- data
  flagged <- routineqc::filter_qc_review(data)
  all_rows <- routineqc::filter_qc_review(data, flagged_only = FALSE)
  region <- routineqc::filter_qc_review(
    data, regions = 'R2', actions = 'exclude', flagged_only = FALSE
  )
  dated <- routineqc::filter_qc_review(
    data, date_range = as.Date(c('2024-02-01', '2024-02-01')), flagged_only = FALSE
  )

  testthat::expect_equal(nrow(all_rows), nrow(data))
  testthat::expect_true(all(flagged$flag_exclude_authorized | flagged$flag_review_recommended))
  testthat::expect_true(all(region$region == 'R2' & region$qc_action == 'exclude'))
  testthat::expect_true(all(dated$month_date == as.Date('2024-02-01')))
  testthat::expect_identical(data, original)
  testthat::expect_error(
    routineqc::filter_qc_review(data, date_range = as.Date(c('2024-02-01', '2024-01-01'))),
    'earliest to latest'
  )
})

testthat::test_that('facility plot selects one facility and includes stored model fit', {
  data <- tibble::tibble(
    facility_id = rep(c('F1', 'F2'), each = 3),
    month_date = rep(seq(as.Date('2024-01-01'), by = 'month', length.out = 3), 2),
    prevalence = c(0.1, 0.2, 0.15, 0.4, 0.5, 0.45),
    p_hat = c(0.12, 0.18, 0.16, 0.42, 0.48, 0.46),
    flag_any_qc_issue = c(FALSE, TRUE, FALSE, FALSE, FALSE, TRUE)
  )
  plot <- routineqc::plot_facility_timeseries(data, 'F1')
  testthat::expect_identical(unique(plot$data$facility_id), 'F1')
  testthat::expect_equal(nrow(plot$data), 3L)
  testthat::expect_equal(length(plot$layers), 4L)
  testthat::expect_identical(rlang::as_label(plot$layers[[3]]$mapping$y), 'p_hat')
})

testthat::test_that('review column sets are focused and have definitions', {
  data <- app_test_run()$data_flagged
  review <- routineqc:::.review_column_spec(data, 'review')
  model <- routineqc:::.review_column_spec(data, 'model')
  all_fields <- routineqc:::.review_column_spec(data, 'all')
  testthat::expect_lt(length(review$fields), length(all_fields$fields))
  testthat::expect_true(all(c('facility_id', 'qc_action', 'qc_reason') %in% review$fields))
  testthat::expect_true(all(c('p_hat', 'prediction_status') %in% model$fields))
  testthat::expect_true(all(nzchar(review$definitions)))
  testthat::expect_error(
    routineqc:::.review_column_spec(data, 'unknown'),
    'Unknown review queue column set'
  )
  container <- routineqc:::.review_table_container(review$labels, review$definitions)
  html <- as.character(container)
  testthat::expect_match(html, 'title=')
  testthat::expect_match(html, 'Stable facility identifier')
})

testthat::test_that('run explorer builds for empty and populated directories', {
  directory <- tempfile('routineqc-app-object-')
  dir.create(directory)
  on.exit(unlink(directory, recursive = TRUE), add = TRUE)
  empty_app <- routineqc:::.qc_run_explorer(directory)
  testthat::expect_s3_class(empty_app, 'shiny.appobj')
  path <- write_app_test_run(directory)
  populated_app <- routineqc:::.qc_run_explorer(directory)
  testthat::expect_s3_class(populated_app, 'shiny.appobj')

  server <- function(input, output, session) {
    routineqc:::.qc_app_server(input, output, session, run_dir = directory)
  }
  shiny::testServer(
    server,
    {
      suppressWarnings(session$setInputs(
        run_path = path, flagged_only = TRUE, column_set = 'review',
        use_date_filter = FALSE
      ))
      testthat::expect_true(length(output$overview) > 0L)
      testthat::expect_true(length(output$review_table) > 0L)
      testthat::expect_match(output$overview, 'app-synthetic')
      testthat::expect_match(output$review_table, 'Reported number tested')
      testthat::expect_match(output$review_count, '1 rows displayed')
      testthat::expect_true(length(output$status_plot) > 0L)
      testthat::expect_true(length(output$facility_burden_plot) > 0L)
      testthat::expect_true(length(output$district_burden_plot) > 0L)
    }
  )
})

testthat::test_that('overview status hierarchy preserves rows and reason combinations', {
  dat <- tibble::tibble(
    qc_action = c('retain', 'review_prevalence', 'review_prevalence', 'exclude_core_invalid'),
    qc_reason = c(
      'No QC issue detected',
      'prevalence residual extreme',
      'prevalence residual extreme; tested-volume anomaly',
      'impossible or missing core tested/positive counts'
    )
  )
  hierarchy <- routineqc:::.qc_status_hierarchy(dat)
  root <- hierarchy[hierarchy$level == 'root', ]
  actions <- hierarchy[hierarchy$level == 'action', ]
  reasons <- hierarchy[hierarchy$level == 'reason', ]
  testthat::expect_equal(root$n, nrow(dat))
  testthat::expect_equal(sum(actions$n), nrow(dat))
  testthat::expect_equal(sum(reasons$n), nrow(dat))
  testthat::expect_true(
    'prevalence residual extreme; tested-volume anomaly' %in% reasons$label
  )
  testthat::expect_s3_class(routineqc:::.plot_qc_status(dat), 'plotly')
})

testthat::test_that('facility and district burden modes remain explicit', {
  dat <- tibble::tibble(
    facility_id = rep(c('F1', 'F2', 'F3'), each = 4),
    district = rep(c('D1', 'D1', 'D2'), each = 4),
    flag_review_recommended = c(
      TRUE, TRUE, FALSE, FALSE,
      FALSE, FALSE, FALSE, FALSE,
      TRUE, TRUE, TRUE, TRUE
    ),
    flag_exclude_authorized = c(
      FALSE, FALSE, FALSE, FALSE,
      TRUE, FALSE, FALSE, FALSE,
      FALSE, FALSE, FALSE, FALSE
    )
  )
  all_burden <- routineqc:::.facility_burden_data(dat, 'flagged')
  review_burden <- routineqc:::.facility_burden_data(dat, 'review')
  exclude_burden <- routineqc:::.facility_burden_data(dat, 'exclude')
  testthat::expect_equal(sum(all_burden$facilities), 3)
  testthat::expect_equal(
    exclude_burden$facilities[as.character(exclude_burden$burden) == '1'], 1
  )
  testthat::expect_equal(
    review_burden$facilities[as.character(review_burden$burden) == '0'], 1
  )
  district <- routineqc:::.district_burden_data(dat, 'flagged', threshold = 1)
  d1 <- district[district$district == 'D1', ]
  testthat::expect_equal(d1$facilities, 2)
  testthat::expect_equal(d1$facilities_over_threshold, 1)
  testthat::expect_error(
    routineqc:::.district_burden_data(dat, threshold = 1.5),
    'non-negative whole number'
  )
})

testthat::test_that('tooltip construction tolerates optional fields', {
  dat <- tibble::tibble(
    facility_id = 'F1', month_date = as.Date('2024-01-01'),
    tested = 20, positive = 2, prevalence = 0.1,
    qc_action = 'review_prevalence', qc_reason = 'prevalence residual extreme'
  )
  tooltip <- routineqc:::.qc_row_tooltip(dat)
  testthat::expect_match(tooltip, 'Facility ID: F1')
  testthat::expect_match(tooltip, 'Observed prevalence: 10.00%')
  testthat::expect_match(tooltip, 'QC reason: prevalence residual extreme')
})

testthat::test_that('district views support heatmaps, facets, and absent districts', {
  dat <- app_test_run()$data_flagged
  heatmap <- routineqc:::.plot_district_heatmap(dat, 'D1', 'prevalence')
  facets <- routineqc:::.plot_district_facets(dat, 'D1', 'tested')
  testthat::expect_s3_class(heatmap, 'plotly')
  testthat::expect_s3_class(facets, 'plotly')
  no_district <- dplyr::select(dat, -district)
  testthat::expect_error(
    routineqc:::.district_plot_data(no_district, 'D1'),
    'Missing required columns'
  )
})

testthat::test_that('QC flow uses meaningful tested-positive language', {
  html <- as.character(routineqc:::.qc_flow_diagram_ui())
  testthat::expect_match(html, 'Tested/positive validity checks', fixed = TRUE)
  testthat::expect_false(grepl('>Core|>core', html))
})

testthat::test_that('review queue reports an explicit empty filtered state', {
  directory <- tempfile('routineqc-app-empty-review-')
  dir.create(directory)
  on.exit(unlink(directory, recursive = TRUE), add = TRUE)
  path <- write_app_test_run(directory)
  server <- function(input, output, session) {
    routineqc:::.qc_app_server(input, output, session, run_dir = directory)
  }
  shiny::testServer(
    server,
    {
      suppressWarnings(session$setInputs(
        run_path = path, flagged_only = FALSE, action = 'not-an-action',
        column_set = 'review', use_date_filter = FALSE
      ))
      testthat::expect_match(output$review_count, '0 rows displayed')
      testthat::expect_match(output$review_state$html, 'No rows match')
    }
  )
})

testthat::test_that('artifact identity disagreement is rejected on selection', {
  directory <- tempfile('routineqc-app-tamper-')
  dir.create(directory)
  on.exit(unlink(directory, recursive = TRUE), add = TRUE)
  path <- write_app_test_run(directory)
  manifest_path <- sub('\\.rds$', '-manifest.json', path)
  manifest <- jsonlite::read_json(manifest_path, simplifyVector = TRUE)
  manifest$analysis_id <- paste0('tampered-', manifest$analysis_id)
  jsonlite::write_json(manifest, manifest_path, auto_unbox = TRUE, pretty = TRUE)

  testthat::expect_true(routineqc::list_qc_runs(directory)$selectable)
  testthat::expect_error(routineqc::read_qc_run(path), 'disagrees')
})

testthat::test_that('app helpers reject invalid arguments', {
  testthat::expect_error(routineqc::list_qc_runs(tempfile()), 'existing directory')
  data <- app_test_run()$data_flagged
  testthat::expect_error(
    routineqc::filter_qc_review(data, flagged_only = NA),
    'TRUE or FALSE'
  )
  directory <- tempfile('routineqc-app-port-')
  dir.create(directory)
  on.exit(unlink(directory, recursive = TRUE), add = TRUE)
  testthat::expect_error(
    routineqc::launch_qc_app(directory, port = 12.5, launch_browser = FALSE),
    'whole number'
  )
})
