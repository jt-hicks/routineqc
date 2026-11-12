testthat::test_that('facility selector updates synchronously without a server-side reload', {
  data <- dashboard_fixture()$data
  facility <- as.character(data$facility_id[[1]])
  selection <- routineqc:::.nav_set(data, routineqc:::.nav_empty(), 'facility', facility)
  session <- shiny::MockShinySession$new()
  messages <- list()
  session$sendInputMessage <- function(inputId, message) {
    messages[[inputId]] <<- message
  }

  routineqc:::.dashboard_sync_inputs(session, data, selection)

  testthat::expect_identical(messages$ex_facility$value, facility)
  testthat::expect_true(is.character(messages$ex_facility$options))
  testthat::expect_match(messages$ex_facility$options, facility, fixed = TRUE)
  testthat::expect_null(messages$ex_facility$url)
  testthat::expect_identical(messages$ex_region$value, selection$region)
  testthat::expect_identical(messages$ex_mid$value, selection$mid)
})

testthat::test_that('dashboard views render and navigation cascades', {
  testthat::expect_match(as.character(routineqc:::.qc_app_ui()),
                         'class="tab-pane active" data-value="Landing page"', fixed = TRUE)
  directory <- tempfile('routineqc-dash-')
  dir.create(directory)
  on.exit(unlink(directory, recursive = TRUE), add = TRUE)
  fx <- dashboard_fixture()
  path <- file.path(directory, 'dash-run.rds')
  routineqc::write_qc_run(fx$qc, path)
  server <- function(input, output, session) {
    routineqc:::.qc_app_server(input, output, session, run_dir = directory)
  }
  shiny::testServer(server, {
    suppressWarnings(session$setInputs(
      run_path = path, flagged_only = TRUE, column_set = 'review', use_date_filter = FALSE,
      ex_metric = 'positivity', ex_panels = FALSE, ex_points = TRUE, fi_scenario = 'moderate',
      fi_metric = 'positivity', gd_domain = 'All', gd_search = ''
    ))
    testthat::expect_true(length(output$ex_plot) > 0L)
    testthat::expect_true(length(output$ex_strip) > 0L)
    testthat::expect_match(output$ex_breadcrumb, 'National')
    testthat::expect_match(output$landing_priorities, 'Critical / high')
    testthat::expect_true(length(output$landing_regions) > 0L)
    testthat::expect_match(output$landing_priority_definitions$html, 'High \\(severe\\)')
    testthat::expect_match(output$landing_series, 'QC baseline replacement')
    testthat::expect_match(output$landing_replacement_note$html, 'saved GAM probability and rolling tested median')
    session$setInputs(landing_metric = 'tested')
    testthat::expect_match(output$landing_series, 'Tested')
    testthat::expect_true(length(output$ov_assessment_plot) > 0L)
    testthat::expect_true(length(output$fi_series) > 0L)
    testthat::expect_true(length(output$fi_strictness) > 0L)
    testthat::expect_true(length(output$fi_table) > 0L)
    testthat::expect_false(grepl('removed_with_facility|facilities_eligible', output$fi_table))
    testthat::expect_match(output$fi_rules, 'no reporting-completeness filter')
    testthat::expect_match(output$fi_rules, 'Facility-month observations')
    testthat::expect_match(output$gd_catalog, 'Flag')
    testthat::expect_match(output$fi_imputation_note$html, 'Imputation has not been run')

    fac <- as.character(fx$data$facility_id[[1]])
    suppressWarnings(session$setInputs(ex_facility = fac))
    testthat::expect_match(output$ex_breadcrumb, fac, fixed = TRUE)
    suppressWarnings(session$setInputs(ex_reset = 1))
    testthat::expect_identical(output$ex_breadcrumb, 'National')

    suppressWarnings(session$setInputs(
      fi_method = 'expected', fi_mode = 'retrospective', fi_n = 4, fi_seed = 3, fi_run_imputation = 1
    ))
    testthat::expect_match(output$fi_imputation_note$html, 'expected')
    testthat::expect_match(output$fi_imputation_note$html, 'no uncertainty')
    testthat::expect_true(length(output$fi_series) > 0L)
  })
})
