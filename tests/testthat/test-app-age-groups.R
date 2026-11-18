age_app_fixture <- local({
  cache <- NULL
  function() {
    if (is.null(cache)) {
      data <- routineqc::simulate_qc_age_data(seed = 3)
      run <- suppressWarnings(routineqc::run_routine_qc(
        data, facility_var = 'facility_id', region_var = 'region', month_var = 'month_date',
        tested_var = 'tested', positive_var = 'positive', attending_var = 'attending',
        attendance_definition = 'first ANC attendance', district_var = 'district',
        age_group_var = 'age_group', config = routineqc::qc_config('anc_first_visit')
      ))
      cache <<- run
    }
    cache
  }
})

testthat::test_that('age-group choices and the filtered run follow the data', {
  run <- age_app_fixture()
  choices <- routineqc:::.age_group_choices(run$data_flagged)
  testthat::expect_identical(unname(choices), c('lt20', 'ge20'))
  testthat::expect_identical(routineqc:::.age_group_choices(routineqc::simulate_qc_data(seed = 1)), character())
  young <- routineqc:::.run_for_age_group(run, 'lt20')
  testthat::expect_true(all(young$data_flagged$age_group_code == 'lt20'))
  testthat::expect_identical(routineqc:::.age_group_of(young), 'lt20')
  for (g in list(NULL, 'all', 'nope')) {
    same <- routineqc:::.run_for_age_group(run, g)
    testthat::expect_identical(nrow(same$data_flagged), nrow(run$data_flagged))
    testthat::expect_null(routineqc:::.age_group_of(same))
  }
  colors <- routineqc:::.age_group_colors(c('ge20', 'lt20'))
  testthat::expect_identical(names(colors), c('ge20', 'lt20'))
  testthat::expect_identical(unname(colors['lt20']), '#2e9a6e')
})

testthat::test_that('Explore data series, points and states are per age group', {
  d <- routineqc:::.explore_prepare(age_app_fixture()$data_flagged)
  fac <- d$facility_id[which(routineqc:::.is_flagged_row(d))[1]]
  sel <- list(facility = fac)
  by_group <- routineqc:::.explore_series_by_group(d, sel)
  testthat::expect_identical(names(by_group), c('lt20', 'ge20'))
  whole <- routineqc:::.explore_series(d, sel)
  tested_sum <- by_group$lt20$tested + by_group$ge20$tested
  both <- !is.na(tested_sum)
  testthat::expect_equal(tested_sum[both], whole$tested[both])
  testthat::expect_true(all(c('expected_positivity', 'expected_tested') %in% names(whole)))
  testthat::expect_true(any(!is.na(whole$expected_positivity)))
  testthat::expect_true(all(whole$expected_positivity >= 0 & whole$expected_positivity <= 1, na.rm = TRUE))

  # Together: one point per flagged facility-month, on the month's total.
  together <- routineqc:::.explore_flagged_points(d, sel, 'tested')
  testthat::expect_false(any(duplicated(paste(together$facility_id, together$month_date))))
  testthat::expect_equal(together$.value, whole$tested[match(together$month_date, whole$month_date)])
  # Compared: each flagged record on its own group's line.
  apart <- routineqc:::.explore_flagged_points(d, sel, 'tested', by_group = TRUE)
  line <- mapply(function(g, m) by_group[[g]]$tested[by_group[[g]]$month_date == m],
                 apart$age_group_code, apart$month_date)
  testthat::expect_equal(apart$.value, unname(line))

  states <- routineqc:::.explore_facility_states(d, fac, by_group = TRUE)
  testthat::expect_identical(nrow(states), 2L * length(routineqc:::.explore_months(d)))
  testthat::expect_setequal(levels(states$group), c('<20', '\u226520'))
  plot <- routineqc:::.plot_explore_series(by_group, 'positivity', apart, expected = TRUE)
  testthat::expect_s3_class(plot, 'plotly')
  testthat::expect_s3_class(routineqc:::.plot_explore_strip(by_group, states), 'plotly')
  testthat::expect_s3_class(routineqc:::.plot_explore_strip(by_group), 'plotly')
})

testthat::test_that('the share of records flagged counts records, not facility-months', {
  d <- routineqc:::.explore_prepare(age_app_fixture()$data_flagged)
  ch <- routineqc:::.explore_children(d, routineqc:::.nav_empty())
  testthat::expect_equal(sum(ch$records), nrow(d))
  testthat::expect_true(all(ch$share_flagged <= 1))
  testthat::expect_equal(ch$share_flagged, ch$review_rows / ch$records)
})

testthat::test_that('a selected record shows every age group of its month', {
  d <- routineqc:::.explore_prepare(age_app_fixture()$data_flagged)
  row <- d[which(routineqc:::.is_flagged_row(d))[1], ]
  tab <- routineqc:::.record_month_table(d, row)
  testthat::expect_identical(names(tab), c('field', '<20', '\u226520'))
  testthat::expect_true(all(c('Tested', 'Positive', 'Positivity', 'Priority') %in% tab$field))
  testthat::expect_identical(attr(tab, 'selected'), as.character(row$age_group))
  single <- routineqc:::.record_month_table(d[d$age_group_code == row$age_group_code, ], row)
  testthat::expect_identical(names(single), c('field', 'Value'))
  queue <- routineqc:::.explore_queue_table(d[routineqc:::.is_flagged_row(d), ], more = TRUE)
  testthat::expect_true(all(c('Facility', 'Month', 'Age group', 'Priority', 'Reasons', 'Expected',
                              'Scaled residual') %in% names(queue)))
})

testthat::test_that('the dashboard follows one age-group setting across tabs', {
  directory <- tempfile('routineqc-age-')
  dir.create(directory)
  on.exit(unlink(directory, recursive = TRUE), add = TRUE)
  path <- file.path(directory, 'age-run.rds')
  routineqc::write_qc_run(age_app_fixture(), path)
  server <- function(input, output, session) {
    routineqc:::.qc_app_server(input, output, session, run_dir = directory)
  }
  shiny::testServer(server, {
    suppressWarnings(session$setInputs(
      run_path = path, flagged_only = TRUE, column_set = 'review', use_date_filter = FALSE,
      ex_metric = 'positivity', ex_panels = FALSE, ex_points = TRUE, fi_scenario = 'moderate',
      fi_metric = 'positivity', gd_domain = 'All', gd_search = '',
      ex_queue_priority = c('critical', 'high', 'medium', 'low')
    ))
    testthat::expect_match(output$age_group_selector$html, 'All age groups')
    testthat::expect_match(output$ex_compare_ui$html, 'Compare age groups')
    session$setInputs(ex_compare = TRUE)
    testthat::expect_match(output$ex_plot, 'Age group')
    count <- function() as.numeric(gsub('[^0-9]', '', output$ex_queue_title))
    all_rows <- count()
    session$setInputs(age_group = 'lt20')
    testthat::expect_lt(count(), all_rows)
    testthat::expect_gt(count(), 0)
    testthat::expect_null(output$ex_compare_ui)
    testthat::expect_match(output$fi_age_note$html, 'only')
    testthat::expect_true(length(output$fi_series) > 0L)
    testthat::expect_match(output$dt_run_strip$html, 'Age group')
    testthat::expect_true(length(output$ex_queue) > 0L)
  })
})

testthat::test_that('the district grid draws one line per age group', {
  data <- age_app_fixture()$data_flagged
  district <- as.character(data$district[1])
  plot <- suppressWarnings(routineqc:::.plot_district_facets(data, district, 'prevalence'))
  testthat::expect_s3_class(plot, 'plotly')
  key <- routineqc:::.district_flag_key('prevalence', routineqc:::.age_group_choices(data))
  testthat::expect_match(as.character(key), 'Age group')
})
