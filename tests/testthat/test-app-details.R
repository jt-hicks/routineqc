details_inputs <- function() {
  fx <- dashboard_fixture()
  data <- .explore_prepare(fx$data)
  catalog <- .qc_flag_catalog(fx$qc$config)
  list(data = data, catalog = catalog, long = .details_flag_long(data, catalog), qc = fx$qc)
}

test_that('flag table has one row per fired flag and records never rank below their flags', {
  x <- details_inputs()
  long <- x$long
  for (i in seq_len(nrow(x$catalog))) {
    col <- x$catalog$flag[[i]]
    fired <- if (col %in% names(x$data)) sum(dplyr::coalesce(as.logical(x$data[[col]]), FALSE)) else 0L
    expect_equal(sum(long$key == x$catalog$key[[i]]), fired, info = col)
  }
  rank <- function(p) match(p, c(.qc_priority_levels, 'none'))
  expect_true(all(rank(long$record_priority) <= rank(long$flag_priority)))
  expect_equal(long$record_priority, as.character(x$data$review_priority[long$.source]))
})

test_that('reason, time, place and facility summaries agree with each other', {
  x <- details_inputs()
  rs <- .details_reason_summary(x$long, nrow(x$data))
  expect_equal(sum(rs$records), nrow(x$long))
  for (key in list(NULL, rs$key[[1]])) {
    n <- length(.details_sources(x$long, key))
    expect_equal(sum(.details_monthly(x$data, x$long, key)$flagged), n)
    expect_equal(.details_concentration(x$data, x$long, key)$flagged_records, n)
    ranked <- .details_ranked_units(x$data, x$long, key)
    expect_equal(sum(ranked$flagged_records), n)
    expect_true(all(ranked$critical_high_records <= ranked$flagged_records))
    um <- .details_unit_month(x$data, x$long, key)
    expect_equal(rowSums(um$flagged), ranked$flagged_records[match(um$units, ranked$unit)],
                 ignore_attr = TRUE)
  }
  conc <- .details_concentration(x$data, x$long)
  expect_equal(utils::tail(conc$curve$share_flagged, 1), 1)
  expect_true(conc$top_share >= 0.05 && conc$top_share <= 1)
})

test_that('persistent facilities count distinct flagged months', {
  data <- tibble::tibble(
    facility_id = rep(c('F1', 'F2'), each = 7), council = 'C1', region = 'R1',
    month_date = rep(seq.Date(as.Date('2024-01-01'), by = 'month', length.out = 7), 2),
    flag_coverage_low = c(rep(TRUE, 6), FALSE, rep(c(TRUE, FALSE), length.out = 7)),
    review_priority = 'low'
  )
  long <- .details_flag_long(data, .qc_flag_catalog(list()))
  ranked <- .details_ranked_units(data, long)
  expect_equal(ranked$persistent_facilities, 1L)
  expect_equal(ranked$flagged_records, 10L)
})

test_that('flow counts flags in both steps and handles an empty run', {
  x <- details_inputs()
  flow <- .details_flow(x$long)
  expect_equal(sum(flow$flags), nrow(x$long))
  focus <- x$long$label == x$long$label[[1]]
  built <- plotly::plotly_build(.plot_details_flow(.details_flow(x$long, focus)))
  link <- built$x$data[[1]]$link
  n_labels <- length(unique(flow$label))
  first <- link$source < n_labels
  expect_equal(sum(link$value[first]), nrow(x$long))
  expect_equal(sum(link$value[!first]), nrow(x$long))
  empty <- x$long[0, ]
  for (p in list(.plot_details_flow(.details_flow(empty)),
                 .plot_details_reasons(.details_reason_summary(empty, 10)),
                 .plot_details_unit_month(.details_unit_month(x$data, empty)),
                 .plot_details_concentration(.details_concentration(x$data, empty)))) {
    expect_s3_class(p, 'plotly')
  }
})

test_that('filters combine flag type, flag priority and record priority', {
  x <- details_inputs()
  long <- x$long
  expect_equal(nrow(.details_filter_long(long)), nrow(long))
  expect_equal(nrow(.details_filter_long(long, '', '', '')), nrow(long))
  f <- .details_filter_long(long, flag_priority = 'low', record_priority = 'high')
  expect_true(nrow(f) > 0L)
  expect_true(all(f$flag_priority == 'low' & f$record_priority == 'high'))
  key <- long$key[[1]]
  expect_true(all(.details_filter_long(long, key)$key == key))
  expect_equal(.details_phrase(), 'any flag')
  expect_equal(.details_phrase('Low testing coverage'), 'the flag "Low testing coverage"')
  expect_equal(.details_phrase(NULL, 'medium', 'high'), 'a medium-priority flag on a high-priority record')
  shares <- .details_unit_share(x$data, f, 'region')
  expect_equal(sum(shares$flagged_records), length(unique(f$.source)))
  expect_s3_class(.details_map(shares), 'leaflet')
})

test_that('QC details server follows the filters and opens units in Explore data', {
  x <- details_inputs()
  opened <- NULL
  server <- function(input, output, session) {
    run <- shiny::reactive(list(data_flagged = x$data, config = x$qc$config, manifest = list()))
    .details_server(input, output, session, run, shiny::reactive(x$data),
                    shiny::reactive(x$catalog), function(field, unit) opened <<- c(field, unit))
  }
  rs <- .details_reason_summary(x$long, nrow(x$data))
  shiny::testServer(server, {
    expect_match(output$dt_where_sub, 'any flag')
    expect_match(output$dt_concentration_sub, 'of all flagged records')
    session$setInputs(dt_flag = rs$key[[1]])
    for (id in c('dt_where_sub', 'dt_map_sub', 'dt_ranked_sub', 'dt_concentration_sub')) {
      expect_match(output[[id]], rs$label[[1]], fixed = TRUE, info = id)
    }
    expect_match(output$dt_concentration_sub, 'of those records')
    session$setInputs(dt_flag = '', dt_record_priority = 'high')
    expect_match(output$dt_where_sub, 'any flag on a high-priority record')
    session$setInputs(dt_record_priority = '')
    ranked <- .details_ranked_units(x$data, x$long)
    session$setInputs(dt_ranked_rows_selected = 1)
    expect_identical(opened, c('council', ranked$unit[[1]]))
  })
})
