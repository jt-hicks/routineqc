nav_data <- function() dashboard_fixture()$data

testthat::test_that('mid level prefers council and never merges council with district', {
  d <- nav_data()
  testthat::expect_identical(routineqc:::.nav_mid_field(d), 'council')
  d$district <- 'D1'
  testthat::expect_identical(routineqc:::.nav_mid_field(d), 'council')
  d$council <- NA_character_
  testthat::expect_identical(routineqc:::.nav_mid_field(d), 'district')
  d$district <- NA
  testthat::expect_null(routineqc:::.nav_mid_field(d))
})

testthat::test_that('choices cascade from region to council to facility', {
  d <- nav_data()
  all <- routineqc:::.nav_choices(d, routineqc:::.nav_empty())
  testthat::expect_length(all$region, 2L)
  testthat::expect_length(all$mid, 6L)
  testthat::expect_length(all$facility, 24L)
  r1 <- routineqc:::.nav_choices(d, list(region = all$region[1]))
  testthat::expect_length(r1$mid, 3L)
  testthat::expect_length(r1$facility, 12L)
  c1 <- routineqc:::.nav_choices(d, list(region = all$region[1], mid = r1$mid[1]))
  testthat::expect_length(c1$facility, 4L)
  testthat::expect_true(all(startsWith(c1$facility, r1$mid[1])))
})

testthat::test_that('changing a parent clears children and a child fills in its parents', {
  d <- nav_data()
  ch <- routineqc:::.nav_choices(d, routineqc:::.nav_empty())
  sel <- routineqc:::.nav_set(d, routineqc:::.nav_empty(), 'facility', ch$facility[15])
  testthat::expect_identical(sel$facility, ch$facility[15])
  testthat::expect_true(startsWith(ch$facility[15], sel$mid))
  testthat::expect_true(startsWith(sel$mid, sel$region))
  testthat::expect_identical(routineqc:::.nav_level(sel), 'facility')

  other_region <- setdiff(ch$region, sel$region)
  moved <- routineqc:::.nav_set(d, sel, 'region', other_region)
  testthat::expect_identical(moved$region, other_region)
  testthat::expect_null(moved$mid)
  testthat::expect_null(moved$facility)

  up <- routineqc:::.nav_set(d, sel, 'mid', sel$mid)
  testthat::expect_null(up$facility)
  testthat::expect_identical(routineqc:::.nav_level(up), 'mid')
  cleared <- routineqc:::.nav_set(d, sel, 'region', NULL)
  testthat::expect_identical(routineqc:::.nav_level(cleared), 'national')
})

testthat::test_that('an invalid or stale selection is reset rather than kept', {
  d <- nav_data()
  testthat::expect_identical(
    routineqc:::.nav_level(routineqc:::.nav_validate(d, list(region = 'Nowhere'))), 'national'
  )
  ch <- routineqc:::.nav_choices(d, routineqc:::.nav_empty())
  bad <- routineqc:::.nav_validate(d, list(region = ch$region[1], mid = 'ghost', facility = ch$facility[1]))
  testthat::expect_null(bad$mid)
  testthat::expect_null(bad$facility)
  wrong_parent <- routineqc:::.nav_validate(d, list(region = ch$region[2], facility = ch$facility[1]))
  testthat::expect_null(wrong_parent$facility)
  testthat::expect_identical(routineqc:::.nav_level(routineqc:::.nav_validate(d, NULL)), 'national')
})

testthat::test_that('subset, breadcrumb and child level follow the selection', {
  d <- nav_data()
  ch <- routineqc:::.nav_choices(d, routineqc:::.nav_empty())
  sel <- routineqc:::.nav_set(d, routineqc:::.nav_empty(), 'mid', ch$mid[2])
  sub <- routineqc:::.nav_subset(d, sel)
  testthat::expect_true(all(sub$council == ch$mid[2]))
  testthat::expect_identical(routineqc:::.nav_breadcrumb(sel), paste('National', sel$region, sel$mid, sep = ' > '))
  testthat::expect_identical(routineqc:::.nav_child_level(sel, 'council'), 'facility')
  testthat::expect_identical(routineqc:::.nav_child_level(routineqc:::.nav_empty(), 'council'), 'region')
  testthat::expect_identical(routineqc:::.nav_child_level(list(region = 'R01'), 'council'), 'mid')
  testthat::expect_identical(routineqc:::.nav_child_level(list(region = 'R01'), NULL), 'facility')
  testthat::expect_true(is.na(routineqc:::.nav_child_level(list(facility = 'x'), 'council')))
  testthat::expect_equal(nrow(routineqc:::.nav_subset(d, routineqc:::.nav_empty())), nrow(d))
})

testthat::test_that('a run without council or district goes from region to facility', {
  d <- nav_data()
  d$council <- NULL
  sel <- routineqc:::.nav_set(d, routineqc:::.nav_empty(), 'region', 'R01')
  testthat::expect_null(routineqc:::.nav_mid_field(d))
  testthat::expect_identical(routineqc:::.nav_child_level(sel, NULL), 'facility')
  testthat::expect_length(routineqc:::.nav_choices(d, sel)$mid, 0L)
})

testthat::test_that('national series totals equal the sum of the regional series', {
  d <- routineqc:::.explore_prepare(nav_data())
  nat <- routineqc:::.explore_series(d, routineqc:::.nav_empty())
  regions <- unique(d$region)
  parts <- lapply(regions, function(r) routineqc:::.explore_series(d, list(region = r)))
  total <- Reduce(`+`, lapply(parts, function(p) ifelse(is.na(p$tested), 0, p$tested)))
  testthat::expect_equal(ifelse(is.na(nat$tested), 0, nat$tested), total)
  testthat::expect_equal(nat$facilities_expected, Reduce(`+`, lapply(parts, `[[`, 'facilities_expected')))
  testthat::expect_equal(nrow(nat), length(unique(d$month_date)))
})

testthat::test_that('missing months are gaps, not zeros, and genuine zeros stay zero', {
  d <- routineqc:::.explore_prepare(nav_data())
  fac <- d$facility_id[1]
  d$tested[d$facility_id == fac & d$month_date == min(d$month_date)] <- NA
  d$positive[d$facility_id == fac & d$month_date == min(d$month_date)] <- NA
  d$tested[d$facility_id == fac & d$month_date == sort(unique(d$month_date))[3]] <- 0
  d$positive[d$facility_id == fac & d$month_date == sort(unique(d$month_date))[3]] <- 0
  s <- routineqc:::.explore_series(d, list(facility = fac))
  testthat::expect_true(is.na(s$tested[1]))
  testthat::expect_true(is.na(s$positivity[3]))
  testthat::expect_identical(s$tested[3], 0)
  st <- routineqc:::.explore_facility_states(d, fac)
  testthat::expect_identical(st$state[1], 'missing count')
  testthat::expect_identical(st$state[3], 'zero tested')
})

testthat::test_that('facility states distinguish absent reports from invalid counts', {
  d <- routineqc:::.explore_prepare(nav_data())
  fac <- d$facility_id[1]
  months <- sort(unique(d$month_date))
  d <- d[!(d$facility_id == fac & d$month_date == months[4]), ]
  i <- which(d$facility_id == fac & d$month_date == months[5])
  d$tested[i] <- 10.5
  d$tested_parse_status[i] <- 'fractional'
  d$flag_count_invalid_value[i] <- TRUE
  st <- routineqc:::.explore_facility_states(d, fac)
  testthat::expect_identical(st$state[4], 'absent')
  testthat::expect_identical(st$state[5], 'invalid count')
})

testthat::test_that('children are the next level down and counts add up', {
  d <- routineqc:::.explore_prepare(nav_data())
  top <- routineqc:::.explore_children(d, routineqc:::.nav_empty())
  testthat::expect_identical(attr(top, 'child_level'), 'region')
  testthat::expect_equal(nrow(top), 2L)
  testthat::expect_equal(sum(top$review_rows), sum(routineqc:::.is_flagged_row(d)))
  councils <- routineqc:::.explore_children(d, list(region = top$unit[1]))
  testthat::expect_identical(attr(councils, 'child_level'), 'mid')
  testthat::expect_equal(nrow(councils), 3L)
  facilities <- routineqc:::.explore_children(d, list(region = top$unit[1], mid = councils$unit[1]))
  testthat::expect_identical(attr(facilities, 'child_level'), 'facility')
  testthat::expect_equal(nrow(facilities), 4L)
  testthat::expect_null(routineqc:::.explore_children(d, list(facility = d$facility_id[1])))
  testthat::expect_true(all(diff(top$high_priority_rows) <= 0))
})

testthat::test_that('flagged points use only valid values and trace back to one record', {
  d <- routineqc:::.explore_prepare(nav_data())
  for (metric in names(routineqc:::.explore_metrics)) {
    pts <- routineqc:::.explore_flagged_points(d, routineqc:::.nav_empty(), metric)
    testthat::expect_true(all(!is.na(pts$.value)), info = metric)
    testthat::expect_false(anyDuplicated(pts$.row) > 0L)
    testthat::expect_true(all(routineqc:::.is_flagged_row(pts)))
  }
  pos <- routineqc:::.explore_flagged_points(d, routineqc:::.nav_empty(), 'positivity')
  testthat::expect_true(all(pos$.value >= 0 & pos$.value <= 1))
})

testthat::test_that('explorer charts build for every metric at every level', {
  d <- routineqc:::.explore_prepare(nav_data())
  fac <- d$facility_id[1]
  for (sel in list(routineqc:::.nav_empty(), list(region = 'R01'), list(facility = fac))) {
    s <- routineqc:::.explore_series(d, sel)
    for (metric in names(routineqc:::.explore_metrics)) {
      p <- routineqc:::.explore_flagged_points(d, sel, metric)
      testthat::expect_s3_class(routineqc:::.plot_explore_series(s, metric, p), 'plotly')
    }
    testthat::expect_s3_class(routineqc:::.plot_explore_strip(s), 'plotly')
  }
  testthat::expect_s3_class(
    routineqc:::.plot_explore_strip(NULL, routineqc:::.explore_facility_states(d, fac)), 'plotly'
  )
  s <- routineqc:::.explore_series(d, routineqc:::.nav_empty())
  testthat::expect_s3_class(routineqc:::.plot_explore_panels(s), 'plotly')
})
