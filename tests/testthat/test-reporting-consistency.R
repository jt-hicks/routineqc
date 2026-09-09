consistency_fixture <- function() {
  months <- function(n, from = '2024-01-01') {
    seq.Date(as.Date(from), by = 'month', length.out = n)
  }
  dplyr::bind_rows(
    # Complete across the whole dataset span.
    tibble::tibble(facility_id = 'F1', month_date = months(6), tested = rep(10, 6)),
    # Two absent months in the middle of the window.
    tibble::tibble(
      facility_id = 'F2',
      month_date = as.Date(c('2024-01-01', '2024-02-01', '2024-05-01', '2024-06-01')),
      tested = rep(10, 4)
    ),
    # Reported but zero and missing tested values.
    tibble::tibble(
      facility_id = 'F3', month_date = months(6),
      tested = c(10, 0, NA, 0, 10, 10)
    ),
    # Never tested.
    tibble::tibble(
      facility_id = 'F4', month_date = months(6),
      tested = c(0, 0, NA, 0, 0, 0)
    ),
    # Short history, internally complete.
    tibble::tibble(
      facility_id = 'F5',
      month_date = as.Date(c('2024-05-01', '2024-06-01')), tested = c(10, 10)
    ),
    # Gap at the start of the window.
    tibble::tibble(
      facility_id = 'F6', month_date = months(6), tested = c(0, 10, 10, 10, 10, 10)
    ),
    # Gap at the end of the window.
    tibble::tibble(
      facility_id = 'F7', month_date = months(6), tested = c(10, 10, 10, 10, 10, 0)
    )
  ) %>%
    dplyr::mutate(positive = dplyr::if_else(is.na(tested), NA_real_, tested * 0.1))
}

testthat::test_that('absent, zero, and missing months are counted separately', {
  out <- routineqc::summarise_qc_reporting_consistency(consistency_fixture())
  testthat::expect_identical(out$facility_id, paste0('F', 1:7))
  testthat::expect_identical(out$months_absent, c(0L, 2L, 0L, 0L, 0L, 0L, 0L))
  testthat::expect_identical(out$months_zero_tested, c(0L, 0L, 2L, 5L, 0L, 1L, 1L))
  testthat::expect_identical(out$months_missing_tested, c(0L, 0L, 1L, 1L, 0L, 0L, 0L))
  testthat::expect_identical(out$months_with_testing, c(6L, 4L, 3L, 0L, 2L, 5L, 5L))
  testthat::expect_identical(out$months_without_testing, c(0L, 2L, 3L, 6L, 0L, 1L, 1L))
})

testthat::test_that('component counts satisfy the reporting-window identities', {
  out <- routineqc::summarise_qc_reporting_consistency(consistency_fixture())
  testthat::expect_identical(
    out$months_reported,
    out$months_with_testing + out$months_zero_tested + out$months_missing_tested
  )
  testthat::expect_identical(
    out$months_in_facility_window, out$months_reported + out$months_absent
  )
  testthat::expect_identical(
    out$months_without_testing,
    out$months_in_facility_window - out$months_with_testing
  )
})

testthat::test_that('longest gap spans absent, zero, and missing months alike', {
  out <- routineqc::summarise_qc_reporting_consistency(consistency_fixture())
  testthat::expect_identical(out$longest_gap_months, c(0L, 2L, 3L, 6L, 0L, 1L, 1L))
  testthat::expect_identical(out$n_gaps, c(0L, 1L, 1L, 1L, 0L, 1L, 1L))
})

testthat::test_that('gaps are located at window edges and interior', {
  out <- routineqc::summarise_qc_reporting_consistency(consistency_fixture())
  gap <- function(id, field) out[[field]][out$facility_id == id]
  testthat::expect_identical(gap('F2', 'longest_gap_start'), as.Date('2024-03-01'))
  testthat::expect_identical(gap('F2', 'longest_gap_end'), as.Date('2024-04-01'))
  testthat::expect_identical(gap('F6', 'longest_gap_start'), as.Date('2024-01-01'))
  testthat::expect_identical(gap('F7', 'longest_gap_end'), as.Date('2024-06-01'))
  testthat::expect_true(is.na(gap('F1', 'longest_gap_start')))
})

testthat::test_that('data collection start and end are reported separately', {
  out <- routineqc::summarise_qc_reporting_consistency(consistency_fixture())
  f6 <- out[out$facility_id == 'F6', ]
  testthat::expect_identical(f6$first_month_reported, as.Date('2024-01-01'))
  testthat::expect_identical(f6$first_month_tested, as.Date('2024-02-01'))
  f7 <- out[out$facility_id == 'F7', ]
  testthat::expect_identical(f7$last_month_reported, as.Date('2024-06-01'))
  testthat::expect_identical(f7$last_month_tested, as.Date('2024-05-01'))
  f4 <- out[out$facility_id == 'F4', ]
  testthat::expect_true(is.na(f4$first_month_tested))
  testthat::expect_true(is.na(f4$last_month_tested))
})

testthat::test_that('facilities with no testing in any month are identified', {
  out <- routineqc::summarise_qc_reporting_consistency(consistency_fixture())
  testthat::expect_identical(out$facility_id[out$never_tested], 'F4')
  testthat::expect_equal(sum(out$never_tested), 1L)
})

testthat::test_that('proportions use both the facility window and the dataset span', {
  out <- routineqc::summarise_qc_reporting_consistency(consistency_fixture())
  testthat::expect_true(all(out$months_in_dataset_window == 6L))
  short <- out[out$facility_id == 'F5', ]
  # Internally complete inside its own two-month window ...
  testthat::expect_equal(short$prop_without_testing_facility_window, 0)
  # ... but covering only a third of the dataset span.
  testthat::expect_equal(short$prop_with_testing_dataset_window, 1 / 3)
  testthat::expect_equal(short$prop_reported_dataset_window, 1 / 3)
  gapped <- out[out$facility_id == 'F2', ]
  testthat::expect_equal(gapped$prop_without_testing_facility_window, 1 / 3)
  testthat::expect_equal(gapped$prop_with_testing_dataset_window, 2 / 3)
})

testthat::test_that('a single-month facility is measured without error', {
  dat <- tibble::tibble(
    facility_id = 'F1', month_date = as.Date('2024-01-01'), tested = 5
  )
  out <- routineqc::summarise_qc_reporting_consistency(dat)
  testthat::expect_identical(out$months_in_facility_window, 1L)
  testthat::expect_identical(out$months_with_testing, 1L)
  testthat::expect_identical(out$longest_gap_months, 0L)
  testthat::expect_false(out$never_tested)
})

testthat::test_that('strictness levels are ordered presets with documented criteria', {
  levels <- routineqc::qc_consistency_levels()
  testthat::expect_identical(
    levels$level,
    c('all', 'any_testing', 'lenient', 'moderate', 'near_complete',
      'complete', 'complete_panel')
  )
  testthat::expect_true(all(is.na(unlist(levels[levels$level == 'all', -1]))))
})

testthat::test_that('the most lenient level requires only one tested month', {
  out <- routineqc::summarise_qc_reporting_consistency(
    consistency_fixture(), strictness = 'any_testing'
  )
  testthat::expect_identical(
    out$facility_id[!out$passes_strictness], 'F4'
  )
})

testthat::test_that('the most severe levels require no month without testing', {
  complete <- routineqc::summarise_qc_reporting_consistency(
    consistency_fixture(), strictness = 'complete'
  )
  testthat::expect_identical(
    complete$facility_id[complete$passes_strictness], c('F1', 'F5')
  )
  panel <- routineqc::summarise_qc_reporting_consistency(
    consistency_fixture(), strictness = 'complete_panel'
  )
  testthat::expect_identical(panel$facility_id[panel$passes_strictness], 'F1')
})

testthat::test_that('a short but internally complete history fails dataset coverage', {
  moderate <- routineqc::summarise_qc_reporting_consistency(
    consistency_fixture(), strictness = 'moderate'
  )
  short <- moderate[moderate$facility_id == 'F5', ]
  testthat::expect_equal(short$prop_without_testing_facility_window, 0)
  testthat::expect_false(short$passes_strictness)
})

testthat::test_that('no filtering is applied at the all level', {
  out <- routineqc::summarise_qc_reporting_consistency(
    consistency_fixture(), strictness = 'all'
  )
  testthat::expect_true(all(out$passes_strictness))
})

testthat::test_that('threshold overrides are applied at their boundaries', {
  gap_two <- routineqc::summarise_qc_reporting_consistency(
    consistency_fixture(), strictness = 'all', max_gap_months = 2
  )
  testthat::expect_identical(
    gap_two$facility_id[gap_two$passes_strictness],
    c('F1', 'F2', 'F5', 'F6', 'F7')
  )
  gap_one <- routineqc::summarise_qc_reporting_consistency(
    consistency_fixture(), strictness = 'all', max_gap_months = 1
  )
  testthat::expect_identical(
    gap_one$facility_id[gap_one$passes_strictness], c('F1', 'F5', 'F6', 'F7')
  )
  min_months <- routineqc::summarise_qc_reporting_consistency(
    consistency_fixture(), strictness = 'all', min_months_with_testing = 5
  )
  testthat::expect_identical(
    min_months$facility_id[min_months$passes_strictness], c('F1', 'F6', 'F7')
  )
})

testthat::test_that('an override replaces the level threshold it names', {
  relaxed <- routineqc::summarise_qc_reporting_consistency(
    consistency_fixture(), strictness = 'complete_panel',
    min_prop_with_testing_dataset_window = 0
  )
  testthat::expect_identical(
    relaxed$facility_id[relaxed$passes_strictness], c('F1', 'F5')
  )
})

testthat::test_that('consistency arguments fail explicitly', {
  testthat::expect_error(
    routineqc::summarise_qc_reporting_consistency(
      consistency_fixture(), strictness = 'perfect'
    ),
    '`strictness` must be one of'
  )
  testthat::expect_error(
    routineqc::summarise_qc_reporting_consistency(
      consistency_fixture(), max_gap_months = -1
    ),
    'non-negative'
  )
  testthat::expect_error(
    routineqc::summarise_qc_reporting_consistency(
      consistency_fixture(), max_gap_months = c(1, 2)
    ),
    'one non-missing number'
  )
  testthat::expect_error(
    routineqc::summarise_qc_reporting_consistency(tibble::tibble(facility_id = 'F1')),
    'Missing required columns'
  )
})

testthat::test_that('cohort filtering keeps every row of a passing facility', {
  data <- consistency_fixture()
  original <- data
  kept <- routineqc::filter_qc_facilities(data, strictness = 'complete')
  testthat::expect_identical(sort(unique(kept$facility_id)), c('F1', 'F5'))
  testthat::expect_equal(
    sum(kept$facility_id == 'F1'), sum(data$facility_id == 'F1')
  )
  testthat::expect_equal(
    sum(kept$facility_id == 'F5'), sum(data$facility_id == 'F5')
  )
  testthat::expect_identical(names(kept), names(dplyr::as_tibble(data)))
  testthat::expect_identical(data, original)
})

testthat::test_that('an empty cohort yields zero rows rather than every row', {
  data <- consistency_fixture()
  empty <- routineqc::filter_qc_facilities(
    data, strictness = 'all', min_months_with_testing = 999
  )
  testthat::expect_equal(nrow(empty), 0L)
  testthat::expect_identical(names(empty), names(dplyr::as_tibble(data)))
})

testthat::test_that('all rows are retained at the all level', {
  data <- consistency_fixture()
  testthat::expect_equal(
    nrow(routineqc::filter_qc_facilities(data, strictness = 'all')), nrow(data)
  )
})

testthat::test_that('a precomputed measurement cache gives the same cohort', {
  data <- consistency_fixture()
  cached <- routineqc::summarise_qc_reporting_consistency(data)
  testthat::expect_identical(
    routineqc::filter_qc_facilities(data, strictness = 'moderate'),
    routineqc::filter_qc_facilities(
      data, strictness = 'moderate', consistency = cached
    )
  )
})

testthat::test_that('the QC wrapper reports facility consistency', {
  sim <- routineqc::simulate_qc_data(
    n_facilities = 6, n_months = 12, seed = 5,
    zero_tested_fraction = 0.05, absent_month_fraction = 0.05
  )
  run <- suppressWarnings(routineqc::run_routine_qc(
    sim,
    facility_var = 'facility', region_var = 'region', month_var = 'month',
    tested_var = 'tested', positive_var = 'positive',
    district_var = 'district', nthreads = 1
  ))
  consistency <- run$summaries$by_facility_reporting
  testthat::expect_true('by_facility_reporting' %in% names(run$summaries))
  testthat::expect_equal(
    nrow(consistency), dplyr::n_distinct(run$data_flagged$facility_id)
  )
  testthat::expect_true(all(consistency$months_without_testing >= 0))
  testthat::expect_invisible(routineqc::validate_qc_run(run))
})

testthat::test_that('simulated zero and absent months are opt-in', {
  plain <- routineqc::simulate_qc_data(n_facilities = 6, n_months = 12, seed = 5)
  injected <- routineqc::simulate_qc_data(
    n_facilities = 6, n_months = 12, seed = 5,
    zero_tested_fraction = 0.1, absent_month_fraction = 0.1
  )
  testthat::expect_equal(nrow(plain), 6 * 12)
  testthat::expect_equal(sum(plain$tested == 0), 0)
  testthat::expect_lt(nrow(injected), nrow(plain))
  testthat::expect_gt(sum(injected$tested == 0), 0)
  testthat::expect_error(
    routineqc::simulate_qc_data(4, 4, zero_tested_fraction = 1.5),
    'between 0 and 1'
  )
})

testthat::test_that('simulated reporting windows survive absent-month injection', {
  injected <- routineqc::simulate_qc_data(
    n_facilities = 6, n_months = 12, seed = 11, absent_month_fraction = 0.1
  )
  spans <- injected %>%
    dplyr::group_by(facility) %>%
    dplyr::summarise(
      first_month = min(month), last_month = max(month), .groups = 'drop'
    )
  testthat::expect_true(all(spans$first_month == min(injected$month)))
  testthat::expect_true(all(spans$last_month == max(injected$month)))
})

testthat::test_that('gap measurement plot data buckets every facility once', {
  consistency <- routineqc::summarise_qc_reporting_consistency(consistency_fixture())
  buckets <- routineqc:::.consistency_gap_buckets(consistency)
  testthat::expect_equal(sum(buckets$facilities), nrow(consistency))
  testthat::expect_s3_class(
    routineqc::plot_reporting_consistency(consistency), 'ggplot'
  )
})
