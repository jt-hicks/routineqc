age_fixture <- local({
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
      cache <<- list(data = data, run = run)
    }
    cache
  }
})

ge20 <- '\u226520'

testthat::test_that('age-band labels get ASCII codes', {
  testthat::expect_identical(
    routineqc:::.age_group_code(c('<20', ge20, '>=20', '10-14', ' 35+ ', '<=19', NA)),
    c('lt20', 'ge20', 'ge20', '10_14', '35plus', 'le19', NA)
  )
})

testthat::test_that('series split by facility, band and band construction', {
  data <- tibble::tibble(
    facility_id = 'F1', month_date = rep(as.Date(c('2020-11-01', '2020-12-01', '2021-01-01', '2021-02-01')), 2),
    age_group = rep(c('<20', ge20), each = 4), bands_expected = c(1, 1, 2, 2, 1, NA, 4, 4)
  )
  data$age_group_code <- routineqc:::.age_group_code(data$age_group)
  ids <- routineqc:::.qc_series_ids(data)
  testthat::expect_length(unique(ids), 4L)
  testthat::expect_identical(ids[1], ids[2])
  testthat::expect_false(ids[2] == ids[3])
  testthat::expect_identical(ids[5], ids[6])
  testthat::expect_length(unique(routineqc:::.qc_series_ids(data, segments = FALSE)), 2L)
  plain <- data[1:4, c('facility_id', 'month_date')]
  testthat::expect_identical(routineqc:::.qc_series_ids(plain), rep('F1', 4))
})

testthat::test_that('the row key includes the age band only when it is declared', {
  data <- routineqc::simulate_qc_age_data(n_months = 8, seed = 2)
  data$attending <- NULL
  args <- list(data = data, facility_var = 'facility_id', region_var = 'region', month_var = 'month_date',
               tested_var = 'tested', positive_var = 'positive')
  plain <- do.call(routineqc::prepare_qc_data, args)
  testthat::expect_error(routineqc::validate_qc_input(plain), 'pass `age_group_var`')
  banded <- do.call(routineqc::prepare_qc_data, c(args, age_group_var = 'age_group'))
  testthat::expect_invisible(routineqc::validate_qc_input(banded))
  testthat::expect_setequal(unique(banded$age_group_code), c('lt20', 'ge20'))
  dup <- rbind(banded, banded[1, ])
  testthat::expect_error(routineqc::validate_qc_input(dup), 'facility-month-age-band')
})

testthat::test_that('row-level band checks fire only on what they describe', {
  data <- tibble::tibble(
    tested = c(10, 10, 10, 10, NA, 10, 10, 10),
    positive = c(1, 1, 0, 0, NA, 2, 1, 1),
    attending = c(12, 13, 12, 12, 12, 12, 12, 12),
    anc_before_12wk = c(4, 4, 4, 4, 4, 4, 4, 4),
    anc_after_12wk = c(8, 8, 8, 8, 8, 8, 8, 8),
    positive_zero_filled = c(FALSE, FALSE, TRUE, FALSE, FALSE, FALSE, FALSE, FALSE),
    bands_expected = c(2, 2, 2, 2, 2, 2, 4, 1),
    tested_bands_reported = c(2, 2, 2, 2, 2, 2, 4, 1),
    positive_bands_reported = c(2, 2, 0, 0, 2, 2, 3, 2),
    anc_before_12wk_bands_reported = c(2, 2, 2, 2, 2, 2, 2, 1),
    anc_after_12wk_bands_reported = 2,
    attendance_definition = 'first ANC attendance'
  )
  out <- routineqc::add_age_band_qc_flags(routineqc::flag_logical_errors(data))
  testthat::expect_identical(out$flag_attendance_sum_mismatch, c(FALSE, TRUE, rep(FALSE, 6)))
  testthat::expect_true(out$flag_attendance_issue[2])
  testthat::expect_identical(out$flag_positive_assumed_zero, c(FALSE, FALSE, TRUE, rep(FALSE, 5)))
  # Zero-filled with no positive band reported is consistent; an unexplained
  # zero with no band reported, a value missing although bands were reported,
  # and more bands reported than exist are not.
  testthat::expect_identical(out$flag_band_metadata_inconsistent,
                             c(FALSE, FALSE, FALSE, TRUE, TRUE, FALSE, FALSE, TRUE))
  testthat::expect_identical(out$flag_band_sum_partial_positive, c(rep(FALSE, 6), TRUE, FALSE))
  testthat::expect_identical(out$flag_band_sum_partial_attending, c(rep(FALSE, 6), TRUE, FALSE))
  testthat::expect_false(any(out$flag_band_sum_partial_tested))
  plain <- routineqc::add_age_band_qc_flags(routineqc::flag_logical_errors(data[c('tested', 'positive')]))
  for (col in grep('^flag_(band|positive_assumed|attendance_sum)', names(plain), value = TRUE)) {
    testthat::expect_false(any(plain[[col]]), info = col)
  }
})

testthat::test_that('the pipeline models age bands and recovers what was injected', {
  fx <- age_fixture()
  x <- fx$run$data_flagged
  testthat::expect_equal(nrow(x), nrow(fx$data))
  testthat::expect_match(paste(deparse(stats::formula(fx$run$model)), collapse = ''), 'age_group_code')
  testthat::expect_setequal(fx$run$model$routineqc_model$age_levels, c('ge20', 'lt20'))
  testthat::expect_true(all(x$flag_band_sum_partial_tested[x$injected_partial]))
  testthat::expect_true(all(x$flag_positive_assumed_zero[x$injected_zero_fill]))
  testthat::expect_true(all(x$qc_action[x$injected_partial & !x$injected_error] != 'retain'))
  testthat::expect_gt(mean(x$flag_any_qc_issue[x$injected_error]), 0.6)
  clean <- !x$injected_error & !x$injected_partial & !x$injected_zero_fill
  testthat::expect_lt(mean(x$flag_any_qc_issue[clean]), 0.05)
})

testthat::test_that('untrusted band rows are not used to fit the model', {
  fx <- age_fixture()
  x <- fx$run$data_flagged
  untrusted <- routineqc:::.age_band_untrusted(x)
  testthat::expect_true(any(untrusted))
  eligible <- !x$flag_core_invalid & !routineqc:::.row_count_invalid(x) & !is.na(x$positive) & x$tested > 0
  testthat::expect_equal(nrow(fx$run$model$model), sum(eligible & !untrusted))
})

testthat::test_that('tested-volume baselines restart when the band construction changes', {
  fx <- age_fixture()
  x <- fx$run$data_flagged
  first_summed <- x$month_date == as.Date('2021-01-01')
  testthat::expect_true(all(x$tested_baseline_n[first_summed] == 0L))
  testthat::expect_false(any(x$flag_tested_large_jump[first_summed] | x$flag_tested_large_drop[first_summed]))
  before <- x$month_date == as.Date('2020-12-01')
  testthat::expect_true(all(x$tested_baseline_n[before] > 0L))
})

testthat::test_that('scenarios omit band flags at their priority', {
  fx <- age_fixture()
  x <- fx$run$data_flagged
  priority <- fx$run$config$priority
  moderate <- routineqc::apply_qc_scenario(x, routineqc::qc_scenario_presets()$moderate, priority)
  very <- routineqc::apply_qc_scenario(x, routineqc::qc_scenario_presets()$very_strict, priority)
  testthat::expect_true(all(moderate$scn_omit_tested[x$flag_band_sum_partial_tested]))
  zero_only <- x$flag_positive_assumed_zero & x$review_priority == 'low'
  testthat::expect_true(any(zero_only))
  testthat::expect_false(any(moderate$scn_omit_positive[zero_only]))
  testthat::expect_true(all(very$scn_omit_positive[zero_only]))
})

testthat::test_that('imputation works per facility band and keeps the facility', {
  fx <- age_fixture()
  x <- fx$run$data_flagged
  imp <- suppressWarnings(routineqc::qc_impute_scenario(
    x, routineqc::qc_scenario_presets()$moderate, method = 'expected', priority = fx$run$config$priority
  ))
  testthat::expect_setequal(unique(imp$scn$facility_id), unique(x$facility_id))
  testthat::expect_true(all(imp$targets$facility_id %in% x$facility_id))
  testthat::expect_true(any(imp$targets$tested_status == 'imputed', na.rm = TRUE))
})

testthat::test_that('an age-band run persists, validates and fingerprints the band', {
  fx <- age_fixture()
  path <- tempfile(fileext = '.rds')
  on.exit(unlink(c(path, sub('\\.rds$', '-manifest.json', path))), add = TRUE)
  routineqc::write_qc_run(fx$run, path)
  back <- routineqc::read_qc_run(path)
  testthat::expect_invisible(routineqc::validate_qc_run(back))
  relabelled <- fx$run$data_flagged
  relabelled$age_group <- rev(relabelled$age_group)
  testthat::expect_false(identical(routineqc::fingerprint_qc_input(relabelled),
                                   routineqc::fingerprint_qc_input(fx$run$data_flagged)))
})

testthat::test_that('the adapter maps age bands and keys duplicates on them', {
  data <- routineqc::simulate_qc_age_data(n_months = 8, seed = 4)
  names(data)[names(data) == 'age_group'] <- 'maternal_age'
  mapping <- list(facility_id = 'facility_id', month_date = 'month_date', region = 'region',
                  tested = 'tested', positive = 'positive', attending = 'attending',
                  age_group = 'maternal_age', anc_before_12wk = 'anc_before_12wk',
                  anc_after_12wk = 'anc_after_12wk', positive_zero_filled = 'positive_zero_filled',
                  bands_expected = 'bands_expected', positive_bands_reported = 'positive_bands_reported')
  adapted <- routineqc::adapt_qc_data(data, mapping, identity_strategy = 'synthetic', dataset_id = 'age-test',
                                      attendance_definition = 'first ANC attendance')
  testthat::expect_true(adapted$ready)
  dup <- routineqc::adapt_qc_data(rbind(data, data[1, ]), mapping, identity_strategy = 'synthetic',
                                  dataset_id = 'age-test', attendance_definition = 'first ANC attendance')
  testthat::expect_false(dup$ready)
  testthat::expect_true('duplicate_facility_month_age' %in% dup$diagnostics$issue_code)
  run <- suppressWarnings(routineqc::run_qc_adapter(adapted))
  testthat::expect_true('age_group_code' %in% names(run$data_flagged))
})

testthat::test_that('the first-ANC profile and stored schema 6 configurations validate', {
  anc <- routineqc::qc_config('anc_first_visit')
  testthat::expect_equal(anc$attendance$coverage_low, 0.85)
  testthat::expect_equal(anc$attendance$coverage_min_attending, 20)
  testthat::expect_identical(anc$priority$positive_assumed_zero, 'low')
  old <- routineqc::qc_config()
  old$schema_version <- 6L
  old$priority[routineqc:::.age_band_priority_keys] <- NULL
  testthat::expect_invisible(routineqc:::.validate_qc_config(old, stored = TRUE))
})

testthat::test_that('the QC guide lists the band flags', {
  catalog <- routineqc:::.qc_flag_catalog(routineqc::qc_config('anc_first_visit'))
  keys <- c('attendance_sum_mismatch', 'band_sum_partial_tested', 'band_sum_partial_positive',
            'band_sum_partial_attending', 'band_metadata_inconsistent', 'positive_assumed_zero')
  testthat::expect_true(all(keys %in% catalog$key))
  testthat::expect_false(any(catalog$authorizes_exclusion[catalog$key %in% keys]))
})
