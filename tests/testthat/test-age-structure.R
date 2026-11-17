age_structure_fixture <- function(months = 18) {
  m <- seq.Date(as.Date('2022-01-01'), by = 'month', length.out = months)
  young <- tibble::tibble(facility_id = 'F1', month_date = m, age_group_code = 'lt20',
                          attending = 40, anc_before_12wk = 12, tested = 40, positive = 8)
  old <- tibble::tibble(facility_id = 'F1', month_date = m, age_group_code = 'ge20',
                        attending = 160, anc_before_12wk = 48, tested = 160, positive = 16)
  d <- dplyr::bind_rows(young, old)
  d$attendance_definition <- 'first ANC attendance'
  d$flag_core_invalid <- FALSE
  d[order(d$month_date, d$age_group_code), ]
}

at_month <- function(d, i, code) which(d$month_date == sort(unique(d$month_date))[i] & d$age_group_code == code)

testthat::test_that('age-structure checks are off without age bands', {
  data <- age_structure_fixture()
  data$age_group_code <- NULL
  out <- routineqc::add_age_structure_qc_flags(data)
  for (f in routineqc:::.age_structure_flags) testthat::expect_false(any(out[[f]]), info = f)
  testthat::expect_false(any(routineqc:::.age_structure_columns %in% names(out)))
})

testthat::test_that('a stable facility raises nothing', {
  out <- routineqc::add_age_structure_qc_flags(age_structure_fixture())
  for (f in routineqc:::.age_structure_flags) testthat::expect_false(any(out[[f]]), info = f)
  testthat::expect_equal(out$age_mix_share[1], 0.2)
  ready <- !is.na(out$age_mix_z)
  testthat::expect_true(all(out$month_date[ready] >= as.Date('2022-07-01')))
  testthat::expect_identical(out$age_mix_z[out$age_group_code == 'lt20'], out$age_mix_z[out$age_group_code == 'ge20'])
})

testthat::test_that('swaps, timing changes and positivity contrasts are flagged on both bands', {
  d <- age_structure_fixture()
  y <- at_month(d, 14, 'lt20'); o <- at_month(d, 14, 'ge20')
  for (col in c('attending', 'anc_before_12wk', 'tested', 'positive')) {
    held <- d[[col]][y]; d[[col]][y] <- d[[col]][o]; d[[col]][o] <- held
  }
  d$anc_before_12wk[at_month(d, 15, 'lt20')] <- 36
  d$anc_before_12wk[at_month(d, 15, 'ge20')] <- 144
  d$positive[at_month(d, 16, 'lt20')] <- 36
  out <- routineqc::add_age_structure_qc_flags(d)
  month <- function(i) out$month_date == sort(unique(out$month_date))[i]
  testthat::expect_true(all(out$flag_age_mix_shift[month(14)] & out$flag_age_bands_swapped[month(14)]))
  testthat::expect_true(all(out$flag_booking_timing_shift[month(15)]))
  testthat::expect_true(all(out$flag_age_positivity_contrast[month(16)]))
  testthat::expect_false(any(out$flag_age_mix_shift[!month(14)]))
  testthat::expect_false(any(out$flag_booking_timing_shift[!month(15)]))
  testthat::expect_false(any(out$flag_age_bands_swapped[!month(14)]))
})

testthat::test_that('small months are not judged and a missing band is flagged', {
  d <- age_structure_fixture()
  y <- at_month(d, 14, 'lt20'); o <- at_month(d, 14, 'ge20')
  d$attending[c(y, o)] <- c(9, 1)
  d$anc_before_12wk[c(y, o)] <- c(3, 0)
  d <- d[-at_month(d, 16, 'ge20'), ]
  out <- routineqc::add_age_structure_qc_flags(d)
  testthat::expect_false(any(out$flag_age_mix_shift))
  lone <- out$month_date == sort(unique(out$month_date))[16]
  testthat::expect_true(all(out$flag_age_band_missing[lone]))
  testthat::expect_equal(sum(out$flag_age_band_missing), 1L)
  testthat::expect_true(is.na(out$age_mix_share[lone]))
  strict <- routineqc::add_age_structure_qc_flags(d, min_bookings = 5)
  testthat::expect_true(all(strict$flag_age_mix_shift[out$month_date == sort(unique(out$month_date))[14]]))
})

testthat::test_that('a centred baseline uses later months too', {
  d <- age_structure_fixture()
  d$attending[at_month(d, 2, 'lt20')] <- 160
  d$attending[at_month(d, 2, 'ge20')] <- 40
  trailing <- routineqc::add_age_structure_qc_flags(d)
  centred <- routineqc::add_age_structure_qc_flags(d, baseline_mode = 'centered')
  testthat::expect_false(any(trailing$flag_age_mix_shift))
  testthat::expect_true(any(centred$flag_age_mix_shift))
})

testthat::test_that('the pipeline finds swapped bands and omits nothing for them', {
  data <- routineqc::simulate_qc_age_data(n_regions = 3, n_months = 48, seed = 5)
  run <- suppressWarnings(routineqc::run_routine_qc(
    data, facility_var = 'facility_id', region_var = 'region', month_var = 'month_date',
    tested_var = 'tested', positive_var = 'positive', attending_var = 'attending',
    attendance_definition = 'first ANC attendance', district_var = 'district',
    age_group_var = 'age_group', config = routineqc::qc_config('anc_first_visit')
  ))
  x <- run$data_flagged
  testthat::expect_gt(mean(x$flag_age_bands_swapped[x$injected_swap]), 0.8)
  testthat::expect_false(any(x$flag_age_mix_shift & !x$injected_swap))
  testthat::expect_true('review_age_structure' %in% x$qc_action)
  testthat::expect_true(all(x$review_priority[x$flag_age_bands_swapped] %in% c('critical', 'high')))
  age_only <- x$qc_action == 'review_age_structure'
  very <- routineqc::apply_qc_scenario(x, routineqc::qc_scenario_presets()$very_strict, run$config$priority)
  testthat::expect_false(any(very$scn_omit_tested[age_only] | very$scn_omit_positive[age_only] |
                               very$scn_omit_attending[age_only] | very$scn_omit_coverage[age_only]))
  summary <- routineqc::summarise_age_band_deviations(x)
  testthat::expect_setequal(unique(summary$quantiles$measure), c('age_mix', 'booking_timing', 'positivity_contrast'))
  testthat::expect_false(any(c('facility_id', 'month_date', 'region', 'district') %in%
                               c(names(summary$quantiles), names(summary$exceedance))))
  testthat::expect_true(all(summary$exceedance$share_flagged >= 0 & summary$exceedance$share_flagged <= 1))
})

testthat::test_that('age-structure settings are configured and validated', {
  config <- routineqc::qc_config(age_structure = list(mix_z_threshold = 6))
  testthat::expect_equal(config$age_structure$mix_z_threshold, 6)
  testthat::expect_error(routineqc::qc_config(age_structure = list(old_band = 'lt20')), 'two different')
  testthat::expect_error(routineqc::qc_config(age_structure = list(mix_min_shift = 2)), 'at most 1')
  testthat::expect_error(routineqc::qc_config(age_structure = list(nope = 1)), 'Unknown')
  old <- routineqc::qc_config()
  old$schema_version <- 8L
  old$age_structure <- NULL
  old$priority[routineqc:::.age_structure_priority_keys] <- NULL
  testthat::expect_invisible(routineqc:::.validate_qc_config(old, stored = TRUE))
  catalog <- routineqc:::.qc_flag_catalog(config)
  testthat::expect_true(all(c('age_mix_shift', 'age_bands_swapped', 'booking_timing_shift',
                              'age_positivity_contrast', 'age_band_missing') %in% catalog$key))
  testthat::expect_match(catalog$definition[catalog$key == 'age_mix_shift'], 'above 6')
})

testthat::test_that('booking-timing flags say whether after-12-week bookings are zero or swapped', {
  d <- age_structure_fixture(24)
  set_before <- function(d, i, young, old) {
    d$anc_before_12wk[at_month(d, i, 'lt20')] <- young
    d$anc_before_12wk[at_month(d, i, 'ge20')] <- old
    d
  }
  d <- set_before(d, 13, 28, 112)   # 70% early against a usual 30%: the columns look swapped
  d <- set_before(d, 16, 40, 160)   # every booking early: none after 12 weeks
  d <- set_before(d, 19, 26, 104)   # 65% early: a shift that is neither
  out <- routineqc::add_age_structure_qc_flags(d)
  month <- function(i) out$month_date == sort(unique(out$month_date))[i]
  testthat::expect_true(all(out$flag_booking_timing_shift[month(13) | month(16)]))
  testthat::expect_true(all(out$flag_booking_columns_swapped[month(13)]))
  testthat::expect_false(any(out$flag_booking_after_zero[month(13)]))
  testthat::expect_true(all(out$flag_booking_after_zero[month(16)]))
  testthat::expect_false(any(out$flag_booking_columns_swapped[month(16)]))
  testthat::expect_false(any((out$flag_booking_after_zero | out$flag_booking_columns_swapped) &
                               !out$flag_booking_timing_shift))
  generic <- out$flag_booking_timing_shift & !out$flag_booking_after_zero & !out$flag_booking_columns_swapped
  if (any(generic)) testthat::expect_true(all(month(19)[generic]))
  acted <- routineqc::assign_qc_action(out)
  testthat::expect_true(all(grepl('booking columns look swapped', acted$qc_reason[month(13)])))
  testthat::expect_true(all(grepl('after 12 weeks reported as zero', acted$qc_reason[month(16)])))
  testthat::expect_false(any(grepl('differs from the facility', acted$qc_reason[month(13) | month(16)])))
  testthat::expect_true(all(acted$review_priority[month(13) | month(16)] == 'low'))
})

testthat::test_that('positivity outliers are classed by the other age band', {
  data <- routineqc::simulate_qc_age_data(n_regions = 3, councils_per_region = 3, facilities_per_council = 6,
                                          n_months = 48, outbreak_fraction = 0.03, outbreak_log_odds = 2.5,
                                          seed = 7)
  run <- suppressWarnings(routineqc::run_routine_qc(
    data, facility_var = 'facility_id', region_var = 'region', month_var = 'month_date',
    tested_var = 'tested', positive_var = 'positive', attending_var = 'attending',
    attendance_definition = 'first ANC attendance', district_var = 'district',
    age_group_var = 'age_group', config = routineqc::qc_config('anc_first_visit')
  ))
  x <- run$data_flagged
  e <- x$flag_resid_extreme
  # Context only: set on outliers, never on their own, and at most one.
  agree <- x$flag_resid_bands_agree; normal <- x$flag_resid_other_band_normal
  unclear <- x$flag_resid_other_band_inconclusive
  testthat::expect_false(any((agree | normal | unclear) & !e))
  testthat::expect_true(all(agree + normal + unclear <= 1))
  errors <- e & x$injected_error & !x$injected_outbreak
  outbreaks <- e & x$injected_outbreak & !x$injected_error
  testthat::expect_gt(sum(errors), 10)
  # An error in one band leaves the other as expected, or too small to tell.
  testthat::expect_gt(mean((normal | unclear)[errors]), 0.8)
  testthat::expect_lt(mean(agree[errors]), 0.1)
  testthat::expect_gt(sum(outbreaks), 5)
  testthat::expect_gt(mean(x$flag_resid_bands_agree[outbreaks]), 0.5)
  # Labels do not change priority: an agreeing outlier keeps its residual priority.
  testthat::expect_true(all(x$review_priority[x$flag_resid_bands_agree] %in% c('critical', 'high')))
  testthat::expect_true(any(grepl('other age band also off in the same direction', x$qc_reason)))
  s <- routineqc::summarise_residual_agreement(x)
  testthat::expect_identical(s$by_class$class, c('other_normal', 'both_agree', 'other_inconclusive', 'opposite', 'other_not_assessed'))
  testthat::expect_equal(sum(s$by_class$outlier_rows), sum(e & !is.na(x$pearson_resid_scaled)))
  testthat::expect_equal(sum(s$by_year$outlier_rows), sum(s$by_class$outlier_rows))
  ids <- c('facility_id', 'month_date', 'region', 'district')
  testthat::expect_false(any(ids %in% unlist(lapply(s, names))))
  shared <- s$by_class$share_shared_in_area
  testthat::expect_gt(shared[2], shared[1])
})

testthat::test_that('a small other band is inconclusive, not as expected', {
  x <- tibble::tibble(
    facility_id = 'F1', month_date = as.Date('2024-01-01'), age_group_code = c('lt20', 'ge20'),
    tested = c(40, 3), positive = c(8, 0), p_hat = 0.05, resid_dispersion = 1,
    pearson_resid = c(4.4, -0.4), pearson_resid_scaled = c(4.4, -0.4), flag_resid_extreme = c(TRUE, FALSE)
  )
  out <- routineqc:::.add_residual_agreement(x, 'lt20', 'ge20')
  testthat::expect_true(out$flag_resid_other_band_inconclusive[1])
  testthat::expect_false(out$flag_resid_other_band_normal[1])
  x$tested[2] <- 60
  out <- routineqc:::.add_residual_agreement(x, 'lt20', 'ge20')
  testthat::expect_true(out$flag_resid_other_band_normal[1])
})
