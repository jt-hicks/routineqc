imputation_fixture <- local({
  cache <- NULL
  function() {
    if (is.null(cache)) {
      src <- routineqc::simulate_qc_data(n_facilities = 24, n_months = 36, seed = 7)
      cache <<- suppressWarnings(routineqc::run_routine_qc(
        src, facility_var = 'facility', region_var = 'region', month_var = 'month',
        tested_var = 'tested', positive_var = 'positive', nthreads = 1
      ))$data_flagged
    }
    cache
  }
})

imp_scenario <- function() routineqc::qc_scenario('imp', omit_through = 'medium')

testthat::test_that('imputation is reproducible for a seed and differs across seeds', {
  d <- imputation_fixture()
  a <- routineqc::qc_impute_scenario(d, imp_scenario(), n_imputations = 4, seed = 11)
  b <- routineqc::qc_impute_scenario(d, imp_scenario(), n_imputations = 4, seed = 11)
  c2 <- routineqc::qc_impute_scenario(d, imp_scenario(), n_imputations = 4, seed = 12)
  testthat::expect_identical(a$tested_draws, b$tested_draws)
  testthat::expect_equal(a$provenance$seed, 11)
  testthat::expect_true(sum(a$targets$tested_status == 'imputed', na.rm = TRUE) > 0)
  testthat::expect_false(identical(a$tested_draws, c2$tested_draws))
})

testthat::test_that('imputed counts are nonnegative integers with positive <= tested', {
  imp <- routineqc::qc_impute_scenario(imputation_fixture(), imp_scenario(), n_imputations = 4, seed = 3)
  for (m in seq_len(4)) {
    f <- routineqc:::.imputed_frame(imp, m)
    ok <- !is.na(f$tested) & !is.na(f$positive)
    testthat::expect_true(all(f$tested[!is.na(f$tested)] >= 0))
    testthat::expect_true(all(f$tested == round(f$tested), na.rm = TRUE))
    testthat::expect_true(all(f$positive[ok] <= f$tested[ok]))
  }
})

testthat::test_that('retained observations are never changed and raw data are untouched', {
  d <- imputation_fixture()
  before <- d
  imp <- routineqc::qc_impute_scenario(d, imp_scenario(), n_imputations = 3, seed = 3)
  testthat::expect_identical(d, before)
  filtered <- routineqc:::.scenario_frame(imp$scn, 'filtered')
  completed <- routineqc:::.imputed_frame(imp, 2)
  untouched <- !filtered$tested_imputed & !completed$tested_imputed
  testthat::expect_identical(completed$tested[untouched], filtered$tested[untouched])
  testthat::expect_identical(completed$positive[!completed$positive_imputed],
                             filtered$positive[!completed$positive_imputed])
})

testthat::test_that('expected-value mode is deterministic and carries no interval', {
  d <- imputation_fixture()
  a <- routineqc::qc_impute_scenario(d, imp_scenario(), method = 'expected', seed = 1)
  b <- routineqc::qc_impute_scenario(d, imp_scenario(), method = 'expected', seed = 99)
  testthat::expect_identical(a$tested_draws, b$tested_draws)
  out <- routineqc::summarise_qc_imputation(a, 'national', FALSE)
  testthat::expect_true(is.na(out$ci_low))
  testthat::expect_identical(out$uncertainty, 'exploratory_expected_value')
})

testthat::test_that('imputed aggregates retain tested and attendance fields required by charts', {
  imp <- routineqc::qc_impute_scenario(imputation_fixture(), imp_scenario(), method = 'expected')
  out <- routineqc::summarise_qc_imputation(imp)
  expected <- routineqc::qc_aggregate_metrics(routineqc:::.imputed_frame(imp), 'national', monthly = TRUE)
  for (field in c('tested_total', 'tested_rows', 'attendance_total', 'attendance_rows')) {
    testthat::expect_equal(out[[field]], expected[[field]], info = field)
  }
})

testthat::test_that('sparse history leaves targets missing with a reason', {
  d <- imputation_fixture()
  d <- d[d$facility_id %in% unique(d$facility_id)[1:2], ]
  imp <- routineqc::qc_impute_scenario(d, imp_scenario(), n_imputations = 2, seed = 1,
                                       min_train_rows = 10000)
  tg <- imp$targets
  if (nrow(tg) > 0L) {
    testthat::expect_true(all(tg$tested_status[tg$need_tested] == 'insufficient_history'))
    testthat::expect_true(all(is.na(imp$tested_draws[tg$need_tested, ])))
  }
})

testthat::test_that('inactive months are not imputed and absent reports need fill_preexisting', {
  d <- imputation_fixture()
  keep <- !(d$facility_id == unique(d$facility_id)[1] & d$month_date == sort(unique(d$month_date))[10])
  d <- d[keep, ]
  off <- routineqc::qc_impute_scenario(d, imp_scenario(), n_imputations = 2, seed = 1)
  testthat::expect_false(any(off$targets$absent_report))
  on <- routineqc::qc_impute_scenario(
    d, routineqc::qc_scenario('imp', omit_through = 'medium', fill_preexisting = TRUE),
    n_imputations = 2, seed = 1
  )
  testthat::expect_true(any(on$targets$absent_report))
  sched <- data.frame(facility_id = unique(d$facility_id)[1],
                      first_active_month = as.Date('2022-01-01'),
                      last_active_month = as.Date('2022-06-01'))
  closed <- routineqc::qc_impute_scenario(
    d, routineqc::qc_scenario('imp', omit_through = 'medium', fill_preexisting = TRUE),
    n_imputations = 2, seed = 1, schedule = sched
  )
  fac <- closed$targets[closed$targets$facility_id == sched$facility_id, ]
  testthat::expect_true(all(fac$month_date <= as.Date('2022-06-01')))
})

testthat::test_that('Rubin combination follows the documented formulas', {
  r <- routineqc:::.rubin(c(0.10, 0.12, 0.14, 0.12), rep(0.0001, 4))
  b <- var(c(0.10, 0.12, 0.14, 0.12))
  testthat::expect_equal(r$q, 0.12)
  testthat::expect_equal(r$se^2, 0.0001 + (1 + 1 / 4) * b)
  testthat::expect_true(r$lo < r$q && r$hi > r$q)
  testthat::expect_true(is.na(routineqc:::.rubin(0.1, 0.01)$se))
})

testthat::test_that('imputed results report provenance and the imputation share', {
  d <- imputation_fixture()
  imp <- routineqc::qc_impute_scenario(d, imp_scenario(), n_imputations = 3, seed = 5)
  res <- routineqc::qc_scenario_results(d, imp_scenario(), 'national', FALSE, imputation = imp)
  testthat::expect_setequal(unique(res$version), c('reported', 'filtered_missing', 'filtered_imputed'))
  prov <- attr(res, 'scenario_provenance')
  testthat::expect_identical(prov$imputation$model_version, 'count-imputation-1')
  imputed <- res[res$version == 'filtered_imputed', ]
  testthat::expect_true(imputed$prop_tested_from_imputation >= 0 && imputed$prop_tested_from_imputation <= 1)
})
