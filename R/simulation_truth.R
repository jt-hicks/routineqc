# Synthetic data with known truth, for checking how well QC scenarios and
# imputation recover aggregate results.
#
# Unlike `simulate_qc_data()` (which assigns geography per row), facilities here
# sit inside a fixed region > council hierarchy, so council, regional and
# national truth is well defined. Truth is the complete, error-free count set;
# the observed data are the truth after injected missingness and recording
# errors. A good result here shows the machinery works when the generating
# assumptions hold. It does not establish validity for real data, where the
# missingness and error mechanisms are unknown.

.truth_missing_mechanisms <- c('none', 'isolated', 'consecutive', 'whole_facility',
                               'volume_related')

#' Simulate Facility-Month Data With Known Truth
#'
#' @param n_regions,councils_per_region,facilities_per_council Hierarchy size.
#' @param n_months Number of months.
#' @param start_month First month.
#' @param missing Missingness mechanisms to apply, any of `"none"`,
#'   `"isolated"` (independent facility-months), `"consecutive"` (blocks of
#'   3-6 months), `"whole_facility"` (facilities that never report counts) and
#'   `"volume_related"` (informative: reports are more likely lost where
#'   positivity is high and volume low).
#' @param missing_rate Approximate share of facility-months affected by each
#'   mechanism.
#' @param error_rate Share of reported facility-months given a recording error:
#'   a positives outlier, an inflated tested count, or a collapsed tested
#'   count.
#' @param abrupt_change Add a genuine abrupt rise in positivity in one region
#'   (not an error) so that QC can be checked against real change.
#' @param change_logit Size of the rise on the logit scale.
#' @param seed Random seed.
#'
#' @return A list with `truth` (complete true counts), `observed` (data to pass
#'   to [run_routine_qc()]; columns `facility`, `region`, `council`, `month`,
#'   `tested`, `positive`, `attending`), `injected` (injected errors) and
#'   `settings`.
#' @export
simulate_qc_truth <- function(n_regions = 3, councils_per_region = 3,
                              facilities_per_council = 5, n_months = 36,
                              start_month = '2021-01-01',
                              missing = 'isolated', missing_rate = 0.06,
                              error_rate = 0.03, abrupt_change = TRUE,
                              change_logit = 1.0, seed = 1) {
  missing <- match.arg(missing, .truth_missing_mechanisms, several.ok = TRUE)
  if (!is.numeric(missing_rate) || missing_rate < 0 || missing_rate > 0.5) {
    rlang::abort('`missing_rate` must be between 0 and 0.5.')
  }
  if (!is.numeric(error_rate) || error_rate < 0 || error_rate > 0.3) {
    rlang::abort('`error_rate` must be between 0 and 0.3.')
  }
  set.seed(seed)
  months <- seq(as.Date(start_month), by = 'month', length.out = n_months)
  hier <- tidyr::expand_grid(r = seq_len(n_regions), c = seq_len(councils_per_region),
                             f = seq_len(facilities_per_council))
  hier$region <- sprintf('R%02d', hier$r)
  hier$council <- sprintf('R%02d_C%02d', hier$r, hier$c)
  hier$facility <- sprintf('R%02d_C%02d_F%02d', hier$r, hier$c, hier$f)
  hier$log_volume <- stats::rnorm(nrow(hier), log(90), 0.6)
  hier$fac_effect <- stats::rnorm(nrow(hier), 0, 0.25)
  hier$rate <- stats::runif(nrow(hier), 0.7, 0.98)
  council_effect <- stats::setNames(stats::rnorm(n_regions * councils_per_region, 0, 0.4),
                                    unique(hier$council))
  region_effect <- stats::setNames(stats::rnorm(n_regions, -1.2, 0.3), unique(hier$region))

  truth <- tidyr::expand_grid(facility = hier$facility, month_date = months)
  truth <- dplyr::left_join(truth, hier[c('facility', 'region', 'council', 'log_volume', 'fac_effect', 'rate')],
                            by = 'facility')
  t_index <- match(truth$month_date, months) - 1L
  season <- sin(2 * pi * (lubridate::month(truth$month_date) - 1) / 12)
  change_region <- unique(hier$region)[1]
  change_start <- months[ceiling(n_months * 0.6)]
  truth$genuine_change <- abrupt_change & truth$region == change_region &
    truth$month_date >= change_start
  logit_p <- region_effect[truth$region] + council_effect[truth$council] + truth$fac_effect +
    0.5 * season + ifelse(truth$genuine_change, change_logit, 0)
  truth$true_positivity <- stats::plogis(logit_p)
  mu <- exp(truth$log_volume + 0.2 * season + 0.004 * t_index)
  truth$tested <- stats::rnbinom(nrow(truth), mu = mu, size = 20)
  truth$positive <- stats::rbinom(nrow(truth), truth$tested, truth$true_positivity)
  truth$attending <- truth$tested + stats::rpois(nrow(truth), pmax(1, truth$tested * (1 / truth$rate - 1)))
  truth$facility_id <- truth$facility
  truth <- truth[c('facility_id', 'region', 'council', 'month_date', 'tested', 'positive',
                   'attending', 'true_positivity', 'genuine_change')]

  n <- nrow(truth)
  lost <- rep(FALSE, n)
  mechanism <- rep(NA_character_, n)
  mark <- function(idx, name) {
    new <- idx & !lost
    lost <<- lost | idx
    mechanism[new] <<- name
  }
  if ('isolated' %in% missing) mark(stats::runif(n) < missing_rate, 'isolated')
  if ('consecutive' %in% missing) {
    facilities <- unique(truth$facility_id)
    pick <- facilities[stats::runif(length(facilities)) < min(1, missing_rate * 5)]
    for (fac in pick) {
      len <- sample(3:6, 1)
      start <- sample(seq_len(max(1, n_months - len)), 1)
      mark(truth$facility_id == fac & match(truth$month_date, months) %in% (start + 0:(len - 1)),
           'consecutive')
    }
  }
  if ('whole_facility' %in% missing) {
    facilities <- unique(truth$facility_id)
    pick <- sample(facilities, max(1, round(length(facilities) * missing_rate)))
    mark(truth$facility_id %in% pick, 'whole_facility')
  }
  if ('volume_related' %in% missing) {
    z_pos <- as.numeric(scale(truth$true_positivity))
    z_vol <- as.numeric(scale(log1p(truth$tested)))
    shift <- stats::uniroot(function(a) mean(stats::plogis(a + 1.2 * z_pos - 0.8 * z_vol)) - missing_rate,
                            c(-12, 4))$root
    mark(stats::runif(n) < stats::plogis(shift + 1.2 * z_pos - 0.8 * z_vol), 'volume_related')
  }

  observed <- dplyr::tibble(
    facility = truth$facility_id, region = truth$region, council = truth$council,
    month = truth$month_date, tested = as.numeric(truth$tested),
    positive = as.numeric(truth$positive), attending = as.numeric(truth$attending)
  )
  observed[lost, c('tested', 'positive')] <- NA_real_

  reported <- which(!lost)
  err <- reported[stats::runif(length(reported)) < error_rate]
  error_type <- rep(NA_character_, n)
  if (length(err) > 0L) {
    kind <- sample(c('positive_outlier', 'tested_spike', 'tested_collapse'), length(err), replace = TRUE)
    error_type[err] <- kind
    po <- err[kind == 'positive_outlier']
    observed$positive[po] <- round(observed$tested[po] * stats::runif(length(po), 0.7, 0.95))
    ts <- err[kind == 'tested_spike']
    observed$tested[ts] <- observed$tested[ts] * 10
    observed$positive[ts] <- pmin(observed$positive[ts], observed$tested[ts])
    tc <- err[kind == 'tested_collapse']
    observed$tested[tc] <- pmax(observed$positive[tc], round(observed$tested[tc] * 0.05))
  }
  injected <- dplyr::tibble(facility_id = truth$facility_id, month_date = truth$month_date,
                            error_type = error_type, missing_mechanism = mechanism)
  list(
    truth = truth, observed = observed, injected = injected,
    settings = list(n_regions = n_regions, councils_per_region = councils_per_region,
                    facilities_per_council = facilities_per_council, n_months = n_months,
                    missing = missing, missing_rate = missing_rate, error_rate = error_rate,
                    abrupt_change = abrupt_change, change_logit = change_logit, seed = seed,
                    change_region = change_region, change_start = change_start)
  )
}

.truth_frame <- function(sim) {
  dplyr::tibble(
    facility_id = sim$truth$facility_id, region = sim$truth$region, council = sim$truth$council,
    district = NA_character_, month_date = sim$truth$month_date,
    tested = as.numeric(sim$truth$tested), positive = as.numeric(sim$truth$positive),
    attending = as.numeric(sim$truth$attending),
    coverage_ok = TRUE, tested_imputed = FALSE, positive_imputed = FALSE
  )
}

.with_unit <- function(x, level) {
  x$unit <- if (level == 'national') 'national' else as.character(x[[level]])
  x
}

#' Evaluate Recovery of Known Aggregates
#'
#' Runs QC on the observed synthetic data, applies each scenario (with and
#' without imputation) and compares council, regional and national positivity
#' for the whole period with the truth. Reports:
#' * bias and error (RMSE, MAE) of the estimate, by scenario version;
#' * coverage and width of the imputation interval (multiple imputation only);
#' * retention: the share of reported tested counts a scenario keeps, and the
#'   share of units with no estimate;
#' * error detection: the share of injected errors omitted, and the share of
#'   clean reported rows wrongly omitted;
#' * survival of genuine change: how many true-change rows are omitted, and
#'   the estimated versus true change in positivity.
#'
#' These are properties of the simulator's assumptions. They do not establish
#' that a scenario is valid for real data, where the missingness and error
#' mechanisms are unknown; informative missingness in particular can bias
#' every version. See [evaluate_qc_recovery_grid()] for a comparison across
#' missingness mechanisms.
#'
#' @param sim A [simulate_qc_truth()] result.
#' @param scenarios A list of [qc_scenario()] objects.
#' @param levels Geographical levels to evaluate.
#' @param impute Whether to include imputed versions.
#' @param n_imputations Number of imputations.
#' @param seed Imputation seed.
#' @param ... Passed to [qc_impute_scenario()].
#'
#' @return A list with `units` (one row per scenario, version, level and unit),
#'   `summary`, `detection`, `change` and `settings`.
#' @export
evaluate_qc_recovery <- function(sim, scenarios = qc_scenario_presets(),
                                 levels = c('council', 'region', 'national'),
                                 impute = TRUE, n_imputations = 10L, seed = 1L, ...) {
  levels <- match.arg(levels, c('national', 'region', 'council'), several.ok = TRUE)
  if (inherits(scenarios, 'qc_scenario')) scenarios <- list(scenarios)
  qc <- suppressWarnings(run_routine_qc(
    sim$observed, facility_var = 'facility', region_var = 'region', month_var = 'month',
    tested_var = 'tested', positive_var = 'positive', attending_var = 'attending',
    attendance_definition = 'all outpatient attendances', council_var = 'council', nthreads = 1
  ))
  flagged <- qc$data_flagged
  truth_frame <- .truth_frame(sim)
  truth_by_level <- lapply(stats::setNames(levels, levels), function(lv) {
    out <- .with_unit(qc_aggregate_metrics(truth_frame, lv, monthly = FALSE), lv)
    out[c('unit', 'positivity')]
  })

  units <- list()
  detection <- list()
  change <- list()
  for (scn in scenarios) {
    applied <- apply_qc_scenario(flagged, scn)
    imp <- if (impute) {
      tryCatch(qc_impute_scenario(flagged, scn, n_imputations = n_imputations, seed = seed, ...),
               error = function(e) NULL)
    }
    for (lv in levels) {
      res <- qc_scenario_results(flagged, scn, lv, monthly = FALSE, imputation = imp)
      res <- .with_unit(res, lv)
      res <- dplyr::left_join(res, setNames(truth_by_level[[lv]], c('unit', 'truth')), by = 'unit')
      res$scenario <- scn$name
      res$level <- lv
      res$error <- res$positivity - res$truth
      res$covered <- if ('ci_low' %in% names(res)) {
        ifelse(is.na(res$ci_low), NA, res$truth >= res$ci_low & res$truth <= res$ci_high)
      } else {
        NA
      }
      keep <- intersect(c('scenario', 'version', 'level', 'unit', 'truth', 'positivity', 'error',
                          'ci_low', 'ci_high', 'covered', 'tested_matched', 'pair_rows',
                          'prop_tested_from_imputation'), names(res))
      units[[length(units) + 1L]] <- res[keep]
    }
    detection[[length(detection) + 1L]] <- .detection_row(applied, sim, scn$name)
    change[[length(change) + 1L]] <- .change_row(applied, sim, scn$name)
  }
  units <- dplyr::bind_rows(units)
  if (!'covered' %in% names(units)) units$covered <- NA
  if (!'ci_low' %in% names(units)) units$ci_low <- NA_real_
  summary <- units %>%
    dplyr::group_by(scenario, version, level) %>%
    dplyr::summarise(
      units = dplyr::n(),
      units_without_estimate = sum(is.na(positivity)),
      bias = mean(error, na.rm = TRUE),
      rmse = sqrt(mean(error^2, na.rm = TRUE)),
      mae = mean(abs(error), na.rm = TRUE),
      interval_coverage = if (all(is.na(covered))) NA_real_ else mean(covered, na.rm = TRUE),
      mean_interval_width = if (all(is.na(ci_low))) NA_real_ else mean(ci_high - ci_low, na.rm = TRUE),
      .groups = 'drop'
    )
  list(units = units, summary = summary, detection = dplyr::bind_rows(detection),
       change = dplyr::bind_rows(change),
       settings = list(sim = sim$settings, n_imputations = n_imputations, seed = seed,
                       note = paste('Recovery under simulator assumptions only; not evidence of',
                                    'validity on real data.')))
}

.detection_row <- function(applied, sim, name) {
  key <- paste(applied$facility_id, applied$month_date)
  inj <- sim$injected[match(key, paste(sim$injected$facility_id, sim$injected$month_date)), ]
  omitted <- !applied$scn_facility_eligible | applied$scn_omit_tested | applied$scn_omit_positive
  counts <- .baseline_counts(applied)
  reported <- !is.na(counts$tested)
  err <- reported & !is.na(inj$error_type)
  clean <- reported & is.na(inj$error_type)
  frame <- .scenario_frame(applied, 'reported')
  filtered <- .scenario_frame(applied, 'filtered')
  dplyr::tibble(
    scenario = name,
    injected_errors = sum(err),
    errors_omitted = mean(omitted[err]),
    clean_rows = sum(clean),
    clean_omitted = mean(omitted[clean]),
    tested_retained = sum(filtered$tested, na.rm = TRUE) / sum(frame$tested, na.rm = TRUE)
  )
}

.change_row <- function(applied, sim, name) {
  s <- sim$settings
  if (!isTRUE(s$abrupt_change)) return(dplyr::tibble())
  key <- paste(applied$facility_id, applied$month_date)
  truth_row <- sim$truth[match(key, paste(sim$truth$facility_id, sim$truth$month_date)), ]
  affected <- applied$region == s$change_region
  post <- applied$month_date >= s$change_start
  omitted <- !applied$scn_facility_eligible | applied$scn_omit_tested | applied$scn_omit_positive
  counts <- .baseline_counts(applied)
  reported <- !is.na(counts$tested) & !is.na(counts$positive)
  filtered <- .scenario_frame(applied, 'filtered')
  est <- function(frame, rows) {
    ok <- rows & !is.na(frame$tested) & !is.na(frame$positive) & frame$positive <= frame$tested
    sum(frame$positive[ok]) / sum(frame$tested[ok])
  }
  tf <- .truth_frame(sim)
  tf_aff <- tf$region == s$change_region
  tf_post <- tf$month_date >= s$change_start
  est_change <- est(filtered, affected & post) - est(filtered, affected & !post)
  true_change <- est(tf, tf_aff & tf_post) - est(tf, tf_aff & !tf_post)
  dplyr::tibble(
    scenario = name,
    change_rows_reported = sum(affected & post & reported),
    change_rows_omitted = mean(omitted[affected & post & reported]),
    other_rows_omitted = mean(omitted[!(affected & post) & reported]),
    true_change = true_change, estimated_change = est_change,
    change_error = est_change - true_change
  )
}

#' Evaluate Recovery Across Missingness Mechanisms
#'
#' Repeats [evaluate_qc_recovery()] under each missingness mechanism, with the
#' same seed and hierarchy, to show how far results depend on how data were lost.
#' The `volume_related` mechanism is informative (missingness depends on the
#' outcome), which no scenario here corrects for; compare its bias with the
#' ignorable mechanisms.
#'
#' @param mechanisms Mechanisms from [simulate_qc_truth()].
#' @param sim_args Named list of further [simulate_qc_truth()] arguments.
#' @param ... Passed to [evaluate_qc_recovery()].
#'
#' @return A list of the stacked `summary`, `detection` and `change` tables with
#'   a `mechanism` column.
#' @export
evaluate_qc_recovery_grid <- function(mechanisms = c('isolated', 'consecutive',
                                                     'whole_facility', 'volume_related'),
                                      sim_args = list(), ...) {
  mechanisms <- match.arg(mechanisms, .truth_missing_mechanisms, several.ok = TRUE)
  runs <- lapply(mechanisms, function(mech) {
    sim <- do.call(simulate_qc_truth, c(sim_args, list(missing = mech)))
    evaluate_qc_recovery(sim, ...)
  })
  stack <- function(part) {
    dplyr::bind_rows(Map(function(x, m) {
      tbl <- x[[part]]
      if (nrow(tbl) > 0L) tbl$mechanism <- m
      tbl
    }, runs, mechanisms))
  }
  list(summary = stack('summary'), detection = stack('detection'), change = stack('change'))
}
