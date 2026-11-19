# Analytical scenarios: what happens to aggregate estimates when QC decisions
# are applied. Everything here is a pure function of a flagged data frame and a
# scenario definition. Nothing is written back to the data, to a stored action,
# or to a saved run: a scenario only produces new `scn_*` columns and results.
#
# Three things are kept separate, on purpose:
#   * view filters   - which results are displayed (applied after calculation);
#   * facility eligibility - which facilities enter the analytical cohort;
#   * observation handling - which metric contributions are retained, omitted
#     or imputed.
# The reporting-consistency strictness levels are review cohorts and are not
# used here; eligibility below has its own, explicitly monotone criteria.

# Which count(s) a flag puts in doubt. A problem confined to attendance does not
# discard tested or positive; `coverage` omits only the testing-coverage pair.
.qc_flag_targets <- list(
  core_invalid = c('tested', 'positive'),
  positive_gt_tested = c('tested', 'positive'),
  zero_tested_positive = c('tested', 'positive'),
  tested_negative = c('tested', 'positive'),
  positive_negative = 'positive',
  all_negative_large_n = 'positive',
  all_positive_large_n = 'positive',
  resid_extreme = 'positive',
  large_monthly_prevalence_change = 'positive',
  tested_extreme_high = 'tested',
  tested_extreme_low = 'tested',
  tested_zero_unusual = 'tested',
  tested_large_jump = 'tested',
  tested_large_drop = 'tested',
  attending_negative = 'attending',
  tested_gt_attending = 'coverage',
  tested_gt_attending_major = 'coverage',
  # Low coverage is what a coverage estimate measures, not an error in it:
  # omitting those months would raise the estimate by selection. Review only.
  coverage_low = character(),
  coverage_very_low = character(),
  band_sum_partial_tested = 'tested',
  band_sum_partial_positive = 'positive',
  band_sum_partial_attending = 'attending',
  band_metadata_inconsistent = c('tested', 'positive'),
  attendance_sum_mismatch = 'attending'
)

# Age-structure flags put a band's counts in doubt but leave the facility's
# totals intact (a count in the wrong band is still in the total). They omit
# counts only from estimates by age band. A swap puts every count of both
# bands in doubt; an age-mix shift the bookings; a positivity contrast the
# positives. Booking timing and a missing band omit nothing: no estimate uses
# the timing split, and a missing band has no counts to omit.
.qc_band_flag_targets <- list(
  age_bands_swapped = c('tested', 'positive', 'attending'),
  age_mix_shift = 'attending',
  age_positivity_contrast = 'positive'
)

.qc_always_critical <- c(
  'core_invalid', 'positive_gt_tested', 'zero_tested_positive',
  'tested_negative', 'positive_negative'
)

# Observation-handling cutoffs, from least to most inclusive. `authorized`
# omits only what the stored policy authorizes; each later level adds the
# flags of one more priority, so the omitted sets are nested.
.qc_omit_levels <- c('none', 'authorized', 'critical', 'high', 'medium', 'low')

.flag_priority_rank <- function(key, priority) {
  level <- if (key %in% .qc_always_critical) {
    'critical'
  } else if (!is.null(priority[[key]])) {
    priority[[key]]
  } else {
    'low'
  }
  match(level, .qc_priority_levels)
}

#' Define an Analytical QC Scenario
#'
#' A scenario states which facilities enter the analytical cohort and how QC
#' flags are turned into omitted contributions. It is a hypothetical
#' analytical choice: it never changes a stored action or an observation.
#'
#' @param name Label for the scenario.
#' @param max_prop_without_testing,max_gap_months,min_months_with_testing
#'   Facility-eligibility criteria (`NA` = not applied). A facility is eligible
#'   when it meets every applied criterion, so stricter values always give a
#'   subset of the facilities. They use all available reports, not a display
#'   filter. Proportions and gaps use each facility's first-to-last reported
#'   window; the minimum tested-month count is absolute and can exclude a
#'   recently reporting facility.
#' @param omit_through How far QC flags are turned into omissions: `"none"`,
#'   `"authorized"` (what the stored policy authorizes), or flags up to
#'   priority `"critical"`, `"high"`, `"medium"` or `"low"`. Each level
#'   includes the previous one.
#' @param include_authorized Whether authorized exclusions are omitted when
#'   `omit_through` is not `"none"`.
#' @param handling `"missing"` leaves omitted contributions missing;
#'   `"impute_expected"` replaces them with expected values (exploratory, no
#'   uncertainty); `"impute_multiple"` uses repeated predictive imputation.
#' @param fill_preexisting Whether gaps that existed before QC (missing counts
#'   and absent reports) are also filled. `FALSE` isolates the effect of QC.
#'
#' @return A `qc_scenario` list.
#' @export
qc_scenario <- function(name = 'custom',
                        max_prop_without_testing = NA_real_,
                        max_gap_months = NA_real_,
                        min_months_with_testing = NA_real_,
                        omit_through = 'authorized',
                        include_authorized = TRUE,
                        handling = c('missing', 'impute_expected', 'impute_multiple'),
                        fill_preexisting = FALSE) {
  handling <- match.arg(handling)
  if (!is.character(name) || length(name) != 1L) rlang::abort('`name` must be one string.')
  if (!omit_through %in% .qc_omit_levels) {
    rlang::abort(paste0('`omit_through` must be one of: ', paste(.qc_omit_levels, collapse = ', '), '.'))
  }
  for (arg in c('max_prop_without_testing', 'max_gap_months', 'min_months_with_testing')) {
    value <- get(arg)
    if (length(value) != 1L || (!is.na(value) && (!is.numeric(value) || value < 0))) {
      rlang::abort(paste0('`', arg, '` must be one non-negative number or NA.'))
    }
  }
  structure(list(
    name = name,
    eligibility = list(
      max_prop_without_testing = as.numeric(max_prop_without_testing),
      max_gap_months = as.numeric(max_gap_months),
      min_months_with_testing = as.numeric(min_months_with_testing)
    ),
    observation = list(
      omit_through = omit_through, include_authorized = isTRUE(include_authorized)
    ),
    handling = list(method = handling, fill_preexisting = isTRUE(fill_preexisting))
  ), class = 'qc_scenario')
}

#' Analytical Scenario Presets
#'
#' Four presets from relaxed to strict. All facilities are included regardless
#' of reporting completeness. With handling settings held fixed, each preset
#' retains a subset of the count contributions of the one before. Flags omit
#' only their affected counts on the flagged facility-month.
#'
#' @param handling Handling method applied to every preset.
#' @param fill_preexisting Passed to [qc_scenario()].
#'
#' @return A named list of `qc_scenario` objects, ordered relaxed to strict.
#' @export
qc_scenario_presets <- function(handling = 'missing', fill_preexisting = FALSE) {
  make <- function(name, ...) qc_scenario(name, ..., handling = handling,
                                          fill_preexisting = fill_preexisting)
  list(
    relaxed = make('relaxed', omit_through = 'authorized'),
    moderate = make('moderate', omit_through = 'high'),
    strict = make('strict', omit_through = 'medium'),
    very_strict = make('very_strict', omit_through = 'low')
  )
}

#' Describe Scenario Presets
#'
#' @param presets Output of [qc_scenario_presets()].
#' @return A tibble with one row per preset and one column per rule.
#' @export
describe_qc_scenarios <- function(presets = qc_scenario_presets()) {
  dplyr::bind_rows(lapply(presets, function(s) {
    dplyr::tibble(
      scenario = s$name,
      omit_through = s$observation$omit_through,
      include_authorized = s$observation$include_authorized,
      max_prop_without_testing = s$eligibility$max_prop_without_testing,
      max_gap_months = s$eligibility$max_gap_months,
      min_months_with_testing = s$eligibility$min_months_with_testing,
      handling = s$handling$method,
      fill_preexisting = s$handling$fill_preexisting
    )
  }))
}

# --- contributions -----------------------------------------------------------

# Valid counts, with the structural cases kept apart. A genuine zero is a valid
# count; an invalid, negative or missing one is NA. Attendance is usable only
# with an approved definition.
.baseline_counts <- function(data) {
  usable <- .usable_counts(data)
  clean <- function(x) ifelse(!is.na(x) & x < 0, NA_real_, x)
  attending <- if ('attending' %in% names(data)) as.numeric(data$attending) else rep(NA_real_, nrow(data))
  if ('attendance_definition' %in% names(data)) {
    defined <- !is.na(data$attendance_definition) & trimws(as.character(data$attendance_definition)) != ''
    attending[!defined] <- NA_real_
  }
  attending[!is.na(attending) & attending < 0] <- NA_real_
  list(tested = clean(usable$tested), positive = clean(usable$positive), attending = attending)
}

# Flags that put each count in doubt, at the cutoff of the scenario. Estimates
# by age band also omit what the age-structure flags put in doubt.
.qc_omission_masks <- function(data, observation, priority = .default_qc_priority(),
                               by_age_band = FALSE) {
  n <- nrow(data)
  omit <- list(
    tested = rep(FALSE, n), positive = rep(FALSE, n),
    attending = rep(FALSE, n), coverage = rep(FALSE, n)
  )
  level <- observation$omit_through
  if (level == 'none') return(omit)
  priority <- utils::modifyList(.default_qc_priority(), priority)
  cutoff <- if (level == 'authorized') 0L else match(level, .qc_priority_levels)
  if (isTRUE(observation$include_authorized) && 'flag_exclude_authorized' %in% names(data)) {
    authorized <- dplyr::coalesce(as.logical(data$flag_exclude_authorized), FALSE)
    omit$tested <- omit$tested | authorized
    omit$positive <- omit$positive | authorized
  }
  targets <- c(.qc_flag_targets, if (isTRUE(by_age_band)) .qc_band_flag_targets)
  for (key in names(targets)) {
    column <- paste0('flag_', key)
    if (!column %in% names(data) || .flag_priority_rank(key, priority) > cutoff) next
    active <- dplyr::coalesce(as.logical(data[[column]]), FALSE)
    for (target in targets[[key]]) omit[[target]] <- omit[[target]] | active
  }
  omit
}

.check_by_age_band <- function(data, by_age_band) {
  if (!rlang::is_bool(by_age_band)) rlang::abort('`by_age_band` must be TRUE or FALSE.')
  if (by_age_band && !.qc_is_age_disaggregated(data)) {
    rlang::abort('Estimates by age band need age-disaggregated data (an `age_group_code` column).')
  }
  invisible(by_age_band)
}

# Facilities that meet the scenario's eligibility criteria, measured on the
# whole dataset (never on the current display filter).
.scenario_eligible_facilities <- function(data, eligibility) {
  facilities <- unique(as.character(data$facility_id))
  applied <- !vapply(eligibility, function(x) is.na(x), logical(1))
  if (!any(applied)) return(facilities)
  consistency <- summarise_qc_reporting_consistency(data)
  keep <- rep(TRUE, nrow(consistency))
  if (applied[['max_prop_without_testing']]) {
    keep <- keep & consistency$prop_without_testing_facility_window <= eligibility$max_prop_without_testing
  }
  if (applied[['max_gap_months']]) {
    keep <- keep & consistency$longest_gap_months <= eligibility$max_gap_months
  }
  if (applied[['min_months_with_testing']]) {
    keep <- keep & consistency$months_with_testing >= eligibility$min_months_with_testing
  }
  as.character(consistency$facility_id[keep])
}

#' Apply a Scenario to Flagged Data
#'
#' Adds `scn_*` columns describing the scenario's cohort and omissions. The
#' source observations, flags and stored actions are not modified.
#'
#' Pre-existing missingness (`missing` counts) is kept apart from missingness
#' created by QC (`qc_omitted`, and `invalid` counts, which QC detected).
#'
#' With `by_age_band = TRUE` the omissions are those for estimates by age
#' band: the age-structure flags also omit the counts they put in doubt (a swap
#' every count of both bands, an age-mix shift the bookings, a positivity
#' contrast the positives), at their configured priority. Estimates across all
#' bands do not omit them, because a count in the wrong band is still in the
#' facility's total.
#'
#' @param data Flagged data, such as `run$data_flagged`.
#' @param scenario A [qc_scenario()].
#' @param priority Flag priorities, as in `qc_config()$priority`.
#' @param by_age_band Whether the omissions are for estimates by age band.
#'   Needs age-disaggregated data.
#'
#' @return `data` with added columns and the scenario in attribute `scenario`.
#' @export
apply_qc_scenario <- function(data, scenario, priority = .default_qc_priority(), by_age_band = FALSE) {
  if (!inherits(scenario, 'qc_scenario')) rlang::abort('`scenario` must come from `qc_scenario()`.')
  .validate_required_columns(data, c('facility_id', 'month_date', 'tested'))
  .check_by_age_band(data, by_age_band)
  out <- dplyr::as_tibble(data)
  counts <- .baseline_counts(out)
  eligible <- as.character(out$facility_id) %in%
    .scenario_eligible_facilities(out, scenario$eligibility)
  omit <- .qc_omission_masks(out, scenario$observation, priority, by_age_band)

  state <- function(raw, usable, omitted, invalid) {
    dplyr::case_when(
      !eligible ~ 'facility_ineligible',
      is.na(raw) & !invalid ~ 'missing',
      invalid | (!is.na(raw) & is.na(usable)) ~ 'invalid',
      omitted ~ 'qc_omitted',
      TRUE ~ 'observed'
    )
  }
  invalid_row <- .row_count_invalid(out)
  out$scn_facility_eligible <- eligible
  out$scn_omit_tested <- omit$tested
  out$scn_omit_positive <- omit$positive
  out$scn_omit_attending <- omit$attending
  out$scn_omit_coverage <- omit$coverage
  out$scn_tested_state <- state(out$tested, counts$tested, omit$tested, invalid_row)
  out$scn_positive_state <- state(out$positive, counts$positive, omit$positive, invalid_row)
  attr(out, 'scenario') <- scenario
  attr(out, 'by_age_band') <- by_age_band
  out
}

# The analysis frame for the "filtered, left missing" version: contributions
# omitted by the scenario are missing, and so are those of ineligible
# facilities. Reported values are never altered.
.scenario_frame <- function(scn, version = c('reported', 'filtered')) {
  version <- match.arg(version)
  counts <- .baseline_counts(scn)
  frame <- dplyr::tibble(
    facility_id = as.character(scn$facility_id),
    region = if ('region' %in% names(scn)) as.character(scn$region) else NA_character_,
    council = if ('council' %in% names(scn)) as.character(scn$council) else NA_character_,
    district = if ('district' %in% names(scn)) as.character(scn$district) else NA_character_,
    month_date = scn$month_date,
    age_group_code = if ('age_group_code' %in% names(scn)) as.character(scn$age_group_code) else NA_character_,
    age_group = if ('age_group' %in% names(scn)) as.character(scn$age_group) else NA_character_,
    tested = counts$tested, positive = counts$positive, attending = counts$attending,
    coverage_ok = TRUE, tested_imputed = FALSE, positive_imputed = FALSE
  )
  if (version == 'filtered') {
    drop_all <- !scn$scn_facility_eligible
    frame$tested[drop_all | scn$scn_omit_tested] <- NA_real_
    frame$positive[drop_all | scn$scn_omit_positive] <- NA_real_
    frame$attending[drop_all | scn$scn_omit_attending] <- NA_real_
    frame$coverage_ok <- !scn$scn_omit_coverage
  }
  frame
}

# Reuse the saved QC expectations, not a new fit. Replacement values are
# expectations (potentially fractional), rather than newly observed counts.
.qc_baseline_replacement <- function(scn) {
  frame <- .scenario_frame(scn, 'filtered')
  column <- function(name) {
    if (name %in% names(scn)) scn[[name]] else rep(NA_real_, nrow(scn))
  }
  targeted <- function(field) {
    raw <- column(field)
    invalid <- .count_value_invalid(raw, scn[[paste0(field, '_parse_status')]])
    scn$scn_facility_eligible & scn[[paste0('scn_', field, '_state')]] %in% c('qc_omitted', 'invalid') &
      (!is.na(raw) | invalid)
  }
  need_t <- targeted('tested')
  need_p <- targeted('positive')
  median <- column('tested_roll_median')
  probability <- column('p_hat')
  baseline_ok <- is.finite(median) & median >= 0
  if ('tested_baseline_ready' %in% names(scn)) {
    baseline_ok <- baseline_ok & dplyr::coalesce(scn$tested_baseline_ready, FALSE)
  }
  prediction_ok <- is.finite(probability) & probability >= 0 & probability <= 1
  if ('prediction_status' %in% names(scn)) {
    prediction_ok <- prediction_ok & scn$prediction_status %in% 'assessed'
  }

  tested_status <- rep('not_targeted', nrow(scn))
  tested_status[need_t] <- 'baseline_unavailable'
  conflict <- need_t & !need_p & !is.na(frame$positive) & median < frame$positive
  conflict[is.na(conflict)] <- FALSE
  tested_status[need_t & baseline_ok & conflict] <- 'baseline_below_observed_positive'
  put_t <- need_t & baseline_ok & !conflict
  frame$tested[put_t] <- median[put_t]
  tested_status[put_t] <- 'replaced'

  positive_status <- rep('not_targeted', nrow(scn))
  positive_status[need_p] <- 'tested_unavailable'
  has_tested <- is.finite(frame$tested) & frame$tested >= 0
  positive_status[need_p & has_tested] <- 'prediction_unavailable'
  put_p <- need_p & has_tested & prediction_ok
  frame$positive[put_p] <- frame$tested[put_p] * probability[put_p]
  positive_status[put_p] <- 'replaced'

  targets <- dplyr::bind_rows(lapply(c('tested', 'positive'), function(count_field) {
    need <- if (count_field == 'tested') need_t else need_p
    status <- if (count_field == 'tested') tested_status else positive_status
    dplyr::tibble(
      facility_id = as.character(scn$facility_id[need]), month_date = scn$month_date[need],
      field = count_field, status = status[need], replacement = frame[[count_field]][need],
      tested_baseline = median[need], predicted_probability = probability[need],
      observed_positive = column('positive')[need]
    )
  }))
  list(frame = frame, targets = targets)
}

# --- aggregation -------------------------------------------------------------

.aggregate_group_vars <- function(level) {
  switch(level, national = character(), region = 'region', council = 'council',
         district = 'district', facility = 'facility_id')
}

#' Aggregate QC Metrics
#'
#' Positivity is the sum of positives over the sum of tested counts, using only
#' contributions where both are valid, so numerator and denominator always
#' come from the same records. Tested count is the sum of usable tested counts.
#' Testing coverage is the sum of tested over the sum of attendance, over
#' records where both are usable and attendance is above zero. Facility
#' percentages are never averaged. `facility_months` counts facility-months,
#' not rows: with age-disaggregated data a month's bands count once, or once
#' per band when `by_age_band = TRUE`.
#'
#' Positives/tested is positivity among those tested, not population prevalence.
#'
#' @param frame A frame with `facility_id`, `month_date`, `tested`, `positive`,
#'   `attending`, `coverage_ok`, `tested_imputed` and `positive_imputed`, and
#'   the grouping columns that `level` needs.
#' @param level `"national"`, `"region"`, `"council"`, `"district"` or `"facility"`.
#' @param monthly Whether to return one row per month (otherwise the whole period).
#' @param by_age_band Whether to return one row per age band (`age_group_code`,
#'   with its label in `age_group`). Use a frame whose omissions were made for
#'   estimates by age band.
#'
#' @return A tibble of aggregate results with explicit denominators.
#' @export
qc_aggregate_metrics <- function(frame, level = c('national', 'region', 'council', 'district', 'facility'),
                                 monthly = TRUE, by_age_band = FALSE) {
  level <- match.arg(level)
  groups <- .aggregate_group_vars(level)
  if (!rlang::is_bool(by_age_band)) rlang::abort('`by_age_band` must be TRUE or FALSE.')
  if (by_age_band) {
    if (!'age_group_code' %in% names(frame) || all(is.na(frame$age_group_code))) {
      rlang::abort('Aggregation by age band needs a populated `age_group_code` field.')
    }
    groups <- c(groups, 'age_group_code')
  }
  .validate_required_columns(frame, c('facility_id', 'month_date', 'tested', 'positive', 'attending'))
  for (g in groups) {
    if (!g %in% names(frame) || all(is.na(frame[[g]]))) {
      rlang::abort(paste0('Aggregation by ', level, ' needs a populated `', g, '` field.'))
    }
  }
  if (!'coverage_ok' %in% names(frame)) frame$coverage_ok <- TRUE
  if (!'tested_imputed' %in% names(frame)) frame$tested_imputed <- FALSE
  if (!'positive_imputed' %in% names(frame)) frame$positive_imputed <- FALSE
  keys <- c(groups, if (monthly) 'month_date')

  f <- dplyr::as_tibble(frame)
  tested_ok <- !is.na(f$tested) & f$tested >= 0
  pair <- tested_ok & !is.na(f$positive) & f$positive >= 0 & f$positive <= f$tested
  cov <- tested_ok & !is.na(f$attending) & f$attending > 0 & dplyr::coalesce(f$coverage_ok, TRUE)
  f$.tested_use <- ifelse(tested_ok, f$tested, 0)
  f$.n_tested_use <- as.integer(tested_ok)
  f$.imp_tested <- ifelse(tested_ok & f$tested_imputed, f$tested, 0)
  f$.pair_t <- ifelse(pair, f$tested, 0)
  f$.pair_p <- ifelse(pair, f$positive, 0)
  f$.n_pair <- as.integer(pair)
  f$.imp_pair_t <- ifelse(pair & (f$tested_imputed | f$positive_imputed), f$tested, 0)
  f$.cov_t <- ifelse(cov, f$tested, 0)
  f$.cov_a <- ifelse(cov, f$attending, 0)
  f$.n_cov <- as.integer(cov)
  f$.att_use <- ifelse(!is.na(f$attending) & f$attending >= 0, f$attending, 0)
  f$.n_att <- as.integer(!is.na(f$attending) & f$attending >= 0)

  by_facility <- f %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(unique(c(keys, 'facility_id'))))) %>%
    dplyr::summarise(
      dplyr::across(dplyr::starts_with('.'), sum), .rows = dplyr::n_distinct(month_date), .groups = 'drop'
    )
  result <- by_facility %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(keys))) %>%
    dplyr::summarise(
      facility_months = sum(.rows),
      facilities = dplyr::n_distinct(facility_id),
      tested_total = sum(.tested_use), tested_rows = sum(.n_tested_use),
      attendance_total = sum(.att_use), attendance_rows = sum(.n_att),
      positive_matched = sum(.pair_p), tested_matched = sum(.pair_t),
      pair_rows = sum(.n_pair),
      pair_facilities = sum(.n_pair > 0),
      positivity_se = {
        g <- sum(.n_pair > 0)
        p <- sum(.pair_p) / sum(.pair_t)
        if (g >= 2L && sum(.pair_t) > 0) {
          resid <- .pair_p - p * .pair_t
          sqrt(g / (g - 1) * sum(resid^2)) / sum(.pair_t)
        } else {
          NA_real_
        }
      },
      coverage_tested = sum(.cov_t), coverage_attendance = sum(.cov_a),
      coverage_rows = sum(.n_cov),
      tested_from_imputation = sum(.imp_tested),
      matched_tested_from_imputation = sum(.imp_pair_t),
      .groups = 'drop'
    ) %>%
    dplyr::mutate(
      positivity = .safe_divide(positive_matched, tested_matched),
      coverage = .safe_divide(coverage_tested, coverage_attendance),
      prop_tested_from_imputation = .safe_divide(matched_tested_from_imputation, tested_matched)
    )
  if (by_age_band) {
    labels <- dplyr::distinct(dplyr::as_tibble(frame)[, c('age_group_code', 'age_group')], age_group_code, .keep_all = TRUE)
    result <- dplyr::left_join(result, labels, by = 'age_group_code') %>%
      dplyr::relocate(age_group, .after = age_group_code)
  }
  if (monthly) result <- dplyr::arrange(result, dplyr::across(dplyr::all_of(keys)))
  result
}

#' Scenario Comparison
#'
#' Computes aggregate results for the reported baseline and the filtered
#' scenario with excluded contributions left missing, side by side, for one
#' geographical level. Imputed versions are added by [qc_impute_scenario()].
#'
#' @param data Flagged data.
#' @param scenario A [qc_scenario()].
#' @param level Aggregation level for [qc_aggregate_metrics()].
#' @param monthly Monthly or whole-period results.
#' @param priority Flag priorities.
#' @param by_age_band Whether to give estimates by age band, with the
#'   omissions for them (see [apply_qc_scenario()]).
#'
#' @return A tibble with a `version` column (`reported`, `filtered_missing`).
#'   Its attribute `scenario_provenance` records eligibility, handling and the
#'   omission counts.
#' @export
compare_qc_scenario <- function(data, scenario, level = 'national', monthly = TRUE,
                                priority = .default_qc_priority(), by_age_band = FALSE) {
  scn <- apply_qc_scenario(data, scenario, priority, by_age_band)
  out <- dplyr::bind_rows(
    dplyr::mutate(qc_aggregate_metrics(.scenario_frame(scn, 'reported'), level, monthly, by_age_band),
                  version = 'reported'),
    dplyr::mutate(qc_aggregate_metrics(.scenario_frame(scn, 'filtered'), level, monthly, by_age_band),
                  version = 'filtered_missing')
  )
  attr(out, 'scenario_provenance') <- .scenario_provenance(scn)
  out
}

.scenario_provenance <- function(scn) {
  scenario <- attr(scn, 'scenario')
  list(
    scenario = scenario,
    by_age_band = isTRUE(attr(scn, 'by_age_band')),
    facilities_total = dplyr::n_distinct(scn$facility_id),
    facilities_eligible = dplyr::n_distinct(scn$facility_id[scn$scn_facility_eligible]),
    facility_months_total = nrow(dplyr::distinct(scn, facility_id, month_date)),
    records_total = nrow(scn),
    omitted_tested = sum(scn$scn_omit_tested & scn$scn_facility_eligible),
    omitted_positive = sum(scn$scn_omit_positive & scn$scn_facility_eligible),
    omitted_attending = sum(scn$scn_omit_attending & scn$scn_facility_eligible),
    omitted_coverage = sum(scn$scn_omit_coverage & scn$scn_facility_eligible),
    tested_state = as.list(table(scn$scn_tested_state)),
    positive_state = as.list(table(scn$scn_positive_state))
  )
}
