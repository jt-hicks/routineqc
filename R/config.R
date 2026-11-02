.merge_config_section <- function(defaults, overrides, section) {
  if (!is.list(overrides) ||
      (length(overrides) > 0L && (is.null(names(overrides)) || any(names(overrides) == '')))) {
    rlang::abort(paste0('`', section, '` overrides must be a named list.'))
  }
  unknown <- setdiff(names(overrides), names(defaults))
  if (length(unknown) > 0L) {
    rlang::abort(paste0('Unknown ', section, ' setting(s): ', paste(unknown, collapse = ', ')))
  }
  utils::modifyList(defaults, overrides, keep.null = TRUE)
}

# Review priority of each flag. A row takes the highest priority among its
# active flags; a row with signals in two or more domains takes at least
# `multiple_signals`. Only impossible counts are authorized for exclusion,
# whatever their priority here.
.qc_priority_levels <- c('critical', 'high', 'medium', 'low')

.default_qc_priority <- function() {
  list(
    positive_gt_tested = 'critical',
    zero_tested_positive = 'critical',
    tested_negative = 'critical',
    positive_negative = 'critical',
    all_negative_large_n = 'critical',
    all_positive_large_n = 'critical',
    resid_extreme = 'high',
    tested_extreme_high = 'high',
    tested_extreme_low = 'high',
    tested_large_jump = 'high',
    tested_large_drop = 'high',
    multiple_signals = 'high',
    large_monthly_prevalence_change = 'medium',
    tested_gt_attending_major = 'medium',
    attending_negative = 'medium',
    coverage_very_low = 'medium',
    tested_zero_unusual = 'low',
    tested_gt_attending = 'low',
    coverage_low = 'low'
  )
}

.qc_profile_defaults <- function(profile) {
  recommended <- list(
    model = list(
      min_prediction_coverage = 0.8
    ),
    prevalence = list(
      resid_threshold = NULL,
      resid_threshold_large = 4,
      resid_threshold_small = 5,
      resid_large_n = 20,
      all_negative_min_tested = 50,
      all_negative_max_probability = 0.001,
      all_positive_min_tested = 30,
      large_change_threshold = 0.5,
      large_change_min_tested = 20,
      large_change_require_adjacent = TRUE,
      prevalence_low_extreme = 0.001,
      prevalence_high_extreme = 0.999,
      use_prevalence_bounds = FALSE
    ),
    temporal = list(require_adjacent_months = TRUE),
    action_policy = list(
      name = 'conservative_review',
      version = 3L
    ),
    attendance = list(
      excess_ratio = 1.5,
      excess_min = 10,
      coverage_low = 0.8,
      coverage_very_low = 0.5,
      coverage_min_attending = 10,
      coverage_from = NULL
    ),
    priority = .default_qc_priority(),
    tested_volume = list(
      baseline_mode = 'trailing',
      baseline_window_months = 6,
      baseline_uses_future = FALSE,
      min_roll_n = 6,
      tested_z_threshold = 5,
      tested_high_ratio = 3,
      tested_low_ratio = 0.25,
      tested_jump_ratio = 5,
      tested_drop_ratio = 0.2,
      require_both_extreme = TRUE,
      tested_high_min = 20,
      baseline_low_min = 20,
      baseline_zero_min = 10,
      tested_jump_min = 20,
      previous_jump_min = 10,
      previous_drop_min = 20
    )
  )

  if (profile %in% c('recommended', 'operational')) return(recommended)

  if (profile == 'retrospective') {
    retrospective <- recommended
    retrospective$tested_volume$baseline_mode <- 'centered'
    retrospective$tested_volume$baseline_uses_future <- TRUE
    return(retrospective)
  }

  sensitivity <- recommended
  sensitivity$prevalence$resid_threshold <- 4
  sensitivity$prevalence$all_negative_min_tested <- 30
  sensitivity$prevalence$large_change_threshold <- 0.35
  sensitivity$prevalence$large_change_min_tested <- 0
  sensitivity$prevalence$large_change_require_adjacent <- FALSE
  sensitivity$prevalence$use_prevalence_bounds <- TRUE
  sensitivity$temporal$require_adjacent_months <- FALSE
  sensitivity$tested_volume$require_both_extreme <- FALSE
  sensitivity
}

#' Create a QC Configuration
#'
#' Creates a named, validated snapshot of thresholds and temporal behavior used
#' by [run_routine_qc()]. The `recommended` profile contains the current agreed
#' defaults and is an alias for `operational`. `operational` uses only earlier
#' months; `retrospective` uses earlier and later months. `permissive_sensitivity`
#' intentionally broadens several rules and is not recommended policy.
#'
#' @param profile One of `recommended`, `operational`, `retrospective`, or
#'   `permissive_sensitivity`.
#' @param prevalence Named list of prevalence-rule overrides.
#' @param model Named list of prevalence-model overrides.
#' @param temporal Named list of temporal-rule overrides.
#' @param action_policy Named list selecting `conservative_review` or
#'   `flags_only` and its policy version.
#' @param tested_volume Named list of tested-volume overrides.
#' @param attendance Named list of attendance-rule overrides: the ratio and
#'   absolute excess at which tested above attendance counts as major, the two
#'   testing-coverage cutoffs, the minimum attendance for a coverage judgement,
#'   and `coverage_from`, an optional first month (`"YYYY-MM-DD"`) for coverage
#'   rules.
#' @param priority Named list overriding the review priority (`critical`,
#'   `high`, `medium`, `low`) of individual flags. See `docs/ACTION_POLICY.md`.
#'
#' @return An object of class `routineqc_config`.
#' @export
qc_config <- function(profile = c('recommended', 'operational', 'retrospective', 'permissive_sensitivity'),
                      model = list(),
                      prevalence = list(),
                      temporal = list(),
                      action_policy = list(),
                      tested_volume = list(),
                      attendance = list(),
                      priority = list()) {
  profile <- match.arg(profile)
  defaults <- .qc_profile_defaults(profile)
  config <- list(
    schema_version = .qc_config_schema_version,
    profile = profile,
    model = .merge_config_section(defaults$model, model, 'model'),
    prevalence = .merge_config_section(defaults$prevalence, prevalence, 'prevalence'),
    temporal = .merge_config_section(defaults$temporal, temporal, 'temporal'),
    action_policy = .merge_config_section(defaults$action_policy, action_policy, 'action_policy'),
    tested_volume = .merge_config_section(defaults$tested_volume, tested_volume, 'tested_volume'),
    attendance = .merge_config_section(defaults$attendance, attendance, 'attendance'),
    priority = .merge_config_section(defaults$priority, priority, 'priority')
  )
  class(config) <- c('routineqc_config', 'list')
  validate_qc_config(config)
  config
}

# Schema 5 added prevalence$all_negative_max_probability; schema 6 added the
# attendance and priority sections. A stored configuration from an earlier
# schema is valid without what was added after it.
.qc_config_schema_version <- 6L
.qc_config_keys_added <- list(`5` = list(prevalence = 'all_negative_max_probability'))
.qc_config_sections_added <- list(`6` = c('attendance', 'priority'))

.validate_attendance_config <- function(attendance) {
  numeric_names <- c('excess_ratio', 'excess_min', 'coverage_low',
                     'coverage_very_low', 'coverage_min_attending')
  if (any(!vapply(attendance[numeric_names], .is_scalar_number, logical(1)))) {
    rlang::abort('All attendance numeric settings must be finite scalar numbers.')
  }
  if (attendance$excess_ratio < 1 || attendance$excess_min < 0 || attendance$coverage_min_attending < 0) {
    rlang::abort('`attendance$excess_ratio` must be at least 1 and the minimums non-negative.')
  }
  if (attendance$coverage_very_low <= 0 || attendance$coverage_very_low > attendance$coverage_low ||
      attendance$coverage_low > 1) {
    rlang::abort('Coverage cutoffs must satisfy 0 < coverage_very_low <= coverage_low <= 1.')
  }
  from <- attendance$coverage_from
  if (!is.null(from) && (!is.character(from) || length(from) != 1L ||
                         is.na(suppressWarnings(as.Date(from, optional = TRUE))))) {
    rlang::abort('`attendance$coverage_from` must be NULL or one date as "YYYY-MM-DD".')
  }
  invisible(attendance)
}

.is_scalar_number <- function(x) {
  is.numeric(x) && length(x) == 1L && !is.na(x) && is.finite(x)
}

#' Validate a QC Configuration
#'
#' Checks the configuration schema, section names, and scalar setting types.
#'
#' @param config A configuration created by [qc_config()].
#'
#' @return `config`, invisibly, after successful validation.
#' @export
validate_qc_config <- function(config) {
  .validate_qc_config(config)
}

# `stored = TRUE` validates the configuration of a persisted run, which may
# carry an action-policy version that new runs can no longer use.
.validate_qc_config <- function(config, stored = FALSE) {
  required <- c('schema_version', 'profile', 'model', 'prevalence', 'temporal', 'action_policy', 'tested_volume')
  if (!is.list(config) || !all(required %in% names(config))) {
    rlang::abort('QC configuration is missing required sections.')
  }
  supported <- if (stored) c(4L, 5L, .qc_config_schema_version) else .qc_config_schema_version
  if (!is.integer(config$schema_version) || length(config$schema_version) != 1L ||
      !config$schema_version %in% supported) {
    rlang::abort('Unsupported QC configuration schema version.')
  }
  if (!is.character(config$profile) || length(config$profile) != 1L ||
      !config$profile %in% c('recommended', 'operational', 'retrospective', 'permissive_sensitivity')) {
    rlang::abort('QC configuration has an unknown profile.')
  }

  defaults <- .qc_profile_defaults(config$profile)
  if (config$schema_version < .qc_config_schema_version) {
    for (version in names(.qc_config_keys_added)) {
      if (config$schema_version >= as.integer(version)) next
      for (section in names(.qc_config_keys_added[[version]])) {
        added <- .qc_config_keys_added[[version]][[section]]
        defaults[[section]] <- defaults[[section]][setdiff(names(defaults[[section]]), added)]
      }
    }
    for (version in names(.qc_config_sections_added)) {
      if (config$schema_version >= as.integer(version)) next
      defaults[.qc_config_sections_added[[version]]] <- NULL
    }
  }
  for (section in names(defaults)) {
    values <- config[[section]]
    if (!is.list(values) || !setequal(names(values), names(defaults[[section]]))) {
      rlang::abort(paste0('QC configuration section `', section, '` has missing or unknown settings.'))
    }
  }

  if (!.is_scalar_number(config$model$min_prediction_coverage) ||
      config$model$min_prediction_coverage < 0 || config$model$min_prediction_coverage > 1) {
    rlang::abort('`model$min_prediction_coverage` must be one number between 0 and 1.')
  }

  numeric_settings <- c(
    'resid_threshold_large', 'resid_threshold_small', 'resid_large_n',
    'all_negative_min_tested', 'all_positive_min_tested',
    'large_change_threshold', 'large_change_min_tested',
    'prevalence_low_extreme', 'prevalence_high_extreme'
  )
  if (!is.null(config$prevalence$resid_threshold) &&
      !.is_scalar_number(config$prevalence$resid_threshold)) {
    rlang::abort('`prevalence$resid_threshold` must be NULL or one finite number.')
  }
  if (any(!vapply(config$prevalence[numeric_settings], .is_scalar_number, logical(1)))) {
    rlang::abort('All prevalence numeric settings must be finite scalar numbers.')
  }
  prevalence_numeric <- unlist(config$prevalence[numeric_settings], use.names = FALSE)
  if (any(prevalence_numeric < 0) ||
      (!is.null(config$prevalence$resid_threshold) && config$prevalence$resid_threshold < 0)) {
    rlang::abort('Prevalence thresholds and count boundaries must be non-negative.')
  }
  if (config$prevalence$prevalence_low_extreme < 0 ||
      config$prevalence$prevalence_high_extreme > 1 ||
      config$prevalence$prevalence_low_extreme >= config$prevalence$prevalence_high_extreme) {
    rlang::abort('Prevalence bounds must satisfy 0 <= low < high <= 1.')
  }
  if (!is.null(config$prevalence$all_negative_max_probability) &&
      (!.is_scalar_number(config$prevalence$all_negative_max_probability) ||
       config$prevalence$all_negative_max_probability <= 0 ||
       config$prevalence$all_negative_max_probability > 1)) {
    rlang::abort('`prevalence$all_negative_max_probability` must be one number in (0, 1].')
  }
  prevalence_logical <- c('large_change_require_adjacent', 'use_prevalence_bounds')
  if (any(!vapply(config$prevalence[prevalence_logical], rlang::is_bool, logical(1)))) {
    rlang::abort('Prevalence logical settings must be TRUE or FALSE.')
  }
  if (!rlang::is_bool(config$temporal$require_adjacent_months)) {
    rlang::abort('`temporal$require_adjacent_months` must be TRUE or FALSE.')
  }
  if (!is.character(config$action_policy$name) || length(config$action_policy$name) != 1L ||
      !config$action_policy$name %in% c('conservative_review', 'flags_only')) {
    rlang::abort('`action_policy$name` must be conservative_review or flags_only.')
  }
  .check_action_policy_version(config$action_policy$version, stored = stored)
  if (!is.null(config$attendance)) .validate_attendance_config(config$attendance)
  if (!is.null(config$priority)) {
    bad <- !vapply(config$priority, function(x) {
      is.character(x) && length(x) == 1L && x %in% .qc_priority_levels
    }, logical(1))
    if (any(bad)) {
      rlang::abort(paste0(
        'Priority must be one of ', paste(.qc_priority_levels, collapse = ', '),
        ': ', paste(names(config$priority)[bad], collapse = ', ')
      ))
    }
  }
  tested_numeric_names <- c(
    'baseline_window_months', 'min_roll_n', 'tested_z_threshold',
    'tested_high_ratio', 'tested_low_ratio', 'tested_jump_ratio',
    'tested_drop_ratio', 'tested_high_min', 'baseline_low_min',
    'baseline_zero_min', 'tested_jump_min', 'previous_jump_min',
    'previous_drop_min'
  )
  if (any(!vapply(config$tested_volume[tested_numeric_names], .is_scalar_number, logical(1)))) {
    rlang::abort('All tested-volume settings must be finite scalar numbers.')
  }
  if (any(unlist(config$tested_volume[tested_numeric_names], use.names = FALSE) < 0)) {
    rlang::abort('Tested-volume settings must be non-negative.')
  }
  if (config$tested_volume$min_roll_n < 1 ||
      config$tested_volume$min_roll_n != as.integer(config$tested_volume$min_roll_n)) {
    rlang::abort('`tested_volume$min_roll_n` must be a positive whole number.')
  }
  if (config$tested_volume$baseline_window_months < 1 ||
      config$tested_volume$baseline_window_months != as.integer(config$tested_volume$baseline_window_months)) {
    rlang::abort('`tested_volume$baseline_window_months` must be a positive whole number.')
  }
  if (!config$tested_volume$baseline_mode %in% c('trailing', 'centered')) {
    rlang::abort('`tested_volume$baseline_mode` must be trailing or centered.')
  }
  if (!rlang::is_bool(config$tested_volume$baseline_uses_future) ||
      !rlang::is_bool(config$tested_volume$require_both_extreme)) {
    rlang::abort('Tested-volume logical settings must be TRUE or FALSE.')
  }
  expected_future <- config$tested_volume$baseline_mode == 'centered'
  if (!identical(config$tested_volume$baseline_uses_future, expected_future)) {
    rlang::abort('`baseline_uses_future` must agree with `baseline_mode`.')
  }
  if (config$tested_volume$tested_low_ratio > 1 ||
      config$tested_volume$tested_drop_ratio > 1 ||
      config$tested_volume$tested_high_ratio < 1 ||
      config$tested_volume$tested_jump_ratio < 1) {
    rlang::abort('Tested-volume low/drop ratios must be <= 1 and high/jump ratios must be >= 1.')
  }

  invisible(config)
}
