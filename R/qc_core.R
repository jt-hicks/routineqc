#' routineqc: Reusable QC Tools For Routine Surveillance Data
#'
#' The package provides modular functions for preparing routine facility-month
#' data, fitting prevalence models, flagging potential anomalies, and producing
#' summaries and plots for QC review.
#'
#' @importFrom magrittr %>%
#' @importFrom rlang .data .env
#'
#' @keywords internal
"_PACKAGE"

.resolve_col <- function(x) {
  if (is.null(x)) {
    return(NULL)
  }

  if (rlang::is_symbol(x)) {
    return(rlang::as_string(x))
  }

  if (is.character(x) && length(x) == 1L) {
    return(x)
  }

  if (rlang::is_quosure(x)) {
    return(rlang::as_name(rlang::get_expr(x)))
  }

  rlang::abort("Column mappings must be a single string or bare column name.")
}

.coerce_month_date <- function(x) {
  if (inherits(x, "Date")) {
    return(lubridate::floor_date(x, unit = "month"))
  }

  if (inherits(x, "POSIXt")) {
    return(lubridate::floor_date(as.Date(x), unit = "month"))
  }

  if (is.numeric(x)) {
    x <- as.character(x)
  }

  x_chr <- as.character(x)
  parsed <- suppressWarnings(lubridate::parse_date_time(
    x_chr,
    orders = c("ymd", "Ymd", "Y-m", "Y/m", "m/Y", "b Y", "B Y", "Y b", "Y B"),
    truncated = 2
  ))

  out <- as.Date(parsed)
  out <- lubridate::floor_date(out, unit = "month")
  out
}

.safe_divide <- function(num, den) {
  dplyr::if_else(is.na(den) | den == 0, NA_real_, as.numeric(num) / as.numeric(den))
}

.validate_required_columns <- function(data, cols) {
  missing_cols <- setdiff(cols, names(data))
  if (length(missing_cols) > 0) {
    rlang::abort(paste0("Missing required columns: ", paste(missing_cols, collapse = ", ")))
  }
}

#' Prepare Routine QC Data
#'
#' Standardises user-specified columns into a common schema used across QC
#' functions and appends derived fields without dropping rows.
#'
#' @param data A data frame with raw routine surveillance records.
#' @param facility_var Facility identifier column.
#' @param region_var Region column.
#' @param month_var Month/date column.
#' @param tested_var Tested count column.
#' @param positive_var Positive count column.
#' @param attending_var Optional attendance denominator column. When supplied,
#'   `attendance_definition` must describe its meaning and source.
#' @param attendance_definition A non-empty scalar definition required with
#'   `attending_var`.
#' @param council_var Optional council column.
#' @param district_var Optional district column.
#'
#' @return A tibble with standardised QC fields appended.
#' @export
prepare_qc_data <- function(data,
                            facility_var,
                            region_var,
                            month_var,
                            tested_var,
                            positive_var,
                            attending_var = NULL,
                            attendance_definition = NULL,
                            council_var = NULL,
                            district_var = NULL) {
  facility_var <- .resolve_col(facility_var)
  region_var <- .resolve_col(region_var)
  month_var <- .resolve_col(month_var)
  tested_var <- .resolve_col(tested_var)
  positive_var <- .resolve_col(positive_var)
  attending_var <- .resolve_col(attending_var)
  council_var <- .resolve_col(council_var)
  district_var <- .resolve_col(district_var)

  required <- c(facility_var, region_var, month_var, tested_var, positive_var, attending_var)
  .validate_required_columns(data, required)

  if (!is.null(attending_var) &&
      (!is.character(attendance_definition) || length(attendance_definition) != 1L ||
       is.na(attendance_definition) || trimws(attendance_definition) == '')) {
    rlang::abort('`attendance_definition` must be a non-empty string when `attending_var` is supplied.')
  }

  out <- dplyr::as_tibble(data) %>%
    dplyr::mutate(
      facility_id = as.character(.data[[facility_var]]),
      region = as.character(.data[[region_var]]),
      month_date = .coerce_month_date(.data[[month_var]]),
      tested = suppressWarnings(as.numeric(.data[[tested_var]])),
      positive = suppressWarnings(as.numeric(.data[[positive_var]]))
    )

  if (!is.null(council_var) && council_var %in% names(out)) {
    out <- dplyr::mutate(out, council = as.character(.data[[council_var]]))
  }

  if (!is.null(district_var) && district_var %in% names(out)) {
    out <- dplyr::mutate(out, district = as.character(.data[[district_var]]))
  }

  if (!is.null(attending_var)) {
    out <- dplyr::mutate(
      out,
      attending = suppressWarnings(as.numeric(.data[[attending_var]])),
      attendance_definition = attendance_definition
    )
  }

  unique_months <- sort(unique(out$month_date))

  out %>%
    dplyr::mutate(
      year = lubridate::year(month_date),
      month_num = lubridate::month(month_date),
      time_index = match(month_date, unique_months),
      prevalence = .safe_divide(positive, tested)
    )
}

.core_count_levels <- c('complete', 'missing_positive', 'missing_tested', 'missing_both')

.core_count_status <- function(tested, positive) {
  dplyr::case_when(
    is.na(tested) & is.na(positive) ~ 'missing_both',
    is.na(tested) ~ 'missing_tested',
    is.na(positive) ~ 'missing_positive',
    TRUE ~ 'complete'
  )
}

# Runs are made under version 3 only. Stored version 1 and 2 runs stay
# readable: their flags were computed under the old policy and are reported as
# such. Version 1 authorized exclusion of rows missing a count; version 2 set
# priority by domain rather than by flag and had no coverage rules.
.qc_policy_version <- 3L

.check_action_policy_version <- function(version, stored = FALSE) {
  if (stored && (identical(version, 1L) || identical(version, 2L))) {
    return(invisible(version))
  }
  if (identical(version, 1L)) {
    rlang::abort(paste(
      'Action policy version 1 is no longer supported. It authorized exclusion',
      'of rows with a missing tested or positive count; current versions record',
      'missingness in `core_count_status` instead. Use version 3, or the',
      'routineqc release that produced a version 1 run.'
    ))
  }
  if (identical(version, 2L)) {
    rlang::abort(paste(
      'Action policy version 2 is no longer supported for new runs. Version 3',
      'sets priority per flag from `config$priority` and adds coverage rules.'
    ))
  }
  if (!identical(version, .qc_policy_version)) {
    rlang::abort('Unsupported action policy version.')
  }
  invisible(version)
}

#' Flag Logical Data Errors
#'
#' Adds flags for impossible or invalid tested/positive combinations. Attendance
#' checks are applied only when non-missing `attending` values are accompanied
#' by a non-empty `attendance_definition`. Data without attendance, or with an
#' entirely missing attendance column, continue through core logical QC without
#' attendance checks.
#'
#' A missing `tested` or `positive` value is not a logical error and sets no
#' flag. It is recorded in `core_count_status` instead, one of `complete`,
#' `missing_positive`, `missing_tested` or `missing_both`, so that missingness
#' can be summarised per facility and area by
#' [summarise_qc_reporting_consistency()] and [summarise_qc_count_completeness()]
#' without marking individual records for exclusion. A missing `tested` alongside
#' a reported `positive` is treated the same way: prevalence is undefined, which
#' is the consequence that matters.
#'
#' @param data A prepared QC data frame.
#'
#' @return The input data with logical error flags and `core_count_status`
#'   appended.
#' @export
flag_logical_errors <- function(data) {
  .validate_required_columns(data, c("tested", "positive"))

  out <- dplyr::as_tibble(data)
  if (!'attending' %in% names(out)) {
    out$attending <- NA_real_
  }
  if (any(!is.na(out$attending)) &&
      (!'attendance_definition' %in% names(out) ||
       any(is.na(out$attendance_definition) |
           trimws(as.character(out$attendance_definition)) == ''))) {
    rlang::abort('Non-missing `attending` values require a non-missing `attendance_definition`.')
  }

  out %>%
    dplyr::mutate(
      flag_positive_gt_tested = positive > tested,
      flag_tested_negative = tested < 0,
      flag_positive_negative = positive < 0,
      flag_zero_tested_positive = tested == 0 & positive > 0,
      core_count_status = .core_count_status(tested, positive),
      flag_tested_gt_attending = !is.na(tested) & !is.na(attending) & tested > attending,
      flag_attending_negative = !is.na(attending) & attending < 0,
      flag_attendance_issue = flag_tested_gt_attending | flag_attending_negative,
      flag_core_invalid = (
        flag_positive_gt_tested |
          flag_tested_negative |
          flag_positive_negative |
          flag_zero_tested_positive
      ) %>%
        dplyr::coalesce(FALSE),
      flag_invalid_logical = flag_core_invalid | flag_attendance_issue
    )
}

#' Add Attendance QC Flags
#'
#' Grades tested above attendance and judges testing coverage, tested divided
#' by attendance. Both need an approved attendance denominator: rows without
#' one, or without a tested count, are not compared and carry FALSE.
#'
#' Tested above attendance is `flag_tested_gt_attending` from
#' [flag_logical_errors()]; `flag_tested_gt_attending_major` marks the subset
#' more than `excess_ratio` times attendance and at least `excess_min` over.
#' Coverage below `coverage_very_low` sets `flag_coverage_very_low`, and below
#' `coverage_low` sets `flag_coverage_low`. Coverage is judged only where
#' attendance is at least `coverage_min_attending` and, when `coverage_from` is
#' given, from that month on.
#'
#' @param data A logically flagged QC data frame.
#' @param excess_ratio,excess_min Thresholds for a major excess.
#' @param coverage_low,coverage_very_low Coverage cutoffs.
#' @param coverage_min_attending Minimum attendance for a coverage judgement.
#' @param coverage_from Optional first month, `"YYYY-MM-DD"`, for coverage rules.
#'
#' @return Data with `testing_coverage` and the attendance flags appended.
#' @export
add_attendance_qc_flags <- function(data,
                                    excess_ratio = 1.5,
                                    excess_min = 10,
                                    coverage_low = 0.8,
                                    coverage_very_low = 0.5,
                                    coverage_min_attending = 10,
                                    coverage_from = NULL) {
  .validate_attendance_config(list(
    excess_ratio = excess_ratio, excess_min = excess_min,
    coverage_low = coverage_low, coverage_very_low = coverage_very_low,
    coverage_min_attending = coverage_min_attending, coverage_from = coverage_from
  ))
  out <- dplyr::as_tibble(data)
  .validate_required_columns(out, c('tested', 'month_date'))
  if (!'attending' %in% names(out)) out$attending <- NA_real_
  if (!'flag_tested_gt_attending' %in% names(out)) out <- flag_logical_errors(out)

  comparable <- !is.na(out$tested) & !is.na(out$attending)
  judged <- comparable & out$attending > 0 & out$attending >= coverage_min_attending
  if (!is.null(coverage_from)) judged <- judged & out$month_date >= as.Date(coverage_from)
  coverage <- ifelse(comparable & out$attending > 0, out$tested / out$attending, NA_real_)

  out$testing_coverage <- coverage
  out$flag_tested_gt_attending_major <- dplyr::coalesce(out$flag_tested_gt_attending, FALSE) &
    comparable & out$tested > excess_ratio * out$attending &
    (out$tested - out$attending) >= excess_min
  out$flag_coverage_very_low <- judged & coverage < coverage_very_low
  out$flag_coverage_low <- judged & coverage < coverage_low
  out
}

#' Fit Prevalence GAM
#'
#' Fits a binomial GAM using logically valid rows with both counts present and
#' tested > 0.
#'
#' @param data A prepared and logically flagged data frame.
#' @param nthreads Number of threads to pass to [mgcv::bam()].
#'
#' @return A fitted `mgcv` model, or `NULL` if insufficient data.
#' @export
fit_prevalence_gam <- function(data, nthreads = 1) {
  .validate_required_columns(
    data,
    c("facility_id", "region", "month_num", "time_index", "tested", "positive")
  )

  train <- dplyr::as_tibble(data)
  if (!"flag_core_invalid" %in% names(train)) {
    train <- flag_logical_errors(train)
  }

  train <- train %>%
    dplyr::filter(
      !flag_core_invalid, !is.na(positive), tested > 0,
      !is.na(month_num), !is.na(time_index)
    ) %>%
    dplyr::mutate(
      facility_id = as.factor(facility_id),
      region = as.factor(region)
    )

  if (nrow(train) < 50 || dplyr::n_distinct(train$facility_id) < 2 || dplyr::n_distinct(train$region) < 1) {
    warning("Insufficient valid rows to fit prevalence model; returning NULL.", call. = FALSE)
    return(NULL)
  }

  fit_once <- function(k_month = 12, k_time = 40) {
    mgcv::bam(
      cbind(positive, tested - positive) ~ region +
        s(month_num, by = region, bs = "cc", k = k_month) +
        s(time_index, k = k_time) +
        s(facility_id, bs = "re"),
      data = train,
      family = stats::binomial(),
      method = "fREML",
      discrete = TRUE,
      nthreads = nthreads,
      knots = list(month_num = c(0.5, 12.5))
    )
  }

  fit <- tryCatch(
    fit_once(k_month = 12, k_time = 40),
    error = function(e) {
      month_support <- train %>%
        dplyr::group_by(region) %>%
        dplyr::summarise(n_month = dplyr::n_distinct(month_num), .groups = "drop") %>%
        dplyr::pull(n_month)

      k_month_fallback <- max(3, min(11, min(month_support, na.rm = TRUE) - 1))
      k_time_fallback <- max(5, min(40, dplyr::n_distinct(train$time_index) - 1))

      warning(
        paste0(
          "Default GAM basis dimensions were too large for the available data; ",
          "refitting with k_month=", k_month_fallback,
          " and k_time=", k_time_fallback, "."
        ),
        call. = FALSE
      )

      fit_once(k_month = k_month_fallback, k_time = k_time_fallback)
    }
  )

  fit
}

#' Add Prevalence Predictions
#'
#' Appends fitted prevalence and residual diagnostics from a fitted GAM.
#'
#' The binomial model fixes its scale at 1, but routine facility data are
#' usually overdispersed: facility-months vary more than binomial sampling
#' allows. Unadjusted, every Pearson residual is inflated by the square root of
#' the dispersion, so a residual cutoff fires far more often than its nominal
#' size, and increasingly so at high tested counts. `resid_dispersion` is the
#' Pearson chi-square over residual degrees of freedom of the fit, floored at 1,
#' and `pearson_resid_scaled` divides the residual by its square root. The
#' residual rule in [add_prevalence_qc_flags()] uses the scaled residual.
#'
#' @param data A prepared QC data frame.
#' @param model A model returned by [fit_prevalence_gam()].
#' @param min_prediction_coverage Warn when the proportion of model-eligible
#'   rows successfully assessed falls below this value.
#'
#' @return The input data with prediction columns appended, including
#'   `pearson_resid`, `resid_dispersion`, and `pearson_resid_scaled`.
#' @export
add_prevalence_predictions <- function(data, model, min_prediction_coverage = 0.8) {
  out <- dplyr::as_tibble(data)
  .validate_required_columns(out, c('tested', 'positive', 'facility_id', 'region', 'month_num', 'time_index'))

  if (!.is_scalar_number(min_prediction_coverage) ||
      min_prediction_coverage < 0 || min_prediction_coverage > 1) {
    rlang::abort('`min_prediction_coverage` must be one number between 0 and 1.')
  }
  if (!'flag_core_invalid' %in% names(out)) out <- flag_logical_errors(out)

  missing_predictor <- is.na(out$facility_id) | is.na(out$region) |
    is.na(out$month_num) | is.na(out$time_index)
  missing_counts <- is.na(out$tested) | is.na(out$positive)
  eligible <- !out$flag_core_invalid & !missing_counts & out$tested > 0 & !missing_predictor
  status <- dplyr::case_when(
    out$flag_core_invalid ~ 'ineligible_core_counts',
    missing_counts ~ 'ineligible_missing_counts',
    out$tested <= 0 ~ 'ineligible_nonpositive_tested',
    missing_predictor ~ 'ineligible_missing_predictor',
    TRUE ~ 'pending'
  )

  pred <- rep(NA_real_, nrow(out))
  if (is.null(model)) {
    status[eligible] <- 'model_unavailable'
  } else {
    model_levels <- model$xlevels
    region_levels <- model_levels$region
    facility_levels <- model_levels$facility_id
    if (is.null(region_levels)) region_levels <- levels(model$model$region)
    if (is.null(facility_levels)) facility_levels <- levels(model$model$facility_id)
    unseen_region <- eligible & !as.character(out$region) %in% region_levels
    unseen_facility <- eligible & !as.character(out$facility_id) %in% facility_levels
    status[unseen_region] <- 'unseen_region'
    status[!unseen_region & unseen_facility] <- 'unseen_facility'
    valid_idx <- which(eligible & !unseen_region & !unseen_facility)

    if (length(valid_idx) > 0L) {
      newdata <- out[valid_idx, , drop = FALSE]
      newdata$facility_id <- factor(newdata$facility_id, levels = facility_levels)
      newdata$region <- factor(newdata$region, levels = region_levels)
      pred_result <- tryCatch(
        suppressWarnings(stats::predict(model, newdata = newdata, type = 'response')),
        error = identity
      )
      if (inherits(pred_result, 'error')) {
        status[valid_idx] <- 'prediction_error'
      } else {
        pred_vals <- as.numeric(pred_result)
        finite <- is.finite(pred_vals)
        pred[valid_idx[finite]] <- pred_vals[finite]
        status[valid_idx[finite]] <- 'assessed'
        status[valid_idx[!finite]] <- 'non_finite_prediction'
      }
    }
  }

  assessed <- status == 'assessed'
  coverage <- if (any(eligible)) sum(assessed) / sum(eligible) else NA_real_
  if (!is.na(coverage) && coverage < min_prediction_coverage) {
    warning(
      sprintf(
        'Prevalence-model assessment coverage is %.1f%%, below the configured %.1f%% threshold.',
        100 * coverage, 100 * min_prediction_coverage
      ),
      call. = FALSE
    )
  }

  out <- out %>%
    dplyr::mutate(
      model_assessment_eligible = eligible,
      model_assessed = assessed,
      prediction_status = status,
      p_hat = pmin(1 - 1e-6, pmax(1e-6, pred)),
      expected_positive = tested * p_hat,
      pearson_resid = dplyr::if_else(
        is.na(tested) | is.na(positive) | is.na(p_hat) | tested <= 0,
        NA_real_,
        (positive - expected_positive) / sqrt(tested * p_hat * (1 - p_hat))
      ),
      resid_dispersion = .model_dispersion(model),
      pearson_resid_scaled = pearson_resid / sqrt(resid_dispersion)
    )

  out
}

.model_dispersion <- function(model) {
  if (is.null(model)) return(NA_real_)
  pr <- stats::residuals(model, type = 'pearson')
  df <- stats::df.residual(model)
  if (!is.finite(df) || df <= 0) return(NA_real_)
  max(1, sum(pr^2) / df)
}

#' Add Prevalence QC Flags
#'
#' Adds prevalence-based anomaly flags using residuals and simple temporal rules.
#' The residual rule uses `pearson_resid_scaled`, the dispersion-adjusted
#' residual from [add_prevalence_predictions()], when present, and the raw
#' `pearson_resid` otherwise.
#'
#' @param data A data frame with prevalence and prediction columns.
#' @param resid_threshold Optional single absolute residual threshold overriding
#'   the size-dependent thresholds. Intended for sensitivity analysis.
#' @param resid_threshold_large Absolute residual threshold when tested is at
#'   least `resid_large_n`.
#' @param resid_threshold_small Absolute residual threshold below `resid_large_n`.
#' @param resid_large_n Tested-count boundary for residual thresholds.
#' @param all_negative_min_tested Minimum tested count for an all-negative flag.
#' @param all_negative_max_probability An all-negative month is flagged only when
#'   the model makes zero positives at least this improbable. The probability is
#'   `(1 - p_hat)^(tested / resid_dispersion)`: the binomial chance of no
#'   positives with the tested count deflated by the dispersion, so that
#'   overdispersed data, in which zeros are commoner, are not over-flagged. A row
#'   the model did not assess is not evaluated. Data without `p_hat` fall back to
#'   the count-only rule.
#' @param all_positive_min_tested Minimum tested count for an all-positive flag.
#' @param large_change_threshold Absolute month-to-month prevalence change threshold.
#'   A month whose predecessor is itself an extreme prevalence month
#'   (`flag_prevalence_extreme`) is not flagged for the change unless it is
#'   extreme too: the return from an outlier is not a second finding.
#' @param large_change_min_tested Minimum tested count required in both adjacent months.
#' @param large_change_require_adjacent Whether large-change comparisons require
#'   consecutive calendar months.
#' @param prevalence_low_extreme Lower bound for extreme observed prevalence.
#' @param prevalence_high_extreme Upper bound for extreme observed prevalence.
#' @param use_prevalence_bounds Whether raw prevalence bounds contribute to
#'   `flag_prevalence_extreme`. Disabled in the compatibility default.
#'
#' @return Data with prevalence QC flags appended.
#' @export
add_prevalence_qc_flags <- function(data,
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
                                    use_prevalence_bounds = FALSE) {
  .validate_required_columns(data, c("facility_id", "month_date", "tested", "positive", "prevalence", "pearson_resid"))

  if (!is.null(resid_threshold)) {
    resid_threshold_large <- resid_threshold
    resid_threshold_small <- resid_threshold
  }

  out <- dplyr::as_tibble(data)
  rule_resid <- if ('pearson_resid_scaled' %in% names(out)) out$pearson_resid_scaled else out$pearson_resid
  out$.rule_resid <- rule_resid
  # Chance of zero positives under the model. TRUE (always implausible) when
  # there is no model, which reproduces the count-only rule.
  out$.zero_implausible <- if ('p_hat' %in% names(out)) {
    dispersion <- if ('resid_dispersion' %in% names(out)) out$resid_dispersion else 1
    (1 - out$p_hat)^(out$tested / dispersion) < all_negative_max_probability
  } else {
    TRUE
  }

  out <- out %>%
    dplyr::group_by(facility_id) %>%
    dplyr::arrange(month_date, .by_group = TRUE) %>%
    dplyr::mutate(
      prev_lag = dplyr::lag(prevalence),
      tested_lag = dplyr::lag(tested),
      month_lag = dplyr::lag(month_date),
      monthly_prev_change = abs(prevalence - prev_lag),
      previous_month_is_adjacent = !is.na(month_lag) &
        (lubridate::year(month_date) * 12L + lubridate::month(month_date)) -
        (lubridate::year(month_lag) * 12L + lubridate::month(month_lag)) == 1L,
      residual_cutoff = dplyr::if_else(
        tested >= resid_large_n, resid_threshold_large, resid_threshold_small
      ),
      flag_resid_extreme = !is.na(.rule_resid) & abs(.rule_resid) > residual_cutoff,
      flag_all_negative_large_n = tested >= all_negative_min_tested & positive == 0 &
        .zero_implausible,
      flag_all_positive_large_n = tested >= all_positive_min_tested & positive == tested,
      flag_large_monthly_prevalence_change = !is.na(monthly_prev_change) &
        (!large_change_require_adjacent | previous_month_is_adjacent) &
        tested >= large_change_min_tested &
        tested_lag >= large_change_min_tested & monthly_prev_change > large_change_threshold,
      flag_raw_prevalence_bound = use_prevalence_bounds & !is.na(prevalence) &
        (prevalence <= prevalence_low_extreme | prevalence >= prevalence_high_extreme),
      flag_prevalence_extreme = (
        flag_raw_prevalence_bound |
          flag_resid_extreme |
          flag_all_negative_large_n |
          flag_all_positive_large_n
      ) %>% dplyr::coalesce(FALSE),
      # The month after an outlier differs from it by construction. Its change
      # is the outlier's, so it is flagged only if it is itself anomalous.
      flag_large_monthly_prevalence_change = flag_large_monthly_prevalence_change &
        !(dplyr::lag(flag_prevalence_extreme, default = FALSE) & !flag_prevalence_extreme)
    ) %>%
    dplyr::ungroup() %>%
    dplyr::select(-prev_lag, -tested_lag, -month_lag, -previous_month_is_adjacent,
                  -residual_cutoff, -monthly_prev_change, -.rule_resid, -.zero_implausible)

  out
}

.calculate_tested_baseline <- function(tested, month_date, mode, window_months) {
  month_index <- lubridate::year(month_date) * 12L + lubridate::month(month_date)
  baseline_n <- integer(length(tested))
  baseline_median <- baseline_mad <- rep(NA_real_, length(tested))

  for (i in seq_along(tested)) {
    if (is.na(month_index[i])) next
    distance <- month_index - month_index[i]
    eligible <- if (mode == 'trailing') {
      distance < 0L & distance >= -window_months
    } else {
      distance != 0L & abs(distance) <= window_months
    }
    values <- tested[eligible & !is.na(tested)]
    baseline_n[i] <- length(values)
    if (length(values) == 0L) next
    baseline_median[i] <- stats::median(values)
    baseline_mad[i] <- stats::mad(values, center = baseline_median[i], na.rm = TRUE)
  }

  dplyr::tibble(
    tested_baseline_n = baseline_n,
    tested_roll_median = baseline_median,
    tested_roll_mad = baseline_mad
  )
}

#' Add Tested Volume QC Flags
#'
#' Adds facility-level tested-count anomaly flags using a calendar-month rolling
#' median and MAD. A `trailing` baseline uses only earlier months and is suitable
#' for operational monitoring, but requires a warm-up period and may adapt slowly
#' after a real structural change. A `centered` baseline uses earlier and later
#' months and is intended for retrospective curation; it introduces look-ahead
#' and flags can change when later data are appended.
#'
#' @param data A prepared data frame.
#' @param baseline_mode Either `trailing` or `centered`.
#' @param baseline_window_months Calendar months before the row, and also after
#'   the row for a centered baseline.
#' @param baseline_uses_future Must agree with `baseline_mode`; recorded in output.
#' @param min_roll_n Minimum non-missing neighboring observations required.
#' @param tested_z_threshold Absolute robust z-score threshold.
#' @param tested_high_ratio High tested-to-median ratio threshold.
#' @param tested_low_ratio Low tested-to-median ratio threshold.
#' @param tested_jump_ratio Current-to-previous tested jump ratio threshold.
#' @param tested_drop_ratio Current-to-previous tested drop ratio threshold.
#' @param require_both_extreme Require both z-score and baseline-ratio evidence
#'   for high/low baseline flags. Recommended default is `TRUE`.
#' @param tested_high_min Minimum current tested count for a high flag.
#' @param baseline_low_min Minimum baseline median for a low flag.
#' @param baseline_zero_min Minimum baseline median for an unusual-zero flag.
#' @param tested_jump_min Minimum current tested count for a jump flag.
#' @param previous_jump_min Minimum previous tested count for a jump flag.
#' @param previous_drop_min Minimum previous tested count for a drop flag.
#'
#' @return Data with baseline metadata, component measurements, and flags appended.
#' @export
add_tested_volume_qc <- function(data,
                                 baseline_mode = c('trailing', 'centered'),
                                 baseline_window_months = 6,
                                 baseline_uses_future = identical(match.arg(baseline_mode), 'centered'),
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
                                 previous_drop_min = 20) {
  .validate_required_columns(data, c('facility_id', 'month_date', 'tested'))
  baseline_mode <- match.arg(baseline_mode)
  if (!identical(baseline_uses_future, baseline_mode == 'centered')) {
    rlang::abort('`baseline_uses_future` must agree with `baseline_mode`.')
  }

  out <- dplyr::as_tibble(data) %>%
    dplyr::group_by(facility_id) %>%
    dplyr::arrange(month_date, .by_group = TRUE) %>%
    dplyr::group_modify(~ dplyr::bind_cols(
      .x,
      .calculate_tested_baseline(.x$tested, .x$month_date, baseline_mode, baseline_window_months)
    )) %>%
    dplyr::ungroup() %>%
    dplyr::group_by(facility_id) %>%
    dplyr::arrange(month_date, .by_group = TRUE) %>%
    dplyr::mutate(tested_prev = dplyr::lag(tested)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(
      tested_baseline_mode = baseline_mode,
      baseline_uses_future = baseline_uses_future,
      tested_baseline_ready = tested_baseline_n >= min_roll_n,
      tested_mad_adjusted = dplyr::if_else(
        is.na(tested_roll_mad), NA_real_, pmax(tested_roll_mad, 1)
      ),
      tested_robust_z = .safe_divide(tested - tested_roll_median, tested_mad_adjusted),
      tested_to_roll_ratio = .safe_divide(tested, tested_roll_median),
      tested_to_prev_ratio = .safe_divide(tested, tested_prev),
      flag_tested_high_z = !is.na(tested_robust_z) & tested_robust_z > tested_z_threshold,
      flag_tested_high_ratio = !is.na(tested_to_roll_ratio) & tested_to_roll_ratio > tested_high_ratio,
      flag_tested_low_z = !is.na(tested_robust_z) & tested_robust_z < -tested_z_threshold,
      flag_tested_low_ratio = !is.na(tested_to_roll_ratio) & tested_to_roll_ratio < tested_low_ratio,
      flag_tested_extreme_high = tested_baseline_ready & tested >= tested_high_min &
        if (require_both_extreme) {
          flag_tested_high_z & flag_tested_high_ratio
        } else {
          flag_tested_high_z | flag_tested_high_ratio
        },
      flag_tested_extreme_low = tested_baseline_ready & tested_roll_median >= baseline_low_min &
        if (require_both_extreme) {
          flag_tested_low_z & flag_tested_low_ratio
        } else {
          flag_tested_low_z | flag_tested_low_ratio
        },
      flag_tested_zero_unusual = tested_baseline_ready & tested == 0 &
        tested_roll_median >= baseline_zero_min,
      flag_tested_large_jump = !is.na(tested_to_prev_ratio) & tested >= tested_jump_min &
        tested_prev >= previous_jump_min & tested_to_prev_ratio > tested_jump_ratio,
      flag_tested_large_drop = !is.na(tested_to_prev_ratio) & tested_prev >= previous_drop_min &
        tested_to_prev_ratio < tested_drop_ratio,
      flag_tested_volume_extreme = (
        flag_tested_extreme_high | flag_tested_extreme_low |
          flag_tested_zero_unusual | flag_tested_large_jump | flag_tested_large_drop
      ) %>% dplyr::coalesce(FALSE)
    )

  out
}

#' Add Temporal QC Flags
#'
#' Identifies isolated statistical extremes within each facility time series.
#'
#' @param data A data frame with statistical residual flags.
#' @param require_adjacent_months Whether both surrounding observations must be
#'   actual adjacent calendar months. The safer default is `TRUE`.
#'
#' @return Data with `flag_isolated_extreme` appended.
#' @export
add_temporal_qc_flags <- function(data, require_adjacent_months = TRUE) {
  .validate_required_columns(data, c('facility_id', 'month_date', 'flag_resid_extreme'))

  dplyr::as_tibble(data) %>%
    dplyr::group_by(facility_id) %>%
    dplyr::arrange(month_date, .by_group = TRUE) %>%
    dplyr::mutate(
      prev_extreme = dplyr::lag(flag_resid_extreme, default = FALSE),
      next_extreme = dplyr::lead(flag_resid_extreme, default = FALSE),
      prev_month_date = dplyr::lag(month_date),
      next_month_date = dplyr::lead(month_date),
      prev_month_is_adjacent = !is.na(prev_month_date) &
        (lubridate::year(month_date) * 12L + lubridate::month(month_date)) -
        (lubridate::year(prev_month_date) * 12L + lubridate::month(prev_month_date)) == 1L,
      next_month_is_adjacent = !is.na(next_month_date) &
        (lubridate::year(next_month_date) * 12L + lubridate::month(next_month_date)) -
        (lubridate::year(month_date) * 12L + lubridate::month(month_date)) == 1L,
      flag_temporal_context_insufficient = flag_resid_extreme &
        !(prev_month_is_adjacent & next_month_is_adjacent),
      flag_isolated_extreme = flag_resid_extreme & !prev_extreme & !next_extreme &
        (!require_adjacent_months | (prev_month_is_adjacent & next_month_is_adjacent))
    ) %>%
    dplyr::ungroup() %>%
    dplyr::select(-prev_extreme, -next_extreme, -prev_month_date, -next_month_date,
                  -prev_month_is_adjacent, -next_month_is_adjacent)
}

.append_qc_reason <- function(reason, flag, label) {
  active <- dplyr::coalesce(flag, FALSE)
  dplyr::if_else(active, dplyr::if_else(reason == '', label, paste0(reason, '; ', label)), reason)
}

#' Assign QC Action
#'
#' Applies a versioned action policy to factual QC flags. The conservative policy
#' authorizes exclusion only for impossible core tested/positive records. The
#' flags-only policy never authorizes exclusion.
#'
#' The action names the domain the evidence comes from: prevalence, tested
#' volume, attendance, or testing coverage, or several at once. The review
#' priority is set per flag by `priority`: a row takes the highest priority
#' among its active flags, and a row with evidence in two or more domains takes
#' at least the `multiple_signals` priority. Priority orders review; it never
#' authorizes exclusion.
#'
#' @param data A data frame with logical, prevalence, temporal, volume, and
#'   attendance flags.
#' @param policy Either `conservative_review` or `flags_only`.
#' @param policy_version Action-policy version. Only version 3 is accepted for
#'   new runs; see `docs/ACTION_POLICY.md` for what earlier versions did.
#' @param priority Named list of flag priorities, as in `qc_config()$priority`.
#'
#' @return Data with primary action, review priority, accumulated reasons, and
#'   explicit authorized-exclusion and review flags.
#' @export
assign_qc_action <- function(data,
                             policy = c('conservative_review', 'flags_only'),
                             policy_version = 3L,
                             priority = .default_qc_priority()) {
  policy <- match.arg(policy)
  .check_action_policy_version(policy_version)
  priority <- utils::modifyList(.default_qc_priority(), priority)
  out <- dplyr::as_tibble(data)

  flag_names <- c(
    'flag_core_invalid', 'flag_attendance_issue', 'flag_resid_extreme',
    'flag_all_negative_large_n', 'flag_all_positive_large_n',
    'flag_large_monthly_prevalence_change', 'flag_prevalence_extreme',
    'flag_tested_volume_extreme', 'flag_isolated_extreme',
    'flag_temporal_context_insufficient', 'flag_positive_gt_tested',
    'flag_zero_tested_positive', 'flag_tested_negative', 'flag_positive_negative',
    'flag_tested_extreme_high', 'flag_tested_extreme_low', 'flag_tested_zero_unusual',
    'flag_tested_large_jump', 'flag_tested_large_drop', 'flag_tested_gt_attending',
    'flag_tested_gt_attending_major', 'flag_attending_negative',
    'flag_coverage_low', 'flag_coverage_very_low'
  )
  for (nm in flag_names) {
    if (!nm %in% names(out)) out[[nm]] <- FALSE
  }
  on <- function(nm) dplyr::coalesce(as.logical(out[[nm]]), FALSE)

  prevalence_issue <- on('flag_resid_extreme') | on('flag_all_negative_large_n') |
    on('flag_all_positive_large_n') | on('flag_large_monthly_prevalence_change') |
    on('flag_prevalence_extreme')
  tested_issue <- on('flag_tested_volume_extreme')
  attendance_issue <- on('flag_attendance_issue')
  coverage_issue <- on('flag_coverage_low') | on('flag_coverage_very_low')
  core_invalid <- on('flag_core_invalid')
  n_domains <- as.integer(prevalence_issue) + as.integer(tested_issue) +
    as.integer(attendance_issue) + as.integer(coverage_issue)
  multiple <- n_domains >= 2L

  # Highest priority among active flags, as an index into the levels (1 =
  # critical); 5 means no flag.
  rank <- rep(5L, nrow(out))
  raise <- function(active, level) {
    rank <<- pmin(rank, ifelse(active, match(level, .qc_priority_levels), 5L))
  }
  for (key in setdiff(names(priority), 'multiple_signals')) {
    raise(on(paste0('flag_', key)), priority[[key]])
  }
  raise(core_invalid, 'critical')
  raise(multiple, priority$multiple_signals)
  # Data carrying only a domain summary flag, without the flags it summarises,
  # still gets a review priority.
  raise(rank == 5L & (prevalence_issue | tested_issue | attendance_issue | coverage_issue), 'medium')
  review_priority <- c(.qc_priority_levels, 'none')[rank]

  qc_action <- dplyr::case_when(
    core_invalid & policy == 'conservative_review' ~ 'exclude_core_invalid',
    core_invalid ~ 'review_core_invalid',
    multiple ~ 'review_multiple_signals',
    prevalence_issue ~ 'review_prevalence',
    tested_issue ~ 'review_tested_volume',
    attendance_issue ~ 'review_attendance',
    coverage_issue ~ 'review_coverage',
    TRUE ~ 'retain'
  )

  out <- out %>%
    dplyr::mutate(
      n_qc_signal_domains = n_domains,
      qc_action = qc_action,
      review_priority = review_priority,
      action_policy = policy,
      action_policy_version = policy_version,
      flag_exclude_authorized = qc_action == 'exclude_core_invalid',
      flag_review_recommended = startsWith(qc_action, 'review_'),
      flag_sensitivity = !core_invalid &
        (attendance_issue | prevalence_issue | tested_issue | coverage_issue),
      flag_any_qc_issue = qc_action != 'retain',
      flag_tested_only_issue = tested_issue & !prevalence_issue & !attendance_issue &
        !coverage_issue & !core_invalid,
      flag_prevalence_only_issue = prevalence_issue & !tested_issue & !attendance_issue &
        !coverage_issue & !core_invalid,
      flag_combined_tested_prevalence_issue = tested_issue & prevalence_issue & !core_invalid
    )

  qc_reason <- rep('', nrow(out))
  add <- function(active, label) qc_reason <<- .append_qc_reason(qc_reason, active, label)
  add(on('flag_positive_gt_tested'), 'more positive than tested')
  add(on('flag_zero_tested_positive'), 'positives with zero tested')
  add(on('flag_tested_negative') | on('flag_positive_negative'), 'negative count')
  add(on('flag_all_negative_large_n'), 'no positives where the model expects some')
  add(on('flag_all_positive_large_n'), 'every test positive')
  add(on('flag_resid_extreme') & on('flag_isolated_extreme'), 'prevalence outlier, single month')
  add(on('flag_resid_extreme') & !on('flag_isolated_extreme') & !on('flag_temporal_context_insufficient'),
      'prevalence outlier, run of months')
  add(on('flag_temporal_context_insufficient'), 'prevalence outlier at the edge of the series or a gap')
  add(on('flag_large_monthly_prevalence_change'), 'prevalence jumps between months')
  add(on('flag_tested_extreme_high'), 'tested far above baseline')
  add(on('flag_tested_extreme_low'), 'tested far below baseline')
  add(on('flag_tested_zero_unusual'), 'zero tested at a busy facility')
  add(on('flag_tested_large_jump'), 'tested jumps from previous month')
  add(on('flag_tested_large_drop'), 'tested drops from previous month')
  add(on('flag_tested_gt_attending_major'), 'tested well above attendance')
  add(on('flag_tested_gt_attending') & !on('flag_tested_gt_attending_major'), 'tested slightly above attendance')
  add(on('flag_attending_negative'), 'negative attendance')
  add(on('flag_coverage_very_low'), 'very low testing coverage')
  add(on('flag_coverage_low') & !on('flag_coverage_very_low'), 'low testing coverage')
  out$qc_reason <- dplyr::if_else(qc_reason == '', 'No QC issue detected', qc_reason)
  out
}

.summarise_qc_group <- function(data, group_vars = character(0)) {
  dat <- dplyr::as_tibble(data)

  needed <- c(
    'tested', 'positive', 'flag_any_qc_issue', 'flag_core_invalid',
    'flag_attendance_issue', 'flag_prevalence_extreme',
    'flag_tested_volume_extreme', 'flag_tested_only_issue',
    'flag_prevalence_only_issue', 'flag_combined_tested_prevalence_issue',
    'flag_exclude_authorized', 'flag_review_recommended', 'flag_sensitivity'
  )

  for (nm in needed) {
    if (!nm %in% names(dat)) {
      dat[[nm]] <- if (nm %in% c("tested", "positive")) NA_real_ else FALSE
    }
  }

  group_syms <- rlang::syms(group_vars)

  # Prevalence is taken over rows carrying both counts. A row with tested but no
  # positive would otherwise add to the denominator and not the numerator.
  dat$.complete_pair <- !is.na(dat$tested) & !is.na(dat$positive)
  dat$.kept_pair <- dat$.complete_pair & !dplyr::coalesce(dat$flag_exclude_authorized, FALSE)

  dat %>%
    dplyr::group_by(!!!group_syms) %>%
    dplyr::summarise(
      total_rows = dplyr::n(),
      incomplete_count_rows = sum(!.complete_pair),
      n_any_qc_issue = sum(flag_any_qc_issue, na.rm = TRUE),
      pct_any_qc_issue = 100 * n_any_qc_issue / total_rows,
      core_invalid_rows = sum(flag_core_invalid, na.rm = TRUE),
      attendance_issue_rows = sum(flag_attendance_issue, na.rm = TRUE),
      prevalence_extreme_rows = sum(flag_prevalence_extreme, na.rm = TRUE),
      tested_volume_extreme_rows = sum(flag_tested_volume_extreme, na.rm = TRUE),
      tested_only_issues = sum(flag_tested_only_issue, na.rm = TRUE),
      prevalence_only_issues = sum(flag_prevalence_only_issue, na.rm = TRUE),
      combined_tested_prevalence_issues = sum(flag_combined_tested_prevalence_issue, na.rm = TRUE),
      authorized_exclusion_rows = sum(flag_exclude_authorized, na.rm = TRUE),
      review_recommended_rows = sum(flag_review_recommended, na.rm = TRUE),
      sensitivity_flags = sum(flag_sensitivity, na.rm = TRUE),
      total_tested_before_qc = sum(tested, na.rm = TRUE),
      total_positive_before_qc = sum(positive, na.rm = TRUE),
      total_tested_after_authorized_exclusions = sum(dplyr::if_else(flag_exclude_authorized, 0, tested), na.rm = TRUE),
      total_positive_after_authorized_exclusions = sum(dplyr::if_else(flag_exclude_authorized, 0, positive), na.rm = TRUE),
      prevalence_before_qc = .safe_divide(
        sum(positive[.complete_pair]),
        sum(tested[.complete_pair])
      ),
      prevalence_after_authorized_exclusions = .safe_divide(
        sum(positive[.kept_pair]),
        sum(tested[.kept_pair])
      ),
      .groups = "drop"
    )
}

#' Summarise QC By Region
#'
#' @param data A QC-flagged data frame.
#'
#' @return Region-level QC summary table.
#' @export
summarise_qc_by_region <- function(data) {
  .validate_required_columns(data, "region")
  .summarise_qc_group(data, group_vars = c("region"))
}

#' Summarise QC By Month
#'
#' @param data A QC-flagged data frame.
#'
#' @return Month-level QC summary table.
#' @export
summarise_qc_by_month <- function(data) {
  .validate_required_columns(data, "month_date")
  .summarise_qc_group(data, group_vars = c("month_date"))
}

#' Summarise QC By Facility
#'
#' @param data A QC-flagged data frame.
#'
#' @return Facility-level QC summary table.
#' @export
summarise_qc_by_facility <- function(data) {
  .validate_required_columns(data, "facility_id")
  .summarise_qc_group(data, group_vars = c("facility_id"))
}

#' Summarise QC By Council
#'
#' @param data A QC-flagged data frame containing `council`.
#'
#' @return Council-level QC summary table.
#' @export
summarise_qc_by_council <- function(data) {
  .validate_required_columns(data, "council")
  .summarise_qc_group(data, group_vars = c("council"))
}

#' Summarise QC By District
#'
#' @param data A QC-flagged data frame containing `district`.
#'
#' @return District-level QC summary table.
#' @export
summarise_qc_by_district <- function(data) {
  .validate_required_columns(data, "district")
  .summarise_qc_group(data, group_vars = c("district"))
}

#' Summarise Before/After QC
#'
#' Summarises prevalence and volume before and after policy-authorized exclusions.
#'
#' @param data A QC-flagged data frame.
#' @param by One of `overall`, `region`, `council`, `district`, `month`.
#'
#' @return A summary tibble.
#' @export
summarise_before_after_qc <- function(data, by = c("overall", "region", "council", "district", "month")) {
  by <- match.arg(by)

  if (by == "overall") {
    return(.summarise_qc_group(data, group_vars = character(0)))
  }

  group_var <- switch(
    by,
    region = "region",
    council = "council",
    district = "district",
    month = "month_date"
  )

  .validate_required_columns(data, group_var)
  .summarise_qc_group(data, group_vars = group_var)
}

#' Run Threshold Sensitivity
#'
#' Re-runs selected prevalence and tested QC thresholds over a grid and returns
#' grouped summaries.
#'
#' @param data A data frame with prepared/predicted columns.
#' @param resid_thresholds Vector of residual thresholds.
#' @param tested_z_thresholds Vector of tested robust-z thresholds.
#' @param by One of `region`, `council`, `district`, or `month`.
#'
#' @return A tibble with threshold combinations and summary metrics.
#' @export
run_threshold_sensitivity <- function(data,
                                      resid_thresholds = c(3, 4, 5),
                                      tested_z_thresholds = c(4, 5, 6),
                                      by = c("region", "council", "district", "month")) {
  by <- match.arg(by)

  grid <- tidyr::crossing(
    resid_threshold = resid_thresholds,
    tested_z_threshold = tested_z_thresholds
  )

  purrr::map2_dfr(
    grid$resid_threshold,
    grid$tested_z_threshold,
    function(r_thr, z_thr) {
      tmp <- data %>%
        add_prevalence_qc_flags(resid_threshold = r_thr) %>%
        add_tested_volume_qc(tested_z_threshold = z_thr) %>%
        add_temporal_qc_flags() %>%
        assign_qc_action()

      summarise_before_after_qc(tmp, by = by) %>%
        dplyr::mutate(
          resid_threshold = r_thr,
          tested_z_threshold = z_thr,
          .before = 1
        )
    }
  )
}

#' Simulate QC Data
#'
#' Generates synthetic facility-month ANC malaria testing data with injected
#' anomalies for testing and examples.
#'
#' @param n_facilities Number of facilities.
#' @param n_months Number of months.
#' @param start_month Start month as date-like string.
#' @param seed Random seed.
#' @param zero_tested_fraction Approximate proportion of facility-months to
#'   report as zero tested, for exercising reporting-consistency measurement.
#'   Zero, the default, injects nothing and draws no random numbers.
#' @param absent_month_fraction Approximate proportion of facility-months to
#'   remove entirely, creating interior reporting gaps. A facility first and
#'   last month are never removed, so each reporting window is preserved. Zero,
#'   the default, injects nothing and draws no random numbers.
#'
#' @return A tibble with simulated routine data and truth labels.
#' @export
simulate_qc_data <- function(n_facilities = 40,
                             n_months = 24,
                             start_month = "2022-01-01",
                             seed = 123,
                             zero_tested_fraction = 0,
                             absent_month_fraction = 0) {
  for (nm in c("zero_tested_fraction", "absent_month_fraction")) {
    value <- get(nm)
    if (!.is_scalar_number(value) || value < 0 || value > 1) {
      rlang::abort(paste0("`", nm, "` must be one number between 0 and 1."))
    }
  }
  set.seed(seed)

  facilities <- paste0("F", sprintf("%03d", seq_len(n_facilities)))
  months <- seq.Date(as.Date(start_month), by = "month", length.out = n_months)

  base <- tidyr::crossing(
    facility = facilities,
    month = months
  ) %>%
    dplyr::mutate(
      region = sample(paste0("Region_", LETTERS[1:4]), dplyr::n(), replace = TRUE),
      district = sample(paste0("District_", seq_len(8)), dplyr::n(), replace = TRUE),
      council = sample(paste0("Council_", seq_len(12)), dplyr::n(), replace = TRUE)
    )

  facility_effect <- stats::rnorm(n_facilities, 0, 0.4)
  names(facility_effect) <- facilities

  dat <- base %>%
    dplyr::mutate(
      month_num = lubridate::month(month),
      season = sin(2 * pi * month_num / 12),
      tested = pmax(0, round(stats::rnorm(dplyr::n(), mean = 120 + 20 * season, sd = 25))),
      lp = -1.2 + 0.4 * season + facility_effect[facility],
      p = stats::plogis(lp),
      positive = stats::rbinom(dplyr::n(), size = pmax(tested, 0), prob = p),
      injected_error = FALSE
    ) %>%
    dplyr::select(facility, region, district, council, month, tested, positive, injected_error)

  n <- nrow(dat)
  idx1 <- sample(seq_len(n), size = max(5, round(0.01 * n)))
  idx2 <- sample(setdiff(seq_len(n), idx1), size = max(5, round(0.01 * n)))
  idx3 <- sample(setdiff(seq_len(n), c(idx1, idx2)), size = max(5, round(0.01 * n)))

  dat$positive[idx1] <- dat$tested[idx1] + sample(1:20, length(idx1), replace = TRUE)
  dat$tested[idx2] <- dat$tested[idx2] * sample(c(5, 6), length(idx2), replace = TRUE)
  dat$positive[idx3] <- 0

  dat$injected_error[c(idx1, idx2, idx3)] <- TRUE

  if (zero_tested_fraction > 0) {
    candidates <- setdiff(seq_len(n), c(idx1, idx2, idx3))
    size <- min(length(candidates), round(zero_tested_fraction * n))
    if (size > 0L) {
      idx_zero <- sample(candidates, size = size)
      dat$tested[idx_zero] <- 0
      dat$positive[idx_zero] <- 0
    }
  }

  if (absent_month_fraction > 0) {
    interior <- which(
      duplicated(dat$facility) & duplicated(dat$facility, fromLast = TRUE)
    )
    size <- min(length(interior), round(absent_month_fraction * n))
    if (size > 0L) {
      dat <- dat[-sample(interior, size = size), , drop = FALSE]
    }
  }

  dplyr::as_tibble(dat)
}

#' Evaluate QC Against Truth
#'
#' Computes simple classification metrics comparing QC flags against known truth.
#'
#' @param data Data containing truth and QC flag columns.
#' @param truth_var Name of binary truth column.
#' @param flag_var Name of binary QC flag column.
#'
#' @return A one-row tibble with confusion matrix counts and metrics.
#' @export
evaluate_qc_against_truth <- function(data,
                                      truth_var = "injected_error",
                                      flag_var = "flag_any_qc_issue") {
  .validate_required_columns(data, c(truth_var, flag_var))

  truth <- as.logical(data[[truth_var]])
  flag <- as.logical(data[[flag_var]])

  tp <- sum(flag & truth, na.rm = TRUE)
  fp <- sum(flag & !truth, na.rm = TRUE)
  fn <- sum(!flag & truth, na.rm = TRUE)
  tn <- sum(!flag & !truth, na.rm = TRUE)

  precision <- .safe_divide(tp, tp + fp)
  recall <- .safe_divide(tp, tp + fn)
  specificity <- .safe_divide(tn, tn + fp)
  f1 <- .safe_divide(2 * precision * recall, precision + recall)

  dplyr::tibble(
    tp = tp,
    fp = fp,
    fn = fn,
    tn = tn,
    precision = precision,
    recall = recall,
    specificity = specificity,
    f1 = f1
  )
}

#' Plot Prevalence Residuals
#'
#' @param data A QC data frame with `pearson_resid` and `month_date`.
#'
#' @return A ggplot object.
#' @export
plot_prevalence_residuals <- function(data) {
  .validate_required_columns(data, c("month_date", "pearson_resid"))

  ggplot2::ggplot(data, ggplot2::aes(x = month_date, y = pearson_resid)) +
    ggplot2::geom_hline(yintercept = c(-4, 4), linetype = "dashed", color = "red") +
    ggplot2::geom_point(alpha = 0.5) +
    ggplot2::theme_minimal() +
    ggplot2::labs(x = "Month", y = "Pearson residual", title = "Prevalence residual diagnostics")
}

#' Plot Tested Robust Z
#'
#' @param data A QC data frame with `tested_robust_z` and `month_date`.
#'
#' @return A ggplot object.
#' @export
plot_tested_robust_z <- function(data) {
  .validate_required_columns(data, c("month_date", "tested_robust_z"))

  ggplot2::ggplot(data, ggplot2::aes(x = month_date, y = tested_robust_z)) +
    ggplot2::geom_hline(yintercept = c(-5, 5), linetype = "dashed", color = "red") +
    ggplot2::geom_point(alpha = 0.5) +
    ggplot2::theme_minimal() +
    ggplot2::labs(x = "Month", y = "Tested robust z", title = "Tested count robust-z diagnostics")
}

#' Plot Facility Time Series
#'
#' Plots observed prevalence for one facility with the stored GAM expectation,
#' when available. Red points identify rows with any QC issue.
#'
#' @param data A QC data frame.
#' @param facility_id Facility identifier value to filter.
#'
#' @return A ggplot object.
#' @export
plot_facility_timeseries <- function(data, facility_id) {
  .validate_required_columns(data, c("facility_id", "month_date", "prevalence"))

  dat <- dplyr::as_tibble(data) %>%
    dplyr::filter(.data$facility_id == .env$facility_id)

  if (!"flag_any_qc_issue" %in% names(dat)) {
    dat$flag_any_qc_issue <- FALSE
  }

  if (!'p_hat' %in% names(dat)) dat$p_hat <- NA_real_
  flagged <- dplyr::coalesce(dat$flag_any_qc_issue, FALSE)

  ggplot2::ggplot(dat, ggplot2::aes(x = month_date)) +
    ggplot2::geom_line(
      ggplot2::aes(y = prevalence, color = 'Observed'), linewidth = 0.7,
      na.rm = TRUE
    ) +
    ggplot2::geom_point(
      ggplot2::aes(y = prevalence, color = 'Observed'), size = 1.6,
      na.rm = TRUE
    ) +
    ggplot2::geom_line(
      ggplot2::aes(y = p_hat, color = 'Model fit'), linewidth = 0.8,
      linetype = 'dashed', na.rm = TRUE
    ) +
    ggplot2::geom_point(
      data = dat[flagged, , drop = FALSE],
      ggplot2::aes(y = prevalence), color = '#e31a1c', size = 2.4,
      na.rm = TRUE
    ) +
    ggplot2::theme_minimal() +
    ggplot2::scale_color_manual(
      values = c('Observed' = '#1f78b4', 'Model fit' = '#ff7f00')
    ) +
    ggplot2::labs(
      x = 'Month', y = 'Prevalence', color = NULL,
      title = paste('Facility', facility_id, 'observed and expected prevalence'),
      subtitle = 'Dashed orange: model fit; red points: rows with any QC issue'
    )
}

#' Plot Before/After Prevalence
#'
#' @param data A QC data frame with `flag_exclude_authorized`.
#'
#' @return A ggplot object.
#' @export
plot_before_after_prevalence <- function(data) {
  .validate_required_columns(data, c('tested', 'positive', 'flag_exclude_authorized'))

  before <- .safe_divide(sum(data$positive, na.rm = TRUE), sum(data$tested, na.rm = TRUE))

  keep <- dplyr::as_tibble(data) %>%
    dplyr::filter(!flag_exclude_authorized)

  after <- .safe_divide(sum(keep$positive, na.rm = TRUE), sum(keep$tested, na.rm = TRUE))

  plot_dat <- dplyr::tibble(
    state = c('Source data', 'After authorized exclusions'),
    prevalence = c(before, after)
  )

  ggplot2::ggplot(plot_dat, ggplot2::aes(x = state, y = prevalence, fill = state)) +
    ggplot2::geom_col(width = 0.6) +
    ggplot2::theme_minimal() +
    ggplot2::guides(fill = "none") +
    ggplot2::labs(x = NULL, y = 'Prevalence', title = 'Prevalence after authorized exclusions')
}

#' Plot QC Summary By Region
#'
#' @param summary_data Output from [summarise_qc_by_region()].
#'
#' @return A ggplot object.
#' @export
plot_qc_summary_by_region <- function(summary_data) {
  .validate_required_columns(summary_data, c("region", "pct_any_qc_issue"))

  ggplot2::ggplot(summary_data, ggplot2::aes(x = stats::reorder(region, pct_any_qc_issue), y = pct_any_qc_issue)) +
    ggplot2::geom_col(fill = "#33a02c") +
    ggplot2::coord_flip() +
    ggplot2::theme_minimal() +
    ggplot2::labs(x = "Region", y = "% rows with any QC issue", title = "QC burden by region")
}

#' Run Routine QC Pipeline
#'
#' Main user-facing wrapper that prepares data, fits prevalence model, applies QC
#' flags, and returns flagged data plus standard summaries.
#'
#' @param raw_data Raw routine surveillance data.
#' @param facility_var Facility identifier column.
#' @param region_var Region column.
#' @param month_var Month/date column.
#' @param tested_var Tested count column.
#' @param positive_var Positive count column.
#' @param attending_var Optional attendance denominator column.
#' @param attendance_definition A non-empty scalar definition required with
#'   `attending_var`.
#' @param council_var Optional council column.
#' @param district_var Optional district column.
#' @param config A validated configuration created by [qc_config()].
#' @param provenance A flat named list of safe upstream identifiers. Credentials,
#'   secrets, tokens, keys, local paths, and raw records must not be supplied.
#' @param nthreads Threads passed to [fit_prevalence_gam()].
#' @param ... Additional arguments reserved for future extensions.
#'
#' @return A validated `routineqc_run` containing flagged data, model, summaries,
#'   configuration, and a reproducibility manifest.
#' @export
run_routine_qc <- function(raw_data,
                           facility_var,
                           region_var,
                           month_var,
                           tested_var,
                           positive_var,
                           attending_var = NULL,
                           attendance_definition = NULL,
                           council_var = NULL,
                           district_var = NULL,
                           config = qc_config(),
                           provenance = list(),
                           nthreads = 1,
                           ...) {
  validate_qc_config(config)

  prepared <- prepare_qc_data(
    data = raw_data,
    facility_var = facility_var,
    region_var = region_var,
    month_var = month_var,
    tested_var = tested_var,
    positive_var = positive_var,
    attending_var = attending_var,
    attendance_definition = attendance_definition,
    council_var = council_var,
    district_var = district_var
  )

  validate_qc_input(prepared)

  dat <- prepared %>%
    flag_logical_errors()
  dat <- do.call(add_attendance_qc_flags, c(list(data = dat), config$attendance))

  model <- fit_prevalence_gam(dat, nthreads = nthreads)

  dat <- add_prevalence_predictions(
    dat, model = model,
    min_prediction_coverage = config$model$min_prediction_coverage
  )
  dat <- do.call(add_prevalence_qc_flags, c(list(data = dat), config$prevalence))
  dat <- do.call(add_tested_volume_qc, c(list(data = dat), config$tested_volume))
  dat <- do.call(add_temporal_qc_flags, c(list(data = dat), config$temporal))
  dat <- assign_qc_action(
    dat,
    policy = config$action_policy$name,
    policy_version = config$action_policy$version,
    priority = config$priority
  )

  summaries <- list(by_region = summarise_qc_by_region(dat))

  if ("council" %in% names(dat)) {
    summaries$by_council <- summarise_qc_by_council(dat)
  }

  if ("district" %in% names(dat)) {
    summaries$by_district <- summarise_qc_by_district(dat)
  }

  summaries$by_month <- summarise_qc_by_month(dat)
  summaries$by_facility <- summarise_qc_by_facility(dat)
  summaries$by_facility_reporting <- summarise_qc_reporting_consistency(dat)
  for (area in intersect(c('district', 'council'), names(dat))) {
    summaries[[paste0('count_completeness_by_', area)]] <-
      summarise_qc_count_completeness(dat, by = area)
  }

  .new_qc_run(
    data_flagged = dat,
    model = model,
    summaries = summaries,
    config = config,
    input_data = prepared,
    provenance = provenance
  )
}
