.qc_month_index <- function(x) {
  as.integer(lubridate::year(x)) * 12L + as.integer(lubridate::month(x))
}

.dataset_month_window <- function(month_date) {
  month_date <- lubridate::floor_date(month_date, unit = 'month')
  month_date <- month_date[!is.na(month_date)]
  if (length(month_date) == 0L) {
    rlang::abort('`month_date` contains no non-missing reporting months.')
  }
  index <- .qc_month_index(month_date)
  list(
    start = min(month_date),
    end = max(month_date),
    months = as.integer(max(index) - min(index) + 1L)
  )
}

.facility_consistency <- function(dat, dataset_months) {
  monthly <- dplyr::tibble(
    month_date = lubridate::floor_date(dat$month_date, unit = 'month'),
    tested = suppressWarnings(as.numeric(dat$tested))
  )
  monthly <- monthly[!is.na(monthly$month_date), , drop = FALSE]

  monthly <- monthly %>%
    dplyr::group_by(month_date) %>%
    dplyr::summarise(
      any_testing = any(!is.na(tested) & tested > 0),
      all_missing = all(is.na(tested)),
      .groups = 'drop'
    ) %>%
    dplyr::arrange(month_date)

  first_reported <- min(monthly$month_date)
  last_reported <- max(monthly$month_date)
  grid <- seq.Date(first_reported, last_reported, by = 'month')
  months_in_window <- length(grid)

  tested_months <- monthly$month_date[monthly$any_testing]
  has_testing <- grid %in% tested_months

  months_reported <- nrow(monthly)
  months_with_testing <- length(tested_months)
  months_missing_tested <- sum(monthly$all_missing)
  months_zero_tested <- months_reported - months_with_testing - months_missing_tested
  months_absent <- months_in_window - months_reported
  months_without_testing <- months_in_window - months_with_testing

  runs <- rle(!has_testing)
  gap_positions <- which(runs$values)
  n_gaps <- length(gap_positions)
  if (n_gaps > 0L) {
    run_end <- cumsum(runs$lengths)
    run_start <- run_end - runs$lengths + 1L
    longest <- gap_positions[which.max(runs$lengths[gap_positions])]
    longest_gap_months <- as.integer(runs$lengths[longest])
    longest_gap_start <- grid[run_start[longest]]
    longest_gap_end <- grid[run_end[longest]]
  } else {
    longest_gap_months <- 0L
    longest_gap_start <- as.Date(NA)
    longest_gap_end <- as.Date(NA)
  }

  dplyr::tibble(
    first_month_reported = first_reported,
    last_month_reported = last_reported,
    first_month_tested = if (months_with_testing > 0L) min(tested_months) else as.Date(NA),
    last_month_tested = if (months_with_testing > 0L) max(tested_months) else as.Date(NA),
    months_in_facility_window = as.integer(months_in_window),
    months_reported = as.integer(months_reported),
    months_absent = as.integer(months_absent),
    months_zero_tested = as.integer(months_zero_tested),
    months_missing_tested = as.integer(months_missing_tested),
    months_with_testing = as.integer(months_with_testing),
    months_without_testing = as.integer(months_without_testing),
    prop_without_testing_facility_window = months_without_testing / months_in_window,
    prop_with_testing_dataset_window = months_with_testing / dataset_months,
    prop_reported_dataset_window = months_reported / dataset_months,
    longest_gap_months = longest_gap_months,
    n_gaps = as.integer(n_gaps),
    longest_gap_start = longest_gap_start,
    longest_gap_end = longest_gap_end,
    never_tested = months_with_testing == 0L
  )
}

#' Reporting-Consistency Strictness Levels
#'
#' Returns the named strictness presets used to judge whether a facility reports
#' consistently enough to enter a review cohort. Levels are ordered from no
#' filtering to the most severe requirement, and `NA` means a criterion is not
#' applied. These presets select facilities for review. They never authorize
#' exclusion of a record and never assign a QC action.
#'
#' @return A tibble with one row per level and one column per criterion.
#' @export
qc_consistency_levels <- function() {
  dplyr::tibble(
    level = c(
      'all', 'any_testing', 'lenient', 'moderate', 'near_complete',
      'complete', 'complete_panel'
    ),
    max_prop_without_testing = c(NA, NA, 0.5, 0.2, 0.05, 0, 0),
    max_gap_months = c(NA, NA, 3, 2, 1, 0, 0),
    min_months_with_testing = c(NA, 1, 1, 1, 1, 1, 1),
    min_prop_with_testing_dataset_window = c(NA, NA, 0.25, 0.5, 0.75, NA, 1)
  )
}

.is_scalar_threshold <- function(x) {
  is.numeric(x) && length(x) == 1L && !is.na(x)
}

.resolve_consistency_thresholds <- function(strictness,
                                            max_prop_without_testing = NULL,
                                            max_gap_months = NULL,
                                            min_months_with_testing = NULL,
                                            min_prop_with_testing_dataset_window = NULL) {
  levels <- qc_consistency_levels()
  if (!is.character(strictness) || length(strictness) != 1L || is.na(strictness) ||
      !strictness %in% levels$level) {
    rlang::abort(paste0(
      '`strictness` must be one of: ', paste(levels$level, collapse = ', '), '.'
    ))
  }
  preset <- levels[levels$level == strictness, , drop = FALSE]
  thresholds <- list(
    max_prop_without_testing = preset$max_prop_without_testing,
    max_gap_months = preset$max_gap_months,
    min_months_with_testing = preset$min_months_with_testing,
    min_prop_with_testing_dataset_window = preset$min_prop_with_testing_dataset_window
  )
  overrides <- list(
    max_prop_without_testing = max_prop_without_testing,
    max_gap_months = max_gap_months,
    min_months_with_testing = min_months_with_testing,
    min_prop_with_testing_dataset_window = min_prop_with_testing_dataset_window
  )
  for (nm in names(overrides)) {
    value <- overrides[[nm]]
    if (is.null(value)) next
    if (!.is_scalar_threshold(value)) {
      rlang::abort(paste0('`', nm, '` must be NULL or one non-missing number.'))
    }
    if (value < 0) {
      rlang::abort(paste0('`', nm, '` must be non-negative.'))
    }
    thresholds[[nm]] <- value
  }
  thresholds$strictness <- strictness
  thresholds
}

.consistency_pass <- function(consistency, thresholds) {
  required <- c(
    'prop_without_testing_facility_window', 'longest_gap_months',
    'months_with_testing', 'prop_with_testing_dataset_window'
  )
  .validate_required_columns(consistency, required)
  pass <- rep(TRUE, nrow(consistency))
  if (!is.na(thresholds$max_prop_without_testing)) {
    pass <- pass &
      consistency$prop_without_testing_facility_window <= thresholds$max_prop_without_testing
  }
  if (!is.na(thresholds$max_gap_months)) {
    pass <- pass & consistency$longest_gap_months <= thresholds$max_gap_months
  }
  if (!is.na(thresholds$min_months_with_testing)) {
    pass <- pass & consistency$months_with_testing >= thresholds$min_months_with_testing
  }
  if (!is.na(thresholds$min_prop_with_testing_dataset_window)) {
    pass <- pass &
      consistency$prop_with_testing_dataset_window >= thresholds$min_prop_with_testing_dataset_window
  }
  pass
}

#' Summarise Facility Reporting Consistency
#'
#' Measures how consistently each facility reports testing activity. A month has
#' testing when a row exists for that facility-month and `tested` is greater than
#' zero. Months with no row at all, months reported as zero or non-positive, and
#' months whose `tested` value is missing all count as months without testing,
#' and are also reported separately so the underlying evidence stays visible.
#'
#' Gap length is measured within each facility own reporting window, from its
#' first to its last reported month. Proportions are reported against both that
#' window and the dataset-wide month span, so a facility with only a few months
#' of history can be identified even when those months are internally complete.
#'
#' This function describes reporting behavior only. It does not assign a QC
#' action, set a row-level flag, authorize exclusion, or drop records.
#'
#' @param data A QC data frame with `facility_id`, `month_date`, and `tested`.
#' @param strictness A level name from [qc_consistency_levels()].
#' @param max_prop_without_testing Optional override for the maximum proportion
#'   of months without testing within a facility own reporting window.
#' @param max_gap_months Optional override for the longest permitted run of
#'   consecutive months without testing.
#' @param min_months_with_testing Optional override for the minimum number of
#'   months with testing.
#' @param min_prop_with_testing_dataset_window Optional override for the minimum
#'   proportion of the dataset-wide month span in which a facility must have
#'   testing. This criterion is scale-free, so it identifies a facility with a
#'   short history even when that history is internally complete.
#'
#' @return A tibble with one row per facility, its reporting-consistency
#'   measurements, and `passes_strictness`.
#' @export
summarise_qc_reporting_consistency <- function(data,
                                               strictness = 'all',
                                               max_prop_without_testing = NULL,
                                               max_gap_months = NULL,
                                               min_months_with_testing = NULL,
                                               min_prop_with_testing_dataset_window = NULL) {
  .validate_required_columns(data, c('facility_id', 'month_date', 'tested'))
  thresholds <- .resolve_consistency_thresholds(
    strictness, max_prop_without_testing, max_gap_months,
    min_months_with_testing, min_prop_with_testing_dataset_window
  )

  dat <- dplyr::as_tibble(data)
  if (nrow(dat) == 0L) {
    rlang::abort('Reporting consistency requires at least one row.')
  }
  dat <- dat[!is.na(dat$facility_id) & !is.na(dat$month_date), , drop = FALSE]
  if (nrow(dat) == 0L) {
    rlang::abort('Reporting consistency requires rows with a facility and month.')
  }
  window <- .dataset_month_window(dat$month_date)

  out <- dat %>%
    dplyr::select(facility_id, month_date, tested) %>%
    dplyr::group_by(facility_id) %>%
    dplyr::group_modify(~ .facility_consistency(.x, window$months)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(
      months_in_dataset_window = window$months,
      dataset_month_min = window$start,
      dataset_month_max = window$end
    ) %>%
    dplyr::arrange(facility_id)

  out$passes_strictness <- .consistency_pass(out, thresholds)
  out
}

#' Filter a Review Cohort by Reporting Consistency
#'
#' Keeps every row belonging to facilities that meet the selected
#' reporting-consistency strictness, and removes no rows from a facility that
#' passes. This is display-only cohort selection for review: it never changes a
#' stored run, never authorizes exclusion, and never edits an observation.
#' Excluding a facility here means its records were not reviewed under the
#' selected cohort, not that its records are wrong.
#'
#' @param data A QC data frame with `facility_id`, `month_date`, and `tested`.
#' @param strictness A level name from [qc_consistency_levels()].
#' @param max_prop_without_testing Optional threshold override.
#' @param max_gap_months Optional threshold override.
#' @param min_months_with_testing Optional threshold override.
#' @param min_prop_with_testing_dataset_window Optional threshold override.
#' @param consistency Optional precomputed output of
#'   [summarise_qc_reporting_consistency()] used as a measurement cache. The
#'   strictness decision is always recomputed from the supplied thresholds.
#'
#' @return A filtered tibble preserving source columns and row order.
#' @export
filter_qc_facilities <- function(data,
                                 strictness = 'all',
                                 max_prop_without_testing = NULL,
                                 max_gap_months = NULL,
                                 min_months_with_testing = NULL,
                                 min_prop_with_testing_dataset_window = NULL,
                                 consistency = NULL) {
  .validate_required_columns(data, 'facility_id')
  thresholds <- .resolve_consistency_thresholds(
    strictness, max_prop_without_testing, max_gap_months,
    min_months_with_testing, min_prop_with_testing_dataset_window
  )
  out <- dplyr::as_tibble(data)

  if (is.null(consistency)) {
    consistency <- summarise_qc_reporting_consistency(
      out, strictness = strictness,
      max_prop_without_testing = max_prop_without_testing,
      max_gap_months = max_gap_months,
      min_months_with_testing = min_months_with_testing,
      min_prop_with_testing_dataset_window = min_prop_with_testing_dataset_window
    )
  } else {
    consistency <- dplyr::as_tibble(consistency)
    .validate_required_columns(consistency, 'facility_id')
  }

  keep_facilities <- consistency$facility_id[.consistency_pass(consistency, thresholds)]
  out[as.character(out$facility_id) %in% as.character(keep_facilities), , drop = FALSE]
}

.consistency_gap_levels <- c('0', '1', '2-3', '4-6', '>6')

.consistency_gap_buckets <- function(consistency) {
  .validate_required_columns(consistency, 'longest_gap_months')
  buckets <- cut(
    consistency$longest_gap_months,
    breaks = c(-Inf, 0, 1, 3, 6, Inf),
    labels = .consistency_gap_levels
  )
  counts <- as.data.frame(table(factor(buckets, levels = .consistency_gap_levels)))
  names(counts) <- c('gap_bucket', 'facilities')
  dplyr::as_tibble(counts) %>%
    dplyr::mutate(
      gap_bucket = factor(as.character(gap_bucket), levels = .consistency_gap_levels),
      proportion = if (nrow(consistency) > 0L) facilities / nrow(consistency) else NA_real_
    )
}

#' Plot Reporting-Consistency Gaps
#'
#' Summarises facilities by their longest run of consecutive months without
#' testing. The chart describes reporting behavior and implies no QC action.
#'
#' @param consistency Output from [summarise_qc_reporting_consistency()].
#'
#' @return A ggplot object.
#' @export
plot_reporting_consistency <- function(consistency) {
  buckets <- .consistency_gap_buckets(consistency)

  ggplot2::ggplot(buckets, ggplot2::aes(x = gap_bucket, y = facilities)) +
    ggplot2::geom_col(fill = '#2c7fb8', width = 0.7) +
    ggplot2::theme_minimal() +
    ggplot2::labs(
      x = 'Longest gap without testing (months)',
      y = 'Number of facilities',
      title = 'Facility reporting consistency',
      subtitle = 'Gaps are measured within each facility own reporting window'
    )
}
