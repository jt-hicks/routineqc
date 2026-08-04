#' Validate Canonical QC Input
#'
#' Validates structural requirements for canonical facility-month QC data.
#' Observation-level count problems such as negative values or `positive > tested`
#' are deliberately retained for the logical QC protocol.
#'
#' @param data A data frame using the canonical QC column names.
#' @param duplicate_action Either `error` (default) or `allow` for duplicate
#'   facility-month keys. Upstream adapters should normally resolve duplicates.
#' @param require_region Whether `region` must be present and non-missing.
#'
#' @return `data`, invisibly, after successful validation.
#' @export
validate_qc_input <- function(data,
                              duplicate_action = c('error', 'allow'),
                              require_region = TRUE) {
  duplicate_action <- match.arg(duplicate_action)
  required <- c('facility_id', 'month_date', 'tested', 'positive')
  if (isTRUE(require_region)) required <- c(required, 'region')
  .validate_required_columns(data, required)

  if (nrow(data) == 0L) {
    rlang::abort('QC input must contain at least one row.')
  }
  if (!inherits(data[['month_date']], 'Date')) {
    rlang::abort('`month_date` must be a Date vector after adapter preparation.')
  }
  if (any(is.na(data[['facility_id']]) | trimws(as.character(data[['facility_id']])) == '')) {
    rlang::abort('`facility_id` contains missing or blank values.')
  }
  if (any(is.na(data[['month_date']]))) {
    rlang::abort('`month_date` contains missing or unparseable values.')
  }
  if (any(as.integer(format(data[['month_date']], '%d')) != 1L)) {
    rlang::abort('`month_date` must use the first day of each reporting month.')
  }
  if (!is.numeric(data[['tested']]) || !is.numeric(data[['positive']])) {
    rlang::abort('`tested` and `positive` must be numeric after adapter preparation.')
  }
  if (isTRUE(require_region) && any(is.na(data[['region']]) | trimws(as.character(data[['region']])) == '')) {
    rlang::abort('`region` contains missing or blank values.')
  }

  duplicate_key <- duplicated(data[c('facility_id', 'month_date')]) |
    duplicated(data[c('facility_id', 'month_date')], fromLast = TRUE)
  if (duplicate_action == 'error' && any(duplicate_key)) {
    rlang::abort(paste0(
      'QC input contains ', sum(duplicate_key),
      ' rows belonging to duplicate facility-month keys. Resolve them in the upstream adapter.'
    ))
  }

  if ('attending' %in% names(data) && any(!is.na(data[['attending']]))) {
    if (!is.numeric(data[['attending']])) {
      rlang::abort('`attending` must be numeric when supplied.')
    }
    if (!'attendance_definition' %in% names(data) ||
        any(is.na(data[['attendance_definition']]) |
            trimws(as.character(data[['attendance_definition']])) == '')) {
      rlang::abort('Non-missing `attending` values require a non-missing `attendance_definition`.')
    }
  }

  invisible(data)
}
