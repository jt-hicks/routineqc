# Count translation shared by `prepare_qc_data()` and `adapt_qc_data()`.
#
# A supplied count is translated to a number without repair. Every element gets
# one status, so a value that was never reported is told apart from one that was
# reported and is not a valid count:
#   ok          a finite whole number
#   missing     NA, or an empty / blank string: genuinely not reported
#   malformed   present but not a number: text such as "n/a" or "1,000", or a
#               logical or other type that is not a count
#   non_finite  NaN or +/-Inf, as a number or as text
#   fractional  a finite number with a fractional part
# `malformed` and `non_finite` values become NA, because there is no number to
# keep. A `fractional` value is kept exactly as supplied and is never rounded.
# Factors are read by label, never by integer code.
.count_status_levels <- c('ok', 'missing', 'malformed', 'non_finite', 'fractional')
.count_invalid_statuses <- c('malformed', 'non_finite', 'fractional')

.parse_counts <- function(x) {
  n <- length(x)
  status <- rep('ok', n)
  value <- rep(NA_real_, n)
  source <- if (is.factor(x)) as.character(x) else x
  x <- source

  if (is.character(x)) {
    trimmed <- trimws(x)
    blank <- is.na(x) | trimmed == ''
    status[blank] <- 'missing'
    value[!blank] <- suppressWarnings(as.numeric(trimmed[!blank]))
    status[!blank & is.na(value) & !is.nan(value)] <- 'malformed'
  } else if (is.numeric(x)) {
    value <- as.numeric(x)
    status[is.na(x) & !is.nan(x)] <- 'missing'
  } else {
    status[is.na(x)] <- 'missing'
    status[!is.na(x)] <- 'malformed'
  }

  non_finite <- status == 'ok' & !is.finite(value)
  status[non_finite] <- 'non_finite'
  value[status %in% c('malformed', 'non_finite', 'missing')] <- NA_real_

  tolerance <- sqrt(.Machine$double.eps)
  fractional <- status == 'ok' & abs(value - round(value)) > tolerance
  status[fractional] <- 'fractional'

  list(value = value, status = status, source = source)
}

.count_status_table <- function(status) {
  counts <- table(factor(status, levels = .count_status_levels))
  as.list(stats::setNames(as.integer(counts), names(counts)))
}

# A count that was reported but is not a valid whole number. Uses the recorded
# translation status when the data carry it, and the value alone otherwise.
.count_value_invalid <- function(value, status = NULL) {
  if (!is.null(status)) {
    return(dplyr::coalesce(status %in% .count_invalid_statuses, FALSE))
  }
  tolerance <- sqrt(.Machine$double.eps)
  dplyr::coalesce(
    is.infinite(value) | (is.finite(value) & abs(value - round(value)) > tolerance),
    FALSE
  )
}

# Translates one count column onto canonical `tested`/`positive`-style fields.
# The original is kept as `<field>_source` and the status as
# `<field>_parse_status`. Data already translated by an adapter, which carries
# both, is passed through untouched so the original is never replaced by its
# own translation.
.translate_count_column <- function(data, field, source_var) {
  source_col <- paste0(field, '_source')
  status_col <- paste0(field, '_parse_status')
  if (identical(source_var, field) && all(c(source_col, status_col) %in% names(data))) {
    return(list(
      value = as.numeric(data[[field]]),
      source = data[[source_col]],
      status = as.character(data[[status_col]]),
      translated_upstream = TRUE
    ))
  }
  parsed <- .parse_counts(data[[source_var]])
  parsed$translated_upstream <- FALSE
  parsed
}

# Whether a row's tested or positive is a reported value that is not a valid
# whole number. Prefers the flag when the data carry it, so every stage agrees.
.row_count_invalid <- function(data) {
  if ('flag_count_invalid_value' %in% names(data)) {
    return(dplyr::coalesce(as.logical(data[['flag_count_invalid_value']]), FALSE))
  }
  invalid <- rep(FALSE, nrow(data))
  for (field in c('tested', 'positive')) {
    if (field %in% names(data)) {
      invalid <- invalid | .count_value_invalid(data[[field]], data[[paste0(field, '_parse_status')]])
    }
  }
  invalid
}

# `tested`/`positive` with rows holding an invalid count set to NA, for use in
# calculations. The data themselves are never changed.
.usable_counts <- function(data) {
  invalid <- .row_count_invalid(data)
  mask <- function(field) {
    if (!field %in% names(data)) return(rep(NA_real_, nrow(data)))
    dplyr::if_else(invalid, NA_real_, as.numeric(data[[field]]))
  }
  list(tested = mask('tested'), positive = mask('positive'))
}

.count_translation_summary <- function(data) {
  out <- list()
  for (field in c('tested', 'positive')) {
    status_col <- paste0(field, '_parse_status')
    if (status_col %in% names(data)) {
      out[[field]] <- .count_status_table(data[[status_col]])
    }
  }
  out
}
