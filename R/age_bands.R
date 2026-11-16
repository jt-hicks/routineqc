# Age-disaggregated input. One row is a facility x month x age band. Every
# time-series rule runs within a *series*: one facility's one age band, split
# where the band's construction changes (supplied directly by the source, or
# summed from finer source bands). Data without age bands are one series per
# facility, so nothing changes for them.
#
# The row-level checks here use optional canonical columns that an upstream
# preparation step supplies:
#   anc_before_12wk, anc_after_12wk   first-ANC bookings by gestation at booking
#   positive_zero_filled              a blank positive count was set to zero
#   bands_expected                    finer source bands making up this band
#   <count>_bands_reported            how many of them had a non-blank value
# A zero-filled blank is not a reported band.

.age_band_count_fields <- c('anc_before_12wk', 'anc_after_12wk')

.band_reported_fields <- c(
  tested = 'tested_bands_reported',
  positive = 'positive_bands_reported',
  anc_before_12wk = 'anc_before_12wk_bands_reported',
  anc_after_12wk = 'anc_after_12wk_bands_reported'
)

.age_band_optional_fields <- c(
  'age_group', .age_band_count_fields, 'positive_zero_filled', 'bands_expected',
  unname(.band_reported_fields), 'source_id'
)

# An ASCII code for an age-band label, for use as a key: '<20' -> 'lt20',
# '>=20' or the single-character form -> 'ge20', '10-14' -> '10_14'. The label
# itself is kept for display.
.age_group_code <- function(label) {
  # Signs are replaced byte-wise first, so the result does not depend on the
  # session's locale.
  x <- enc2utf8(as.character(label))
  x <- gsub(enc2utf8('\u2265'), 'ge', x, fixed = TRUE, useBytes = TRUE)
  x <- gsub(enc2utf8('\u2264'), 'le', x, fixed = TRUE, useBytes = TRUE)
  # Non-UTF-8 sessions may spell the signs as escapes instead.
  x <- gsub('<U+2265>', 'ge', x, fixed = TRUE)
  x <- gsub('<U+2264>', 'le', x, fixed = TRUE)
  x <- gsub('<e2><89><a5>', 'ge', x, fixed = TRUE)
  x <- gsub('<e2><89><a4>', 'le', x, fixed = TRUE)
  x <- trimws(x)
  x <- gsub('>=', 'ge', x, fixed = TRUE)
  x <- gsub('<=', 'le', x, fixed = TRUE)
  x <- gsub('<', 'lt', x, fixed = TRUE)
  x <- gsub('>', 'gt', x, fixed = TRUE)
  x <- gsub('+', 'plus', x, fixed = TRUE)
  x <- gsub('[^A-Za-z0-9]+', '_', x)
  x <- gsub('^_+|_+$', '', x)
  x <- tolower(x)
  x[is.na(label) | x == ''] <- NA_character_
  x
}

.qc_is_age_disaggregated <- function(data) 'age_group_code' %in% names(data)

# The series each row belongs to. `segments = FALSE` ignores changes in how a
# band was built, for uses (such as imputation) that model one facility's band
# as a whole.
.qc_series_ids <- function(data, segments = TRUE) {
  base <- as.character(data$facility_id)
  if (.qc_is_age_disaggregated(data)) base <- paste(base, data$age_group_code, sep = ' | ')
  if (!segments || !'bands_expected' %in% names(data)) return(base)
  summed <- suppressWarnings(as.numeric(data$bands_expected)) > 1
  segment <- integer(nrow(data))
  for (rows in split(seq_len(nrow(data)), base)) {
    rows <- rows[order(data$month_date[rows])]
    s <- summed[rows]
    # A month without the information belongs to the segment it falls in.
    for (i in seq_along(s)[-1L]) if (is.na(s[i])) s[i] <- s[i - 1L]
    if (length(s) > 0L && is.na(s[1L])) s[1L] <- s[!is.na(s)][1L]
    changes <- c(0L, as.integer(!is.na(s[-1L]) & !is.na(s[-length(s)]) & s[-1L] != s[-length(s)]))
    segment[rows] <- 1L + cumsum(changes)
  }
  paste0(base, ' | s', segment)
}

# The grouping key for time-series rules: the series when the pipeline has
# recorded one, otherwise the facility.
.qc_group_key <- function(data) {
  if ('qc_series' %in% names(data)) as.character(data$qc_series) else as.character(data$facility_id)
}

.as_flag <- function(x) {
  if (is.logical(x)) return(x)
  v <- tolower(trimws(as.character(x)))
  out <- rep(NA, length(v))
  out[v %in% c('true', 't', '1', 'yes', 'y')] <- TRUE
  out[v %in% c('false', 'f', '0', 'no', 'n')] <- FALSE
  out
}

# Standardises the optional age-band columns in prepared data.
.prepare_age_columns <- function(out, data, age_group_var = NULL) {
  if (!is.null(age_group_var)) {
    out$age_group <- as.character(data[[age_group_var]])
    out$age_group_code <- .age_group_code(out$age_group)
  }
  for (field in .age_band_count_fields) {
    if (field %in% names(out)) out[[field]] <- .parse_counts(out[[field]])$value
  }
  for (field in c('bands_expected', unname(.band_reported_fields))) {
    if (field %in% names(out)) out[[field]] <- suppressWarnings(as.numeric(as.character(out[[field]])))
  }
  if ('positive_zero_filled' %in% names(out)) out$positive_zero_filled <- .as_flag(out$positive_zero_filled)
  out$qc_series <- .qc_series_ids(out)
  out
}

#' Add Age-Band and First-ANC QC Flags
#'
#' Row-level checks for data supplied one row per facility, month and age band,
#' typically first antenatal-care (ANC) attendance by maternal age. Each check
#' uses optional canonical columns and is `FALSE` when they are absent, so the
#' function is safe on data without age bands.
#'
#' * `flag_attendance_sum_mismatch`: `attending` differs from
#'   `anc_before_12wk + anc_after_12wk`.
#' * `flag_band_sum_partial_tested`, `flag_band_sum_partial_positive`,
#'   `flag_band_sum_partial_attending`: the count was summed from finer source
#'   bands and only some of them were reported (`0 < <count>_bands_reported <
#'   bands_expected`), so it is likely an undercount. Attendance is partial when
#'   either booking count is.
#' * `flag_band_metadata_inconsistent`: the band counts contradict the values,
#'   for example a value is present although no band was reported (and the
#'   value was not zero-filled), a value is missing although bands were
#'   reported, or more bands were reported than expected.
#' * `flag_positive_assumed_zero`: `positive_zero_filled` is `TRUE`; the zero is
#'   an assumption, not a report. Such rows are left out of the prevalence model
#'   fit.
#'
#' An attendance mismatch also sets `flag_attendance_issue`.
#'
#' @param data A logically flagged QC data frame (see [flag_logical_errors()]).
#'
#' @return The data with the flags above appended.
#' @export
add_age_band_qc_flags <- function(data) {
  out <- dplyr::as_tibble(data)
  n <- nrow(out)
  num <- function(col) {
    if (col %in% names(out)) suppressWarnings(as.numeric(out[[col]])) else rep(NA_real_, n)
  }
  before <- num('anc_before_12wk')
  after <- num('anc_after_12wk')
  attending <- num('attending')
  expected <- num('bands_expected')
  zero_filled <- if ('positive_zero_filled' %in% names(out)) {
    dplyr::coalesce(.as_flag(out$positive_zero_filled), FALSE)
  } else {
    rep(FALSE, n)
  }
  reported <- function(field) num(.band_reported_fields[[field]])
  # A value counts as missing only if it was not supplied at all; a malformed
  # value is an invalid count, flagged elsewhere.
  value_missing <- function(field) {
    status <- paste0(field, '_parse_status')
    if (status %in% names(out)) out[[status]] == 'missing' else is.na(num(field))
  }
  partial <- function(r) !is.na(r) & !is.na(expected) & r > 0 & r < expected
  inconsistent <- function(field, allow_zero_fill = FALSE) {
    r <- reported(field)
    known <- !is.na(r) & !is.na(expected)
    bad_meta <- known & (r < 0 | r != round(r) | r > expected | expected < 1 | expected != round(expected))
    present <- !value_missing(field)
    excused <- allow_zero_fill & zero_filled
    bad_meta | (known & r == 0 & present & !excused) | (known & r > 0 & !present)
  }

  out$flag_attendance_sum_mismatch <- !is.na(before) & !is.na(after) & !is.na(attending) &
    abs(attending - (before + after)) > 1e-8
  out$flag_band_sum_partial_tested <- partial(reported('tested'))
  out$flag_band_sum_partial_positive <- partial(reported('positive'))
  out$flag_band_sum_partial_attending <- partial(reported('anc_before_12wk')) |
    partial(reported('anc_after_12wk'))
  out$flag_band_metadata_inconsistent <- inconsistent('tested') |
    inconsistent('positive', allow_zero_fill = TRUE) |
    inconsistent('anc_before_12wk') | inconsistent('anc_after_12wk')
  out$flag_positive_assumed_zero <- zero_filled
  if ('flag_attendance_issue' %in% names(out)) {
    out$flag_attendance_issue <- dplyr::coalesce(out$flag_attendance_issue, FALSE) |
      out$flag_attendance_sum_mismatch
  }
  out
}

# Rows the prevalence model must not learn from: assumed zeros, partial sums
# of tested or positive, and contradictory band counts.
.age_band_untrusted <- function(data) {
  flag <- function(col) {
    if (col %in% names(data)) dplyr::coalesce(as.logical(data[[col]]), FALSE) else rep(FALSE, nrow(data))
  }
  zero_filled <- if ('positive_zero_filled' %in% names(data)) {
    dplyr::coalesce(.as_flag(data$positive_zero_filled), FALSE)
  } else {
    rep(FALSE, nrow(data))
  }
  zero_filled | flag('flag_band_sum_partial_tested') | flag('flag_band_sum_partial_positive') |
    flag('flag_band_metadata_inconsistent')
}

#' Simulate Age-Disaggregated First-ANC Data
#'
#' Synthetic facility x month x maternal-age-band data in the shape produced by
#' an age-disaggregated preparation step, for tests and examples. Bands are
#' `<20` and `>=20` (written with the single-character "greater than or equal"
#' sign). Before `break_month` each band is supplied directly
#' (`bands_expected = 1`); from it on, `<20` is summed from two finer bands and
#' `>=20` from four. Some rows have partial sums, zero-filled positives and
#' injected errors in one band only; truth columns record which.
#'
#' @param n_regions,councils_per_region,facilities_per_council Hierarchy size.
#' @param n_months Number of months.
#' @param start_month First month, as `"YYYY-MM-DD"`.
#' @param break_month First month in which bands are summed from finer bands.
#' @param partial_fraction,zero_fill_fraction,error_fraction Approximate
#'   proportions of rows given a partial sum, a zero-filled positive, and an
#'   injected count error.
#' @param seed Random seed.
#'
#' @return A tibble, one row per facility, month and age band.
#' @export
simulate_qc_age_data <- function(n_regions = 2, councils_per_region = 2, facilities_per_council = 4,
                                 n_months = 36, start_month = '2019-07-01', break_month = '2021-01-01',
                                 partial_fraction = 0.03, zero_fill_fraction = 0.03,
                                 error_fraction = 0.02, seed = 1) {
  set.seed(seed)
  months <- seq.Date(as.Date(start_month), by = 'month', length.out = n_months)
  facilities <- dplyr::bind_rows(lapply(seq_len(n_regions), function(r) {
    dplyr::bind_rows(lapply(seq_len(councils_per_region), function(cn) {
      dplyr::tibble(
        region = sprintf('Region %02d', r),
        district = sprintf('Council %02d-%02d', r, cn),
        facility_id = sprintf('FAC%02d%02d%02d', r, cn, seq_len(facilities_per_council))
      )
    }))
  }))
  facilities$facility_name <- paste('Facility', facilities$facility_id)
  nf <- nrow(facilities)
  facilities$size <- round(stats::rlnorm(nf, log(60), 0.4))
  facilities$young_share <- stats::plogis(stats::rnorm(nf, stats::qlogis(0.25), 0.3))
  facilities$early_share <- stats::plogis(stats::rnorm(nf, stats::qlogis(0.3), 0.3))
  facilities$effect <- stats::rnorm(nf, 0, 0.4)

  labels <- c(lt20 = '<20', ge20 = '\u226520')
  grid <- tidyr::crossing(facility_id = facilities$facility_id, month_date = months,
                          code = names(labels))
  grid <- dplyr::left_join(grid, facilities, by = 'facility_id')
  n <- nrow(grid)
  season <- sin(2 * pi * lubridate::month(grid$month_date) / 12)
  share <- ifelse(grid$code == 'lt20', grid$young_share, 1 - grid$young_share)
  attending <- stats::rpois(n, grid$size * share * (1 + 0.15 * season))
  before <- stats::rbinom(n, attending, grid$early_share)
  after <- attending - before
  tested <- stats::rbinom(n, attending, 0.95)
  lp <- -1.6 + 0.5 * season + grid$effect + ifelse(grid$code == 'lt20', 0.4, 0)
  positive <- stats::rbinom(n, tested, stats::plogis(lp))

  summed <- grid$month_date >= as.Date(break_month)
  expected <- ifelse(summed, ifelse(grid$code == 'lt20', 2, 4), 1)
  out <- dplyr::tibble(
    facility_id = grid$facility_id, facility_name = grid$facility_name, month_date = grid$month_date,
    region = grid$region, district = grid$district, age_group = unname(labels[grid$code]),
    attending = as.numeric(attending), tested = as.numeric(tested), positive = as.numeric(positive),
    anc_before_12wk = as.numeric(before), anc_after_12wk = as.numeric(after),
    positive_zero_filled = FALSE, bands_expected = expected,
    tested_bands_reported = expected, positive_bands_reported = expected,
    anc_before_12wk_bands_reported = expected, anc_after_12wk_bands_reported = expected,
    source_id = ifelse(summed, 'synthetic-5yr-bands', 'synthetic-2-bands'),
    injected_error = FALSE, injected_partial = FALSE, injected_zero_fill = FALSE
  )

  pick <- function(pool, fraction) {
    size <- min(length(pool), max(1L, round(fraction * n)))
    if (length(pool) <= 1L) pool[seq_len(size)] else sample(pool, size)
  }
  # Partial sums: one finer band of tested and positive missing.
  part <- pick(which(summed), partial_fraction)
  keep <- (out$bands_expected[part] - 1) / out$bands_expected[part]
  out$tested[part] <- round(out$tested[part] * keep)
  out$positive[part] <- pmin(round(out$positive[part] * keep), out$tested[part])
  out$tested_bands_reported[part] <- out$bands_expected[part] - 1
  out$positive_bands_reported[part] <- out$bands_expected[part] - 1
  out$injected_partial[part] <- TRUE
  # Zero-filled positives: every positive band blank.
  zero <- pick(setdiff(seq_len(n), part), zero_fill_fraction)
  out$positive[zero] <- 0
  out$positive_zero_filled[zero] <- TRUE
  out$positive_bands_reported[zero] <- 0
  out$injected_zero_fill[zero] <- TRUE
  # Count errors in one band only: positives entered as the tested count, or a
  # tested count inflated tenfold.
  err <- pick(setdiff(seq_len(n), c(part, zero)), error_fraction)
  half <- seq_along(err) %% 2L == 0L
  out$positive[err[half]] <- out$tested[err[half]]
  out$tested[err[!half]] <- out$tested[err[!half]] * 10
  out$injected_error[err] <- TRUE
  out
}
