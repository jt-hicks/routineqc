# Age-structure checks: the bands of one facility-month compared with each
# other and with the facility's own history. They need age-disaggregated data
# (see `R/age_bands.R`) and are FALSE otherwise.
#
# Every check is count-based. A share such as "under-20 bookings out of all
# bookings" is compared with the facility's baseline share from nearby months,
# as a binomial z-score inflated by the month-to-month overdispersion seen in
# that baseline. A flag needs both a large z-score and a large shift, so a big
# facility is not flagged for a trivial change and a small one is not flagged
# for noise. The baseline restarts where the source changes how bands are
# built, as the per-band rules do.
#
# The flags are review-only: a value entered in the wrong band leaves the
# facility's totals intact, so no count is omitted from all-band estimates.

.age_structure_flags <- c(
  'flag_age_band_missing', 'flag_age_mix_shift', 'flag_age_bands_swapped',
  'flag_booking_timing_shift', 'flag_age_positivity_contrast'
)

.age_structure_columns <- c(
  'age_mix_bookings', 'age_mix_share', 'age_mix_baseline', 'age_mix_z',
  'booking_early_share', 'booking_early_baseline', 'booking_early_z',
  'age_positivity_min_tested', 'age_positivity_log_or', 'age_positivity_log_or_baseline',
  'age_positivity_contrast_z'
)

.age_structure_defaults <- function() {
  list(
    young_band = 'lt20', old_band = 'ge20',
    baseline_window_months = 12, min_baseline_months = 6,
    min_bookings = 20, mix_z_threshold = 4, mix_min_shift = 0.15, swap_z_max = 2,
    timing_z_threshold = 4, timing_min_shift = 0.2,
    min_tested_band = 20, contrast_z_threshold = 4, contrast_min_log_or_shift = 1
  )
}

.validate_age_structure_config <- function(cfg) {
  bands <- c('young_band', 'old_band')
  if (any(!vapply(cfg[bands], function(x) is.character(x) && length(x) == 1L && !is.na(x) && nzchar(x),
                  logical(1))) || identical(cfg$young_band, cfg$old_band)) {
    rlang::abort('`age_structure$young_band` and `old_band` must be two different band codes.')
  }
  numeric_names <- setdiff(names(.age_structure_defaults()), bands)
  if (any(!vapply(cfg[numeric_names], .is_scalar_number, logical(1))) ||
      any(unlist(cfg[numeric_names]) < 0)) {
    rlang::abort('Age-structure numeric settings must be non-negative finite numbers.')
  }
  whole <- c('baseline_window_months', 'min_baseline_months')
  if (any(unlist(cfg[whole]) < 1) || any(unlist(cfg[whole]) != round(unlist(cfg[whole])))) {
    rlang::abort('`age_structure` window and minimum months must be positive whole numbers.')
  }
  if (cfg$mix_min_shift > 1 || cfg$timing_min_shift > 1) {
    rlang::abort('Age-structure share shifts must be at most 1.')
  }
  invisible(cfg)
}

.baseline_window <- function(month_index, i, mode, window) {
  d <- month_index - month_index[i]
  if (mode == 'trailing') d < 0L & d >= -window else d != 0L & abs(d) <= window
}

# Successes `x` out of `n` against the pooled share of the baseline window.
.binomial_shift <- function(x, n, month_index, mode, window, min_months, swap = FALSE) {
  m <- length(x)
  p0 <- z <- z_swap <- rep(NA_real_, m)
  ok <- !is.na(x) & !is.na(n) & n > 0 & x >= 0 & x <= n
  for (i in seq_len(m)) {
    if (!ok[i]) next
    w <- ok & .baseline_window(month_index, i, mode, window)
    k <- sum(w)
    if (k < min_months) next
    p <- (sum(x[w]) + 0.5) / (sum(n[w]) + 1)
    v <- n[w] * p * (1 - p)
    phi <- if (k > 1L) max(1, sum((x[w] - n[w] * p)^2 / v) / (k - 1L)) else 1
    sd <- sqrt(n[i] * p * (1 - p) * phi)
    p0[i] <- p
    z[i] <- (x[i] - n[i] * p) / sd
    if (swap) z_swap[i] <- ((n[i] - x[i]) - n[i] * p) / sd
  }
  list(p0 = p0, z = z, z_swap = z_swap)
}

# The log odds ratio of positivity, young over old band, against the pooled
# log odds ratio of the baseline window. Half-counts keep zeros finite.
.contrast_shift <- function(yp, yt, op, ot, month_index, mode, window, min_months) {
  m <- length(yp)
  ok <- !is.na(yp) & !is.na(yt) & !is.na(op) & !is.na(ot) & yt > 0 & ot > 0 &
    yp >= 0 & op >= 0 & yp <= yt & op <= ot
  lor_of <- function(a, b, c, d) log((a + 0.5) / (b - a + 0.5)) - log((c + 0.5) / (d - c + 0.5))
  var_of <- function(a, b, c, d) 1 / (a + 0.5) + 1 / (b - a + 0.5) + 1 / (c + 0.5) + 1 / (d - c + 0.5)
  lor <- ifelse(ok, lor_of(yp, yt, op, ot), NA_real_)
  v <- ifelse(ok, var_of(yp, yt, op, ot), NA_real_)
  base <- z <- rep(NA_real_, m)
  for (i in seq_len(m)) {
    if (!ok[i]) next
    w <- ok & .baseline_window(month_index, i, mode, window)
    k <- sum(w)
    if (k < min_months) next
    a <- sum(yp[w]); b <- sum(yt[w]); c <- sum(op[w]); d <- sum(ot[w])
    lb <- lor_of(a, b, c, d)
    phi <- if (k > 1L) max(1, sum((lor[w] - lb)^2 / v[w]) / (k - 1L)) else 1
    base[i] <- lb
    z[i] <- (lor[i] - lb) / sqrt(phi * v[i] + var_of(a, b, c, d))
  }
  list(lor = lor, base = base, z = z)
}

# One row per facility-month carrying both bands, with the counts the checks use.
.age_structure_frame <- function(data, young_band, old_band) {
  key <- paste(data$facility_id, data$month_date)
  usable <- .usable_counts(data)
  counts <- .baseline_counts(data)
  core_bad <- if ('flag_core_invalid' %in% names(data)) dplyr::coalesce(as.logical(data$flag_core_invalid), FALSE) else FALSE
  tested <- ifelse(core_bad, NA_real_, usable$tested)
  positive <- ifelse(core_bad, NA_real_, usable$positive)
  before <- if ('anc_before_12wk' %in% names(data)) suppressWarnings(as.numeric(data$anc_before_12wk)) else NA_real_
  before <- rep_len(before, nrow(data))
  before[!is.na(before) & before < 0] <- NA_real_
  be <- if ('bands_expected' %in% names(data)) suppressWarnings(as.numeric(data$bands_expected)) else rep(NA_real_, nrow(data))

  young <- which(data$age_group_code == young_band)
  old <- which(data$age_group_code == old_band)
  keys <- intersect(key[young], key[old])
  y <- young[match(keys, key[young])]
  o <- old[match(keys, key[old])]
  fm <- data.frame(
    key = keys, facility_id = as.character(data$facility_id[y]), month_date = data$month_date[y],
    y_att = counts$attending[y], o_att = counts$attending[o],
    before = before[y] + before[o],
    y_tested = tested[y], y_pos = positive[y], o_tested = tested[o], o_pos = positive[o],
    bands_expected = pmax(be[y], be[o], na.rm = TRUE),
    stringsAsFactors = FALSE
  )
  fm$bands_expected[!is.finite(fm$bands_expected)] <- NA_real_
  fm$series <- .qc_series_ids(fm)
  fm$month_index <- .qc_month_index(fm$month_date)
  fm
}

#' Add Age-Structure QC Flags
#'
#' Checks that compare the age bands of one facility-month with each other and
#' with the facility's own recent history. They need age-disaggregated data
#' (`age_group_var` in [prepare_qc_data()]) with a younger and an older band;
#' otherwise every flag is `FALSE`.
#'
#' * `flag_age_band_missing`: a facility-month has rows for some bands but not
#'   all the bands seen in the data.
#' * `flag_age_mix_shift`: the share of first-ANC bookings in the younger band
#'   differs from the facility's baseline share.
#' * `flag_age_bands_swapped`: an age-mix shift that would fit the baseline if
#'   the two bands were exchanged, as when values are entered in the wrong
#'   columns.
#' * `flag_booking_timing_shift`: the share of bookings before 12 weeks differs
#'   from the facility's baseline.
#' * `flag_age_positivity_contrast`: positivity in the younger band relative to
#'   the older one (a log odds ratio) differs from the facility's usual
#'   contrast. No direction is assumed.
#'
#' The baseline is the facility's pooled counts over the months in the window
#' (earlier months only for a `trailing` baseline; earlier and later for
#' `centered`), restarting where `bands_expected` changes between 1 and more
#' than 1. A check needs at least `min_baseline_months` usable months. The
#' z-score is binomial, inflated by the overdispersion of the baseline months. A
#' flag needs both a z-score beyond its threshold and a shift at least as large
#' as the minimum, and the current month must have at least `min_bookings`
#' bookings (age mix, timing) or `min_tested_band` tested in each band
#' (positivity).
#'
#' The measurements are kept on both band rows: `age_mix_bookings`,
#' `age_mix_share`, `age_mix_baseline`, `age_mix_z`, `booking_early_share`,
#' `booking_early_baseline`, `booking_early_z`, `age_positivity_min_tested`,
#' `age_positivity_log_or`, `age_positivity_log_or_baseline` and
#' `age_positivity_contrast_z`. [summarise_age_band_deviations()] summarises
#' them for calibration.
#'
#' These flags are review-only. A value entered in the wrong band leaves the
#' facility's totals intact, so no count is omitted from all-band estimates.
#'
#' @param data Flagged age-disaggregated data.
#' @param young_band,old_band `age_group_code` of the younger and older band.
#' @param baseline_mode `"trailing"` or `"centered"`.
#' @param baseline_window_months Months either side (or before) in the baseline.
#' @param min_baseline_months Usable baseline months required.
#' @param min_bookings Minimum bookings in the current month for the age-mix and
#'   timing checks.
#' @param mix_z_threshold,mix_min_shift Age-mix z-score and share shift.
#' @param swap_z_max Largest absolute z-score of the exchanged bands for a
#'   shift to be called a swap.
#' @param timing_z_threshold,timing_min_shift Booking-timing z-score and shift.
#' @param min_tested_band Minimum tested in each band for the positivity check.
#' @param contrast_z_threshold,contrast_min_log_or_shift Positivity-contrast
#'   z-score and shift in the log odds ratio.
#'
#' @return The data with the flags and measurements above.
#' @export
add_age_structure_qc_flags <- function(data,
                                       young_band = 'lt20', old_band = 'ge20',
                                       baseline_mode = c('trailing', 'centered'),
                                       baseline_window_months = 12, min_baseline_months = 6,
                                       min_bookings = 20, mix_z_threshold = 4, mix_min_shift = 0.15,
                                       swap_z_max = 2, timing_z_threshold = 4, timing_min_shift = 0.2,
                                       min_tested_band = 20, contrast_z_threshold = 4,
                                       contrast_min_log_or_shift = 1) {
  baseline_mode <- match.arg(baseline_mode)
  settings <- list(young_band = young_band, old_band = old_band,
                   baseline_window_months = baseline_window_months, min_baseline_months = min_baseline_months,
                   min_bookings = min_bookings, mix_z_threshold = mix_z_threshold, mix_min_shift = mix_min_shift,
                   swap_z_max = swap_z_max, timing_z_threshold = timing_z_threshold,
                   timing_min_shift = timing_min_shift, min_tested_band = min_tested_band,
                   contrast_z_threshold = contrast_z_threshold,
                   contrast_min_log_or_shift = contrast_min_log_or_shift)
  .validate_age_structure_config(settings)
  out <- dplyr::as_tibble(data)
  out <- out[setdiff(names(out), .age_structure_columns)]
  for (f in .age_structure_flags) out[[f]] <- FALSE
  if (!.qc_is_age_disaggregated(out)) return(out)

  codes <- unique(stats::na.omit(out$age_group_code))
  key <- paste(out$facility_id, out$month_date)
  if (length(codes) > 1L) {
    present <- tapply(out$age_group_code, key, function(x) length(unique(stats::na.omit(x))))
    out$flag_age_band_missing <- as.integer(present[key]) < length(codes)
  }
  if (!all(c(young_band, old_band) %in% codes)) return(out)

  fm <- .age_structure_frame(out, young_band, old_band)
  n <- fm$y_att + fm$o_att
  fm$age_mix_bookings <- n
  fm$age_mix_share <- ifelse(!is.na(n) & n > 0, fm$y_att / n, NA_real_)
  fm$booking_early_share <- ifelse(!is.na(n) & n > 0 & !is.na(fm$before), fm$before / n, NA_real_)
  fm$age_positivity_min_tested <- pmin(fm$y_tested, fm$o_tested)
  for (col in c('age_mix_baseline', 'age_mix_z', '.mix_z_swap', 'booking_early_baseline', 'booking_early_z',
                'age_positivity_log_or', 'age_positivity_log_or_baseline', 'age_positivity_contrast_z')) {
    fm[[col]] <- NA_real_
  }
  window <- baseline_window_months
  for (rows in split(seq_len(nrow(fm)), fm$series)) {
    rows <- rows[order(fm$month_index[rows])]
    mi <- fm$month_index[rows]
    mix <- .binomial_shift(fm$y_att[rows], n[rows], mi, baseline_mode, window, min_baseline_months, swap = TRUE)
    fm$age_mix_baseline[rows] <- mix$p0
    fm$age_mix_z[rows] <- mix$z
    fm$.mix_z_swap[rows] <- mix$z_swap
    timing <- .binomial_shift(fm$before[rows], n[rows], mi, baseline_mode, window, min_baseline_months)
    fm$booking_early_baseline[rows] <- timing$p0
    fm$booking_early_z[rows] <- timing$z
    contrast <- .contrast_shift(fm$y_pos[rows], fm$y_tested[rows], fm$o_pos[rows], fm$o_tested[rows],
                                mi, baseline_mode, window, min_baseline_months)
    fm$age_positivity_log_or[rows] <- contrast$lor
    fm$age_positivity_log_or_baseline[rows] <- contrast$base
    fm$age_positivity_contrast_z[rows] <- contrast$z
  }

  enough <- !is.na(n) & n >= min_bookings
  mix_flag <- enough & !is.na(fm$age_mix_z) & abs(fm$age_mix_z) > mix_z_threshold &
    abs(fm$age_mix_share - fm$age_mix_baseline) >= mix_min_shift
  swapped <- mix_flag & !is.na(fm$.mix_z_swap) & abs(fm$.mix_z_swap) <= swap_z_max
  timing_flag <- enough & !is.na(fm$booking_early_z) & abs(fm$booking_early_z) > timing_z_threshold &
    abs(fm$booking_early_share - fm$booking_early_baseline) >= timing_min_shift
  contrast_flag <- !is.na(fm$age_positivity_min_tested) & fm$age_positivity_min_tested >= min_tested_band &
    !is.na(fm$age_positivity_contrast_z) & abs(fm$age_positivity_contrast_z) > contrast_z_threshold &
    abs(fm$age_positivity_log_or - fm$age_positivity_log_or_baseline) >= contrast_min_log_or_shift

  at <- match(key, fm$key)
  for (col in .age_structure_columns) out[[col]] <- fm[[col]][at]
  pick <- function(x) dplyr::coalesce(x[at], FALSE)
  out$flag_age_mix_shift <- pick(mix_flag)
  out$flag_age_bands_swapped <- pick(swapped)
  out$flag_booking_timing_shift <- pick(timing_flag)
  out$flag_age_positivity_contrast <- pick(contrast_flag)
  out
}

#' Summarise Within-Facility Age-Structure Deviations
#'
#' Aggregate distributions of the age-structure measurements from
#' [add_age_structure_qc_flags()], one facility-month counted once, for
#' choosing thresholds. The result has no facility, area or month identifiers,
#' so it can be shared when the data cannot.
#'
#' @param data Flagged data from an age-disaggregated run.
#' @param min_bookings,min_tested_band Minimum counts a facility-month needs to
#'   be counted, as in the checks.
#' @param z_thresholds Absolute z-score cutoffs to tabulate.
#' @param probs Quantiles to report.
#'
#' @return A list of two tibbles: `quantiles` (z-score and absolute shift by
#'   measure) and `exceedance` (share of assessed facility-months beyond each
#'   pair of z-score and shift cutoffs).
#' @export
summarise_age_band_deviations <- function(data, min_bookings = 20, min_tested_band = 20,
                                          z_thresholds = c(3, 4, 5, 6),
                                          probs = c(0.5, 0.9, 0.95, 0.99, 0.999)) {
  .validate_required_columns(data, c('facility_id', 'month_date', 'age_mix_z'))
  fm <- dplyr::as_tibble(data)[!duplicated(paste(data$facility_id, data$month_date)), , drop = FALSE]
  measures <- list(
    age_mix = list(z = fm$age_mix_z, shift = abs(fm$age_mix_share - fm$age_mix_baseline),
                   keep = !is.na(fm$age_mix_bookings) & fm$age_mix_bookings >= min_bookings,
                   shifts = c(0.05, 0.1, 0.15, 0.2, 0.3)),
    booking_timing = list(z = fm$booking_early_z, shift = abs(fm$booking_early_share - fm$booking_early_baseline),
                          keep = !is.na(fm$age_mix_bookings) & fm$age_mix_bookings >= min_bookings,
                          shifts = c(0.1, 0.2, 0.3, 0.4)),
    positivity_contrast = list(z = fm$age_positivity_contrast_z,
                               shift = abs(fm$age_positivity_log_or - fm$age_positivity_log_or_baseline),
                               keep = !is.na(fm$age_positivity_min_tested) &
                                 fm$age_positivity_min_tested >= min_tested_band,
                               shifts = c(0.5, 1, 1.5, 2))
  )
  quantiles <- list()
  exceedance <- list()
  for (nm in names(measures)) {
    m <- measures[[nm]]
    ok <- m$keep & !is.na(m$z) & !is.na(m$shift)
    z <- abs(m$z[ok])
    s <- m$shift[ok]
    quantiles[[nm]] <- dplyr::tibble(
      measure = nm, assessed = sum(ok), quantile = probs,
      abs_z = if (any(ok)) unname(stats::quantile(z, probs)) else NA_real_,
      abs_shift = if (any(ok)) unname(stats::quantile(s, probs)) else NA_real_
    )
    grid <- expand.grid(z_cutoff = z_thresholds, shift_cutoff = m$shifts)
    grid$share_flagged <- vapply(seq_len(nrow(grid)), function(i) {
      if (!any(ok)) return(NA_real_)
      mean(z > grid$z_cutoff[i] & s >= grid$shift_cutoff[i])
    }, numeric(1))
    exceedance[[nm]] <- dplyr::as_tibble(cbind(measure = nm, assessed = sum(ok), grid))
  }
  list(quantiles = dplyr::bind_rows(quantiles), exceedance = dplyr::bind_rows(exceedance))
}
