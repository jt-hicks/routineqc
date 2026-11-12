# Auditable count imputation for QC scenarios.
#
# Two models, both fitted only to contributions the scenario retains, so a
# flagged value is never used to validate its own replacement:
#   * testing volume: negative-binomial GAM (facility random effect, seasonal
#     and time smooths where the history supports them);
#   * positives given tested: binomial GAM with the same structure plus
#     log(tested + 1), with extra-binomial variation from a beta-binomial draw
#     when the fit is overdispersed.
# The prevalence GAM and the tested-volume baselines used for anomaly detection
# are not reused: the first conditions on tested and cannot reconstruct volume,
# and a rolling baseline is not a validated predictive model.
#
# Targets are QC-created gaps (omitted or invalid counts). Gaps that existed
# before QC (missing counts, absent reports) are filled only when the scenario
# sets `fill_preexisting`. Periods outside a facility's active window are never
# filled. Anything the models cannot support stays missing with a reason.

.imputation_model_version <- 'count-imputation-1'

.count_model_ladder <- function(df, k_season = 6L, k_time = 8L) {
  n_season <- length(unique(df$month_num))
  n_time <- length(unique(df$month_index))
  re <- if (nlevels(droplevels(df$facility_f)) > 1L) 's(facility_f, bs = "re")' else NULL
  season <- if (n_season >= 8L) paste0('s(month_num, bs = "cc", k = ', min(k_season, n_season - 1L), ')')
  time_smooth <- if (n_time >= 10L) paste0('s(month_index, k = ', min(k_time, n_time - 1L), ')')
  time_linear <- if (n_time >= 3L) 'month_index'
  list(
    full = c(re, season, time_smooth),
    no_season = c(re, time_smooth),
    linear_time = c(re, time_linear),
    facility_only = re
  )
}

# Fits `response ~ terms`, falling back to simpler structure when the history
# does not support the full one. Returns NULL (with a reason) when nothing fits.
.fit_count_model <- function(df, response, family, extra = NULL) {
  attempts <- .count_model_ladder(df)
  seen <- character()
  for (label in names(attempts)) {
    terms <- c(extra, attempts[[label]])
    rhs <- if (length(terms) == 0L) '1' else paste(terms, collapse = ' + ')
    if (rhs %in% seen) next
    seen <- c(seen, rhs)
    fit <- tryCatch(suppressWarnings(
      mgcv::bam(stats::as.formula(paste(response, '~', rhs)), data = df, family = family,
                method = 'fREML', discrete = TRUE,
                knots = list(month_num = c(0.5, 12.5)))
    ), error = function(e) NULL)
    if (!is.null(fit) && all(is.finite(stats::coef(fit)))) {
      return(list(model = fit, structure = label, formula = paste(response, '~', rhs)))
    }
  }
  NULL
}

.design_data <- function(rows, facility_levels, month_origin, tested = NULL) {
  facility <- as.character(rows$facility_id)
  known <- facility %in% facility_levels
  facility[!known] <- facility_levels[[1]]
  out <- data.frame(
    facility_f = factor(facility, levels = facility_levels),
    month_num = as.numeric(lubridate::month(rows$month_date)),
    month_index = as.numeric(.qc_month_index(rows$month_date) - month_origin),
    stringsAsFactors = FALSE
  )
  if (!is.null(tested)) out$log1p_tested <- log1p(tested)
  attr(out, 'known_facility') <- known
  out
}

.lpmatrix <- function(model, newdata, zero_facility = NULL) {
  X <- suppressWarnings(stats::predict(model, newdata = newdata, type = 'lpmatrix'))
  if (!is.null(zero_facility) && any(zero_facility)) {
    re_cols <- grepl('^s\\(facility_f\\)', colnames(X))
    X[zero_facility, re_cols] <- 0
  }
  X
}

# Draws of the coefficient vector from the (unconditional) Bayesian posterior,
# so smoothing-parameter and coefficient uncertainty both carry through.
.draw_coefficients <- function(model, n) {
  beta <- stats::coef(model)
  if (n == 0L) return(matrix(beta, ncol = 1L))
  V <- tryCatch(stats::vcov(model, unconditional = TRUE), error = function(e) NULL)
  if (is.null(V) || anyNA(V)) V <- stats::vcov(model)
  t(mgcv::rmvn(n, beta, V))
}

.rnb_truncated <- function(mu, theta, lower) {
  lower <- pmax(0, lower)
  p_low <- ifelse(lower > 0, stats::pnbinom(lower - 1, mu = mu, size = theta), 0)
  u <- stats::runif(length(mu), min = p_low, max = 1)
  x <- stats::qnbinom(pmin(u, 1 - 1e-12), mu = mu, size = theta)
  pmax(x, lower)
}

.rbetabinom <- function(size, p, rho) {
  if (!is.finite(rho) || rho <= 1e-8) return(stats::rbinom(length(size), size, p))
  a <- pmax(p * (1 - rho) / rho, 1e-6)
  b <- pmax((1 - p) * (1 - rho) / rho, 1e-6)
  stats::rbinom(length(size), size, stats::rbeta(length(size), a, b))
}

# Facility windows. A supplied schedule (facility_id, first_active_month,
# last_active_month; NA = open-ended) takes precedence; otherwise the window is
# the first to last month in which the facility reported, which is an
# assumption and is recorded as such.
.facility_windows <- function(data, schedule = NULL) {
  inferred <- data %>%
    dplyr::group_by(facility_id = as.character(facility_id)) %>%
    dplyr::summarise(first = min(month_date), last = max(month_date), .groups = 'drop')
  inferred$source <- 'inferred_from_reports'
  if (is.null(schedule)) return(inferred)
  .validate_required_columns(schedule, c('facility_id', 'first_active_month', 'last_active_month'))
  schedule <- dplyr::as_tibble(schedule)
  schedule$facility_id <- as.character(schedule$facility_id)
  merged <- dplyr::left_join(inferred, schedule, by = 'facility_id')
  has <- merged$facility_id %in% schedule$facility_id
  merged$first <- dplyr::if_else(has & !is.na(merged$first_active_month), merged$first_active_month, merged$first)
  merged$last <- dplyr::if_else(has & !is.na(merged$last_active_month), merged$last_active_month, merged$last)
  merged$source <- dplyr::if_else(has, 'supplied_schedule', 'inferred_from_reports')
  merged[c('facility_id', 'first', 'last', 'source')]
}

.absent_report_rows <- function(scn, windows) {
  months <- sort(unique(lubridate::floor_date(scn$month_date, 'month')))
  grid <- dplyr::bind_rows(lapply(seq_len(nrow(windows)), function(i) {
    m <- months[months >= windows$first[i] & months <= windows$last[i]]
    if (length(m) == 0L) return(NULL)
    dplyr::tibble(facility_id = windows$facility_id[i], month_date = m)
  }))
  if (is.null(grid) || nrow(grid) == 0L) return(NULL)
  present <- paste(as.character(scn$facility_id), scn$month_date)
  absent <- grid[!paste(grid$facility_id, grid$month_date) %in% present, , drop = FALSE]
  absent[!paste(absent$facility_id, absent$month_date) %in% present, ]
}

#' Impute QC-Created Gaps in a Scenario
#'
#' Replaces the contributions a scenario omits with model-based counts and
#' records exactly what was done. Reported values, flags and stored actions are
#' never changed. Use [summarise_qc_imputation()] for aggregate results.
#'
#' `method = "multiple"` draws repeated predictive completions: coefficients
#' from their posterior (parameter uncertainty), then counts from the fitted
#' distribution (outcome variation). `method = "expected"` is a deterministic
#' expected-value replacement and is an exploratory scenario with no
#' uncertainty; it is not multiple imputation.
#'
#' `mode = "operational"` refits for each target month using only data up to
#' that month; `"retrospective"` fits once to all retained data.
#'
#' @param data Flagged data, such as `run$data_flagged`.
#' @param scenario A [qc_scenario()].
#' @param mode `"retrospective"` or `"operational"`.
#' @param method `"multiple"` or `"expected"`.
#' @param n_imputations Number of completed datasets for `"multiple"`.
#' @param seed Random seed, recorded in the result.
#' @param min_train_rows Minimum retained rows needed to fit a model.
#' @param min_facility_months Minimum retained months a facility needs for its
#'   own gaps to be filled.
#' @param allow_unseen_facility Whether a facility with no retained history is
#'   filled from the population-level model. `FALSE` leaves it missing.
#' @param schedule Optional active-facility schedule (`facility_id`,
#'   `first_active_month`, `last_active_month`). Without it the facility window
#'   is the first to last month it reported, and that is recorded.
#' @param priority Flag priorities.
#'
#' @return A `qc_imputation` object.
#' @export
qc_impute_scenario <- function(data, scenario,
                               mode = c('retrospective', 'operational'),
                               method = c('multiple', 'expected'),
                               n_imputations = 20L, seed = 1L,
                               min_train_rows = 60L, min_facility_months = 3L,
                               allow_unseen_facility = FALSE, schedule = NULL,
                               priority = .default_qc_priority()) {
  mode <- match.arg(mode)
  method <- match.arg(method)
  if (!.is_scalar_number(n_imputations) || n_imputations < 2) {
    rlang::abort('`n_imputations` must be at least 2.')
  }
  scn <- apply_qc_scenario(data, scenario, priority)
  fill_pre <- isTRUE(scenario$handling$fill_preexisting)
  windows <- .facility_windows(scn, schedule)
  scn$.absent <- FALSE

  if (fill_pre) {
    absent <- .absent_report_rows(scn, windows)
    if (!is.null(absent) && nrow(absent) > 0L) {
      template <- scn[match(absent$facility_id, as.character(scn$facility_id)), , drop = FALSE]
      template$facility_id <- absent$facility_id
      template$month_date <- absent$month_date
      for (nm in c('tested', 'positive', 'attending')) if (nm %in% names(template)) template[[nm]] <- NA_real_
      template$scn_tested_state <- 'missing'
      template$scn_positive_state <- 'missing'
      template$scn_omit_tested <- FALSE
      template$scn_omit_positive <- FALSE
      template$scn_omit_attending <- FALSE
      template$scn_omit_coverage <- FALSE
      template$tested_parse_status <- 'missing'
      template$positive_parse_status <- 'missing'
      template$.absent <- TRUE
      scn <- dplyr::bind_rows(scn, template)
    }
  }

  counts <- .baseline_counts(scn)
  month_origin <- min(.qc_month_index(scn$month_date))
  in_window <- {
    w <- windows[match(as.character(scn$facility_id), windows$facility_id), ]
    scn$month_date >= w$first & scn$month_date <= w$last
  }
  targetable <- function(state) {
    state %in% c('qc_omitted', 'invalid') | (fill_pre & state == 'missing')
  }
  need_t <- scn$scn_facility_eligible & in_window & targetable(scn$scn_tested_state)
  need_p <- scn$scn_facility_eligible & in_window & targetable(scn$scn_positive_state)
  obs_t <- scn$scn_facility_eligible & scn$scn_tested_state == 'observed' & !is.na(counts$tested)
  obs_p <- obs_t & scn$scn_positive_state == 'observed' & !is.na(counts$positive) &
    counts$positive <= counts$tested

  target_idx <- which(need_t | need_p)
  n_tg <- length(target_idx)
  M <- if (method == 'multiple') as.integer(n_imputations) else 1L
  tested_draw <- matrix(NA_real_, n_tg, M)
  positive_draw <- matrix(NA_real_, n_tg, M)
  status_t <- rep(NA_character_, n_tg)
  status_p <- rep(NA_character_, n_tg)
  tg_month <- scn$month_date[target_idx]
  fits <- list()
  set.seed(seed)

  groups <- if (mode == 'operational') split(seq_len(n_tg), as.character(tg_month)) else list(all = seq_len(n_tg))
  for (g in names(groups)) {
    pos <- groups[[g]]
    if (length(pos) == 0L) next
    cutoff <- if (mode == 'operational') as.Date(g) else max(scn$month_date)
    window <- scn$month_date <= cutoff
    train_t <- which(obs_t & window)
    train_p <- which(obs_p & window & counts$tested > 0)
    fail <- function(reason) {
      status_t[pos][need_t[target_idx[pos]]] <<- reason
      status_p[pos][need_p[target_idx[pos]]] <<- reason
    }
    if (length(train_t) < min_train_rows) {
      fail('insufficient_history')
      next
    }
    facility_levels <- sort(unique(as.character(scn$facility_id[train_t])))
    df_t <- cbind(.design_data(scn[train_t, ], facility_levels, month_origin),
                  tested = counts$tested[train_t])
    fit_t <- .fit_count_model(df_t, 'tested', mgcv::nb())
    fit_p <- NULL
    if (length(train_p) >= min_train_rows) {
      df_p <- cbind(
        .design_data(scn[train_p, ], facility_levels, month_origin, counts$tested[train_p]),
        positive = counts$positive[train_p], negative = counts$tested[train_p] - counts$positive[train_p]
      )
      fit_p <- .fit_count_model(df_p, 'cbind(positive, negative)', stats::binomial(),
                                extra = 'log1p_tested')
    }
    fits[[g]] <- list(
      cutoff = cutoff, training_rows_tested = length(train_t),
      training_rows_positive = length(train_p),
      structure_tested = if (is.null(fit_t)) NA_character_ else fit_t$structure,
      structure_positive = if (is.null(fit_p)) NA_character_ else fit_p$structure,
      formula_tested = if (is.null(fit_t)) NA_character_ else fit_t$formula,
      formula_positive = if (is.null(fit_p)) NA_character_ else fit_p$formula,
      training_fingerprint = digest::digest(
        list(as.character(scn$facility_id[train_t]), scn$month_date[train_t], counts$tested[train_t],
             counts$positive[train_t]), algo = 'sha256'
      )
    )
    if (is.null(fit_t)) {
      fail('model_unavailable')
      next
    }
    rows <- target_idx[pos]
    facility_n <- table(as.character(scn$facility_id[train_t]))
    d_t <- .design_data(scn[rows, ], facility_levels, month_origin)
    known <- attr(d_t, 'known_facility')
    sparse <- known & (facility_n[as.character(scn$facility_id[rows])] < min_facility_months)
    sparse[is.na(sparse)] <- FALSE
    unseen <- !known
    blocked <- sparse | (unseen & !allow_unseen_facility)
    reason <- ifelse(sparse, 'sparse_facility_history',
                     ifelse(unseen & !allow_unseen_facility, 'unseen_facility', NA_character_))

    X_t <- .lpmatrix(fit_t$model, d_t, zero_facility = unseen)
    theta <- tryCatch(fit_t$model$family$getTheta(TRUE), error = function(e) NA_real_)
    if (!is.finite(theta)) theta <- 1
    B_t <- .draw_coefficients(fit_t$model, if (method == 'multiple') M else 0L)
    mu_t <- exp(pmin(X_t %*% B_t, 20))
    phi <- if (!is.null(fit_p)) {
      p_resid <- stats::residuals(fit_p$model, type = 'pearson')
      max(1, sum(p_resid^2) / max(1, length(p_resid) - sum(fit_p$model$edf)))
    } else {
      1
    }
    rho <- (phi - 1) / max(1, mean(counts$tested[train_p], na.rm = TRUE) - 1)
    B_p <- if (!is.null(fit_p)) .draw_coefficients(fit_p$model, if (method == 'multiple') M else 0L)

    for (m in seq_len(M)) {
      lower <- ifelse(need_t[rows], 0, 0)
      keep_obs_t <- !need_t[rows]
      obs_tested_here <- ifelse(keep_obs_t, counts$tested[rows], NA_real_)
      pos_floor <- ifelse(need_t[rows] & !need_p[rows] & !is.na(counts$positive[rows]),
                          counts$positive[rows], 0)
      tested_m <- if (method == 'multiple') {
        .rnb_truncated(mu_t[, m], theta, pos_floor)
      } else {
        pmax(round(mu_t[, m]), pos_floor)
      }
      tested_m <- ifelse(keep_obs_t, obs_tested_here, tested_m)
      tested_m[blocked & need_t[rows]] <- NA_real_
      tested_m[blocked & !need_t[rows]] <- obs_tested_here[blocked & !need_t[rows]]
      tested_draw[pos, m] <- tested_m
      if (need_p_any <- any(need_p[rows])) {
        if (is.null(fit_p)) {
          next
        }
        tested_for_p <- tested_m
        ok <- need_p[rows] & !is.na(tested_for_p) & !blocked
        if (any(ok)) {
          d_p <- .design_data(scn[rows[ok], ], facility_levels, month_origin, tested_for_p[ok])
          X_p <- .lpmatrix(fit_p$model, d_p, zero_facility = unseen[ok])
          p_hat <- stats::plogis(X_p %*% B_p[, if (method == 'multiple') m else 1])
          draw <- if (method == 'multiple') {
            .rbetabinom(tested_for_p[ok], as.numeric(p_hat), rho)
          } else {
            round(as.numeric(p_hat) * tested_for_p[ok])
          }
          out_p <- rep(NA_real_, length(rows))
          out_p[ok] <- pmin(pmax(draw, 0), tested_for_p[ok])
          positive_draw[pos, m] <- out_p
        }
      }
    }
    keep_pos <- !need_p[rows]
    positive_draw[pos[keep_pos], ] <- counts$positive[rows[keep_pos]]
    st_t <- ifelse(!need_t[rows], NA_character_,
                   ifelse(!is.na(reason), reason, ifelse(is.na(tested_draw[pos, 1]), 'model_unavailable', 'imputed')))
    st_p <- ifelse(!need_p[rows], NA_character_,
                   ifelse(!is.na(reason), reason,
                          ifelse(is.null(fit_p), 'model_unavailable',
                                 ifelse(is.na(positive_draw[pos, 1]),
                                        ifelse(is.na(tested_draw[pos, 1]), 'tested_unavailable', 'model_unavailable'),
                                        'imputed'))))
    status_t[pos] <- st_t
    status_p[pos] <- st_p
  }

  targets <- dplyr::tibble(
    row = target_idx,
    facility_id = as.character(scn$facility_id[target_idx]),
    month_date = scn$month_date[target_idx],
    need_tested = need_t[target_idx], need_positive = need_p[target_idx],
    absent_report = scn$.absent[target_idx],
    tested_status = status_t, positive_status = status_p
  )
  structure(list(
    scenario = scenario, scn = scn, targets = targets,
    tested_draws = tested_draw, positive_draws = positive_draw,
    provenance = list(
      model_version = .imputation_model_version,
      package_version = .package_version(),
      mode = mode, method = method, n_imputations = M, seed = seed,
      min_train_rows = min_train_rows, min_facility_months = min_facility_months,
      allow_unseen_facility = allow_unseen_facility,
      fill_preexisting = fill_pre,
      window_source = unique(windows$source),
      fits = fits,
      uncertainty = if (method == 'multiple') 'multiple_imputation' else 'exploratory_expected_value',
      statement = paste(
        'Imputed counts are replacement values, not observations: they are not added to',
        'the observed sample size. Masking experiments do not establish validity under every',
        'missingness mechanism, and removal driven by extreme outcomes can be informative.'
      )
    )
  ), class = 'qc_imputation')
}

#' @export
print.qc_imputation <- function(x, ...) {
  cat('<qc_imputation> ', x$provenance$method, ', ', x$provenance$mode, ', scenario ',
      x$scenario$name, '\n', sep = '')
  print(summary(x))
  invisible(x)
}

#' @export
summary.qc_imputation <- function(object, ...) {
  tg <- object$targets
  status <- c(tg$tested_status[tg$need_tested], tg$positive_status[tg$need_positive])
  field <- c(rep('tested', sum(tg$need_tested)), rep('positive', sum(tg$need_positive)))
  dplyr::count(dplyr::tibble(field = field, status = status), field, status)
}

# The completed frame for draw `m`: retained observations unchanged, targeted
# contributions replaced where the models supported it.
.imputed_frame <- function(object, m = 1L) {
  frame <- .scenario_frame(object$scn, 'filtered')
  tg <- object$targets
  if (nrow(tg) == 0L) return(frame)
  tested <- object$tested_draws[, m]
  positive <- object$positive_draws[, m]
  put_t <- tg$need_tested & !is.na(tested)
  put_p <- tg$need_positive & !is.na(positive)
  frame$tested[tg$row[put_t]] <- tested[put_t]
  frame$positive[tg$row[put_p]] <- positive[put_p]
  frame$tested_imputed[tg$row[put_t]] <- TRUE
  frame$positive_imputed[tg$row[put_p]] <- TRUE
  frame
}

.rubin <- function(estimate, variance) {
  M <- length(estimate)
  ok <- is.finite(estimate)
  if (sum(ok) < 2L) return(list(q = if (any(ok)) mean(estimate[ok]) else NA_real_, se = NA_real_,
                                 lo = NA_real_, hi = NA_real_, fmi = NA_real_, between = NA_real_))
  q <- estimate[ok]
  u <- variance[ok]
  m <- length(q)
  qbar <- mean(q)
  ubar <- if (any(is.finite(u))) mean(u[is.finite(u)]) else NA_real_
  b <- stats::var(q)
  total <- ubar + (1 + 1 / m) * b
  if (!is.finite(total)) return(list(q = qbar, se = NA_real_, lo = NA_real_, hi = NA_real_,
                                     fmi = NA_real_, between = b))
  r <- (1 + 1 / m) * b / ubar
  df <- if (is.finite(r) && r > 0) (m - 1) * (1 + 1 / r)^2 else Inf
  half <- stats::qt(0.975, df) * sqrt(total)
  list(q = qbar, se = sqrt(total), lo = qbar - half, hi = qbar + half,
       fmi = (1 + 1 / m) * b / total, between = b)
}

#' Aggregate Imputed Scenario Results
#'
#' Aggregates within each completed dataset and, for multiple imputation,
#' combines the estimates with Rubin's rules: the total variance is the mean
#' within-imputation variance plus (1 + 1/M) times the between-imputation
#' variance, with a t reference distribution. Within-imputation variances come
#' from a facility-clustered ratio estimator. Imputed counts are not added to
#' the observed sample size; the between-imputation term carries their
#' uncertainty. The interval is a statistical interval for one scenario: it is
#' not a measure of sensitivity across scenarios.
#'
#' An `"expected"` imputation has no interval (`ci_low`/`ci_high` are `NA`).
#'
#' @param object A [qc_impute_scenario()] result.
#' @param level,monthly As for [qc_aggregate_metrics()].
#'
#' @return A tibble with positivity (and interval), coverage, tested and attendance
#'   totals and usable-row counts, counts retained,
#'   counts from imputation, the share of tested from imputation, and the
#'   number of contributions left missing.
#' @export
summarise_qc_imputation <- function(object, level = 'national', monthly = TRUE) {
  if (!inherits(object, 'qc_imputation')) rlang::abort('`object` must come from `qc_impute_scenario()`.')
  M <- object$provenance$n_imputations
  keys <- c(.aggregate_group_vars(level), if (monthly) 'month_date')
  draws <- lapply(seq_len(M), function(m) {
    out <- qc_aggregate_metrics(.imputed_frame(object, m), level, monthly)
    out$.draw <- m
    out
  })
  stacked <- dplyr::bind_rows(draws)
  multiple <- object$provenance$method == 'multiple'
  coverage_se <- function(t, a) NA_real_
  combined <- stacked %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(keys))) %>%
    dplyr::summarise(
      facility_months = dplyr::first(facility_months),
      facilities = dplyr::first(facilities),
      pos = list(.rubin(positivity, positivity_se^2)),
      cov = list(.rubin(coverage, rep(NA_real_, dplyr::n()))),
      tested_total = mean(tested_total),
      tested_rows = round(mean(.data$tested_rows)),
      attendance_total = mean(.data$attendance_total),
      attendance_rows = round(mean(.data$attendance_rows)),
      tested_total_between_sd = if (dplyr::n() > 1L) stats::sd(tested_total) else NA_real_,
      tested_matched = mean(tested_matched), positive_matched = mean(positive_matched),
      pair_rows = round(mean(pair_rows)),
      coverage_rows = round(mean(coverage_rows)),
      tested_from_imputation = mean(matched_tested_from_imputation),
      prop_tested_from_imputation = mean(prop_tested_from_imputation),
      .groups = 'drop'
    )
  combined$positivity <- vapply(combined$pos, `[[`, numeric(1), 'q')
  combined$positivity_se <- vapply(combined$pos, `[[`, numeric(1), 'se')
  combined$ci_low <- if (multiple) pmax(0, vapply(combined$pos, `[[`, numeric(1), 'lo')) else NA_real_
  combined$ci_high <- if (multiple) pmin(1, vapply(combined$pos, `[[`, numeric(1), 'hi')) else NA_real_
  combined$fraction_missing_information <- if (multiple) vapply(combined$pos, `[[`, numeric(1), 'fmi') else NA_real_
  combined$coverage <- vapply(combined$cov, `[[`, numeric(1), 'q')
  combined$pos <- NULL
  combined$cov <- NULL
  tg <- object$targets
  combined$uncertainty <- object$provenance$uncertainty
  attr(combined, 'unsupported') <- dplyr::count(
    dplyr::bind_rows(
      dplyr::transmute(tg[tg$need_tested, ], field = 'tested', status = tested_status),
      dplyr::transmute(tg[tg$need_positive, ], field = 'positive', status = positive_status)
    ), field, status
  )
  combined
}

#' Scenario Results for Display
#'
#' Stacks the reported baseline, the filtered scenario with excluded
#' contributions left missing, and (when supplied) the imputed scenario in one
#' table for a geographical level, with the contributions omitted and left
#' missing.
#'
#' @param data Flagged data.
#' @param scenario A [qc_scenario()].
#' @param level,monthly As for [qc_aggregate_metrics()].
#' @param imputation Optional [qc_impute_scenario()] result for the same data
#'   and scenario.
#' @param priority Flag priorities.
#'
#' @return A tibble with a `version` column.
#' @export
qc_scenario_results <- function(data, scenario, level = 'national', monthly = TRUE,
                                imputation = NULL, priority = .default_qc_priority()) {
  out <- compare_qc_scenario(data, scenario, level, monthly, priority)
  provenance <- attr(out, 'scenario_provenance')
  if (!is.null(imputation)) {
    imputed <- summarise_qc_imputation(imputation, level, monthly)
    imputed$version <- 'filtered_imputed'
    out <- dplyr::bind_rows(out, imputed)
    provenance$imputation <- imputation$provenance
  }
  attr(out, 'scenario_provenance') <- provenance
  out
}
