# Overview calculations. The central rule: a record a check could not assess
# is not a record that passed it, so every check reports how many records it
# assessed, how many it could not and why. There is deliberately no composite
# quality score.

.assessment_states <- c('assessed, not flagged', 'assessed, flagged', 'missing or unusable inputs',
                        'insufficient history', 'prediction unavailable')

.assessment_checks <- c(
  count_consistency = 'Count consistency',
  prevalence = 'Positivity versus model',
  tested_volume = 'Tested-volume history',
  attendance = 'Attendance and coverage'
)

.lg <- function(data, col) {
  if (col %in% names(data)) dplyr::coalesce(as.logical(data[[col]]), FALSE) else rep(FALSE, nrow(data))
}

.map_prediction_status <- function(status) {
  status <- as.character(status)
  dplyr::case_when(
    status == 'assessed' ~ 'assessed',
    grepl('history|sparse|insufficient', status) ~ 'insufficient history',
    grepl('missing|invalid|core|nonpositive|predictor', status) ~ 'missing or unusable inputs',
    grepl('unavailable', status) ~ 'prediction unavailable',
    is.na(status) ~ 'prediction unavailable',
    TRUE ~ 'prediction unavailable'
  )
}

.row_assessment <- function(data) {
  counts <- .usable_counts(data)
  base <- .baseline_counts(data)
  both <- !is.na(counts$tested) & !is.na(counts$positive)
  n <- nrow(data)
  family_flag <- list(
    count_consistency = .lg(data, 'flag_core_invalid') | .lg(data, 'flag_count_invalid_value'),
    prevalence = .lg(data, 'flag_resid_extreme') | .lg(data, 'flag_all_negative_large_n') |
      .lg(data, 'flag_all_positive_large_n') | .lg(data, 'flag_large_monthly_prevalence_change'),
    tested_volume = .lg(data, 'flag_tested_volume_extreme'),
    attendance = .lg(data, 'flag_tested_gt_attending') | .lg(data, 'flag_attending_negative') |
      .lg(data, 'flag_coverage_low') | .lg(data, 'flag_coverage_very_low')
  )
  finish <- function(assessed, other, flagged) {
    out <- ifelse(assessed, ifelse(flagged, 'assessed, flagged', 'assessed, not flagged'), other)
    factor(out, levels = .assessment_states)
  }
  prediction <- if ('prediction_status' %in% names(data)) .map_prediction_status(data$prediction_status) else
    rep('prediction unavailable', n)
  baseline_ready <- .lg(data, 'tested_baseline_ready')
  attend_ok <- !is.na(base$attending)
  # Count consistency needs both counts as entered (invalid ones are themselves the finding).
  count_assessed <- both | .lg(data, 'flag_count_invalid_value')
  dplyr::tibble(
    count_consistency = finish(count_assessed, 'missing or unusable inputs', family_flag$count_consistency),
    prevalence = finish(prediction == 'assessed', prediction, family_flag$prevalence),
    tested_volume = finish(
      !is.na(counts$tested) & baseline_ready,
      ifelse(is.na(counts$tested), 'missing or unusable inputs', 'insufficient history'),
      family_flag$tested_volume
    ),
    attendance = finish(!is.na(counts$tested) & attend_ok, 'missing or unusable inputs', family_flag$attendance)
  )
}

.assessment_summary <- function(data) {
  states <- .row_assessment(data)
  out <- dplyr::bind_rows(lapply(names(.assessment_checks), function(k) {
    counts <- table(states[[k]])
    dplyr::tibble(check = .assessment_checks[[k]], state = factor(names(counts), levels = .assessment_states),
                  n = as.integer(counts))
  }))
  out$share <- .safe_divide(out$n, rep(nrow(data), nrow(out)))
  out
}

# Completeness within each facility's active window. Without a supplied
# schedule the window is first to last report, which cannot see a facility that
# stopped reporting or started late; the result says so.
.reporting_completeness <- function(data, schedule = NULL) {
  windows <- .facility_windows(data, schedule)
  months <- .explore_months(data)
  expected <- sum(vapply(seq_len(nrow(windows)), function(i) {
    sum(months >= windows$first[i] & months <= windows$last[i])
  }, integer(1)))
  usable <- .usable_counts(data)$tested
  in_window <- {
    w <- windows[match(as.character(data$facility_id), windows$facility_id), ]
    data$month_date >= w$first & data$month_date <= w$last
  }
  reporting <- sum(!is.na(usable) & in_window)
  list(
    expected = expected, reporting = reporting, share = .safe_divide(reporting, expected),
    rows_without_usable_tested = sum(is.na(usable)),
    absent = max(0L, expected - sum(in_window)),
    basis = if (is.null(schedule)) 'inferred from first to last report (no schedule supplied)'
            else 'supplied facility schedule'
  )
}

.unit_field <- function(data) {
  field <- .nav_mid_field(data)
  if (is.null(field)) 'region' else field
}

.family_flags <- function(data) {
  list(
    `Count consistency` = .lg(data, 'flag_core_invalid') | .lg(data, 'flag_count_invalid_value'),
    Prevalence = .lg(data, 'flag_resid_extreme') | .lg(data, 'flag_all_negative_large_n') |
      .lg(data, 'flag_all_positive_large_n') | .lg(data, 'flag_large_monthly_prevalence_change'),
    `Tested volume` = .lg(data, 'flag_tested_volume_extreme'),
    Attendance = .lg(data, 'flag_tested_gt_attending') | .lg(data, 'flag_attending_negative'),
    `Testing coverage` = .lg(data, 'flag_coverage_low') | .lg(data, 'flag_coverage_very_low')
  )
}

.family_assessed <- function(data) {
  states <- .row_assessment(data)
  ok <- function(x) x %in% c('assessed, not flagged', 'assessed, flagged')
  list(
    `Count consistency` = ok(states$count_consistency), Prevalence = ok(states$prevalence),
    `Tested volume` = ok(states$tested_volume), Attendance = ok(states$attendance),
    `Testing coverage` = ok(states$attendance)
  )
}

# Council (or region) by issue family: the share of ASSESSED records flagged,
# with the number assessed beside it so small denominators are visible.
.unit_family_matrix <- function(data) {
  field <- .unit_field(data)
  unit <- as.character(data[[field]])
  flags <- .family_flags(data)
  assessed <- .family_assessed(data)
  rows <- lapply(names(flags), function(f) {
    keep <- !is.na(unit) & trimws(unit) != ''
    tot <- tapply(assessed[[f]][keep], unit[keep], sum)
    hit <- tapply((flags[[f]] & assessed[[f]])[keep], unit[keep], sum)
    dplyr::tibble(unit = names(tot), family = f, assessed = as.integer(tot), flagged = as.integer(hit),
                  rate = .safe_divide(as.integer(hit), as.integer(tot)))
  })
  out <- dplyr::bind_rows(rows)
  attr(out, 'unit_field') <- field
  out
}

.ranked_units <- function(data) {
  field <- .unit_field(data)
  unit <- as.character(data[[field]])
  keep <- !is.na(unit) & trimws(unit) != ''
  d <- data[keep, , drop = FALSE]
  unit <- unit[keep]
  flagged <- .is_flagged_row(d)
  high <- flagged & as.character(d$review_priority) %in% c('critical', 'high')
  states <- .row_assessment(d)
  unassessed <- states$prevalence %in% c('missing or unusable inputs', 'insufficient history',
                                         'prediction unavailable')
  out <- dplyr::tibble(
    unit = sort(unique(unit))
  )
  by <- function(x) as.numeric(tapply(x, factor(unit, levels = out$unit), sum))
  out$facilities <- as.integer(tapply(as.character(d$facility_id), factor(unit, levels = out$unit),
                                      function(x) length(unique(x))))
  out$records <- as.integer(tapply(rep(1L, length(unit)), factor(unit, levels = out$unit), sum))
  out$review_rows <- as.integer(by(flagged))
  out$high_priority_rows <- as.integer(by(high))
  out$share_flagged <- .safe_divide(out$review_rows, out$records)
  out$share_not_assessed_for_positivity <- .safe_divide(as.integer(by(unassessed)), out$records)
  attr(out, 'unit_field') <- field
  out[order(-out$high_priority_rows, -out$share_flagged, out$unit), ]
}

.overview_trend <- function(data) {
  months <- .explore_months(data)
  flagged <- .is_flagged_row(data)
  priority <- as.character(data$review_priority)
  priority[!priority %in% names(.qc_priority_palette)] <- 'none'
  rows <- lapply(c('critical', 'high', 'medium', 'low'), function(p) {
    counts <- table(factor(data$month_date[flagged & priority == p], levels = as.character(months)))
    dplyr::tibble(month_date = months, priority = p, rows = as.integer(counts))
  })
  total <- as.integer(table(factor(data$month_date, levels = as.character(months))))
  out <- dplyr::bind_rows(rows)
  out$records <- rep(total, 4L)
  out$share <- .safe_divide(out$rows, out$records)
  out
}

.plot_assessment <- function(summary) {
  palette <- c(`assessed, not flagged` = '#7fb77e', `assessed, flagged` = '#e66101',
               `missing or unusable inputs` = '#f2cf5b', `insufficient history` = '#bdbdbd',
               `prediction unavailable` = '#8da0cb')
  plot <- plotly::plot_ly()
  for (s in .assessment_states) {
    d <- summary[summary$state == s, ]
    plot <- plotly::add_bars(
      plot, y = d$check, x = d$share, name = s, orientation = 'h', marker = list(color = palette[[s]]),
      text = paste0(d$check, '<br>', s, ': ', format(d$n, big.mark = ','), ' records (',
                    sprintf('%.1f%%', 100 * d$share), ')'), hoverinfo = 'text'
    )
  }
  plotly::layout(plot, barmode = 'stack', xaxis = list(title = 'Share of records', tickformat = '.0%',
                                                        range = c(0, 1)),
                 yaxis = list(title = '', autorange = 'reversed'),
                 legend = list(orientation = 'h', y = -0.25), margin = list(l = 170))
}

.plot_unit_family_heatmap <- function(matrix, max_units = 40L) {
  field <- attr(matrix, 'unit_field')
  ranked <- unique(matrix$unit[order(-matrix$flagged, matrix$unit)])
  by_unit <- tapply(matrix$flagged, matrix$unit, sum)
  units <- names(sort(by_unit, decreasing = TRUE))
  units <- utils::head(units, max_units)
  m <- matrix[matrix$unit %in% units, ]
  m$unit <- factor(m$unit, levels = rev(units))
  m$text <- paste0(m$unit, '<br>', m$family, '<br>',
                   ifelse(m$assessed == 0L, 'No records assessed',
                          paste0(m$flagged, ' of ', m$assessed, ' assessed records flagged (',
                                 sprintf('%.1f%%', 100 * m$rate), ')')))
  plotly::plot_ly(m, x = ~family, y = ~unit, z = ~rate, type = 'heatmap', text = ~text, hoverinfo = 'text',
                  colorscale = list(c(0, '#f7fbff'), c(1, '#b2182b')), zmin = 0,
                  colorbar = list(title = 'Flagged', tickformat = '.0%')) %>%
    plotly::layout(xaxis = list(title = '', side = 'top'),
                   yaxis = list(title = .if_null(.nav_mid_label(field), 'Region')))
}

.plot_overview_trend <- function(trend) {
  plot <- plotly::plot_ly()
  for (p in c('critical', 'high', 'medium', 'low')) {
    d <- trend[trend$priority == p, ]
    plot <- plotly::add_bars(plot, x = d$month_date, y = d$share, name = p,
                             marker = list(color = .qc_priority_palette[[p]]),
                             text = paste0(format(d$month_date, '%b %Y'), '<br>', p, ': ', d$rows, ' of ',
                                           d$records, ' records'), hoverinfo = 'text')
  }
  plotly::layout(plot, barmode = 'stack', xaxis = list(title = ''),
                 yaxis = list(title = 'Records flagged for review', tickformat = '.0%'),
                 legend = list(orientation = 'h', y = -0.2))
}
