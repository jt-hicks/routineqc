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

.unit_field <- function(data) {
  field <- .nav_mid_field(data)
  if (is.null(field)) 'region' else field
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
                 legend = list(orientation = 'h', y = -0.35), margin = list(l = 170)) %>%
    plotly::style(textposition = 'none')
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
                 legend = list(orientation = 'h', y = -0.2)) %>%
    plotly::style(textposition = 'none')
}
