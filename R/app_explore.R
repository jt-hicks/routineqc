# Data preparation and plots for the Explore data view. The calculations are
# pure functions of the flagged data and a selection; the Shiny layer only wires
# them to inputs.

# A stable row id so a click on a plotted point or a table row can be traced
# back to exactly one record.
.explore_prepare <- function(data) {
  data <- dplyr::as_tibble(data)
  data$.row <- seq_len(nrow(data))
  data$facility_id <- as.character(data$facility_id)
  data
}

.explore_months <- function(data) {
  seq(min(data$month_date), max(data$month_date), by = 'month')
}

.is_flagged_row <- function(data) {
  review <- if ('flag_review_recommended' %in% names(data)) dplyr::coalesce(data$flag_review_recommended, FALSE) else FALSE
  exclude <- if ('flag_exclude_authorized' %in% names(data)) dplyr::coalesce(data$flag_exclude_authorized, FALSE) else FALSE
  review | exclude
}

# Monthly series for the selected unit. Months with no usable contribution are
# NA, not zero, so gaps stay visible; a genuine zero tested stays zero.
.explore_series <- function(data, selection) {
  months <- .explore_months(data)
  sub <- .nav_subset(data, selection)
  grid <- dplyr::tibble(month_date = months)
  if (nrow(sub) == 0L) {
    return(dplyr::mutate(grid, positivity = NA_real_, tested = NA_real_, attendance = NA_real_,
                         coverage = NA_real_, facilities_expected = 0L, facilities_reporting = 0L,
                         pair_rows = 0L, flagged_rows = 0L, priority_rows = 0L))
  }
  frame <- .scenario_frame(sub, 'reported')
  agg <- qc_aggregate_metrics(frame, 'national', monthly = TRUE)
  agg <- agg[c('month_date', 'positivity', 'tested_total', 'tested_rows', 'attendance_total',
               'attendance_rows', 'coverage', 'pair_rows')]
  agg$tested <- ifelse(agg$tested_rows > 0L, agg$tested_total, NA_real_)
  agg$attendance <- ifelse(agg$attendance_rows > 0L, agg$attendance_total, NA_real_)

  windows <- .facility_windows(sub)
  expected <- vapply(months, function(m) sum(windows$first <= m & windows$last >= m), integer(1))
  reporting <- vapply(months, function(m) {
    length(unique(frame$facility_id[frame$month_date == m & !is.na(frame$tested)]))
  }, integer(1))
  flagged <- .is_flagged_row(sub)
  high <- flagged & as.character(sub$review_priority) %in% c('critical', 'high')
  per_month <- function(flag) {
    counts <- table(factor(sub$month_date[flag], levels = as.character(months)))
    as.integer(counts)
  }
  grid$facilities_expected <- expected
  grid$facilities_reporting <- reporting
  grid$flagged_rows <- per_month(flagged)
  grid$priority_rows <- per_month(high)
  out <- dplyr::left_join(grid, agg[c('month_date', 'positivity', 'tested', 'attendance', 'coverage', 'pair_rows')],
                          by = 'month_date')
  out$pair_rows[is.na(out$pair_rows)] <- 0L
  out
}

# Per-month state of one facility, separating the ways a value can be absent.
.explore_facility_states <- function(data, facility) {
  months <- .explore_months(data)
  sub <- data[data$facility_id == facility, , drop = FALSE]
  invalid <- if (nrow(sub) > 0L) .row_count_invalid(sub) else logical()
  usable <- if (nrow(sub) > 0L) .usable_counts(sub)$tested else numeric()
  state <- rep('absent', length(months))
  idx <- match(months, sub$month_date)
  has <- !is.na(idx)
  state[has] <- ifelse(invalid[idx[has]], 'invalid count',
                       ifelse(is.na(usable[idx[has]]), 'missing count',
                              ifelse(usable[idx[has]] == 0, 'zero tested', 'reported')))
  dplyr::tibble(month_date = months, state = state)
}

.explore_state_palette <- c(reported = '#4c78a8', `zero tested` = '#9ecae1', `missing count` = '#f2cf5b',
                            `invalid count` = '#b2182b', absent = '#d9d9d9')

.explore_children <- function(data, selection) {
  mid_field <- .nav_mid_field(data)
  child <- .nav_child_level(.nav_normalise(selection), mid_field)
  if (is.na(child)) return(NULL)
  sub <- .nav_subset(data, selection)
  if (nrow(sub) == 0L) return(NULL)
  field <- switch(child, region = 'region', mid = mid_field, facility = 'facility_id')
  frame <- .scenario_frame(sub, 'reported')
  unit <- as.character(sub[[field]])
  keep <- !is.na(unit) & trimws(unit) != ''
  frame <- frame[keep, , drop = FALSE]
  sub <- sub[keep, , drop = FALSE]
  if (nrow(frame) == 0L) return(NULL)
  agg_level <- switch(child, region = 'region', mid = 'district', facility = 'facility')
  if (child == 'region') frame$region <- unit[keep]
  if (child == 'mid') frame$district <- unit[keep]
  agg <- qc_aggregate_metrics(frame, agg_level, monthly = FALSE)
  key <- switch(child, region = 'region', mid = 'district', facility = 'facility_id')
  out <- dplyr::tibble(unit = as.character(agg[[key]]))
  out$facilities <- if ('facilities' %in% names(agg)) agg$facilities else 1L
  out$facility_months <- agg$facility_months
  out$positivity <- agg$positivity
  out$tested <- ifelse(agg$tested_rows > 0L, agg$tested_total, NA_real_)
  out$coverage <- agg$coverage
  flagged <- .is_flagged_row(sub)
  high <- flagged & as.character(sub$review_priority) %in% c('critical', 'high')
  unit_of_row <- as.character(sub[[field]])
  out$review_rows <- as.integer(tapply(flagged, factor(unit_of_row, levels = out$unit), sum))
  out$high_priority_rows <- as.integer(tapply(high, factor(unit_of_row, levels = out$unit), sum))
  out$review_rows[is.na(out$review_rows)] <- 0L
  out$high_priority_rows[is.na(out$high_priority_rows)] <- 0L
  out$share_flagged <- .safe_divide(out$review_rows, out$facility_months)
  attr(out, 'child_level') <- child
  out[order(-out$high_priority_rows, -out$share_flagged, out$unit), ]
}

# Records to show for a selection, with the facility's own series values so a
# flagged point can be drawn on the chart.
.explore_flagged_points <- function(data, selection, metric) {
  sub <- .nav_subset(data, selection)
  sub <- sub[.is_flagged_row(sub), , drop = FALSE]
  if (nrow(sub) == 0L) return(sub)
  counts <- .usable_counts(sub)
  attending <- .baseline_counts(sub)$attending
  value <- switch(
    metric,
    positivity = ifelse(!is.na(counts$tested) & !is.na(counts$positive) & counts$tested > 0 &
                          counts$positive <= counts$tested, counts$positive / counts$tested, NA_real_),
    tested = counts$tested,
    attendance = attending,
    coverage = ifelse(!is.na(counts$tested) & !is.na(attending) & attending > 0, counts$tested / attending, NA_real_)
  )
  sub$.value <- value
  sub[!is.na(sub$.value), , drop = FALSE]
}

.format_metric <- function(x, metric) {
  if (.explore_metrics[[metric]]$percent) {
    ifelse(is.na(x), 'no valid data', sprintf('%.1f%%', 100 * x))
  } else {
    ifelse(is.na(x), 'no valid data', format(round(x), big.mark = ',', trim = TRUE))
  }
}

.plot_explore_series <- function(series, metric, points = NULL, title = NULL, source = 'explore') {
  spec <- .explore_metrics[[metric]]
  y <- series[[metric]]
  text <- paste0(
    format(series$month_date, '%b %Y'), '<br>', spec$short, ': ', .format_metric(y, metric),
    '<br>Facilities reporting: ', series$facilities_reporting, ' of ', series$facilities_expected,
    '<br>Review or excluded rows: ', series$flagged_rows
  )
  plot <- plotly::plot_ly(source = source) %>%
    plotly::add_trace(
      x = series$month_date, y = y, type = 'scatter', mode = 'lines+markers',
      connectgaps = FALSE, line = list(color = '#4c78a8', width = 2),
      marker = list(color = '#4c78a8', size = 5), text = text, hoverinfo = 'text',
      name = spec$short, showlegend = FALSE
    )
  if (!is.null(points) && nrow(points) > 0L) {
    tip <- .qc_row_tooltip(points)
    priority <- as.character(points$review_priority)
    priority[is.na(priority) | !priority %in% names(.qc_priority_palette)] <- 'none'
    for (level in intersect(c('critical', 'high', 'medium', 'low', 'none'), unique(priority))) {
      sel <- priority == level
      plot <- plotly::add_trace(
        plot, x = points$month_date[sel], y = points$.value[sel], type = 'scatter', mode = 'markers',
        key = points$.row[sel], text = tip[sel], hoverinfo = 'text',
        marker = list(color = .qc_priority_palette[[level]], size = 11, symbol = 'diamond',
                      line = list(color = 'white', width = 1)),
        name = paste('Flagged:', level)
      )
    }
  }
  yaxis <- list(title = spec$short, rangemode = 'tozero',
                tickformat = if (spec$percent) '.0%' else ',d')
  plotly::layout(
    plot, title = list(text = .if_null(title, spec$label), x = 0, font = list(size = 14)),
    xaxis = list(title = ''), yaxis = yaxis, hovermode = 'closest',
    legend = list(orientation = 'h', y = -0.2), margin = list(t = 40)
  ) %>% plotly::event_register('plotly_click')
}

.plot_explore_strip <- function(series, states = NULL) {
  if (!is.null(states)) {
    plot <- plotly::plot_ly()
    for (s in names(.explore_state_palette)) {
      sel <- states$state == s
      if (!any(sel)) next
      plot <- plotly::add_bars(
        plot, x = states$month_date[sel], y = 1, name = s,
        marker = list(color = .explore_state_palette[[s]]),
        text = paste0(format(states$month_date[sel], '%b %Y'), ': ', s), hoverinfo = 'text'
      )
    }
    return(plotly::layout(plot, barmode = 'stack', yaxis = list(visible = FALSE),
                          xaxis = list(title = ''), legend = list(orientation = 'h', y = -0.5),
                          margin = list(t = 5, b = 30)) %>% plotly::style(textposition = 'none'))
  }
  silent <- pmax(series$facilities_expected - series$facilities_reporting, 0L)
  hover <- paste0(format(series$month_date, '%b %Y'), '<br>Reporting: ', series$facilities_reporting,
                  '<br>Expected but without usable tested count: ', silent,
                  '<br>Review or excluded rows: ', series$flagged_rows)
  plotly::plot_ly() %>%
    plotly::add_bars(x = series$month_date, y = series$facilities_reporting, name = 'Reporting',
                     marker = list(color = '#4c78a8'), text = hover, hoverinfo = 'text') %>%
    plotly::add_bars(x = series$month_date, y = silent, name = 'Expected, no usable count',
                     marker = list(color = '#d9d9d9'), text = hover, hoverinfo = 'text') %>%
    plotly::add_trace(x = series$month_date, y = series$flagged_rows, type = 'scatter', mode = 'lines',
                      name = 'Review or excluded rows', yaxis = 'y2', line = list(color = '#b2182b', width = 1.5),
                      text = hover, hoverinfo = 'text') %>%
    plotly::layout(
      barmode = 'stack', xaxis = list(title = ''), yaxis = list(title = 'Facilities'),
      yaxis2 = list(overlaying = 'y', side = 'right', title = 'Flagged rows', rangemode = 'tozero',
                    showgrid = FALSE),
      legend = list(orientation = 'h', y = -0.4), margin = list(t = 5, b = 30)
    ) %>%
    plotly::style(textposition = 'none')
}

.plot_explore_panels <- function(series, points_by_metric = NULL) {
  plots <- lapply(names(.explore_metrics), function(m) {
    .plot_explore_series(series, m, points_by_metric[[m]], source = 'explore_panels')
  })
  plotly::subplot(plots, nrows = 2, shareX = FALSE, titleY = TRUE, margin = 0.06)
}

.record_details_table <- function(row) {
  fields <- c(facility_id = 'Facility', region = 'Region', council = 'Council', district = 'District',
              month_date = 'Month', tested = 'Tested', positive = 'Positive', attending = 'Attendance',
              prevalence = 'Observed positivity', qc_action = 'Action', review_priority = 'Priority',
              qc_reason = 'Reasons', prediction_status = 'Model assessment')
  present <- intersect(names(fields), names(row))
  dplyr::tibble(
    field = unname(fields[present]),
    value = vapply(present, function(f) {
      x <- row[[f]][[1]]
      if (f == 'prevalence' && is.numeric(x)) .format_metric(x, 'positivity')
      else if (inherits(x, 'Date')) format(x, '%Y-%m')
      else if (is.na(x)) 'NA' else as.character(x)
    }, character(1))
  )
}
