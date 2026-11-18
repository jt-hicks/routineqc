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
  expectation <- .explore_expectation(sub, frame)
  out$expected_positivity <- expectation$positivity[match(out$month_date, expectation$month_date)]
  out$expected_tested <- expectation$tested[match(out$month_date, expectation$month_date)]
  out
}

# What the QC model expected each month: positives expected from the model over
# the tested counts it assessed, and the sum of the rolling tested baselines
# when every usable record of the month has one.
.explore_expectation <- function(sub, frame) {
  p_hat <- if ('p_hat' %in% names(sub)) as.numeric(sub$p_hat) else rep(NA_real_, nrow(sub))
  base <- if ('tested_roll_median' %in% names(sub)) as.numeric(sub$tested_roll_median) else rep(NA_real_, nrow(sub))
  ok_p <- !is.na(frame$tested) & frame$tested > 0 & !is.na(p_hat)
  usable <- !is.na(frame$tested)
  dplyr::tibble(month_date = frame$month_date, t = ifelse(ok_p, frame$tested, 0), e = ifelse(ok_p, frame$tested * p_hat, 0),
                b = base, usable = usable) %>%
    dplyr::group_by(month_date) %>%
    dplyr::summarise(
      positivity = if (sum(t) > 0) sum(e) / sum(t) else NA_real_,
      tested = if (any(usable) && all(!is.na(b[usable]))) sum(b[usable]) else NA_real_,
      .groups = 'drop'
    )
}

# One series per age group, named by code with the labels in attribute
# `labels`. NULL for data without age groups.
.explore_series_by_group <- function(data, selection) {
  groups <- .age_group_choices(data)
  if (length(groups) == 0L) return(NULL)
  out <- lapply(unname(groups), function(g) {
    s <- .explore_series(data[as.character(data$age_group_code) == g, , drop = FALSE], selection)
    s
  })
  names(out) <- unname(groups)
  # Facilities expected and reporting count a facility once, across groups.
  whole <- .explore_series(data, selection)
  for (g in names(out)) {
    out[[g]]$facilities_expected <- whole$facilities_expected
  }
  attr(out, 'labels') <- stats::setNames(names(groups), unname(groups))
  out
}

# Per-month state of one facility, separating the ways a value can be absent.
# With age bands a month has several rows: it is invalid if any band is, and
# missing only if no band has a usable tested count.
.explore_facility_states <- function(data, facility, by_group = FALSE) {
  if (isTRUE(by_group) && length(.age_group_choices(data)) > 0L) {
    groups <- .age_group_choices(data)
    months <- .explore_months(data)
    parts <- lapply(seq_along(groups), function(i) {
      g <- unname(groups)[i]
      rows <- data[as.character(data$age_group_code) == g, , drop = FALSE]
      st <- .explore_facility_states(rows, facility)
      st <- st[match(months, st$month_date), , drop = FALSE]
      st$month_date <- months
      st$state[is.na(st$state)] <- 'absent'
      st$group <- names(groups)[i]
      st
    })
    out <- dplyr::bind_rows(parts)
    out$group <- factor(out$group, levels = rev(names(groups)))
    return(out)
  }
  months <- .explore_months(data)
  sub <- data[data$facility_id == facility, , drop = FALSE]
  state <- rep('absent', length(months))
  if (nrow(sub) > 0L) {
    invalid <- .row_count_invalid(sub)
    usable <- .usable_counts(sub)$tested
    m <- match(sub$month_date, months)
    for (i in unique(m[!is.na(m)])) {
      rows <- which(m == i)
      state[i] <- if (any(invalid[rows])) 'invalid count'
        else if (all(is.na(usable[rows]))) 'missing count'
        else if (sum(usable[rows], na.rm = TRUE) == 0) 'zero tested'
        else 'reported'
    }
  }
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
  out$records <- as.integer(table(factor(unit_of_row, levels = out$unit)))
  out$review_rows <- as.integer(tapply(flagged, factor(unit_of_row, levels = out$unit), sum))
  out$high_priority_rows <- as.integer(tapply(high, factor(unit_of_row, levels = out$unit), sum))
  out$review_rows[is.na(out$review_rows)] <- 0L
  out$high_priority_rows[is.na(out$high_priority_rows)] <- 0L
  # Records, not facility-months: with age groups a month has one record per group.
  out$share_flagged <- .safe_divide(out$review_rows, out$records)
  attr(out, 'child_level') <- child
  out[order(-out$high_priority_rows, -out$share_flagged, out$unit), ]
}

# Records to show for a selection, with the facility's own series values so a
# flagged point can be drawn on the chart.
.explore_flagged_points <- function(data, selection, metric, by_group = FALSE) {
  if (!isTRUE(by_group) && length(.age_group_choices(data)) > 0L) {
    return(.explore_flagged_months(data, selection, metric))
  }
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

# With age groups shown together, a flagged record is drawn at its month's
# total (all groups), so it sits on the line; the click opens the month's
# highest-priority flagged record.
.explore_flagged_months <- function(data, selection, metric) {
  sub <- .nav_subset(data, selection)
  flagged <- .is_flagged_row(sub)
  if (!any(flagged)) return(sub[0, , drop = FALSE])
  counts <- .usable_counts(sub)
  attending <- .baseline_counts(sub)$attending
  key <- paste(sub$facility_id, sub$month_date)
  totals <- dplyr::tibble(key = key, t = counts$tested, p = counts$positive, a = attending) %>%
    dplyr::group_by(key) %>%
    dplyr::summarise(
      tested = if (all(is.na(t))) NA_real_ else sum(t, na.rm = TRUE),
      pair_t = sum(ifelse(!is.na(t) & !is.na(p) & p <= t, t, 0)),
      pair_p = sum(ifelse(!is.na(t) & !is.na(p) & p <= t, p, 0)),
      attending = if (all(is.na(a))) NA_real_ else sum(a, na.rm = TRUE),
      .groups = 'drop'
    )
  rank <- match(as.character(sub$review_priority), c('critical', 'high', 'medium', 'low'), nomatch = 5L)
  top <- sub[flagged, , drop = FALSE]
  top <- top[order(key[flagged], rank[flagged]), , drop = FALSE]
  top <- top[!duplicated(paste(top$facility_id, top$month_date)), , drop = FALSE]
  m <- match(paste(top$facility_id, top$month_date), totals$key)
  tt <- totals[m, ]
  top$.value <- switch(
    metric,
    positivity = ifelse(tt$pair_t > 0, tt$pair_p / tt$pair_t, NA_real_),
    tested = tt$tested,
    attendance = tt$attending,
    coverage = ifelse(!is.na(tt$tested) & !is.na(tt$attending) & tt$attending > 0, tt$tested / tt$attending, NA_real_)
  )
  top[!is.na(top$.value), , drop = FALSE]
}

.format_metric <- function(x, metric) {
  if (.explore_metrics[[metric]]$percent) {
    ifelse(is.na(x), 'no valid data', sprintf('%.1f%%', 100 * x))
  } else {
    ifelse(is.na(x), 'no valid data', format(round(x), big.mark = ',', trim = TRUE))
  }
}

# `series` is one monthly series, or a named list of them (one per age group,
# with their labels in attribute `labels`). With `expected = TRUE` the model
# expectation is drawn dashed, for positivity and tested.
.plot_explore_series <- function(series, metric, points = NULL, title = NULL, source = 'explore',
                                 expected = FALSE) {
  spec <- .explore_metrics[[metric]]
  grouped <- !is.data.frame(series)
  lines <- if (grouped) series else list(all = series)
  labels <- if (grouped) attr(series, 'labels') else c(all = spec$short)
  colors <- if (grouped) .age_group_colors(names(lines)) else c(all = '#4c78a8')
  expected_col <- switch(metric, positivity = 'expected_positivity', tested = 'expected_tested', NULL)
  plot <- plotly::plot_ly(source = source)
  for (g in names(lines)) {
    s <- lines[[g]]
    y <- s[[metric]]
    group_text <- if (grouped) paste0('Age group ', labels[[g]], '<br>') else ''
    text <- paste0(
      group_text, format(s$month_date, '%b %Y'), '<br>', spec$short, ': ', .format_metric(y, metric),
      if (!is.null(expected_col) && expected_col %in% names(s)) {
        paste0('<br>Model expectation: ', .format_metric(s[[expected_col]], metric))
      } else '',
      '<br>Facilities reporting: ', s$facilities_reporting, ' of ', s$facilities_expected,
      '<br>Review or excluded records: ', s$flagged_rows
    )
    plot <- plotly::add_trace(
      plot, x = s$month_date, y = y, type = 'scatter', mode = 'lines+markers',
      connectgaps = FALSE, line = list(color = colors[[g]], width = 2),
      marker = list(color = colors[[g]], size = 5), text = text, hoverinfo = 'text',
      name = if (grouped) paste('Age group', labels[[g]]) else spec$short, showlegend = grouped,
      legendgroup = g
    )
    if (isTRUE(expected) && !is.null(expected_col) && expected_col %in% names(s) && any(!is.na(s[[expected_col]]))) {
      plot <- plotly::add_trace(
        plot, x = s$month_date, y = s[[expected_col]], type = 'scatter', mode = 'lines',
        connectgaps = FALSE, line = list(color = colors[[g]], width = 1.5, dash = 'dash'), opacity = 0.75,
        hoverinfo = 'skip', name = if (grouped) paste('Expected,', labels[[g]]) else 'Model expectation',
        showlegend = TRUE, legendgroup = g
      )
    }
  }
  if (!is.null(points) && nrow(points) > 0L) {
    tip <- .qc_row_tooltip(points)
    if (grouped && 'age_group' %in% names(points)) tip <- paste0('Age group ', points$age_group, '<br>', tip)
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
  default_title <- if (grouped) paste0(spec$label, ', by age group') else spec$label
  plotly::layout(
    plot, title = list(text = .if_null(title, default_title), x = 0, font = list(size = 14)),
    xaxis = list(title = ''), yaxis = yaxis, hovermode = 'closest',
    legend = list(orientation = 'h', y = -0.2), margin = list(t = 40)
  ) %>% plotly::event_register('plotly_click')
}

# Reporting over time. For one facility, a cell per month (and per age group
# when there are several); otherwise facilities reporting against those
# expected, on one axis.
.plot_explore_strip <- function(series, states = NULL) {
  if (!is.null(states)) {
    y <- if ('group' %in% names(states)) as.character(states$group) else rep('Reporting', nrow(states))
    levels <- if ('group' %in% names(states)) levels(states$group) else 'Reporting'
    plot <- plotly::plot_ly()
    for (s in names(.explore_state_palette)) {
      sel <- states$state == s
      if (!any(sel)) next
      plot <- plotly::add_trace(
        plot, x = states$month_date[sel], y = y[sel], type = 'scatter', mode = 'markers', name = s,
        marker = list(color = .explore_state_palette[[s]], symbol = 'square', size = 13),
        text = paste0(format(states$month_date[sel], '%b %Y'),
                      if ('group' %in% names(states)) paste0(', age group ', y[sel]) else '', ': ', s),
        hoverinfo = 'text'
      )
    }
    return(plotly::layout(plot, yaxis = list(title = '', type = 'category', categoryorder = 'array',
                                             categoryarray = levels, fixedrange = TRUE),
                          xaxis = list(title = ''), legend = list(orientation = 'h', y = -0.45),
                          margin = list(t = 5, b = 30)))
  }
  if (!is.data.frame(series)) series <- series[[1]]
  silent <- pmax(series$facilities_expected - series$facilities_reporting, 0L)
  hover <- paste0(format(series$month_date, '%b %Y'), '<br>Reporting: ', series$facilities_reporting,
                  '<br>Expected but without usable tested count: ', silent)
  plotly::plot_ly() %>%
    plotly::add_bars(x = series$month_date, y = series$facilities_reporting, name = 'Reporting',
                     marker = list(color = '#4c78a8'), text = hover, hoverinfo = 'text') %>%
    plotly::add_bars(x = series$month_date, y = silent, name = 'Expected, no usable count',
                     marker = list(color = '#d9d9d9'), text = hover, hoverinfo = 'text') %>%
    plotly::layout(
      barmode = 'stack', xaxis = list(title = ''), yaxis = list(title = 'Facilities', rangemode = 'tozero'),
      legend = list(orientation = 'h', y = -0.4), margin = list(t = 5, b = 30)
    ) %>%
    plotly::style(textposition = 'none')
}

.plot_explore_panels <- function(series, points_by_metric = NULL, expected = FALSE) {
  plots <- lapply(names(.explore_metrics), function(m) {
    .plot_explore_series(series, m, points_by_metric[[m]], source = 'explore_panels', expected = expected)
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

# Every age group of the selected record's facility-month, side by side, so a
# label such as "other age group as expected" can be checked at a glance.
# Without age groups this is the record alone.
.record_month_table <- function(data, row) {
  rows <- data[as.character(data$facility_id) == as.character(row$facility_id[[1]]) &
                 data$month_date == row$month_date[[1]], , drop = FALSE]
  if (.qc_is_age_disaggregated(rows) && nrow(rows) > 1L) {
    code <- as.character(rows$age_group_code)
    rows <- rows[order(!startsWith(code, 'lt'), code), , drop = FALSE]
    heads <- if ('age_group' %in% names(rows)) as.character(rows$age_group) else as.character(rows$age_group_code)
  } else {
    rows <- row
    heads <- 'Value'
  }
  num <- function(x, digits = 0) ifelse(is.na(x), '', formatC(x, format = 'f', digits = digits, big.mark = ','))
  pct <- function(x) ifelse(is.na(x), '', sprintf('%.1f%%', 100 * x))
  col <- function(name) if (name %in% names(rows)) rows[[name]] else rep(NA, nrow(rows))
  counts <- .usable_counts(rows)
  positivity <- ifelse(!is.na(counts$tested) & counts$tested > 0 & !is.na(counts$positive) &
                         counts$positive <= counts$tested, counts$positive / counts$tested, NA_real_)
  resid <- if ('pearson_resid_scaled' %in% names(rows)) rows$pearson_resid_scaled else col('pearson_resid')
  fields <- list(
    Tested = num(as.numeric(col('tested'))),
    Positive = num(as.numeric(col('positive'))),
    Positivity = pct(positivity),
    `Model expectation` = pct(as.numeric(col('p_hat'))),
    `Scaled residual` = ifelse(is.na(resid), '', sprintf('%+.1f', as.numeric(resid))),
    Attendance = num(as.numeric(col('attending'))),
    `Booked before 12 weeks` = num(as.numeric(col('anc_before_12wk'))),
    `Booked after 12 weeks` = num(as.numeric(col('anc_after_12wk'))),
    Priority = ifelse(is.na(col('review_priority')) | col('review_priority') %in% 'none', 'not flagged',
                      as.character(col('review_priority'))),
    Action = ifelse(is.na(col('qc_action')), '', .qc_action_label(as.character(col('qc_action'))))
  )
  keep <- vapply(fields, function(v) any(nzchar(v)), logical(1))
  fields <- fields[keep]
  out <- dplyr::tibble(field = names(fields))
  for (i in seq_along(heads)) out[[heads[i]]] <- vapply(fields, `[`, character(1), i)
  attr(out, 'selected') <- if (length(heads) > 1L) heads[match(row$.row[[1]], rows$.row)] else NULL
  out
}

# Flagged records in a selection, highest priority first, with readable
# columns. `more = TRUE` adds the model and tested-volume measures.
.explore_queue_table <- function(queue, more = FALSE) {
  counts <- .usable_counts(queue)
  positivity <- ifelse(!is.na(counts$tested) & counts$tested > 0 & !is.na(counts$positive) &
                         counts$positive <= counts$tested, counts$positive / counts$tested, NA_real_)
  col <- function(name) if (name %in% names(queue)) queue[[name]] else rep(NA, nrow(queue))
  out <- dplyr::tibble(
    Facility = as.character(queue$facility_id),
    Month = format(queue$month_date, '%Y-%m')
  )
  if (.qc_is_age_disaggregated(queue)) {
    out$`Age group` <- if ('age_group' %in% names(queue)) as.character(queue$age_group) else as.character(queue$age_group_code)
  }
  out$Priority <- as.character(col('review_priority'))
  out$Action <- .qc_action_label(as.character(col('qc_action')))
  out$Reasons <- as.character(col('qc_reason'))
  out$Tested <- as.numeric(col('tested'))
  out$Positive <- as.numeric(col('positive'))
  out$Positivity <- positivity
  out$Expected <- as.numeric(col('p_hat'))
  if (isTRUE(more)) {
    out$Attendance <- as.numeric(col('attending'))
    out$`Scaled residual` <- as.numeric(if ('pearson_resid_scaled' %in% names(queue)) queue$pearson_resid_scaled else col('pearson_resid'))
    out$`Tested baseline` <- as.numeric(col('tested_roll_median'))
    out$`Tested robust z` <- as.numeric(col('tested_robust_z'))
    out$`Model assessment` <- as.character(col('prediction_status'))
  }
  out
}
