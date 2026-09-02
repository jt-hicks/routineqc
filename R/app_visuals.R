.qc_flag_vector <- function(data, mode = c('flagged', 'review', 'exclude')) {
  mode <- match.arg(mode)
  required <- switch(
    mode,
    flagged = c('flag_review_recommended', 'flag_exclude_authorized'),
    review = 'flag_review_recommended',
    exclude = 'flag_exclude_authorized'
  )
  .validate_required_columns(data, required)
  review <- if ('flag_review_recommended' %in% names(data)) {
    dplyr::coalesce(as.logical(data$flag_review_recommended), FALSE)
  } else {
    rep(FALSE, nrow(data))
  }
  exclude <- if ('flag_exclude_authorized' %in% names(data)) {
    dplyr::coalesce(as.logical(data$flag_exclude_authorized), FALSE)
  } else {
    rep(FALSE, nrow(data))
  }
  switch(mode, flagged = review | exclude, review = review, exclude = exclude)
}

.value_or <- function(x, default) {
  if (is.null(x) || length(x) == 0L || any(is.na(x))) default else x
}

.qc_action_label <- function(x) {
  labels <- c(
    retain = 'Retain',
    exclude_core_invalid = 'Authorized exclusion',
    review_core_invalid = 'Review: invalid tested/positive counts',
    review_multiple_signals = 'Review: multiple evidence domains',
    review_attendance = 'Review: attendance',
    review_prevalence = 'Review: prevalence',
    review_tested_volume = 'Review: tested volume',
    review_temporal_context = 'Review: temporal context'
  )
  out <- unname(labels[x])
  missing <- is.na(out)
  out[missing] <- gsub('_', ' ', x[missing], fixed = TRUE)
  out
}

.qc_action_color <- function(x) {
  colors <- c(
    retain = '#4daf4a',
    exclude_core_invalid = '#d73027',
    review_core_invalid = '#7b3294',
    review_multiple_signals = '#542788',
    review_attendance = '#8073ac',
    review_prevalence = '#2c7fb8',
    review_tested_volume = '#fdae61',
    review_temporal_context = '#bdbdbd'
  )
  out <- unname(colors[x])
  out[is.na(out)] <- '#636363'
  out
}

.qc_action_summary <- function(data) {
  .validate_required_columns(data, 'qc_action')
  total <- nrow(data)
  dplyr::as_tibble(data) %>%
    dplyr::mutate(qc_action = as.character(qc_action)) %>%
    dplyr::count(qc_action, name = 'n') %>%
    dplyr::mutate(
      label = .qc_action_label(qc_action),
      color = .qc_action_color(qc_action),
      proportion = if (total > 0L) n / total else NA_real_
    )
}

.qc_reason_summary <- function(data) {
  .validate_required_columns(data, c('qc_action', 'qc_reason'))
  total <- nrow(data)
  reasons <- strsplit(
    dplyr::coalesce(as.character(data$qc_reason), 'No QC issue detected'),
    ';\\s*'
  )
  reason_rows <- dplyr::tibble(
    source_row = rep(seq_along(reasons), lengths(reasons)),
    qc_reason = trimws(unlist(reasons, use.names = FALSE))
  ) %>%
    dplyr::filter(
      qc_reason != '',
      qc_reason != 'No QC issue detected'
    )
  if (nrow(reason_rows) == 0L) {
    return(dplyr::tibble(qc_reason = character(), n = integer(), proportion = double()))
  }
  reason_rows %>%
    dplyr::distinct(source_row, qc_reason) %>%
    dplyr::count(qc_reason, name = 'n') %>%
    dplyr::mutate(
      proportion = if (total > 0L) n / total else NA_real_
    ) %>%
    dplyr::arrange(n)
}

.facility_burden_data <- function(data, mode = c('flagged', 'review', 'exclude')) {
  .validate_required_columns(data, 'facility_id')
  affected <- .qc_flag_vector(data, match.arg(mode))
  facility <- dplyr::as_tibble(data) %>%
    dplyr::mutate(.affected = affected) %>%
    dplyr::group_by(facility_id) %>%
    dplyr::summarise(flagged_rows = sum(.affected), .groups = 'drop') %>%
    dplyr::mutate(
      burden = cut(
        flagged_rows, breaks = c(-Inf, 0, 1, 5, 10, Inf),
        labels = c('0', '1', '2-5', '6-10', '>10')
      )
    )
  levels <- c('0', '1', '2-5', '6-10', '>10')
  counts <- as.data.frame(table(factor(facility$burden, levels = levels)))
  names(counts) <- c('burden', 'facilities')
  dplyr::as_tibble(counts) %>%
    dplyr::mutate(
      burden = factor(as.character(burden), levels = levels),
      proportion = if (nrow(facility) > 0L) facilities / nrow(facility) else NA_real_
    )
}

.district_burden_data <- function(data, mode = c('flagged', 'review', 'exclude'), threshold = 3L) {
  .validate_required_columns(data, c('facility_id', 'district'))
  if (!is.numeric(threshold) || length(threshold) != 1L || is.na(threshold) ||
      threshold < 0 || threshold != as.integer(threshold)) {
    rlang::abort('`threshold` must be one non-negative whole number.')
  }
  affected <- .qc_flag_vector(data, match.arg(mode))
  dat <- dplyr::as_tibble(data) %>%
    dplyr::mutate(.affected = affected) %>%
    dplyr::filter(!is.na(district), trimws(as.character(district)) != '')
  if (nrow(dat) == 0L) {
    return(dplyr::tibble(
      district = character(), facilities = integer(),
      facilities_over_threshold = integer(), proportion_over_threshold = double()
    ))
  }
  dat %>%
    dplyr::group_by(district, facility_id) %>%
    dplyr::summarise(flagged_rows = sum(.affected), .groups = 'drop') %>%
    dplyr::group_by(district) %>%
    dplyr::summarise(
      facilities = dplyr::n(),
      facilities_over_threshold = sum(flagged_rows > threshold),
      proportion_over_threshold = facilities_over_threshold / facilities,
      .groups = 'drop'
    )
}

.format_tooltip_value <- function(x, field) {
  if (inherits(x, 'Date')) return(format(x, '%Y-%m-%d'))
  if (field %in% c('prevalence', 'p_hat') && is.numeric(x)) {
    return(ifelse(is.na(x), 'NA', sprintf('%.2f%%', 100 * x)))
  }
  if (is.numeric(x)) return(ifelse(is.na(x), 'NA', format(round(x, 3), trim = TRUE)))
  ifelse(is.na(x), 'NA', as.character(x))
}

.qc_row_tooltip <- function(data) {
  fields <- c(
    facility_name = 'Facility', facility_id = 'Facility ID',
    district = 'District', region = 'Region', month_date = 'Month',
    tested = 'Tested', positive = 'Positive', prevalence = 'Observed prevalence',
    p_hat = 'Expected prevalence', pearson_resid = 'Pearson residual',
    residual_rule_threshold = 'Residual rule threshold',
    tested_roll_median = 'Tested rolling median', tested_robust_z = 'Tested robust z',
    tested_z_rule_threshold = 'Tested robust-z threshold',
    qc_action = 'QC action', review_priority = 'Review priority',
    qc_reason = 'QC reason', prediction_status = 'Prediction status'
  )
  present <- intersect(names(fields), names(data))
  if (length(present) == 0L) return(rep('', nrow(data)))
  pieces <- lapply(present, function(field) {
    paste0(
      unname(fields[field]), ': ',
      .format_tooltip_value(data[[field]], field)
    )
  })
  do.call(paste, c(pieces, sep = '<br>'))
}

.plot_qc_actions <- function(data) {
  summary <- .qc_action_summary(data)
  plotly::plot_ly(
    summary, labels = ~label, values = ~n, type = 'pie', hole = 0.55,
    marker = list(colors = summary$color),
    text = ~paste0(
      label, '<br>Rows: ', n,
      '<br>All rows: ', sprintf('%.1f%%', 100 * proportion)
    ),
    textinfo = 'percent', hovertemplate = '%{text}<extra></extra>',
    sort = FALSE
  ) %>%
    plotly::layout(
      title = list(text = 'QC action across all rows'),
      showlegend = TRUE,
      margin = list(l = 10, r = 10, b = 10, t = 55)
    )
}

.plot_qc_reasons <- function(data) {
  summary <- .qc_reason_summary(data)
  if (nrow(summary) == 0L) return(NULL)
  plotly::plot_ly(
    summary, x = ~n, y = ~qc_reason, type = 'bar', orientation = 'h',
    marker = list(color = '#3182bd'),
    text = ~paste0(
      'QC reason: ', qc_reason, '<br>Rows: ', n,
      '<br>All rows: ', sprintf('%.1f%%', 100 * proportion)
    ),
    hovertemplate = '%{text}<extra></extra>'
  ) %>%
    plotly::layout(
      title = list(text = 'Individual QC reasons'),
      xaxis = list(title = 'Number of rows'),
      yaxis = list(title = NULL, automargin = TRUE),
      margin = list(l = 210, t = 55)
    )
}

.plot_facility_burden <- function(data, mode) {
  summary <- .facility_burden_data(data, mode)
  plotly::plot_ly(
    summary, x = ~burden, y = ~facilities, type = 'bar',
    marker = list(color = '#2c7fb8'),
    text = ~paste0(
      'Affected rows: ', burden, '<br>Facilities: ', facilities,
      '<br>All facilities: ', sprintf('%.1f%%', 100 * proportion)
    ),
    hovertemplate = '%{text}<extra></extra>'
  ) %>%
    plotly::layout(
      title = list(text = 'Facility QC burden'),
      xaxis = list(title = 'Affected rows per facility'),
      yaxis = list(title = 'Number of facilities'),
      margin = list(t = 55)
    )
}

.plot_district_burden <- function(data, mode, threshold, metric = c('proportion', 'count')) {
  metric <- match.arg(metric)
  summary <- .district_burden_data(data, mode, threshold)
  if (nrow(summary) == 0L) return(NULL)
  value <- if (metric == 'proportion') summary$proportion_over_threshold else summary$facilities_over_threshold
  summary <- summary[order(value), , drop = FALSE]
  plotly::plot_ly(
    summary, x = value, y = ~district, type = 'bar', orientation = 'h',
    marker = list(color = '#756bb1'),
    text = ~paste0(
      'District: ', district,
      '<br>Facilities above threshold: ', facilities_over_threshold,
      '<br>Total facilities: ', facilities,
      '<br>Proportion: ', sprintf('%.1f%%', 100 * proportion_over_threshold)
    ),
    hovertemplate = '%{text}<extra></extra>'
  ) %>%
    plotly::layout(
      title = list(text = paste0('Districts with facilities having >', threshold, ' affected rows')),
      xaxis = list(title = if (metric == 'proportion') 'Proportion of facilities' else 'Number of facilities',
                   tickformat = if (metric == 'proportion') '.0%' else NULL),
      yaxis = list(title = NULL, automargin = TRUE),
      margin = list(t = 55, l = 130)
    )
}

.qc_flow_diagram_ui <- function() {
  box <- function(title, items, class = 'qc-flow-box') {
    shiny::div(
      class = class,
      shiny::tags$strong(title),
      shiny::tags$ul(lapply(items, shiny::tags$li))
    )
  }
  shiny::div(
    class = 'qc-flow',
    shiny::tags$h4('How QC actions are assigned'),
    shiny::p(
      'Each row is evaluated for factual evidence first. The selected action policy ',
      'then maps that evidence to retention, review, or authorized exclusion.'
    ),
    shiny::div(
      class = 'qc-flow-grid',
      box(
        'Tested/positive validity checks',
        c(
          'Missing tested or positive count',
          'Negative tested or positive count',
          'Positive count greater than tested',
          'Positive count with zero tested'
        )
      ),
      box(
        'Attendance evidence',
        c(
          'Tested exceeds an approved attendance denominator',
          'Attendance is negative'
        )
      ),
      box(
        'Prevalence and statistical evidence',
        c(
          'Model residual is extreme',
          'All-negative or all-positive with a large tested count',
          'Large change between adjacent months',
          'An extreme residual is isolated in time'
        )
      ),
      box(
        'Tested-volume evidence',
        c(
          'Extreme high or low relative to the rolling baseline',
          'Abrupt jump or drop',
          'Unexpected zero after adequate prior volume'
        )
      ),
      box(
        'Calendar context',
        c('An extreme residual lacks both adjacent calendar months')
      )
    ),
    shiny::div(
      class = 'qc-flow-outcomes',
      shiny::tags$p(
        shiny::tags$strong('Conservative policy: '),
        'invalid tested/positive counts may authorize exclusion; other evidence recommends review.'
      ),
      shiny::tags$p(
        shiny::tags$strong('Multiple evidence domains: '),
        'attendance, prevalence, and tested-volume evidence together increase review priority.'
      ),
      shiny::tags$p(
        shiny::tags$strong('Important: '),
        'an anomaly is evidence for investigation, not proof that a record is wrong.'
      )
    )
  )
}

.selected_facility_data <- function(data, facility_id) {
  .validate_required_columns(data, c('facility_id', 'month_date'))
  dat <- dplyr::as_tibble(data) %>%
    dplyr::filter(.data$facility_id == .env$facility_id) %>%
    dplyr::arrange(month_date)
  dat$.tooltip <- .qc_row_tooltip(dat)
  dat
}

.plot_facility_prevalence_interactive <- function(data, facility_id) {
  dat <- .selected_facility_data(data, facility_id)
  .validate_required_columns(dat, 'prevalence')
  if (!'p_hat' %in% names(dat)) dat$p_hat <- NA_real_
  flagged <- if ('flag_any_qc_issue' %in% names(dat)) {
    dplyr::coalesce(dat$flag_any_qc_issue, FALSE)
  } else {
    rep(FALSE, nrow(dat))
  }
  plotly::plot_ly(dat, x = ~month_date) %>%
    plotly::add_lines(
      y = ~prevalence, name = 'Observed', line = list(color = '#1f78b4'),
      text = ~.tooltip, hovertemplate = '%{text}<extra>Observed</extra>'
    ) %>%
    plotly::add_markers(
      y = ~prevalence, name = 'Observed', marker = list(color = '#1f78b4', size = 7),
      text = ~.tooltip, hovertemplate = '%{text}<extra>Observed</extra>',
      showlegend = FALSE
    ) %>%
    plotly::add_lines(
      y = ~p_hat, name = 'Model expectation',
      line = list(color = '#ff7f00', dash = 'dash'),
      text = ~.tooltip, hovertemplate = '%{text}<extra>Model expectation</extra>'
    ) %>%
    plotly::add_markers(
      data = dat[flagged, , drop = FALSE], y = ~prevalence, name = 'QC-affected row',
      marker = list(color = '#e31a1c', size = 10, symbol = 'circle-open', line = list(width = 2)),
      text = ~.tooltip, hovertemplate = '%{text}<extra>QC-affected row</extra>'
    ) %>%
    plotly::layout(
      title = list(text = paste('Facility', facility_id, 'prevalence')),
      xaxis = list(title = 'Month'), yaxis = list(title = 'Prevalence', tickformat = '.1%'),
      hovermode = 'closest', margin = list(t = 55)
    )
}

.plot_facility_tested_interactive <- function(data, facility_id) {
  dat <- .selected_facility_data(data, facility_id)
  .validate_required_columns(dat, 'tested')
  if (!'tested_roll_median' %in% names(dat)) dat$tested_roll_median <- NA_real_
  flagged <- if ('flag_any_qc_issue' %in% names(dat)) {
    dplyr::coalesce(dat$flag_any_qc_issue, FALSE)
  } else {
    rep(FALSE, nrow(dat))
  }
  plotly::plot_ly(dat, x = ~month_date) %>%
    plotly::add_lines(
      y = ~tested, name = 'Number tested', line = list(color = '#1b9e77'),
      text = ~.tooltip, hovertemplate = '%{text}<extra>Number tested</extra>'
    ) %>%
    plotly::add_markers(
      y = ~tested, marker = list(color = '#1b9e77', size = 7),
      text = ~.tooltip, hovertemplate = '%{text}<extra>Number tested</extra>',
      name = 'Number tested', showlegend = FALSE
    ) %>%
    plotly::add_lines(
      y = ~tested_roll_median, name = 'Rolling baseline',
      line = list(color = '#7570b3', dash = 'dash'),
      text = ~.tooltip, hovertemplate = '%{text}<extra>Rolling baseline</extra>'
    ) %>%
    plotly::add_markers(
      data = dat[flagged, , drop = FALSE], y = ~tested, name = 'QC-affected row',
      marker = list(color = '#e31a1c', size = 10, symbol = 'circle-open', line = list(width = 2)),
      text = ~.tooltip, hovertemplate = '%{text}<extra>QC-affected row</extra>'
    ) %>%
    plotly::layout(
      title = list(text = paste('Facility', facility_id, 'testing volume')),
      xaxis = list(title = 'Month'), yaxis = list(title = 'Number tested'),
      hovermode = 'closest', margin = list(t = 55)
    )
}

.plot_diagnostic_points <- function(data, value, title, y_title, thresholds = numeric()) {
  .validate_required_columns(data, c('month_date', value))
  dat <- dplyr::as_tibble(data)
  dat$.tooltip <- .qc_row_tooltip(dat)
  dat$.value <- dat[[value]]
  dat$.action <- .qc_action_label(as.character(dat$qc_action))
  plot <- plotly::plot_ly(
    dat, x = ~month_date, y = ~.value, color = ~.action,
    type = 'scatter', mode = 'markers',
    marker = list(size = 7, opacity = 0.65),
    text = ~.tooltip, hovertemplate = '%{text}<extra></extra>'
  )
  if (length(thresholds) > 0L) {
    for (threshold in thresholds) {
      plot <- plotly::add_lines(
        plot, x = range(dat$month_date, na.rm = TRUE), y = rep(threshold, 2),
        inherit = FALSE, showlegend = FALSE, hoverinfo = 'skip',
        line = list(color = '#d73027', dash = 'dash')
      )
    }
  }
  plotly::layout(
    plot, title = list(text = title),
    xaxis = list(title = 'Month'), yaxis = list(title = y_title),
    hovermode = 'closest', margin = list(t = 55)
  )
}

.plot_before_after_interactive <- function(data) {
  .validate_required_columns(data, c('tested', 'positive', 'flag_exclude_authorized'))
  before <- .safe_divide(sum(data$positive, na.rm = TRUE), sum(data$tested, na.rm = TRUE))
  keep <- !dplyr::coalesce(data$flag_exclude_authorized, FALSE)
  after <- .safe_divide(sum(data$positive[keep], na.rm = TRUE), sum(data$tested[keep], na.rm = TRUE))
  dat <- dplyr::tibble(
    state = c('Source data', 'After authorized exclusions'),
    prevalence = c(before, after)
  )
  plotly::plot_ly(
    dat, x = ~state, y = ~prevalence, type = 'bar',
    marker = list(color = c('#3182bd', '#31a354')),
    text = ~paste0(state, '<br>Prevalence: ', sprintf('%.2f%%', 100 * prevalence)),
    hovertemplate = '%{text}<extra></extra>'
  ) %>%
    plotly::layout(
      title = list(text = 'Prevalence after authorized exclusions'),
      xaxis = list(title = NULL), yaxis = list(title = 'Prevalence', tickformat = '.1%'),
      margin = list(t = 55)
    )
}

.district_plot_data <- function(data, district) {
  .validate_required_columns(data, c('district', 'facility_id', 'month_date'))
  dat <- dplyr::as_tibble(data) %>%
    dplyr::filter(as.character(.data$district) == .env$district) %>%
    dplyr::arrange(facility_id, month_date)
  dat$.tooltip <- .qc_row_tooltip(dat)
  dat$.affected <- .qc_flag_vector(dat, 'flagged')
  dat
}

.plot_district_facets <- function(data, district, metric = c('prevalence', 'tested')) {
  metric <- match.arg(metric)
  dat <- .district_plot_data(data, district)
  .validate_required_columns(dat, metric)
  dat$.metric <- dat[[metric]]
  dat$.reference <- if (metric == 'prevalence' && 'p_hat' %in% names(dat)) {
    dat$p_hat
  } else if (metric == 'tested' && 'tested_roll_median' %in% names(dat)) {
    dat$tested_roll_median
  } else {
    NA_real_
  }
  plot <- ggplot2::ggplot(
    dat, ggplot2::aes(x = month_date, y = .metric, group = facility_id)
  ) +
    ggplot2::geom_line(color = '#2c7fb8', na.rm = TRUE) +
    suppressWarnings(
      ggplot2::geom_point(
        ggplot2::aes(text = .tooltip), color = '#2c7fb8', size = 1.2, na.rm = TRUE
      )
    ) +
    ggplot2::geom_line(
      ggplot2::aes(y = .reference), color = '#fdae61', linetype = 'dashed', na.rm = TRUE
    ) +
    suppressWarnings(
      ggplot2::geom_point(
        data = dat[dat$.affected, , drop = FALSE],
        ggplot2::aes(text = .tooltip), color = '#d73027', shape = 1, size = 2.2,
        stroke = 0.8, na.rm = TRUE
      )
    ) +
    ggplot2::facet_wrap(~facility_id, ncol = 3, scales = if (metric == 'tested') 'free_y' else 'fixed') +
    ggplot2::theme_minimal() +
    ggplot2::labs(
      x = 'Month', y = if (metric == 'prevalence') 'Prevalence' else 'Number tested',
      title = paste(district, 'facility time series'),
      subtitle = 'Dashed: model expectation or rolling baseline; red circles: QC-affected rows'
    )
  if (metric == 'prevalence') {
    plot <- plot + ggplot2::scale_y_continuous(labels = scales::label_percent(accuracy = 1))
  }
  plotly::ggplotly(plot, tooltip = 'text') %>%
    plotly::layout(margin = list(t = 80))
}
