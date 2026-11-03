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

.if_null <- function(x, default) if (is.null(x)) default else x

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
    review_coverage = 'Review: testing coverage',
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
    review_coverage = '#1f5fa8',
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
    pearson_resid_scaled = 'Dispersion-scaled residual',
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

# Flag families and the charts they belong on. A chart marks only the evidence
# it can show: an attendance flag has nothing to say about prevalence.
.qc_flag_families <- list(
  impossible = list(
    label = 'Impossible count', color = '#b3261e',
    flags = c('core_invalid'), metrics = c('prevalence', 'tested')
  ),
  prevalence = list(
    label = 'Prevalence', color = '#6a4c93',
    flags = c('resid_extreme', 'all_negative_large_n', 'all_positive_large_n',
              'large_monthly_prevalence_change'),
    umbrella = 'prevalence_extreme', metrics = 'prevalence'
  ),
  tested = list(
    label = 'Tested volume', color = '#b8620a',
    flags = c('tested_extreme_high', 'tested_extreme_low', 'tested_zero_unusual',
              'tested_large_jump', 'tested_large_drop'),
    umbrella = 'tested_volume_extreme', metrics = 'tested'
  ),
  attendance = list(
    label = 'Tested above attendance', color = '#2f7d6d',
    flags = c('tested_gt_attending', 'tested_gt_attending_major', 'attending_negative'),
    umbrella = 'attendance_issue', metrics = 'coverage'
  ),
  coverage = list(
    label = 'Low testing coverage', color = '#1f5fa8',
    flags = c('coverage_low', 'coverage_very_low'), metrics = 'coverage'
  )
)

.qc_priority_style <- dplyr::tibble(
  priority = c('critical', 'high', 'medium', 'low'),
  label = c('Critical', 'High', 'Medium', 'Low'),
  size = c(3.4, 2.8, 2.4, 1.8),
  shape = c(16L, 16L, 21L, 21L),
  stroke = c(0.6, 0.6, 1.3, 0.9)
)

# For each row, the family to mark on `metric` and its priority there: the
# highest-priority family among the flags this chart can show.
.district_flag_marks <- function(data, metric, priority = NULL) {
  priority <- utils::modifyList(.default_qc_priority(), .if_null(priority, list()))
  levels <- c(.qc_priority_levels, 'none')
  on <- function(nm) {
    col <- paste0('flag_', nm)
    if (col %in% names(data)) dplyr::coalesce(as.logical(data[[col]]), FALSE) else rep(FALSE, nrow(data))
  }
  best_rank <- rep(length(levels), nrow(data))
  best_family <- rep(NA_character_, nrow(data))
  for (fam in names(.qc_flag_families)) {
    spec <- .qc_flag_families[[fam]]
    if (!metric %in% spec$metrics) next
    rank <- rep(length(levels), nrow(data))
    for (f in spec$flags) {
      level <- if (f == 'core_invalid') 'critical' else .if_null(priority[[f]], 'medium')
      rank <- pmin(rank, ifelse(on(f), match(level, levels), length(levels)))
    }
    if (!is.null(spec$umbrella)) {
      rank <- pmin(rank, ifelse(on(spec$umbrella) & rank == length(levels), match('medium', levels), length(levels)))
    }
    better <- rank < best_rank
    best_rank[better] <- rank[better]
    best_family[better] <- fam
  }
  dplyr::tibble(.mark_family = best_family, .mark_priority = levels[best_rank])
}

.district_metric_value <- function(data, metric) {
  if (metric == 'coverage') {
    if ('testing_coverage' %in% names(data)) return(data$testing_coverage)
    if (!'attending' %in% names(data)) return(rep(NA_real_, nrow(data)))
    return(ifelse(!is.na(data$tested) & !is.na(data$attending) & data$attending > 0,
                  data$tested / data$attending, NA_real_))
  }
  data[[metric]]
}

.district_plot_data <- function(data, district, hide_untested = FALSE) {
  .validate_required_columns(data, c('district', 'facility_id', 'month_date'))
  if (!rlang::is_bool(hide_untested)) {
    rlang::abort('`hide_untested` must be TRUE or FALSE.')
  }
  dat <- dplyr::as_tibble(data) %>%
    dplyr::filter(as.character(.data$district) == .env$district) %>%
    dplyr::arrange(facility_id, month_date)
  if (isTRUE(hide_untested) && 'tested' %in% names(dat)) {
    tested_any <- dat %>%
      dplyr::group_by(facility_id) %>%
      dplyr::summarise(
        any_testing = any(!is.na(tested) & tested > 0), .groups = 'drop'
      )
    keep <- tested_any$facility_id[tested_any$any_testing]
    dat <- dat[as.character(dat$facility_id) %in% as.character(keep), , drop = FALSE]
  }
  dat$.tooltip <- .qc_row_tooltip(dat)
  dat$.affected <- .qc_flag_vector(dat, 'flagged')
  dat
}

# Pixel height of the facility facets, three to a row. The app container and
# ggplotly() must agree on it: without an explicit height ggplotly() lays rows
# out against a small default, and a district with many facilities collapses
# every row after the first.
.district_plot_height <- function(n_facilities) {
  max(500, ceiling(n_facilities / 3) * 240)
}

.plot_district_facets <- function(data, district,
                                  metric = c('prevalence', 'tested', 'coverage'),
                                  hide_untested = FALSE, priority = NULL) {
  metric <- match.arg(metric)
  dat <- .district_plot_data(data, district, hide_untested = hide_untested)
  dat$.metric <- .district_metric_value(dat, metric)
  dat$.reference <- if (metric == 'prevalence' && 'p_hat' %in% names(dat)) {
    dat$p_hat
  } else if (metric == 'tested' && 'tested_roll_median' %in% names(dat)) {
    dat$tested_roll_median
  } else {
    NA_real_
  }
  dat <- dplyr::bind_cols(dat, .district_flag_marks(dat, metric, priority))
  marks <- dat[!is.na(dat$.mark_family) & !is.na(dat$.metric), , drop = FALSE]
  marks <- dplyr::left_join(marks, .qc_priority_style, by = c('.mark_priority' = 'priority'))
  marks$.color <- vapply(marks$.mark_family, function(f) .qc_flag_families[[f]]$color, character(1))
  # Draw higher priorities last so they sit on top.
  marks <- marks[order(match(marks$.mark_priority, .qc_priority_levels), decreasing = TRUE), , drop = FALSE]

  plot <- ggplot2::ggplot(
    dat, ggplot2::aes(x = month_date, y = .metric, group = facility_id)
  )
  if (metric == 'coverage') {
    plot <- plot +
      ggplot2::geom_hline(yintercept = 1, color = '#9aa59f', linetype = 'dotted') +
      ggplot2::geom_hline(yintercept = 0.8, color = '#9aa59f', linetype = 'dashed')
  }
  plot <- plot +
    ggplot2::geom_line(color = '#5d6a63', linewidth = 0.4, na.rm = TRUE) +
    suppressWarnings(
      ggplot2::geom_point(
        ggplot2::aes(text = .tooltip), color = '#5d6a63', size = 0.7, na.rm = TRUE
      )
    ) +
    ggplot2::geom_line(
      ggplot2::aes(y = .reference), color = '#9aa59f', linetype = 'dashed', na.rm = TRUE
    )
  if (nrow(marks) > 0L) {
    plot <- plot + suppressWarnings(
      ggplot2::geom_point(
        data = marks,
        ggplot2::aes(text = .tooltip, size = I(size), shape = I(shape),
                     color = I(.color), fill = I(ifelse(shape == 21L, '#ffffff', .color)),
                     stroke = I(stroke)),
        na.rm = TRUE
      )
    )
  }
  y_label <- switch(metric, prevalence = 'Prevalence', tested = 'Number tested',
                    coverage = 'Tested / first attendance')
  plot <- plot +
    ggplot2::facet_wrap(~facility_id, ncol = 3, scales = if (metric == 'tested') 'free_y' else 'fixed') +
    ggplot2::theme_minimal() +
    ggplot2::labs(x = 'Month', y = y_label, title = paste(district, 'facility time series'))
  if (metric %in% c('prevalence', 'coverage')) {
    plot <- plot + ggplot2::scale_y_continuous(labels = scales::label_percent(accuracy = 1))
  }
  plotly::ggplotly(
    plot, tooltip = 'text',
    height = .district_plot_height(dplyr::n_distinct(dat$facility_id))
  ) %>%
    plotly::layout(margin = list(t = 60), showlegend = FALSE)
}

# Key for the facet marks, as HTML so it reads without hovering.
.district_flag_key <- function(metric) {
  fams <- Filter(function(s) metric %in% s$metrics, .qc_flag_families)
  swatch <- function(color, filled, size) {
    shiny::tags$span(style = paste0(
      'display:inline-block;width:', size, 'px;height:', size, 'px;border-radius:50%;',
      'border:2px solid ', color, ';background:', if (filled) color else '#fff', ';',
      'vertical-align:middle;margin-right:6px;'
    ))
  }
  item <- function(...) shiny::tags$span(style = 'margin-right:18px;white-space:nowrap;', ...)
  shiny::div(
    style = 'margin:6px 0 10px;font-size:13px;line-height:2;',
    shiny::tags$strong('Flag type: ', style = 'margin-right:8px;'),
    lapply(fams, function(s) item(swatch(s$color, TRUE, 12), s$label)),
    shiny::tags$br(),
    shiny::tags$strong('Priority: ', style = 'margin-right:8px;'),
    item(swatch('#3f4744', TRUE, 15), 'Critical'),
    item(swatch('#3f4744', TRUE, 12), 'High'),
    item(swatch('#3f4744', FALSE, 12), 'Medium'),
    item(swatch('#3f4744', FALSE, 9), 'Low'),
    if (metric == 'coverage') item('Dashed line 80%, dotted 100%')
  )
}

.district_summary_series <- function(data, district) {
  .validate_required_columns(data, c('district', 'month_date', 'tested', 'positive'))
  dat <- dplyr::as_tibble(data) %>%
    dplyr::filter(
      !is.na(.data$district),
      as.character(.data$district) == .env$district
    )
  if (nrow(dat) == 0L) {
    return(dplyr::tibble())
  }
  .summarise_qc_group(dat, group_vars = 'month_date') %>%
    dplyr::arrange(month_date)
}

.plot_district_summary_series <- function(data, district,
                                          metric = c('prevalence', 'tested', 'coverage')) {
  metric <- match.arg(metric)
  summary <- .district_summary_series(data, district)
  if (nrow(summary) == 0L) {
    return(NULL)
  }
  if (metric == 'coverage') {
    dat <- dplyr::as_tibble(data)
    dat <- dat[!is.na(dat$district) & as.character(dat$district) == district, , drop = FALSE]
    if (!'attending' %in% names(dat)) dat$attending <- NA_real_
    cov <- dat %>%
      dplyr::filter(!is.na(tested), !is.na(attending)) %>%
      dplyr::group_by(month_date) %>%
      dplyr::summarise(coverage = .safe_divide(sum(tested), sum(attending)), .groups = 'drop')
    summary <- dplyr::left_join(summary, cov, by = 'month_date')
  }
  summary$summary_value <- switch(
    metric,
    prevalence = summary$prevalence_before_qc,
    tested = summary$total_tested_before_qc,
    coverage = summary$coverage
  )
  hover <- paste0(
    'Month: ', format(summary$month_date, '%Y-%m'),
    '<br>Facility-months reported: ', summary$total_rows,
    '<br>Total tested: ', summary$total_tested_before_qc,
    '<br>Total positive: ', summary$total_positive_before_qc,
    '<br>District prevalence: ',
    ifelse(
      is.na(summary$prevalence_before_qc), 'NA',
      sprintf('%.2f%%', 100 * summary$prevalence_before_qc)
    ),
    '<br>Rows with authorized exclusion: ', summary$authorized_exclusion_rows
  )
  y_title <- switch(metric, prevalence = 'District prevalence', tested = 'Total number tested',
                    coverage = 'District testing coverage')

  plotly::plot_ly(
    summary, x = ~month_date, y = ~summary_value, type = 'scatter',
    mode = 'lines+markers', line = list(color = '#08519c', width = 2),
    marker = list(color = '#08519c', size = 7),
    text = hover, hovertemplate = '%{text}<extra></extra>',
    name = y_title
  ) %>%
    plotly::layout(
      title = list(text = paste(district, 'district total, all reporting facilities')),
      xaxis = list(title = 'Month'),
      yaxis = list(
        title = y_title,
        tickformat = if (metric == 'tested') NULL else '.1%',
        rangemode = 'tozero'
      ),
      hovermode = 'closest', margin = list(t = 55), showlegend = FALSE
    )
}

.plot_consistency_gaps <- function(consistency) {
  buckets <- .consistency_gap_buckets(consistency)
  plotly::plot_ly(
    buckets, x = ~gap_bucket, y = ~facilities, type = 'bar',
    marker = list(color = '#2c7fb8'),
    text = ~paste0(
      'Longest gap: ', gap_bucket, ' month(s)',
      '<br>Facilities: ', facilities,
      '<br>All facilities: ', sprintf('%.1f%%', 100 * proportion)
    ),
    hovertemplate = '%{text}<extra></extra>'
  ) %>%
    plotly::layout(
      title = list(text = 'Longest gap without testing'),
      xaxis = list(title = 'Consecutive months without testing'),
      yaxis = list(title = 'Number of facilities'),
      margin = list(t = 55)
    )
}

# Attendance evaluation. Only rows carrying both tested and attending can be
# compared, so every proportion here is over those rows, never over all rows.
.attendance_available <- function(data) {
  'attending' %in% names(data) && any(!is.na(data$attending))
}

.attendance_comparable <- function(data) {
  dat <- dplyr::as_tibble(data)
  dat$.comparable <- !is.na(dat$tested) & !is.na(dat$attending)
  dat$.over <- dat$.comparable & dat$tested > dat$attending
  dat
}

.attendance_summary <- function(data, group_vars = character(0)) {
  .validate_required_columns(data, c('facility_id', 'tested', 'attending'))
  .attendance_comparable(data) %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(group_vars))) %>%
    dplyr::summarise(
      rows_with_attendance = sum(!is.na(attending)),
      comparable_rows = sum(.comparable),
      tested_gt_attending_rows = sum(.over),
      prop_tested_gt_attending = .safe_divide(tested_gt_attending_rows, comparable_rows),
      facilities = dplyr::n_distinct(facility_id[.comparable]),
      facilities_tested_gt_attending = dplyr::n_distinct(facility_id[.over]),
      excess_tested = sum(tested[.over] - attending[.over]),
      .groups = 'drop'
    )
}

.plot_attendance_monthly <- function(data) {
  monthly <- .attendance_summary(data, 'month_date') %>%
    dplyr::filter(comparable_rows > 0L)
  if (nrow(monthly) == 0L) return(NULL)
  plotly::plot_ly(
    monthly, x = ~month_date, y = ~prop_tested_gt_attending,
    type = 'scatter', mode = 'lines+markers',
    line = list(color = '#238b45'), marker = list(color = '#238b45', size = 5),
    text = ~paste0(
      'Month: ', format(month_date, '%Y-%m'),
      '<br>Tested above attendance: ', tested_gt_attending_rows,
      '<br>Comparable rows: ', comparable_rows,
      '<br>Proportion: ', sprintf('%.1f%%', 100 * prop_tested_gt_attending)
    ),
    hovertemplate = '%{text}<extra></extra>'
  ) %>%
    plotly::layout(
      title = list(text = 'Rows with tested above attendance, by month'),
      xaxis = list(title = ''),
      yaxis = list(title = 'Proportion of comparable rows', tickformat = '.1%', rangemode = 'tozero'),
      margin = list(t = 55)
    )
}
