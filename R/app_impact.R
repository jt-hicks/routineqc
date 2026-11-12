# Filter impact: how QC decisions change aggregate estimates. A scenario is
# a hypothetical analytical choice, applied here to a copy of the data. Nothing
# in this file changes an observation, a stored flag or a stored action.

.impact_scenarios <- function(custom = NULL, handling = 'missing') {
  scenarios <- qc_scenario_presets(handling = handling)
  if (!is.null(custom)) scenarios[[custom$name]] <- custom
  scenarios
}

# Applies every scenario once, to the whole dataset, before selecting geography.
.impact_apply <- function(data, scenarios, priority = .default_qc_priority()) {
  lapply(scenarios, function(s) apply_qc_scenario(data, s, priority))
}

.impact_geography <- function(selection, data) {
  selection <- .nav_normalise(selection)
  mid_field <- .nav_mid_field(data)
  level <- .nav_level(selection)
  switch(
    level,
    national = list(level = 'national', unit = NULL, label = 'National'),
    region = list(level = 'region', unit = selection$region, label = paste('Region', selection$region)),
    mid = list(level = mid_field, unit = selection$mid,
               label = paste(.nav_mid_label(mid_field), selection$mid)),
    facility = list(level = 'facility', unit = selection$facility,
                    label = paste('Facility', selection$facility))
  )
}

.unit_key <- function(level) {
  switch(level, national = NULL, region = 'region', council = 'council', district = 'district',
         facility = 'facility_id')
}

.pick_unit <- function(result, geography) {
  key <- .unit_key(geography$level)
  if (is.null(key) || is.null(geography$unit)) return(result)
  result[as.character(result[[key]]) %in% geography$unit, , drop = FALSE]
}

# Reported and filtered aggregates for one geography under every scenario.
.impact_compute <- function(applied, geography, monthly = TRUE, imputed = NULL) {
  first <- applied[[1]]
  reported <- .pick_unit(qc_aggregate_metrics(.scenario_frame(first, 'reported'), geography$level, monthly),
                         geography)
  reported$version <- 'reported'
  reported$scenario <- 'reported'
  filtered <- lapply(names(applied), function(nm) {
    out <- .pick_unit(qc_aggregate_metrics(.scenario_frame(applied[[nm]], 'filtered'), geography$level,
                                           monthly), geography)
    out$version <- 'filtered_missing'
    out$scenario <- nm
    out
  })
  parts <- c(list(reported), filtered)
  if (!is.null(imputed)) {
    for (nm in names(imputed)) {
      out <- .pick_unit(summarise_qc_imputation(imputed[[nm]], geography$level, monthly), geography)
      out$version <- 'filtered_imputed'
      out$scenario <- nm
      parts[[length(parts) + 1L]] <- out
    }
  }
  dplyr::bind_rows(parts)
}

# One row per scenario: the estimate, its change from the reported value in
# percentage points, and exactly what was retained, omitted and still missing.
.impact_summary <- function(applied, geography, imputed = NULL) {
  res <- .impact_compute(applied, geography, monthly = FALSE, imputed = imputed)
  rep <- res[res$version == 'reported', ]
  rows <- lapply(names(applied), function(nm) {
    scn <- applied[[nm]]
    f <- res[res$version == 'filtered_missing' & res$scenario == nm, ]
    key <- .unit_key(geography$level)
    in_geo <- if (is.null(key) || is.null(geography$unit)) rep(TRUE, nrow(scn)) else
      as.character(if (key == 'facility_id') scn$facility_id else scn[[key]]) %in% geography$unit
    sc <- scn[in_geo, , drop = FALSE]
    pos_state <- sc$scn_positive_state
    tested_state <- sc$scn_tested_state
    row <- dplyr::tibble(
      scenario = nm,
      positivity_reported = rep$positivity[1],
      positivity_filtered = f$positivity[1],
      change_pp = 100 * (f$positivity[1] - rep$positivity[1]),
      tested_reported = rep$tested_total[1],
      tested_retained = f$tested_total[1],
      share_tested_retained = .safe_divide(f$tested_total[1], rep$tested_total[1]),
      pairs_reported = rep$pair_rows[1], pairs_retained = f$pair_rows[1],
      facilities = dplyr::n_distinct(sc$facility_id),
      facilities_eligible = dplyr::n_distinct(sc$facility_id[sc$scn_facility_eligible]),
      omitted_by_qc = sum(tested_state %in% c('qc_omitted', 'invalid') & sc$scn_facility_eligible) +
        sum(pos_state %in% c('qc_omitted', 'invalid') & sc$scn_facility_eligible),
      removed_with_facility = sum(!sc$scn_facility_eligible) * 2L,
      missing_before_qc = sum(tested_state == 'missing') + sum(pos_state == 'missing')
    )
    imp <- res[res$version == 'filtered_imputed' & res$scenario == nm, ]
    if (nrow(imp) > 0L) {
      row$positivity_imputed <- imp$positivity[1]
      row$change_imputed_pp <- 100 * (imp$positivity[1] - rep$positivity[1])
      row$ci_low <- imp$ci_low[1]
      row$ci_high <- imp$ci_high[1]
      row$share_tested_imputed <- imp$prop_tested_from_imputation[1]
    }
    row
  })
  dplyr::bind_rows(rows)
}

# The units whose estimate moves most under a scenario.
.impact_unit_changes <- function(applied, scenario, min_pairs = 20L) {
  scn <- applied[[scenario]]
  field <- .nav_mid_field(scn)
  level <- if (is.null(field)) 'region' else field
  rep <- qc_aggregate_metrics(.scenario_frame(scn, 'reported'), level, monthly = FALSE)
  fil <- qc_aggregate_metrics(.scenario_frame(scn, 'filtered'), level, monthly = FALSE)
  out <- dplyr::tibble(
    unit = as.character(rep[[level]]),
    positivity_reported = rep$positivity,
    positivity_filtered = fil$positivity[match(rep[[level]], fil[[level]])],
    pairs_reported = rep$pair_rows,
    pairs_retained = fil$pair_rows[match(rep[[level]], fil[[level]])],
    tested_reported = rep$tested_total,
    tested_retained = fil$tested_total[match(rep[[level]], fil[[level]])]
  )
  out$change_pp <- 100 * (out$positivity_filtered - out$positivity_reported)
  out$share_tested_retained <- .safe_divide(out$tested_retained, out$tested_reported)
  out$thin <- out$pairs_retained < min_pairs
  out$lost_estimate <- is.na(out$positivity_filtered) & !is.na(out$positivity_reported)
  attr(out, 'unit_field') <- level
  out[order(-abs(dplyr::coalesce(out$change_pp, 0)), out$unit), ]
}

.describe_scenario_rules <- function(scenario) {
  e <- scenario$eligibility
  o <- scenario$observation
  fac <- c(
    if (!is.na(e$max_prop_without_testing)) paste0('at most ', 100 * e$max_prop_without_testing,
                                                    '% of the facility\'s months without testing'),
    if (!is.na(e$max_gap_months)) paste0('no gap longer than ', e$max_gap_months, ' months'),
    if (!is.na(e$min_months_with_testing)) paste0('at least ', e$min_months_with_testing, ' months with testing')
  )
  omit <- switch(
    o$omit_through,
    none = 'no flagged counts omitted',
    authorized = 'only counts authorized for exclusion (impossible values) omitted',
    critical = 'counts behind critical flags omitted',
    high = 'counts behind critical and high-priority flags omitted',
    medium = 'counts behind critical, high and medium-priority flags omitted',
    low = 'counts behind every flag omitted'
  )
  paste0(
    'Facilities: ', if (length(fac) == 0L) 'all included; no reporting-completeness filter' else paste(fac, collapse = '; '),
    '. Facility-month observations: ', omit, '. A flag omits only the count(s) it puts in doubt; ',
    'other months are unchanged. Invalid counts are always left out of calculations.'
  )
}

# Imputed has its own hue: orange is the high-priority colour.
.impact_palette <- c(reported = '#7f7f7f', filtered_missing = '#4c78a8', filtered_imputed = '#7b5ea7',
                      qc_baseline = '#238b45')
.impact_version_labels <- c(reported = 'Reported', filtered_missing = 'Filtered, excluded left missing',
                            filtered_imputed = 'Filtered, imputed', qc_baseline = 'QC baseline replacement')

.plot_impact_series <- function(results, scenario, metric = 'positivity') {
  d <- results[results$scenario %in% c('reported', scenario), ]
  spec <- .explore_metrics[[metric]]
  plot <- plotly::plot_ly()
  line_types <- c(reported = 'solid', filtered_missing = 'dash', filtered_imputed = 'dot', qc_baseline = 'dot')
  for (v in c('reported', 'filtered_missing', 'filtered_imputed', 'qc_baseline')) {
    x <- d[d$version == v, ]
    if (nrow(x) == 0L) next
    x <- x[order(x$month_date), ]
    y <- x[[spec$column]]
    if (metric == 'tested') y <- ifelse(x$tested_rows > 0L, y, NA_real_)
    if (metric == 'attendance') y <- ifelse(x$attendance_rows > 0L, y, NA_real_)
    plot <- plotly::add_trace(
      plot, x = x$month_date, y = y, type = 'scatter', mode = 'lines+markers', connectgaps = FALSE,
      name = .impact_version_labels[[v]],
      line = list(color = .impact_palette[[v]], width = 2, dash = line_types[[v]]),
      marker = list(color = .impact_palette[[v]], size = 4),
      text = paste0(format(x$month_date, '%b %Y'), '<br>', .impact_version_labels[[v]], ': ',
                    .format_metric(y, metric)), hoverinfo = 'text'
    )
    if (v == 'filtered_imputed' && metric == 'positivity' && 'ci_low' %in% names(x) && any(!is.na(x$ci_low))) {
      plot <- plotly::add_ribbons(plot, x = x$month_date, ymin = x$ci_low, ymax = x$ci_high,
                                  name = 'Imputation uncertainty (95%)', showlegend = TRUE,
                                  fillcolor = 'rgba(123,94,167,0.15)', line = list(width = 0),
                                  hoverinfo = 'skip')
    }
  }
  plotly::layout(plot, xaxis = list(title = ''),
                 yaxis = list(title = spec$short, rangemode = 'tozero',
                              tickformat = if (spec$percent) '.0%' else ',d'),
                 legend = list(orientation = 'h', y = -0.2))
}

.plot_impact_strictness <- function(summary) {
  order <- summary$scenario
  plot <- plotly::plot_ly() %>%
    plotly::add_trace(x = order, y = rep(summary$positivity_reported[1], length(order)), type = 'scatter',
                      mode = 'lines', name = 'Reported', line = list(color = .impact_palette[['reported']], dash = 'dash'),
                      hoverinfo = 'skip') %>%
    plotly::add_trace(x = order, y = summary$positivity_filtered, type = 'scatter', mode = 'lines+markers',
                      name = 'Filtered, excluded left missing',
                      line = list(color = .impact_palette[['filtered_missing']]),
                      text = paste0(order, ': ', .format_metric(summary$positivity_filtered, 'positivity'),
                                    '<br>', sprintf('%+.2f', summary$change_pp), ' percentage points<br>',
                                    .format_metric(100 * summary$share_tested_retained, 'tested'),
                                    '% of tested retained'), hoverinfo = 'text')
  if ('positivity_imputed' %in% names(summary)) {
    plot <- plotly::add_trace(
      plot, x = order, y = summary$positivity_imputed, type = 'scatter', mode = 'lines+markers',
      name = 'Filtered, imputed', line = list(color = .impact_palette[['filtered_imputed']]),
      error_y = list(type = 'data', symmetric = FALSE,
                     array = summary$ci_high - summary$positivity_imputed,
                     arrayminus = summary$positivity_imputed - summary$ci_low,
                     color = .impact_palette[['filtered_imputed']]),
      text = paste0(order, ': ', .format_metric(summary$positivity_imputed, 'positivity')), hoverinfo = 'text'
    )
  }
  plotly::layout(plot, xaxis = list(title = 'Scenario (more strict to the right)', categoryorder = 'array',
                                    categoryarray = order),
                 yaxis = list(title = 'Positivity', tickformat = '.0%', rangemode = 'tozero'),
                 legend = list(orientation = 'h', y = -0.3))
}

.plot_impact_change <- function(summary) {
  cols <- c('change_pp', if ('change_imputed_pp' %in% names(summary)) 'change_imputed_pp')
  plot <- plotly::plot_ly()
  plot <- plotly::add_bars(plot, x = summary$scenario, y = summary$change_pp, name = 'Excluded left missing',
                           marker = list(color = .impact_palette[['filtered_missing']]),
                           text = sprintf('%+.2f pp', summary$change_pp), hoverinfo = 'text')
  if ('change_imputed_pp' %in% cols) {
    plot <- plotly::add_bars(plot, x = summary$scenario, y = summary$change_imputed_pp, name = 'Imputed',
                             marker = list(color = .impact_palette[['filtered_imputed']]),
                             text = sprintf('%+.2f pp', summary$change_imputed_pp), hoverinfo = 'text')
  }
  plotly::layout(plot, barmode = 'group',
                 xaxis = list(title = '', categoryorder = 'array', categoryarray = summary$scenario),
                 yaxis = list(title = 'Change in positivity (percentage points)', zeroline = TRUE),
                 legend = list(orientation = 'h', y = -0.2)) %>%
    plotly::style(textposition = 'none')
}

.plot_impact_retention <- function(summary) {
  plotly::plot_ly() %>%
    plotly::add_bars(x = summary$scenario, y = summary$share_tested_retained, name = 'Tested retained',
                     marker = list(color = '#4c78a8'),
                     text = sprintf('%.1f%% of tested counts retained', 100 * summary$share_tested_retained),
                     hoverinfo = 'text') %>%
    plotly::layout(xaxis = list(title = '', categoryorder = 'array', categoryarray = summary$scenario),
                   yaxis = list(title = 'Share of reported tested counts', tickformat = '.0%', range = c(0, 1))) %>%
    plotly::style(textposition = 'none')
}

.plot_impact_units <- function(changes, n = 15L) {
  top <- utils::head(changes[!is.na(changes$change_pp), ], n)
  if (nrow(top) == 0L) return(plotly::plotly_empty())
  top$unit <- factor(top$unit, levels = rev(top$unit))
  plotly::plot_ly(top, x = ~change_pp, y = ~unit, type = 'bar', orientation = 'h',
                  marker = list(color = ifelse(top$thin, '#bdbdbd', '#4c78a8')),
                  text = paste0(top$unit, ': ', sprintf('%+.2f pp', top$change_pp), '<br>',
                                top$pairs_retained, ' of ', top$pairs_reported, ' pairs retained',
                                ifelse(top$thin, '<br>Few pairs retained: interpret with care', '')),
                  hoverinfo = 'text') %>%
    plotly::layout(xaxis = list(title = 'Change in positivity (percentage points)'),
                   yaxis = list(title = '')) %>%
    plotly::style(textposition = 'none')
}
