.landing_ui <- function() {
  card <- function(title, ...) {
    shiny::tags$section(class = 'qc-landing-card', shiny::tags$h4(title), ...)
  }
  shiny::tagList(
    shiny::tags$style(shiny::HTML('
      .qc-landing-card { border: 1px solid #dce4e6; border-radius: 8px;
        padding: 12px 15px; margin: 12px 0; background: #fff; }
      .qc-landing-card h4 { margin: 0 0 10px; color: #17363c; }
      .qc-landing-card .help-block { color: #485b62; }
      .qc-landing-steps { padding-left: 22px; }
      .qc-landing-steps li { margin-bottom: 8px; }
      .qc-landing-levels dt { margin-top: 8px; }
      .qc-landing-regions { max-height: 400px; overflow-y: auto; }
    ')),
    shiny::fluidRow(
      shiny::column(4, card(
        'Records by QC priority',
        plotly::plotlyOutput('landing_priorities', height = '265px'),
        shiny::helpText('Each submitted facility-month is counted once at its assigned review priority.')
      )),
      shiny::column(8, card(
        'National trends under Moderate QC',
        shiny::radioButtons('landing_metric', 'Indicator', choices = c(
          'Positivity' = 'positivity', 'Tested' = 'tested', 'Attendance' = 'attendance',
          'Testing coverage' = 'coverage'
        ), selected = 'positivity', inline = TRUE),
        plotly::plotlyOutput('landing_series', height = '285px'),
        shiny::helpText('Moderate omits counts targeted by critical or high priority rules and authorized exclusions.'),
        shiny::uiOutput('landing_replacement_note'),
        shiny::tags$details(
          shiny::tags$summary('Unresolved QC baseline replacements'),
          DT::DTOutput('landing_replacement_unresolved')
        )
      ))
    ),
    shiny::fluidRow(
      shiny::column(5, card(
        'How QC is applied',
        shiny::tags$ol(
          class = 'qc-landing-steps',
          shiny::tags$li(shiny::strong('Check reported counts. '),
                         'Check whether tested and positive counts are possible and compare tested with attendance where available.'),
          shiny::tags$li(shiny::strong('Fit the positivity GAM. '),
                         'Estimate expected positivity using facility, season and time patterns. Flag unusual departures.'),
          shiny::tags$li(shiny::strong('Check reporting patterns. '),
                         'Compare testing volume with its rolling baseline and use neighbouring months to interpret unusual values.'),
          shiny::tags$li(shiny::strong('Assign review priority and action. '),
                         'Apply the rules saved with the run. The QC guide lists the individual rules and thresholds.')
        ),
        shiny::h4('QC priority definitions'),
        shiny::uiOutput('landing_priority_definitions')
      )),
      shiny::column(7,
        card('Flagged records by region',
             shiny::div(class = 'qc-landing-regions', shiny::uiOutput('landing_region_chart'))),
        card('Flagged records over time', plotly::plotlyOutput('ov_trend_plot', height = '295px'))
      )
    )
  )
}

.landing_priority_labels <- c(critical = 'Critical', high = 'High (severe)', medium = 'Medium',
                              low = 'Low', none = 'No priority assigned')

.landing_priority_summary <- function(data, by_region = FALSE) {
  priority <- as.character(data$review_priority)
  priority[!priority %in% .qc_priority_levels] <- 'none'
  rows <- dplyr::tibble(priority = priority)
  if (by_region) {
    region <- trimws(as.character(data$region))
    region[is.na(region) | region == ''] <- 'Region not recorded'
    rows$region <- region
  }
  by <- if (by_region) 'region' else character()
  rows %>%
    dplyr::count(dplyr::across(dplyr::all_of(by)), priority) %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(by))) %>%
    dplyr::mutate(records = sum(.data$n), share = .safe_divide(.data$n, .data$records)) %>%
    dplyr::ungroup()
}

.plot_landing_priorities <- function(summary) {
  summary <- summary[match(intersect(names(.landing_priority_labels), summary$priority),
                           summary$priority), ]
  labels <- unname(.landing_priority_labels[summary$priority])
  severe <- sum(summary$share[summary$priority %in% c('critical', 'high')])
  plotly::plot_ly(
    labels = labels, values = summary$n, type = 'pie', hole = 0.69, sort = FALSE,
    marker = list(colors = unname(.qc_priority_palette[summary$priority])),
    textinfo = 'none', hovertemplate = '%{label}<br>%{value:,} records (%{percent})<extra></extra>'
  ) %>%
    plotly::layout(
      annotations = list(list(x = 0.5, y = 0.5, xref = 'paper', yref = 'paper',
                              text = sprintf('<b>%.1f%%</b><br>Critical / high', 100 * severe),
                              showarrow = FALSE)),
      legend = list(orientation = 'h', x = 0.5, xanchor = 'center', y = -0.1),
      margin = list(l = 6, r = 6, b = 45, t = 10)
    )
}

.plot_landing_regions <- function(summary) {
  flagged <- summary[summary$priority != 'none', ]
  if (nrow(flagged) == 0L) {
    return(plotly::layout(plotly::plot_ly(x = 0, y = 0, type = 'scatter', visible = FALSE),
                          xaxis = list(visible = FALSE), yaxis = list(visible = FALSE),
                          annotations = list(list(x = 0.5, y = 0.5, xref = 'paper', yref = 'paper',
                                                  text = 'No records with a QC priority in this run',
                                                  showarrow = FALSE))))
  }
  # Retain regions with zero flags on the axis.
  rates <- stats::setNames(rep(0, length(unique(summary$region))), unique(summary$region))
  hits <- tapply(flagged$share, flagged$region, sum)
  rates[names(hits)] <- hits
  plot <- plotly::plot_ly()
  for (p in .qc_priority_levels) {
    d <- flagged[flagged$priority == p, ]
    if (nrow(d) == 0L) next
    plot <- plotly::add_bars(
      plot, y = d$region, x = d$share, name = .landing_priority_labels[[p]], orientation = 'h',
      marker = list(color = .qc_priority_palette[[p]]),
      text = sprintf('%s<br>%s: %s of %s records (%.1f%%)', d$region,
                     .landing_priority_labels[[p]], d$n, d$records, 100 * d$share),
      hoverinfo = 'text', textposition = 'none'
    )
  }
  plotly::layout(
    plot, barmode = 'stack',
    xaxis = list(title = 'Share of submitted records flagged', tickformat = '.0%', rangemode = 'tozero'),
    yaxis = list(title = '', type = 'category', categoryorder = 'array',
                 categoryarray = names(sort(rates)), tickmode = 'array',
                 tickvals = names(sort(rates)), range = c(-0.5, length(rates) - 0.5), automargin = TRUE),
    legend = list(orientation = 'h', y = -0.22), margin = list(t = 10)
  )
}

.landing_server <- function(input, output, selected_run, data_r, catalog_r) {
  moderate <- qc_scenario_presets()[['moderate']]
  national <- list(level = 'national', unit = NULL)
  applied <- shiny::reactive({
    run <- selected_run()
    .impact_apply(run$data_flagged, list(moderate = moderate),
                  .if_null(run$config$priority, .default_qc_priority()))
  })
  observed <- shiny::reactive(.impact_compute(applied(), national))
  replacement <- shiny::reactive({
    result <- .qc_baseline_replacement(applied()[['moderate']])
    series <- qc_aggregate_metrics(result$frame, 'national', monthly = TRUE)
    series$scenario <- 'moderate'
    series$version <- 'qc_baseline'
    list(series = series, targets = result$targets)
  })
  output$landing_series <- plotly::renderPlotly({
    metric <- .value_or(input$landing_metric, 'positivity')
    series <- dplyr::bind_rows(observed(), replacement()$series)
    .plot_impact_series(series, 'moderate', metric)
  })
  output$landing_replacement_note <- shiny::renderUI({
    targets <- replacement()$targets
    filled <- sum(targets$status == 'replaced')
    shiny::helpText(sprintf(
      'QC baseline replacement: %s of %s targeted tested or positive counts filled; %s unresolved.',
      format(filled, big.mark = ','), format(nrow(targets), big.mark = ','),
      format(nrow(targets) - filled, big.mark = ',')
    ),
    'Uses the saved GAM probability and rolling tested median without fitting new models or uncertainty intervals. ',
    'Missing values remain missing; attendance is not replaced. The saved expectations may be influenced by ',
    'suspect observations, and replacements tend to smooth the series.')
  })
  output$landing_replacement_unresolved <- DT::renderDT({
    targets <- replacement()$targets
    unresolved <- targets[targets$status != 'replaced', , drop = FALSE]
    reasons <- c(
      baseline_unavailable = 'No usable rolling median (missing, invalid or insufficient history)',
      baseline_below_observed_positive = 'Rolling median is below observed positives; tested left missing',
      tested_unavailable = 'No usable tested count for the positivity calculation',
      prediction_unavailable = 'No usable saved GAM probability'
    )
    unresolved$status <- unname(reasons[unresolved$status])
    display <- unresolved[c('facility_id', 'month_date', 'field', 'status', 'tested_baseline',
                            'observed_positive', 'predicted_probability')]
    names(display) <- c('Facility', 'Month', 'Targeted count', 'Reason', 'Rolling median tested',
                        'Observed positives', 'GAM probability')
    DT::datatable(display, rownames = FALSE, options = list(pageLength = 5, scrollX = TRUE))
  })
  output$landing_priorities <- plotly::renderPlotly(.plot_landing_priorities(.landing_priority_summary(data_r())))
  regions <- shiny::reactive(.landing_priority_summary(data_r(), TRUE))
  output$landing_region_chart <- shiny::renderUI({
    height <- max(295L, 22L * dplyr::n_distinct(regions()$region) + 105L)
    plotly::plotlyOutput('landing_regions', height = paste0(height, 'px'))
  })
  output$landing_regions <- plotly::renderPlotly(.plot_landing_regions(regions()))
  output$landing_priority_definitions <- shiny::renderUI({
    catalog <- catalog_r()
    meanings <- c(
      critical = 'Highest priority. Includes impossible counts, such as more positives than tests.',
      high = 'Urgent review of strong or combined signals.',
      medium = 'Review of anomalies assigned medium priority by the run settings.',
      low = 'Lower priority signals for review.'
    )
    shiny::tags$dl(class = 'qc-landing-levels', lapply(.qc_priority_levels, function(p) {
      examples <- unique(catalog$label[catalog$priority == p])
      shiny::tagList(
        shiny::tags$dt(.landing_priority_labels[[p]]),
        shiny::tags$dd(meanings[[p]],
                       if (length(examples)) paste0(' Examples in this run include ',
                                                    paste(utils::head(examples, 3), collapse = ', '), '.'))
      )
    }), shiny::tags$dt('No priority assigned'),
    shiny::tags$dd('No review priority was recorded. Check assessment coverage in Data usability.'))
  })
}
