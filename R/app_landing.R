.landing_region_data <- function(data, region) {
  if (is.null(region)) return(data)
  data[trimws(as.character(data$region)) %in% region, , drop = FALSE]
}

# ---- UI ---------------------------------------------------------------------

.landing_ui <- function() {
  card <- function(title, ..., help_id = NULL) {
    header <- if (!is.null(help_id)) {
      shiny::tags$div(
        class = 'qc-landing-card-header',
        shiny::tags$h4(title),
        shiny::tags$details(
          class = 'qc-priority-help',
          shiny::tags$summary('?'),
          shiny::uiOutput(help_id)
        )
      )
    } else {
      shiny::tags$h4(title)
    }
    shiny::tags$section(class = 'qc-landing-card', header, ...)
  }

  shiny::tagList(
    shiny::tags$style(shiny::HTML('
      .qc-landing-card { border: 1px solid #dce4e6; border-radius: 8px;
        padding: 12px 15px; margin: 12px 0; background: #fff; }
      .qc-landing-card h4 { margin: 0 0 10px; color: #17363c; }
      .qc-landing-card-header { display: flex; align-items: flex-start;
        gap: 6px; margin-bottom: 10px; }
      .qc-landing-card-header h4 { margin: 0; }
      .qc-priority-help > summary { cursor: pointer; list-style: none;
        font-size: 11px; font-weight: 700; color: #fff; background: #485b62;
        border-radius: 50%; width: 18px; height: 18px; line-height: 18px;
        text-align: center; display: inline-block; flex-shrink: 0; margin-top: 2px; }
      .qc-priority-help > summary::-webkit-details-marker { display: none; }
      .qc-priority-help[open] { position: relative; }
      .qc-priority-help[open] > div { position: absolute; z-index: 200;
        background: #fff; border: 1px solid #dce4e6; border-radius: 6px;
        padding: 10px 14px; min-width: 320px; max-width: 420px;
        box-shadow: 0 4px 12px rgba(0,0,0,.12); }
      .qc-landing-levels dt { margin-top: 8px; font-weight: 600; }
      .qc-landing-levels dd { margin-bottom: 2px; }
      .qc-region-pill { display: inline-flex; align-items: center; gap: 5px;
        background: #e8f0f2; border: 1px solid #aac0c8; border-radius: 12px;
        padding: 2px 8px 2px 10px; font-size: 12px; color: #17363c;
        margin-bottom: 6px; }
      .qc-pill-dismiss { background: none; border: none; cursor: pointer;
        color: #485b62; font-size: 14px; padding: 0 2px; line-height: 1;
        font-weight: 700; }
      .qc-pill-dismiss:hover { color: #c0392b; }
      .leaflet-container { background: #f0f0f0; }
      .qc-map-legend > br + div { clear: both; white-space: nowrap; padding-top: 4px; }
    ')),
    shiny::fluidRow(
      shiny::column(4, card(
        'Records by QC priority',
        help_id = 'landing_priority_definitions',
        plotly::plotlyOutput('landing_priorities', height = '265px'),
        shiny::helpText('Each submitted facility-month is counted once at its assigned review priority.')
      )),
      shiny::column(8, card(
        'QC filter impact',
        shiny::radioButtons('landing_metric', 'Indicator', choices = c(
          'Positivity' = 'positivity', 'Tested' = 'tested', 'Attendance' = 'attendance',
          'Testing coverage' = 'coverage'
        ), selected = 'positivity', inline = TRUE),
        shiny::uiOutput('landing_impact_region_pill'),
        plotly::plotlyOutput('landing_series', height = '265px'),
        shiny::uiOutput('landing_replacement_note'),
        shiny::tags$details(
          shiny::tags$summary('Unresolved imputed counts'),
          DT::DTOutput('landing_replacement_unresolved')
        )
      ))
    ),
    shiny::fluidRow(
      shiny::column(5, card(
        'Proportion of records flagged critical by region',
        leaflet::leafletOutput('landing_region_map', height = '350px')
      )),
      shiny::column(7, card(
        'Flagged records over time',
        shiny::uiOutput('landing_trend_region_pill'),
        plotly::plotlyOutput('ov_trend_plot', height = '295px')
      ))
    )
  )
}

# ---- Priority labels / summary helpers --------------------------------------

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

# ---- Plot: priorities donut -------------------------------------------------

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

# ---- Server -----------------------------------------------------------------

.landing_server <- function(input, output, selected_run, data_r, catalog_r, selected_region,
                            boundaries = NULL) {
  moderate <- qc_scenario_presets()[['moderate']]

  # Reset region selection when the run changes.
  shiny::observeEvent(selected_run(), {
    selected_region(NULL)
  }, ignoreInit = TRUE)

  # Data filtered to the selected region (for QC filter impact).
  filtered_flagged_r <- shiny::reactive({
    .landing_region_data(selected_run()$data_flagged, selected_region())
  })

  applied <- shiny::reactive({
    data <- filtered_flagged_r()
    shiny::validate(shiny::need(nrow(data) > 0L, 'No observations in the selected region.'))
    .impact_apply(data, list(moderate = moderate),
                  .if_null(selected_run()$config$priority, .default_qc_priority()))
  })
  observed <- shiny::reactive(
    .impact_compute(applied(), list(level = 'national', unit = NULL))
  )

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
    labels <- .impact_version_labels
    labels[c('filtered_missing', 'qc_baseline')] <- c('Filtered', 'Imputed')
    .plot_impact_series(series, 'moderate', metric, version_labels = labels)
  })
  output$landing_replacement_note <- shiny::renderUI({
    targets <- replacement()$targets
    filled <- sum(targets$status == 'replaced')
    shiny::helpText(
      'Moderate omits only counts targeted by critical/high priority rules and authorized exclusions. ',
      sprintf('Imputed (QC baseline replacement): %s of %s targeted tested or positive counts filled; %s unresolved.',
              format(filled, big.mark = ','), format(nrow(targets), big.mark = ','),
              format(nrow(targets) - filled, big.mark = ',')),
      ' Uses the saved GAM probability and rolling tested median without fitting new models or uncertainty intervals. ',
      'Pre-existing missing values remain missing; attendance is not replaced. The saved expectations may be ',
      'influenced by suspect observations, and replacements tend to smooth the series.'
    )
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

  # Priority donut.
  output$landing_priorities <- plotly::renderPlotly(
    .plot_landing_priorities(.landing_priority_summary(data_r()))
  )

  # Priority definitions — rendered into the ? popover.
  output$landing_priority_definitions <- shiny::renderUI({
    catalog <- catalog_r()
    meanings <- c(
      critical = paste0('Impossible counts: more positives than tests, negative counts, ',
                        'or counts flagged for authorized exclusion.'),
      high = 'Urgent review of strong or combined signals.',
      medium = paste0('Statistical anomalies — unusual counts relative to modelled ',
                      'expectation or the facility’s own history.'),
      low = 'Minor signals, review when capacity allows.'
    )
    shiny::tags$div(
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
    )
  })

  # --- Choropleth map ---------------------------------------------------------

  sf_with_data_r <- shiny::reactive({
    data <- data_r()
    shares <- .details_unit_share(data, NULL, 'region')
    region <- trimws(as.character(data$region))
    critical <- as.character(data$review_priority) %in% 'critical'
    shares$flagged_records <- vapply(shares$unit, function(unit) {
      sum(critical & region %in% unit)
    }, integer(1))
    shares$share_flagged <- .safe_divide(shares$flagged_records, shares$records)
    .qc_map_data(boundaries, 'region', shares)
  })

  output$landing_region_map <- leaflet::renderLeaflet(
    .plot_qc_map(sf_with_data_r(), 'region', 'flagged critical')
  )

  # Redraw selection highlight via proxy (avoids full map re-render).
  shiny::observeEvent(selected_region(), {
    region <- selected_region()
    proxy <- leaflet::leafletProxy('landing_region_map')
    leaflet::clearGroup(proxy, 'selected_highlight')
    if (!is.null(region) && !is.null(sf_with_data_r())) {
      sel <- sf_with_data_r()[sf_with_data_r()$unit %in% region, ]
      if (nrow(sel) > 0L) {
        leaflet::addPolygons(proxy,
          data = sel, group = 'selected_highlight',
          fillColor = 'transparent', fillOpacity = 0,
          color = '#17363c', weight = 3, options = leaflet::pathOptions(interactive = FALSE)
        )
      }
    }
  }, ignoreNULL = FALSE)

  # Map click -> update selected region.
  shiny::observeEvent(input$landing_region_map_shape_click, {
    click <- input$landing_region_map_shape_click
    if (!is.null(click$id)) selected_region(click$id)
  })

  # --- Region filter pills ----------------------------------------------------

  .region_pill_ui <- function() {
    region <- selected_region()
    if (is.null(region)) return(NULL)
    shiny::tags$div(
      class = 'qc-region-pill',
      shiny::tags$span(region),
      shiny::tags$button(
        class = 'qc-pill-dismiss',
        onclick = "Shiny.setInputValue('landing_clear_region', Math.random(), {priority: 'event'})",
        '×'
      )
    )
  }
  output$landing_impact_region_pill <- shiny::renderUI(.region_pill_ui())
  output$landing_trend_region_pill  <- shiny::renderUI(.region_pill_ui())

  shiny::observeEvent(input$landing_clear_region, {
    selected_region(NULL)
  })
}
