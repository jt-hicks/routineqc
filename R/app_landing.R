# ---- Helper: dummy shapefile ------------------------------------------------

.dummy_region_sf <- function(regions) {
  n <- length(regions)
  cols <- max(1L, ceiling(sqrt(n)))
  polys <- lapply(seq_along(regions), function(i) {
    row_i <- (i - 1L) %/% cols
    col_i <- (i - 1L) %% cols
    sf::st_polygon(list(matrix(c(
      col_i,       row_i,
      col_i + 0.9, row_i,
      col_i + 0.9, row_i + 0.9,
      col_i,       row_i + 0.9,
      col_i,       row_i
    ), ncol = 2L, byrow = TRUE)))
  })
  sf::st_sf(region = regions, geometry = sf::st_sfc(polys), crs = 4326)
}

.landing_region_shapefile <- function(data) {
  regions <- sort(unique(trimws(as.character(data$region[!is.na(data$region)]))))
  .dummy_region_sf(regions)
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
        plotly::plotlyOutput('landing_series', height = '265px')
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

.landing_server <- function(input, output, selected_run, data_r, catalog_r, selected_region) {
  moderate <- qc_scenario_presets()[['moderate']]

  # Reset region selection when the run changes.
  shiny::observeEvent(selected_run(), {
    selected_region(NULL)
  }, ignoreInit = TRUE)

  # Data filtered to the selected region (for QC filter impact).
  filtered_flagged_r <- shiny::reactive({
    dat <- selected_run()$data_flagged
    region <- selected_region()
    if (!is.null(region)) {
      dat <- dat[trimws(as.character(dat$region)) == region, , drop = FALSE]
    }
    dat
  })

  applied <- shiny::reactive({
    .impact_apply(filtered_flagged_r(), list(moderate = moderate),
                  .if_null(selected_run()$config$priority, .default_qc_priority()))
  })
  observed <- shiny::reactive(
    .impact_compute(applied(), list(level = 'national', unit = NULL))
  )

  # QC filter impact chart — rename filtered trace and add Imputed placeholder.
  output$landing_series <- plotly::renderPlotly({
    metric <- .value_or(input$landing_metric, 'positivity')
    p <- .plot_impact_series(observed(), 'moderate', metric)
    # Rename 'Filtered, excluded left missing' -> 'Filtered' and move footnote to hover.
    for (i in seq_along(p$x$data)) {
      tr <- p$x$data[[i]]
      if (!is.null(tr$name) && grepl('excluded left missing', tr$name, fixed = TRUE)) {
        p$x$data[[i]]$name <- 'Filtered'
        p$x$data[[i]]$text <- paste0(
          tr$text,
          '<br><i>Counts flagged critical/high omitted; excluded left missing</i>'
        )
      }
    }
    # Add Imputed placeholder trace (empty — data not yet computed).
    p <- plotly::add_trace(
      p, x = numeric(0), y = numeric(0), type = 'scatter', mode = 'lines',
      name = 'Imputed', line = list(color = '#e66101', dash = 'dot'),
      hoverinfo = 'skip'
    )
    p
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

  shapefile_r <- shiny::reactive(.landing_region_shapefile(data_r()))

  sf_with_data_r <- shiny::reactive({
    shp <- shapefile_r()
    summary <- .landing_priority_summary(data_r(), by_region = TRUE)
    critical_df <- summary[summary$priority == 'critical', c('region', 'share'), drop = FALSE]
    names(critical_df)[2] <- 'pct_critical'
    critical_df$pct_critical <- critical_df$pct_critical * 100
    shp <- merge(shp, critical_df, by = 'region', all.x = TRUE)
    shp$pct_critical[is.na(shp$pct_critical)] <- 0
    shp
  })

  output$landing_region_map <- leaflet::renderLeaflet({
    sf_data <- sf_with_data_r()
    if (nrow(sf_data) == 0L) return(leaflet::leaflet())
    pal <- leaflet::colorNumeric(
      palette = c('#fff7bc', '#fe9929', '#d95f0e', '#993404'),
      domain = c(0, 100), na.color = '#cccccc'
    )
    bb <- sf::st_bbox(sf_data)
    leaflet::leaflet(sf_data,
      options = leaflet::leafletOptions(zoomControl = TRUE, attributionControl = FALSE)) %>%
      leaflet::fitBounds(bb[['xmin']], bb[['ymin']], bb[['xmax']], bb[['ymax']]) %>%
      leaflet::addPolygons(
        fillColor = ~pal(pct_critical), fillOpacity = 0.8,
        color = '#ffffff', weight = 1.5,
        highlightOptions = leaflet::highlightOptions(
          color = '#17363c', weight = 3, bringToFront = TRUE, fillOpacity = 0.9
        ),
        label = ~paste0(region, ': ', round(pct_critical, 1), '% critical'),
        layerId = ~region
      ) %>%
      leaflet::addLegend(
        pal = pal, values = ~pct_critical, opacity = 0.9,
        title = '% of records', position = 'bottomright',
        labFormat = leaflet::labelFormat(suffix = '%')
      )
  })

  # Redraw selection highlight via proxy (avoids full map re-render).
  shiny::observeEvent(selected_region(), {
    region <- selected_region()
    proxy <- leaflet::leafletProxy('landing_region_map')
    leaflet::clearGroup(proxy, 'selected_highlight')
    if (!is.null(region)) {
      sel <- sf_with_data_r()[sf_with_data_r()$region == region, ]
      if (nrow(sel) > 0L) {
        leaflet::addPolygons(proxy,
          data = sel, group = 'selected_highlight',
          fillColor = 'transparent', fillOpacity = 0,
          color = '#17363c', weight = 3, options = leaflet::pathOptions(interactive = FALSE)
        )
      }
    }
  })

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
