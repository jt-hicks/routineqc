# Shiny wiring for the redesigned dashboard. The calculations live in the
# app_*.R helper files; this file only connects them to inputs and outputs.

.dashboard_ui <- function(legacy) {
  nested <- function(id, ...) shiny::tabsetPanel(id = id, ...)
  shiny::tabsetPanel(
    id = 'main_view',
    shiny::tabPanel('Overview', .landing_ui()),
    shiny::tabPanel('QC details', .details_ui()),
    shiny::tabPanel(
      'Explore data',
      nested(
        'explore_tabs',
        shiny::tabPanel('Explorer', .explorer_ui()),
        legacy$review_queue, legacy$diagnostics, legacy$district_grid
      )
    ),
    shiny::tabPanel(
      'Filter impact',
      nested('impact_tabs', shiny::tabPanel('Scenarios', .impact_ui()), legacy$consistency)
    ),
    shiny::tabPanel('QC guide', .guide_ui()),
    shiny::tabPanel('Run details', nested(
      'details_tabs', legacy$configuration, legacy$attendance,
      shiny::tabPanel(
        'Run and artifacts',
        shiny::verbatimTextOutput('run_details'),
        shiny::h4('Artifact inventory'), shiny::tableOutput('artifact_inventory')
      )
    ))
  )
}

.explorer_ui <- function() {
  shiny::tagList(
    shiny::fluidRow(
      shiny::column(3, shiny::selectInput('ex_region', 'Region', choices = '')),
      shiny::column(3, shiny::selectInput('ex_mid', 'Council or district', choices = '')),
      shiny::column(3, shiny::selectizeInput('ex_facility', 'Facility', choices = '')),
      shiny::column(3, shiny::br(), shiny::actionButton('ex_reset', 'Reset to national'))
    ),
    shiny::strong(shiny::textOutput('ex_breadcrumb', inline = TRUE)),
    shiny::fluidRow(
      shiny::column(
        6,
        shiny::radioButtons('ex_metric', 'Measure', choices = .explore_metric_labels(),
                            selected = 'positivity', inline = TRUE)
      ),
      shiny::column(3, shiny::checkboxInput('ex_panels', 'Show all four measures together', FALSE)),
      shiny::column(3, shiny::checkboxInput('ex_points', 'Mark flagged records', TRUE))
    ),
    shiny::uiOutput('ex_chart_ui'),
    shiny::helpText(
      'Gaps are months with no valid value, not zeros. Select a facility to mark its flagged records, ',
      'then click a marked point to see why it was flagged; the table below lists flagged records for any selection.'
    ),
    plotly::plotlyOutput('ex_strip', height = '150px'),
    shiny::h4(shiny::textOutput('ex_children_title', inline = TRUE)),
    shiny::helpText('Select a row to move down a level.'),
    DT::DTOutput('ex_children'),
    shiny::h4('Records to review in this selection'),
    DT::DTOutput('ex_queue'),
    shiny::h4('Selected record'),
    shiny::fluidRow(
      shiny::column(5, shiny::tableOutput('ex_record')),
      shiny::column(7, shiny::uiOutput('ex_explanation'))
    )
  )
}

.impact_ui <- function() {
  shiny::tagList(
    shiny::p(
      'What would change in the aggregate estimates if stricter QC decisions were applied? Each scenario is ',
      'hypothetical and is applied to a copy of the data: no observation, flag or stored action is changed. ',
      'Positivity is always total positives over total tested for records where both counts are usable, never ',
      'an average of facility percentages. The geography follows the selection in Explore data. ',
      'All facilities are included regardless of reporting history. Each scenario omits only affected counts ',
      'in flagged facility-months according to its priority and authorized-exclusion rules.'
    ),
    shiny::strong(shiny::textOutput('fi_geography', inline = TRUE)),
    shiny::fluidRow(
      shiny::column(
        4,
        shiny::selectInput('fi_scenario', 'Scenario to show over time',
                           choices = names(qc_scenario_presets()), selected = 'moderate'),
        shiny::textOutput('fi_rules')
      ),
      shiny::column(
        4,
        shiny::radioButtons('fi_metric', 'Measure', choices = .explore_metric_labels(),
                            selected = 'positivity')
      ),
      shiny::column(
        4,
        shiny::tags$details(
          shiny::tags$summary(shiny::tags$strong('Advanced: imputation (exploratory)')),
          shiny::helpText(
            'Excluded values are left missing by default. Imputation replaces them with model-based ',
            'values and is shown separately, with its own uncertainty. Models are fitted to retained ',
            'observations only. This can be slow on large runs.'
          ),
          shiny::radioButtons(
            'fi_method', 'Method',
            choices = c('Repeated predictive imputation, with uncertainty' = 'multiple',
                        'Expected values (exploratory, no uncertainty)' = 'expected')
          ),
          shiny::radioButtons(
            'fi_mode', 'Information used',
            choices = c('Retrospective: all retained data' = 'retrospective',
                        'Operational: data up to each month only' = 'operational')
          ),
          shiny::numericInput('fi_n', 'Imputations', value = 10, min = 2, max = 100),
          shiny::numericInput('fi_seed', 'Random seed', value = 1, min = 1, step = 1),
          shiny::actionButton('fi_run_imputation', 'Run imputation for this scenario')
        )
      )
    ),
    shiny::uiOutput('fi_imputation_note'),
    plotly::plotlyOutput('fi_series', height = '380px'),
    shiny::fluidRow(
      shiny::column(6, plotly::plotlyOutput('fi_strictness', height = '320px')),
      shiny::column(6, plotly::plotlyOutput('fi_change', height = '320px'))
    ),
    shiny::fluidRow(
      shiny::column(6, plotly::plotlyOutput('fi_retention', height = '300px')),
      shiny::column(6, shiny::uiOutput('fi_units_ui'))
    ),
    shiny::h4('What each scenario keeps and removes'),
    shiny::helpText(
      'Omitted counts are individual tested or positive values taken out of the calculation. ',
      'Missing before QC are gaps that existed in the data and are unaffected by QC.'
    ),
    DT::DTOutput('fi_table'),
    shiny::tags$details(
      shiny::tags$summary(shiny::tags$strong('Scenario rules')),
      shiny::uiOutput('fi_rule_list')
    ),
    shiny::helpText(
      'Differences between scenarios are scenario sensitivity. The band on an imputed series is ',
      'imputation uncertainty within one scenario. Neither is evidence about which scenario is closest ',
      'to the truth.'
    )
  )
}

.guide_ui <- function() {
  shiny::tagList(
    shiny::p('Every flag in plain language, with the thresholds used by the selected run.'),
    shiny::uiOutput('gd_defaulted'),
    shiny::fluidRow(
      shiny::column(5, shiny::textInput('gd_search', 'Search', placeholder = 'for example residual, coverage')),
      shiny::column(4, shiny::selectInput(
        'gd_domain', 'Issue family',
        choices = c('All', 'Count consistency', 'Prevalence', 'Tested volume', 'Attendance', 'Testing coverage')
      ))
    ),
    DT::DTOutput('gd_catalog'),
    shiny::h4('Context modifiers'),
    shiny::tableOutput('gd_modifiers'),
    shiny::h4('Flags, priorities and actions'),
    shiny::uiOutput('gd_text'),
    .qc_flow_diagram_ui()
  )
}

.dashboard_sync_inputs <- function(session, data, sel) {
  ch <- .nav_choices(data, sel)
  mid_label <- .if_null(.nav_mid_label(.nav_mid_field(data)), 'Council or district')
  shiny::updateSelectInput(session, 'ex_region', choices = c('All regions' = '', ch$region),
                           selected = .if_null(sel$region, ''))
  shiny::updateSelectInput(session, 'ex_mid', label = mid_label,
                           choices = stats::setNames(c('', ch$mid), c(paste0('All ', tolower(mid_label), 's'), ch$mid)),
                           selected = .if_null(sel$mid, ''))
  # Server-side selectize clears the current value while asynchronously loading
  # options, feeding a transient empty selection back into the navigation.
  shiny::updateSelectizeInput(session, 'ex_facility',
                              choices = c('All facilities' = '', ch$facility),
                              selected = .if_null(sel$facility, ''), server = FALSE)
}

.dashboard_server <- function(input, output, session, selected_run) {
  data_r <- shiny::reactive(.explore_prepare(selected_run()$data_flagged))
  catalog_r <- shiny::reactive(.qc_flag_catalog(selected_run()$config))
  selection <- shiny::reactiveVal(.nav_empty())
  record <- shiny::reactiveVal(NULL)

  sync_inputs <- function(data, sel) .dashboard_sync_inputs(session, data, sel)
  move <- function(level, value) {
    data <- data_r()
    current <- selection()
    value <- .nav_clean(value)
    wanted <- if (length(value) == 0L) NULL else value[[1]]
    if (identical(.nav_normalise(current)[[level]], wanted)) return(invisible())
    updated <- .nav_set(data, current, level, wanted)
    selection(updated)
    record(NULL)
    sync_inputs(data, updated)
  }

  shiny::observeEvent(selected_run(), {
    selection(.nav_empty())
    record(NULL)
    sync_inputs(data_r(), .nav_empty())
  }, ignoreInit = FALSE)
  shiny::observeEvent(input$ex_region, move('region', input$ex_region), ignoreInit = TRUE)
  shiny::observeEvent(input$ex_mid, move('mid', input$ex_mid), ignoreInit = TRUE)
  shiny::observeEvent(input$ex_facility, move('facility', input$ex_facility), ignoreInit = TRUE)
  shiny::observeEvent(input$ex_reset, {
    selection(.nav_empty())
    record(NULL)
    sync_inputs(data_r(), .nav_empty())
  })

  sub_r <- shiny::reactive(.nav_subset(data_r(), selection()))
  series_r <- shiny::reactive(.explore_series(data_r(), selection()))
  children_r <- shiny::reactive(.explore_children(data_r(), selection()))
  mid_field_r <- shiny::reactive(.nav_mid_field(data_r()))

  output$ex_breadcrumb <- shiny::renderText(.nav_breadcrumb(selection(), mid_field_r()))

  points_for <- function(metric) {
    if (!isTRUE(input$ex_points) || is.null(.nav_normalise(selection())$facility)) return(NULL)
    .explore_flagged_points(data_r(), selection(), metric)
  }
  output$ex_chart_ui <- shiny::renderUI({
    if (isTRUE(input$ex_panels)) plotly::plotlyOutput('ex_panels_plot', height = '640px')
    else plotly::plotlyOutput('ex_plot', height = '420px')
  })
  output$ex_plot <- plotly::renderPlotly({
    metric <- .value_or(input$ex_metric, 'positivity')
    p <- .plot_explore_series(series_r(), metric, points_for(metric))
    plotly::event_register(p, 'plotly_click')
  })
  output$ex_panels_plot <- plotly::renderPlotly({
    pts <- stats::setNames(lapply(names(.explore_metrics), points_for), names(.explore_metrics))
    .plot_explore_panels(series_r(), pts)
  })
  output$ex_strip <- plotly::renderPlotly({
    sel <- .nav_normalise(selection())
    states <- if (!is.null(sel$facility)) .explore_facility_states(data_r(), sel$facility) else NULL
    .plot_explore_strip(series_r(), states)
  })

  output$ex_children_title <- shiny::renderText({
    child <- .nav_child_level(.nav_normalise(selection()), mid_field_r())
    if (is.na(child)) 'Facility' else paste('Within this selection:', switch(
      child, region = 'regions', mid = tolower(paste0(.nav_mid_label(mid_field_r()), 's')),
      facility = 'facilities'))
  })
  output$ex_children <- DT::renderDT({
    ch <- children_r()
    shiny::validate(shiny::need(!is.null(ch), 'Facility selected: there is no lower level.'))
    show <- dplyr::transmute(
      ch, Unit = unit, Facilities = facilities, `Positivity` = positivity, Tested = tested,
      Coverage = coverage, `Review rows` = review_rows, `High priority rows` = high_priority_rows,
      `Share of records flagged` = share_flagged
    )
    DT::datatable(show, rownames = FALSE, selection = 'single', options = list(pageLength = 10)) %>%
      DT::formatPercentage(c('Positivity', 'Coverage', 'Share of records flagged'), 1) %>%
      DT::formatRound('Tested', 0)
  })
  shiny::observeEvent(input$ex_children_rows_selected, {
    ch <- children_r()
    i <- input$ex_children_rows_selected
    if (is.null(ch) || length(i) != 1L) return()
    level <- attr(ch, 'child_level')
    move(level, ch$unit[[i]])
  })

  queue_r <- shiny::reactive({
    sub <- sub_r()
    sub <- sub[.is_flagged_row(sub), , drop = FALSE]
    rank <- match(as.character(sub$review_priority), c('critical', 'high', 'medium', 'low'), nomatch = 5L)
    sub[order(rank, sub$facility_id, sub$month_date), , drop = FALSE]
  })
  output$ex_queue <- DT::renderDT({
    q <- queue_r()
    cols <- intersect(c('facility_id', 'month_date', 'review_priority', 'qc_action', 'qc_reason', 'tested',
                        'positive', 'attending'), names(q))
    DT::datatable(q[cols], rownames = FALSE, selection = 'single', options = list(pageLength = 10))
  })
  shiny::observeEvent(input$ex_queue_rows_selected, {
    i <- input$ex_queue_rows_selected
    q <- queue_r()
    if (length(i) == 1L && i <= nrow(q)) record(q$.row[[i]])
  })
  shiny::observeEvent(plotly::event_data('plotly_click', source = 'explore'), {
    click <- plotly::event_data('plotly_click', source = 'explore')
    key <- suppressWarnings(as.integer(click$key[[1]]))
    if (length(key) == 1L && !is.na(key)) record(key)
  })

  record_row <- shiny::reactive({
    r <- record()
    if (is.null(r)) return(NULL)
    row <- data_r()[data_r()$.row == r, , drop = FALSE]
    if (nrow(row) == 1L) row else NULL
  })
  output$ex_record <- shiny::renderTable({
    row <- record_row()
    shiny::validate(shiny::need(!is.null(row), 'Click a marked point or select a record above.'))
    .record_details_table(row)
  }, colnames = FALSE)
  output$ex_explanation <- shiny::renderUI({
    row <- record_row()
    if (is.null(row)) return(NULL)
    shiny::tagList(lapply(.explain_record(row, catalog_r()), shiny::p))
  })

  # Overview
  selected_landing_region <- shiny::reactiveVal(NULL)
  output$ov_trend_plot <- plotly::renderPlotly({
    data <- data_r()
    region <- selected_landing_region()
    if (!is.null(region)) {
      data <- data[trimws(as.character(data$region)) == region, , drop = FALSE]
    }
    .plot_overview_trend(.overview_trend(data))
  })
  .landing_server(input, output, selected_run, data_r, catalog_r, selected_landing_region)

  # QC details. A row in its ranked table opens that unit in Explore data.
  open_unit <- function(field, unit) {
    level <- if (identical(field, 'region')) 'region' else 'mid'
    data <- data_r()
    updated <- .nav_set(data, .nav_empty(), level, unit)
    selection(updated)
    record(NULL)
    sync_inputs(data, updated)
    shiny::updateTabsetPanel(session, 'explore_tabs', selected = 'Explorer')
    shiny::updateTabsetPanel(session, 'main_view', selected = 'Explore data')
  }
  .details_server(input, output, session, selected_run, data_r, catalog_r, open_unit)

  # Filter impact
  geography_r <- shiny::reactive(.impact_geography(selection(), data_r()))
  output$fi_geography <- shiny::renderText(paste('Geography:', geography_r()$label))
  priority_r <- shiny::reactive(.if_null(selected_run()$config$priority, .default_qc_priority()))
  applied_r <- shiny::reactive({
    .impact_apply(selected_run()$data_flagged, .impact_scenarios(), priority_r())
  })
  imputed <- shiny::reactiveVal(NULL)
  shiny::observeEvent(selected_run(), imputed(NULL))
  shiny::observeEvent(input$fi_run_imputation, {
    scenario <- .impact_scenarios()[[input$fi_scenario]]
    result <- tryCatch(
      shiny::withProgress(message = 'Fitting imputation models', value = 0.3, {
        qc_impute_scenario(selected_run()$data_flagged, scenario, mode = input$fi_mode,
                           method = input$fi_method, n_imputations = as.integer(input$fi_n),
                           seed = as.integer(input$fi_seed), priority = priority_r())
      }),
      error = function(e) e
    )
    if (inherits(result, 'error')) {
      shiny::showNotification(paste('Imputation failed:', conditionMessage(result)), type = 'error')
      return()
    }
    imputed(stats::setNames(list(result), input$fi_scenario))
  })
  imputed_now <- shiny::reactive({
    x <- imputed()
    if (is.null(x) || !input$fi_scenario %in% names(x)) NULL else x
  })
  results_r <- shiny::reactive(.impact_compute(applied_r(), geography_r(), TRUE, imputed_now()))
  summary_r <- shiny::reactive(.impact_summary(applied_r(), geography_r(), imputed_now()))
  output$fi_rules <- shiny::renderText(.describe_scenario_rules(.impact_scenarios()[[input$fi_scenario]]))
  output$fi_imputation_note <- shiny::renderUI({
    x <- imputed_now()
    if (is.null(x)) {
      return(shiny::helpText('Showing excluded values left missing. Imputation has not been run.'))
    }
    pv <- x[[1]]$provenance
    shiny::tags$div(
      class = 'alert alert-warning',
      paste0('Imputed series shown for the ', input$fi_scenario, ' scenario: ', pv$method, ', ', pv$mode,
             ', seed ', pv$seed, if (pv$method == 'expected') '. Exploratory: no uncertainty is available.' else
               paste0(', ', pv$n_imputations, ' imputations.'))
    )
  })
  output$fi_series <- plotly::renderPlotly({
    .plot_impact_series(results_r(), input$fi_scenario, .value_or(input$fi_metric, 'positivity'))
  })
  output$fi_strictness <- plotly::renderPlotly(.plot_impact_strictness(summary_r()))
  output$fi_change <- plotly::renderPlotly(.plot_impact_change(summary_r()))
  output$fi_retention <- plotly::renderPlotly(.plot_impact_retention(summary_r()))
  output$fi_units_ui <- shiny::renderUI({
    shiny::tagList(
      shiny::h5(paste('Units with the largest change under', input$fi_scenario)),
      plotly::plotlyOutput('fi_units', height = '280px')
    )
  })
  output$fi_units <- plotly::renderPlotly({
    .plot_impact_units(.impact_unit_changes(applied_r(), input$fi_scenario))
  })
  output$fi_table <- DT::renderDT({
    s <- summary_r()
    s <- s[, setdiff(names(s), c('facilities_eligible', 'removed_with_facility')), drop = FALSE]
    DT::datatable(s, rownames = FALSE, options = list(scrollX = TRUE, dom = 't')) %>%
      DT::formatPercentage(intersect(c('positivity_reported', 'positivity_filtered', 'positivity_imputed',
                                       'share_tested_retained', 'ci_low', 'ci_high',
                                       'share_tested_imputed'), names(s)), 2) %>%
      DT::formatRound(intersect(c('change_pp', 'change_imputed_pp'), names(s)), 2)
  })
  output$fi_rule_list <- shiny::renderUI({
    scenarios <- .impact_scenarios()
    shiny::tags$ul(lapply(names(scenarios), function(nm) {
      shiny::tags$li(shiny::strong(nm), ': ', .describe_scenario_rules(scenarios[[nm]]))
    }))
  })

  # QC guide
  output$gd_defaulted <- shiny::renderUI({
    d <- attr(catalog_r(), 'defaulted_sections')
    if (length(d) == 0L) return(NULL)
    shiny::tags$div(class = 'alert alert-warning', paste(
      'This run was saved without settings for:', paste(d, collapse = ', '),
      '. Defaults for the current policy are shown for those sections.'))
  })
  output$gd_catalog <- DT::renderDT({
    cat <- catalog_r()
    if (!identical(input$gd_domain, 'All') && !is.null(input$gd_domain)) {
      cat <- cat[as.character(cat$domain) == input$gd_domain, , drop = FALSE]
    }
    term <- trimws(.value_or(input$gd_search, ''))
    if (nzchar(term)) {
      hay <- tolower(paste(cat$label, cat$definition, cat$domain, cat$flag))
      cat <- cat[grepl(tolower(term), hay, fixed = TRUE), , drop = FALSE]
    }
    show <- dplyr::transmute(
      cat, Flag = label, `Issue family` = as.character(domain), Definition = definition,
      Priority = priority, `Counts in doubt` = counts_in_doubt,
      `Authorizes exclusion` = ifelse(authorizes_exclusion, 'yes', 'no'), Needs = needs
    )
    DT::datatable(show, rownames = FALSE, options = list(pageLength = 15))
  })
  output$gd_modifiers <- shiny::renderTable(.qc_modifier_notes()[c('label', 'definition', 'effect')])
  output$gd_text <- shiny::renderUI({
    txt <- .qc_flag_vs_action_text()
    shiny::tags$ul(lapply(names(txt), function(k) shiny::tags$li(shiny::strong(paste0(k, ': ')), txt[[k]])))
  })

  invisible(list(selection = selection, record = record))
}
