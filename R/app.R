.manifest_value <- function(manifest, name, default = NA_character_) {
  value <- manifest[[name]]
  if (is.null(value) || length(value) == 0L) return(default)
  as.character(value[[1]])
}

#' List Persisted QC Runs
#'
#' Discovers QC run RDS/JSON pairs in one directory using metadata-only JSON
#' reads. Complete artifacts are not loaded until selected with [read_qc_run()].
#'
#' @param run_dir Directory containing files written by [write_qc_run()].
#'
#' @return A tibble with one row per complete or incomplete artifact stem,
#'   including selection readiness and safe manifest metadata.
#' @export
list_qc_runs <- function(run_dir) {
  if (!is.character(run_dir) || length(run_dir) != 1L || is.na(run_dir) ||
      !dir.exists(run_dir)) {
    rlang::abort('`run_dir` must be one existing directory.')
  }
  run_dir <- normalizePath(run_dir, winslash = '/', mustWork = TRUE)
  rds <- list.files(run_dir, pattern = '\\.rds$', full.names = FALSE, ignore.case = TRUE)
  manifests <- list.files(
    run_dir, pattern = '-manifest\\.json$', full.names = FALSE, ignore.case = TRUE
  )
  stems <- sort(unique(c(
    sub('\\.rds$', '', rds, ignore.case = TRUE),
    sub('-manifest\\.json$', '', manifests, ignore.case = TRUE)
  )))
  if (length(stems) == 0L) {
    return(dplyr::tibble(
      run_name = character(), path = character(), manifest_path = character(),
      artifact_status = character(), selectable = logical(), dataset_id = character(),
      created_at_utc = character(), config_profile = character(),
      analysis_id = character(), execution_id = character(),
      model_prediction_coverage = double()
    ))
  }

  rows <- lapply(stems, function(stem) {
    path <- file.path(run_dir, paste0(stem, '.rds'))
    manifest_path <- file.path(run_dir, paste0(stem, '-manifest.json'))
    has_rds <- file.exists(path)
    has_manifest <- file.exists(manifest_path)
    manifest <- NULL
    parse_error <- FALSE
    if (has_manifest) {
      manifest <- tryCatch(
        jsonlite::read_json(manifest_path, simplifyVector = TRUE),
        error = function(e) {
          parse_error <<- TRUE
          NULL
        }
      )
    }
    supported <- !is.null(manifest) &&
      identical(as.integer(manifest$run_schema_version), .qc_run_schema_version) &&
      identical(as.integer(manifest$manifest_schema_version), .qc_manifest_schema_version)
    status <- if (!has_rds) {
      'missing_rds'
    } else if (!has_manifest) {
      'missing_manifest'
    } else if (parse_error) {
      'invalid_manifest'
    } else if (!supported) {
      'unsupported_schema'
    } else {
      'available'
    }
    provenance <- if (is.list(manifest$provenance)) manifest$provenance else list()
    dplyr::tibble(
      run_name = stem,
      path = normalizePath(path, winslash = '/', mustWork = FALSE),
      manifest_path = normalizePath(manifest_path, winslash = '/', mustWork = FALSE),
      artifact_status = status,
      selectable = identical(status, 'available'),
      dataset_id = .manifest_value(provenance, 'dataset_id'),
      created_at_utc = .manifest_value(manifest, 'created_at_utc'),
      config_profile = .manifest_value(manifest, 'config_profile'),
      analysis_id = .manifest_value(manifest, 'analysis_id'),
      execution_id = .manifest_value(manifest, 'execution_id'),
      model_prediction_coverage = suppressWarnings(as.numeric(
        .manifest_value(manifest, 'model_prediction_coverage', NA_real_)
      ))
    )
  })
  dplyr::bind_rows(rows)
}

.filter_if_selected <- function(data, column, selected) {
  if (is.null(selected) || length(selected) == 0L || !column %in% names(data)) return(data)
  data[as.character(data[[column]]) %in% as.character(selected), , drop = FALSE]
}

#' Filter a QC Review Queue
#'
#' Applies display-only filters to flagged QC data. It never changes the stored
#' run or authorizes new actions.
#'
#' @param data Flagged data from a `routineqc_run`.
#' @param actions Optional `qc_action` values.
#' @param priorities Optional `review_priority` values.
#' @param regions Optional region values.
#' @param facilities Optional facility identifiers.
#' @param reasons Optional `qc_reason` values.
#' @param prediction_statuses Optional model prediction statuses.
#' @param date_range Optional two-element inclusive Date range.
#' @param flagged_only Whether to retain only exclusion-authorized or
#'   review-recommended rows.
#'
#' @return A filtered tibble preserving source columns and row order.
#' @export
filter_qc_review <- function(data,
                             actions = NULL,
                             priorities = NULL,
                             regions = NULL,
                             facilities = NULL,
                             reasons = NULL,
                             prediction_statuses = NULL,
                             date_range = NULL,
                             flagged_only = TRUE) {
  out <- dplyr::as_tibble(data)
  .validate_required_columns(
    out, c('facility_id', 'month_date', 'qc_action', 'flag_exclude_authorized',
           'flag_review_recommended')
  )
  if (!rlang::is_bool(flagged_only)) {
    rlang::abort('`flagged_only` must be TRUE or FALSE.')
  }
  if (flagged_only) {
    keep <- out$flag_exclude_authorized | out$flag_review_recommended
    out <- out[dplyr::coalesce(keep, FALSE), , drop = FALSE]
  }
  out <- .filter_if_selected(out, 'qc_action', actions)
  out <- .filter_if_selected(out, 'review_priority', priorities)
  out <- .filter_if_selected(out, 'region', regions)
  out <- .filter_if_selected(out, 'facility_id', facilities)
  out <- .filter_if_selected(out, 'qc_reason', reasons)
  out <- .filter_if_selected(out, 'prediction_status', prediction_statuses)
  if (!is.null(date_range)) {
    if (length(date_range) != 2L || any(is.na(date_range))) {
      rlang::abort('`date_range` must contain two non-missing dates.')
    }
    date_range <- as.Date(date_range)
    if (date_range[1] > date_range[2]) {
      rlang::abort('`date_range` must be ordered from earliest to latest.')
    }
    out <- out[out$month_date >= date_range[1] & out$month_date <= date_range[2], , drop = FALSE]
  }
  dplyr::as_tibble(out)
}

.review_column_definitions <- function() {
  c(
    source_record_id = 'Stable source-row identifier supplied by the adapter.',
    facility_id = 'Stable facility identifier used for grouping and model effects.',
    facility_name = 'Human-readable facility name, when supplied.',
    month_date = 'Reporting month represented by its first calendar day.',
    region = 'Canonical top-level geographic grouping.',
    district = 'Optional canonical district.',
    council = 'Optional canonical council.',
    tested = 'Reported number tested in this facility-month.',
    positive = 'Reported number testing positive in this facility-month.',
    attending = 'Optional approved attendance denominator.',
    prevalence = 'Observed positive divided by tested; NA when tested is zero or missing.',
    p_hat = 'Expected prevalence from the fitted GAM; NA when the row was not assessed.',
    expected_positive = 'Expected positive count: tested multiplied by model prevalence.',
    pearson_resid = 'Pearson residual comparing observed and model-expected positives.',
    prediction_status = 'Reason the row was assessed or not assessed by the model.',
    model_assessment_eligible = 'Whether the row has valid counts and complete model predictors.',
    model_assessed = 'Whether a finite prevalence prediction was obtained.',
    tested_roll_median = 'Calendar-aware rolling median used as the tested-volume baseline.',
    tested_robust_z = 'Robust deviation of tested volume from its rolling baseline.',
    qc_action = 'Action assigned by the selected versioned action policy.',
    review_priority = 'Relative priority for human review.',
    qc_reason = 'Machine-readable reasons contributing to the assigned QC action.',
    flag_exclude_authorized = 'Whether the active policy authorizes exclusion.',
    flag_review_recommended = 'Whether human review is recommended.',
    flag_core_invalid = 'Impossible or missing core tested/positive counts.',
    flag_resid_extreme = 'Model residual exceeds the configured size-dependent threshold.',
    flag_tested_volume_extreme = 'Tested volume has corroborated evidence of an extreme value.'
  )
}

.review_column_spec <- function(data, column_set = 'review') {
  sets <- list(
    review = c(
      'source_record_id', 'facility_id', 'facility_name', 'month_date', 'region',
      'tested', 'positive', 'prevalence', 'qc_action', 'review_priority', 'qc_reason'
    ),
    counts = c(
      'source_record_id', 'facility_id', 'month_date', 'region', 'district',
      'council', 'tested', 'positive', 'attending', 'prevalence',
      'flag_core_invalid', 'flag_exclude_authorized', 'flag_review_recommended'
    ),
    model = c(
      'source_record_id', 'facility_id', 'month_date', 'region', 'prevalence',
      'p_hat', 'expected_positive', 'pearson_resid', 'prediction_status',
      'model_assessment_eligible', 'model_assessed', 'flag_resid_extreme'
    ),
    volume = c(
      'source_record_id', 'facility_id', 'month_date', 'region', 'tested',
      'tested_roll_median', 'tested_robust_z', 'flag_tested_volume_extreme',
      'qc_action', 'review_priority', 'qc_reason'
    ),
    all = names(data)
  )
  if (!is.character(column_set) || length(column_set) != 1L ||
      !column_set %in% names(sets)) {
    rlang::abort('Unknown review queue column set.')
  }
  fields <- intersect(sets[[column_set]], names(data))
  labels <- stats::setNames(gsub('_', ' ', fields, fixed = TRUE), fields)
  definitions <- .review_column_definitions()
  definitions <- vapply(fields, function(field) {
    if (field %in% names(definitions)) definitions[[field]] else {
      paste0('Package output field: ', field, '.')
    }
  }, character(1))
  list(fields = fields, labels = unname(labels), definitions = unname(definitions))
}

.qc_app_ui <- function() {
  shiny::fluidPage(
    shiny::titlePanel('routineqc Run Explorer'),
    shiny::p('Read-only local review of validated persisted QC runs.'),
    shiny::uiOutput('run_selector'),
    shiny::tabsetPanel(
      shiny::tabPanel(
        'Overview', shiny::tableOutput('overview'),
        shiny::h4('Artifact inventory'), shiny::tableOutput('artifact_inventory')
      ),
      shiny::tabPanel(
        'Review queue',
        shiny::fluidRow(
          shiny::column(3, shiny::checkboxInput('flagged_only', 'Flagged/review rows only', TRUE)),
          shiny::column(3, shiny::selectInput('action', 'Action', choices = NULL, multiple = TRUE)),
          shiny::column(3, shiny::selectInput('priority', 'Priority', choices = NULL, multiple = TRUE)),
          shiny::column(3, shiny::selectInput('region', 'Region', choices = NULL, multiple = TRUE))
        ),
        shiny::fluidRow(
          shiny::column(3, shiny::selectInput('facility', 'Facility', choices = NULL, multiple = TRUE)),
          shiny::column(3, shiny::selectInput('reason', 'QC reason', choices = NULL, multiple = TRUE)),
          shiny::column(3, shiny::selectInput('prediction_status', 'Prediction status', choices = NULL, multiple = TRUE)),
          shiny::column(3, shiny::dateRangeInput('date_range', 'Reporting months'))
        ),
        shiny::fluidRow(
          shiny::column(
            4,
            shiny::selectInput(
              'column_set', 'Columns',
              choices = c(
                'Review essentials' = 'review', 'Counts and context' = 'counts',
                'Model assessment' = 'model', 'Tested-volume assessment' = 'volume',
                'All available fields' = 'all'
              ),
              selected = 'review'
            )
          ),
          shiny::column(8, shiny::helpText('Hover over a column heading for its definition.'))
        ),
        DT::DTOutput('review_table')
      ),
      shiny::tabPanel(
        'Diagnostics',
        shiny::selectInput('plot_facility', 'Facility time series', choices = NULL),
        shiny::plotOutput('facility_plot'),
        shiny::fluidRow(
          shiny::column(6, shiny::plotOutput('residual_plot')),
          shiny::column(6, shiny::plotOutput('tested_plot'))
        ),
        shiny::fluidRow(
          shiny::column(6, shiny::plotOutput('region_plot')),
          shiny::column(6, shiny::plotOutput('before_after_plot'))
        )
      ),
      shiny::tabPanel(
        'Configuration and provenance',
        shiny::h4('Configuration'), shiny::verbatimTextOutput('configuration'),
        shiny::h4('Safe provenance'), shiny::verbatimTextOutput('provenance')
      )
    )
  )
}

.qc_app_server <- function(input, output, session, run_dir) {
  run_index <- list_qc_runs(run_dir)
  available <- run_index[run_index$selectable, , drop = FALSE]
  labels <- if (nrow(available) > 0L) {
    label <- ifelse(
      is.na(available$dataset_id), available$run_name,
      paste0(available$dataset_id, ' - ', available$created_at_utc)
    )
    stats::setNames(available$path, label)
  } else {
    character()
  }

  output$run_selector <- shiny::renderUI({
    if (length(labels) == 0L) {
      return(shiny::div(class = 'alert alert-warning', 'No selectable QC run pairs were found.'))
    }
    shiny::selectInput('run_path', 'QC run', choices = labels, selected = labels[[1]])
  })
  output$artifact_inventory <- shiny::renderTable({
    run_index[c('run_name', 'artifact_status', 'dataset_id', 'created_at_utc')]
  })

  loaded <- shiny::reactive({
    shiny::req(input$run_path)
    tryCatch(
      list(ok = TRUE, run = read_qc_run(input$run_path), message = NULL),
      error = function(e) list(ok = FALSE, run = NULL, message = conditionMessage(e))
    )
  })
  selected_run <- shiny::reactive({
    value <- loaded()
    shiny::validate(shiny::need(value$ok, paste('Run validation failed:', value$message)))
    value$run
  })

  shiny::observeEvent(selected_run(), {
    data <- selected_run()$data_flagged
    choices <- function(column) {
      if (!column %in% names(data)) return(character())
      sort(unique(as.character(data[[column]][!is.na(data[[column]])])))
    }
    shiny::updateSelectInput(session, 'action', choices = choices('qc_action'))
    shiny::updateSelectInput(session, 'priority', choices = choices('review_priority'))
    shiny::updateSelectInput(session, 'region', choices = choices('region'))
    shiny::updateSelectInput(session, 'facility', choices = choices('facility_id'))
    shiny::updateSelectInput(session, 'reason', choices = choices('qc_reason'))
    shiny::updateSelectInput(session, 'prediction_status', choices = choices('prediction_status'))
    facilities <- choices('facility_id')
    shiny::updateSelectInput(
      session, 'plot_facility', choices = facilities,
      selected = if (length(facilities) > 0L) facilities[[1]] else character()
    )
    shiny::updateDateRangeInput(
      session, 'date_range',
      start = min(data$month_date), end = max(data$month_date),
      min = min(data$month_date), max = max(data$month_date)
    )
  }, ignoreInit = FALSE)

  review_data <- shiny::reactive({
    data <- selected_run()$data_flagged
    filter_qc_review(
      data,
      actions = input$action, priorities = input$priority, regions = input$region,
      facilities = input$facility, reasons = input$reason,
      prediction_statuses = input$prediction_status,
      date_range = input$date_range, flagged_only = isTRUE(input$flagged_only)
    )
  })

  output$overview <- shiny::renderTable({
    run <- selected_run()
    overall <- summarise_before_after_qc(run$data_flagged, by = 'overall')
    dplyr::tibble(
      metric = c(
        'Dataset ID', 'Rows', 'Facilities', 'Authorized exclusions',
        'Review recommended', 'Model prediction coverage', 'Configuration profile',
        'Action policy', 'Analysis ID', 'Execution ID'
      ),
      value = c(
        .manifest_value(run$manifest$provenance, 'dataset_id'),
        run$manifest$input_rows, run$manifest$input_facilities,
        overall$authorized_exclusion_rows, overall$review_recommended_rows,
        ifelse(is.na(run$manifest$model_prediction_coverage), 'NA',
               sprintf('%.1f%%', 100 * run$manifest$model_prediction_coverage)),
        run$manifest$config_profile, run$manifest$action_policy,
        run$manifest$analysis_id, run$manifest$execution_id
      )
    )
  }, striped = TRUE)
  output$review_table <- DT::renderDT({
    data <- review_data()
    column_set <- if (is.null(input$column_set)) 'review' else input$column_set
    spec <- .review_column_spec(data, column_set)
    display <- data[spec$fields]
    names(display) <- spec$labels
    tooltips <- as.character(jsonlite::toJSON(spec$definitions, auto_unbox = TRUE))
    DT::datatable(
      display,
      extensions = 'FixedColumns',
      options = list(
        pageLength = 25, scrollX = TRUE,
        fixedColumns = list(leftColumns = min(2L, ncol(display))),
        headerCallback = DT::JS(paste0(
          'function(thead) { var tips = ', tooltips, '; ',
          '$(thead).find(th).each(function(i) { this.title = tips[i] || "; }); }'
        ))
      ),
      rownames = FALSE,
      escape = TRUE, selection = 'single'
    )
  })
  output$facility_plot <- shiny::renderPlot({
    shiny::req(input$plot_facility)
    plot_facility_timeseries(selected_run()$data_flagged, input$plot_facility)
  })
  output$residual_plot <- shiny::renderPlot({
    plot_prevalence_residuals(selected_run()$data_flagged)
  })
  output$tested_plot <- shiny::renderPlot({
    plot_tested_robust_z(selected_run()$data_flagged)
  })
  output$region_plot <- shiny::renderPlot({
    plot_qc_summary_by_region(selected_run()$summaries$by_region)
  })
  output$before_after_plot <- shiny::renderPlot({
    plot_before_after_prevalence(selected_run()$data_flagged)
  })
  output$configuration <- shiny::renderPrint({ selected_run()$config })
  output$provenance <- shiny::renderPrint({ selected_run()$manifest$provenance })

  invisible(list(selected_run = selected_run, review_data = review_data))
}

.qc_run_explorer <- function(run_dir) {
  force(run_dir)
  shiny::shinyApp(
    ui = .qc_app_ui(),
    server = function(input, output, session) {
      .qc_app_server(input, output, session, run_dir = run_dir)
    }
  )
}

#' Launch the Local QC Run Explorer
#'
#' Starts a read-only Shiny application for selecting and reviewing validated
#' persisted QC runs. The application binds to localhost and performs no writes.
#'
#' @param run_dir Directory containing paired QC run RDS/JSON artifacts.
#' @param port Optional local port passed to [shiny::runApp()].
#' @param launch_browser Whether to open a browser. Defaults to interactive use.
#'
#' @return The result of [shiny::runApp()]. Called for its side effect.
#' @export
launch_qc_app <- function(run_dir, port = NULL, launch_browser = interactive()) {
  list_qc_runs(run_dir)
  if (!is.null(port) && (!is.numeric(port) || length(port) != 1L ||
                        is.na(port) || port < 1 || port > 65535 ||
                        port != as.integer(port))) {
    rlang::abort('`port` must be NULL or one whole number between 1 and 65535.')
  }
  if (!rlang::is_bool(launch_browser)) {
    rlang::abort('`launch_browser` must be TRUE or FALSE.')
  }
  shiny::runApp(
    .qc_run_explorer(run_dir), host = '127.0.0.1', port = port,
    launch.browser = launch_browser
  )
}
