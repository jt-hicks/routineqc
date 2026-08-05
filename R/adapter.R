.qc_adapter_schema_version <- 1L

.adapter_required_fields <- c(
  'facility_id', 'month_date', 'region', 'tested', 'positive'
)
.adapter_optional_fields <- c(
  'district', 'council', 'facility_name', 'attending', 'source_record_id'
)

.validate_adapter_mapping <- function(mapping, source_names = NULL) {
  if (!is.list(mapping) || is.null(names(mapping)) || any(names(mapping) == '') ||
      anyDuplicated(names(mapping))) {
    rlang::abort('`mapping` must be a named list with unique canonical field names.')
  }
  unknown <- setdiff(names(mapping), c(.adapter_required_fields, .adapter_optional_fields))
  missing <- setdiff(.adapter_required_fields, names(mapping))
  if (length(unknown) > 0L) {
    rlang::abort(paste0('Unknown canonical mapping field(s): ', paste(unknown, collapse = ', ')))
  }
  if (length(missing) > 0L) {
    rlang::abort(paste0('Missing required canonical mapping field(s): ', paste(missing, collapse = ', ')))
  }
  valid_value <- vapply(mapping, function(x) {
    is.character(x) && length(x) == 1L && !is.na(x) && nzchar(x)
  }, logical(1))
  if (any(!valid_value)) {
    rlang::abort('Every `mapping` value must be one non-empty source column name.')
  }
  mapped <- unlist(mapping, use.names = FALSE)
  if (anyDuplicated(mapped)) {
    rlang::abort('A source column cannot be mapped to more than one canonical field.')
  }
  if (!is.null(source_names)) {
    absent <- setdiff(mapped, source_names)
    if (length(absent) > 0L) {
      rlang::abort(paste0('Mapped source column(s) not found: ', paste(absent, collapse = ', ')))
    }
  }
  mapping
}

.adapter_diagnostic <- function(rows, source_ids, severity, issue_code, field) {
  if (length(rows) == 0L) return(NULL)
  dplyr::tibble(
    source_row = as.integer(rows),
    source_record_id = as.character(source_ids[rows]),
    severity = severity,
    issue_code = issue_code,
    field = field
  )
}

#' Construct a QC Adapter Result
#'
#' Creates a validated boundary object containing canonical data, translation
#' diagnostics, safe provenance, and the mapping used by an upstream adapter.
#'
#' @param data Canonical data produced by an adapter. Rows must not be dropped.
#' @param diagnostics A data frame with `source_row`, `source_record_id`,
#'   `severity`, `issue_code`, and `field` columns.
#' @param provenance A flat named list of safe dataset and adapter identifiers.
#' @param mapping Named canonical-to-source column mapping.
#'
#' @return An object of class `routineqc_adapter_result`.
#' @export
as_qc_adapter_result <- function(data, diagnostics, provenance, mapping) {
  mapping <- .validate_adapter_mapping(mapping)
  provenance <- .validate_provenance(provenance)
  diagnostics <- dplyr::as_tibble(diagnostics)
  diagnostic_fields <- c(
    'source_row', 'source_record_id', 'severity', 'issue_code', 'field'
  )
  .validate_required_columns(diagnostics, diagnostic_fields)
  ready <- !any(diagnostics$severity == 'error')
  result <- list(
    data = dplyr::as_tibble(data),
    diagnostics = diagnostics,
    provenance = provenance,
    mapping = mapping,
    ready = ready,
    schema_version = .qc_adapter_schema_version
  )
  class(result) <- c('routineqc_adapter_result', 'list')
  validate_qc_adapter_result(result)
  result
}

#' Validate a QC Adapter Result
#'
#' Checks adapter schema, diagnostic consistency, provenance safety, and the
#' canonical input contract when the result is ready for QC.
#'
#' @param result A `routineqc_adapter_result`.
#'
#' @return `result`, invisibly, after successful validation.
#' @export
validate_qc_adapter_result <- function(result) {
  required <- c('data', 'diagnostics', 'provenance', 'mapping', 'ready', 'schema_version')
  if (!inherits(result, 'routineqc_adapter_result') || !is.list(result) ||
      !all(required %in% names(result))) {
    rlang::abort('Object is not a complete `routineqc_adapter_result`.')
  }
  if (!identical(result$schema_version, .qc_adapter_schema_version)) {
    rlang::abort('Unsupported QC adapter schema version.')
  }
  if (!rlang::is_bool(result$ready)) {
    rlang::abort('Adapter readiness must be TRUE or FALSE.')
  }
  .validate_adapter_mapping(result$mapping)
  .validate_provenance(result$provenance)
  diagnostic_fields <- c(
    'source_row', 'source_record_id', 'severity', 'issue_code', 'field'
  )
  .validate_required_columns(result$diagnostics, diagnostic_fields)
  if (any(!result$diagnostics$severity %in% c('error', 'warning'))) {
    rlang::abort('Adapter diagnostic severity must be error or warning.')
  }
  if (nrow(result$diagnostics) > 0L &&
      any(is.na(result$diagnostics$source_row) |
          result$diagnostics$source_row < 1L |
          result$diagnostics$source_row > nrow(result$data))) {
    rlang::abort('Adapter diagnostics contain invalid source row numbers.')
  }
  expected_ready <- !any(result$diagnostics$severity == 'error')
  if (!identical(result$ready, expected_ready)) {
    rlang::abort('Adapter readiness does not agree with its diagnostics.')
  }
  if (result$ready) validate_qc_input(result$data)
  invisible(result)
}

#' Adapt Source Data to the Canonical QC Contract
#'
#' Declaratively maps one source table into canonical routineqc fields without
#' dropping, aggregating, imputing, or repairing source observations.
#'
#' @param data Source data frame.
#' @param mapping Named list mapping canonical fields to source column names.
#' @param identity_strategy Non-empty description of the facility identity used.
#' @param adapter Non-empty adapter name.
#' @param adapter_version Non-empty adapter version.
#' @param dataset_id Non-empty safe identifier for the input dataset version.
#' @param source_system Optional safe source-system identifier.
#' @param attendance_definition Required when `attending` is mapped.
#' @param provenance Additional safe scalar provenance fields.
#'
#' @return A `routineqc_adapter_result`. Error diagnostics block QC handoff;
#'   warning diagnostics remain visible but do not drop rows.
#' @export
adapt_qc_data <- function(data,
                          mapping,
                          identity_strategy,
                          adapter = 'generic_mapping',
                          adapter_version = '1',
                          dataset_id,
                          source_system = NULL,
                          attendance_definition = NULL,
                          provenance = list()) {
  if (!is.data.frame(data) || nrow(data) == 0L) {
    rlang::abort('`data` must be a non-empty data frame.')
  }
  mapping <- .validate_adapter_mapping(mapping, names(data))
  identifiers <- list(
    adapter = adapter,
    adapter_version = adapter_version,
    dataset_id = dataset_id,
    identity_strategy = identity_strategy
  )
  if (!is.null(source_system)) identifiers$source_system <- source_system
  for (nm in names(identifiers)) {
    value <- identifiers[[nm]]
    if (!is.character(value) || length(value) != 1L || is.na(value) || !nzchar(trimws(value))) {
      rlang::abort(paste0('`', nm, '` must be one non-empty safe identifier.'))
    }
  }
  duplicated_provenance <- intersect(names(identifiers), names(provenance))
  if (length(duplicated_provenance) > 0L) {
    rlang::abort(paste0(
      'Additional provenance duplicates reserved field(s): ',
      paste(duplicated_provenance, collapse = ', ')
    ))
  }
  provenance <- .validate_provenance(c(identifiers, provenance))

  if ('attending' %in% names(mapping) &&
      (!is.character(attendance_definition) || length(attendance_definition) != 1L ||
       is.na(attendance_definition) || !nzchar(trimws(attendance_definition)))) {
    rlang::abort('`attendance_definition` is required when `attending` is mapped.')
  }

  source_ids <- if ('source_record_id' %in% names(mapping)) {
    as.character(data[[mapping$source_record_id]])
  } else {
    as.character(seq_len(nrow(data)))
  }
  canonical <- dplyr::tibble(
    facility_id = as.character(data[[mapping$facility_id]]),
    month_date = .coerce_month_date(data[[mapping$month_date]]),
    region = as.character(data[[mapping$region]]),
    tested = suppressWarnings(as.numeric(data[[mapping$tested]])),
    positive = suppressWarnings(as.numeric(data[[mapping$positive]]))
  )
  for (field in intersect(.adapter_optional_fields, names(mapping))) {
    if (field == 'attending') {
      canonical[[field]] <- suppressWarnings(as.numeric(data[[mapping[[field]]]]))
    } else {
      canonical[[field]] <- as.character(data[[mapping[[field]]]])
    }
  }
  if ('attending' %in% names(mapping)) {
    canonical$attendance_definition <- attendance_definition
  }

  diagnostics <- list()
  add_diagnostic <- function(rows, severity, issue_code, field) {
    diagnostics[[length(diagnostics) + 1L]] <<- .adapter_diagnostic(
      rows, source_ids, severity, issue_code, field
    )
  }
  facility_bad <- is.na(canonical$facility_id) | trimws(canonical$facility_id) == ''
  region_bad <- is.na(canonical$region) | trimws(canonical$region) == ''
  month_source <- data[[mapping$month_date]]
  month_bad <- is.na(canonical$month_date)
  add_diagnostic(which(facility_bad), 'error', 'missing_facility_id', 'facility_id')
  add_diagnostic(which(region_bad), 'error', 'missing_region', 'region')
  add_diagnostic(
    which(month_bad & !is.na(month_source) & trimws(as.character(month_source)) != ''),
    'error', 'unparseable_month', 'month_date'
  )
  add_diagnostic(
    which(month_bad & (is.na(month_source) | trimws(as.character(month_source)) == '')),
    'error', 'missing_month', 'month_date'
  )

  for (field in c('tested', 'positive')) {
    source <- data[[mapping[[field]]]]
    bad <- is.na(canonical[[field]])
    add_diagnostic(
      which(bad & !is.na(source) & trimws(as.character(source)) != ''),
      'warning', 'unparseable_count', field
    )
    add_diagnostic(
      which(bad & (is.na(source) | trimws(as.character(source)) == '')),
      'warning', 'missing_count', field
    )
  }
  if ('attending' %in% names(mapping)) {
    source <- data[[mapping$attending]]
    bad <- is.na(canonical$attending) & !is.na(source) & trimws(as.character(source)) != ''
    add_diagnostic(which(bad), 'warning', 'unparseable_attendance', 'attending')
  }

  duplicate <- !facility_bad & !month_bad & (
    duplicated(canonical[c('facility_id', 'month_date')]) |
      duplicated(canonical[c('facility_id', 'month_date')], fromLast = TRUE)
  )
  add_diagnostic(which(duplicate), 'error', 'duplicate_facility_month', 'facility_id,month_date')
  diagnostics <- dplyr::bind_rows(diagnostics)
  if (nrow(diagnostics) == 0L) {
    diagnostics <- dplyr::tibble(
      source_row = integer(), source_record_id = character(), severity = character(),
      issue_code = character(), field = character()
    )
  }

  as_qc_adapter_result(canonical, diagnostics, provenance, mapping)
}

#' Run QC from an Adapter Result
#'
#' Validates a ready adapter result and passes its canonical data and provenance
#' into [run_routine_qc()]. Results with blocking diagnostics cannot run.
#'
#' @param adapter_result A `routineqc_adapter_result`.
#' @param config A validated configuration created by [qc_config()].
#' @param nthreads Threads passed to [fit_prevalence_gam()].
#'
#' @return A validated `routineqc_run`.
#' @export
run_qc_adapter <- function(adapter_result, config = qc_config(), nthreads = 1) {
  validate_qc_adapter_result(adapter_result)
  if (!adapter_result$ready) {
    n_errors <- sum(adapter_result$diagnostics$severity == 'error')
    rlang::abort(paste0(
      'Adapter result is not ready for QC; resolve ', n_errors,
      ' blocking diagnostic(s) without dropping source rows.'
    ))
  }
  data <- adapter_result$data
  args <- list(
    raw_data = data,
    facility_var = 'facility_id', region_var = 'region', month_var = 'month_date',
    tested_var = 'tested', positive_var = 'positive',
    config = config, provenance = adapter_result$provenance, nthreads = nthreads
  )
  if ('district' %in% names(data)) args$district_var <- 'district'
  if ('council' %in% names(data)) args$council_var <- 'council'
  if ('attending' %in% names(data) && any(!is.na(data$attending))) {
    args$attending_var <- 'attending'
    args$attendance_definition <- unique(data$attendance_definition)[1]
  }
  do.call(run_routine_qc, args)
}
