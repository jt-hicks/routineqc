.qc_run_schema_version <- 1L
.qc_manifest_schema_version <- 1L

.normalise_fingerprint_data <- function(data) {
  required_fields <- c('facility_id', 'month_date', 'region', 'tested', 'positive')
  optional_fields <- c(
    'district', 'council', 'facility_name', 'attending',
    'attendance_definition', 'source_record_id'
  )
  supplied_optional <- optional_fields[
    optional_fields %in% names(data) &
      vapply(optional_fields, function(nm) {
        nm %in% names(data) && any(!is.na(data[[nm]]))
      }, logical(1))
  ]
  fields <- c(required_fields, supplied_optional)
  out <- as.data.frame(data[fields], stringsAsFactors = FALSE)
  for (nm in names(out)) {
    if (inherits(out[[nm]], 'Date')) {
      out[[nm]] <- format(out[[nm]], '%Y-%m-%d')
    } else if (is.factor(out[[nm]])) {
      out[[nm]] <- as.character(out[[nm]])
    }
  }
  order_fields <- intersect(c('facility_id', 'month_date', 'source_record_id'), names(out))
  if (length(order_fields) > 0L) {
    ordering <- do.call(order, c(out[order_fields], list(na.last = TRUE, method = 'radix')))
    out <- out[ordering, , drop = FALSE]
  }
  rownames(out) <- NULL
  out
}

#' Fingerprint Canonical QC Input
#'
#' Creates a deterministic SHA-256 identifier from canonical QC fields after
#' normalizing row order and date representation. The fingerprint identifies
#' data; it is not encryption and does not make confidential data safe to share.
#'
#' @param data Canonical prepared QC input.
#'
#' @return A SHA-256 hexadecimal string.
#' @export
fingerprint_qc_input <- function(data) {
  validate_qc_input(data)
  normalized <- .normalise_fingerprint_data(data)
  digest::digest(
    list(fingerprint_schema_version = 1L, fields = names(normalized), data = normalized),
    algo = 'sha256', serialize = TRUE
  )
}

.validate_provenance <- function(provenance) {
  if (!is.list(provenance) ||
      (length(provenance) > 0L && (is.null(names(provenance)) || any(names(provenance) == '')))) {
    rlang::abort('`provenance` must be a flat named list.')
  }
  if (anyDuplicated(names(provenance))) {
    rlang::abort('`provenance` names must be unique.')
  }
  unsafe_names <- grepl(
    'password|passwd|token|secret|credential|api[_-]?key|(^|_)path($|_)',
    names(provenance), ignore.case = TRUE
  )
  if (any(unsafe_names)) {
    rlang::abort('Provenance names must not contain credentials, secrets, tokens, keys, or local paths.')
  }
  valid_value <- function(x) {
    if (inherits(x, 'Date')) return(length(x) == 1L && !is.na(x))
    typeof(x) %in% c('character', 'double', 'integer', 'logical') &&
      length(x) == 1L && !is.na(x)
  }
  if (any(!vapply(provenance, valid_value, logical(1)))) {
    rlang::abort('Provenance values must be scalar atomic values or Dates.')
  }
  provenance
}

.package_version <- function() {
  as.character(utils::packageVersion('routineqc'))
}

.config_fingerprint <- function(config) {
  digest::digest(unclass(config), algo = 'sha256', serialize = TRUE)
}

.analysis_id <- function(run_schema_version, package_version, input_fingerprint, config_fingerprint) {
  digest::digest(
    list(
      run_schema_version = run_schema_version,
      package_version = package_version,
      input_fingerprint = input_fingerprint,
      config_fingerprint = config_fingerprint
    ),
    algo = 'sha256', serialize = TRUE
  )
}

.new_qc_run <- function(data_flagged, model, summaries, config, input_data, provenance = list()) {
  validate_qc_config(config)
  provenance <- .validate_provenance(provenance)
  input_fingerprint <- fingerprint_qc_input(input_data)
  package_version <- .package_version()
  config_fingerprint <- .config_fingerprint(config)
  analysis_id <- .analysis_id(
    .qc_run_schema_version, package_version, input_fingerprint, config_fingerprint
  )
  created_at_utc <- format(Sys.time(), '%Y-%m-%dT%H:%M:%OS6Z', tz = 'UTC')
  execution_id <- digest::digest(
    list(analysis_id, created_at_utc, Sys.getpid(), tempfile('routineqc-execution-')),
    algo = 'sha256', serialize = TRUE
  )
  optional_fields <- intersect(
    c('district', 'council', 'facility_name', 'attending',
      'attendance_definition', 'source_record_id'),
    names(input_data)
  )

  manifest <- list(
    manifest_schema_version = .qc_manifest_schema_version,
    run_schema_version = .qc_run_schema_version,
    analysis_id = analysis_id,
    execution_id = execution_id,
    created_at_utc = created_at_utc,
    package_version = package_version,
    input_fingerprint = input_fingerprint,
    config_fingerprint = config_fingerprint,
    input_rows = nrow(input_data),
    input_facilities = dplyr::n_distinct(input_data$facility_id),
    input_month_min = format(min(input_data$month_date), '%Y-%m-%d'),
    input_month_max = format(max(input_data$month_date), '%Y-%m-%d'),
    optional_fields = optional_fields,
    attendance_supplied = 'attending' %in% names(input_data) && any(!is.na(input_data$attending)),
    config_schema_version = config$schema_version,
    config_profile = config$profile,
    action_policy = config$action_policy$name,
    action_policy_version = config$action_policy$version,
    tested_baseline_mode = config$tested_volume$baseline_mode,
    baseline_uses_future = config$tested_volume$baseline_uses_future,
    config = unclass(config),
    provenance = provenance
  )

  run <- list(
    data_flagged = data_flagged,
    model = model,
    summaries = summaries,
    config = config,
    manifest = manifest
  )
  class(run) <- c('routineqc_run', 'list')
  validate_qc_run(run)
  run
}

#' Validate a QC Run
#'
#' Validates the run schema and checks that configuration and manifest identities
#' agree with the stored object.
#'
#' @param run A `routineqc_run` object.
#'
#' @return `run`, invisibly, after successful validation.
#' @export
validate_qc_run <- function(run) {
  required <- c('data_flagged', 'model', 'summaries', 'config', 'manifest')
  if (!inherits(run, 'routineqc_run') || !is.list(run) || !all(required %in% names(run))) {
    rlang::abort('Object is not a complete `routineqc_run`.')
  }
  validate_qc_config(run$config)
  manifest <- run$manifest
  manifest_required <- c(
    'manifest_schema_version', 'run_schema_version', 'analysis_id',
    'execution_id', 'input_fingerprint', 'config_fingerprint', 'input_rows'
  )
  if (!is.list(manifest) || !all(manifest_required %in% names(manifest))) {
    rlang::abort('QC run manifest is incomplete.')
  }
  if (!identical(manifest$manifest_schema_version, .qc_manifest_schema_version) ||
      !identical(manifest$run_schema_version, .qc_run_schema_version)) {
    rlang::abort('Unsupported QC run or manifest schema version.')
  }
  if (!identical(manifest$config_fingerprint, .config_fingerprint(run$config))) {
    rlang::abort('QC run configuration does not match its manifest.')
  }
  if (!identical(manifest$config_fingerprint, digest::digest(manifest$config, algo = 'sha256', serialize = TRUE))) {
    rlang::abort('QC manifest configuration payload does not match its fingerprint.')
  }
  if (!identical(as.integer(manifest$input_rows), as.integer(nrow(run$data_flagged)))) {
    rlang::abort('QC run row count does not match its manifest.')
  }
  if (!identical(manifest$input_fingerprint, fingerprint_qc_input(run$data_flagged))) {
    rlang::abort('QC run data does not match its input fingerprint.')
  }
  expected_analysis_id <- .analysis_id(
    manifest$run_schema_version, manifest$package_version,
    manifest$input_fingerprint, manifest$config_fingerprint
  )
  if (!identical(manifest$analysis_id, expected_analysis_id)) {
    rlang::abort('QC run analysis identity does not match its manifest components.')
  }
  invisible(run)
}

.qc_manifest_path <- function(path) {
  sub('\\.rds$', '-manifest.json', path, ignore.case = TRUE)
}

#' Write a QC Run
#'
#' Writes the complete run as RDS and a human-readable metadata-only JSON
#' manifest beside it. Existing files are never replaced unless `overwrite` is
#' explicitly `TRUE`.
#'
#' @param run A validated `routineqc_run`.
#' @param path Destination ending in `.rds`.
#' @param overwrite Whether both existing run and manifest files may be replaced.
#'
#' @return Paths to the RDS and JSON files, invisibly.
#' @export
write_qc_run <- function(run, path, overwrite = FALSE) {
  validate_qc_run(run)
  if (!is.character(path) || length(path) != 1L || is.na(path) ||
      !grepl('\\.rds$', path, ignore.case = TRUE)) {
    rlang::abort('`path` must be one non-missing path ending in `.rds`.')
  }
  if (!rlang::is_bool(overwrite)) {
    rlang::abort('`overwrite` must be TRUE or FALSE.')
  }
  parent <- dirname(path)
  if (!dir.exists(parent)) {
    rlang::abort('Destination directory does not exist.')
  }
  manifest_path <- .qc_manifest_path(path)
  targets <- c(path, manifest_path)
  if (!overwrite && any(file.exists(targets))) {
    rlang::abort('QC run or manifest already exists; set `overwrite = TRUE` to replace both.')
  }

  temp_rds <- tempfile('routineqc-run-', tmpdir = parent, fileext = '.rds')
  temp_json <- tempfile('routineqc-manifest-', tmpdir = parent, fileext = '.json')
  on.exit(unlink(c(temp_rds, temp_json)), add = TRUE)
  saveRDS(run, temp_rds, version = 3)
  jsonlite::write_json(
    run$manifest, temp_json, auto_unbox = TRUE, pretty = TRUE,
    null = 'null', na = 'null', digits = NA
  )

  if (overwrite) unlink(targets[file.exists(targets)])
  if (!file.rename(temp_rds, path)) {
    rlang::abort('Could not place the QC run RDS at its destination.')
  }
  if (!file.rename(temp_json, manifest_path)) {
    unlink(path)
    rlang::abort('Could not place the QC manifest; the incomplete RDS was removed.')
  }
  invisible(list(run = path, manifest = manifest_path))
}

#' Read a QC Run
#'
#' Reads and validates a persisted QC run and verifies that its adjacent JSON
#' manifest agrees with the stored object identity.
#'
#' @param path Path to a run `.rds` written by [write_qc_run()].
#'
#' @return A validated `routineqc_run`.
#' @export
read_qc_run <- function(path) {
  if (!is.character(path) || length(path) != 1L || is.na(path) || !file.exists(path)) {
    rlang::abort('QC run RDS does not exist.')
  }
  manifest_path <- .qc_manifest_path(path)
  if (!file.exists(manifest_path)) {
    rlang::abort('QC run JSON manifest does not exist.')
  }
  run <- readRDS(path)
  validate_qc_run(run)
  disk_manifest <- jsonlite::read_json(manifest_path, simplifyVector = TRUE)
  identity_fields <- c(
    'analysis_id', 'execution_id', 'input_fingerprint', 'config_fingerprint'
  )
  for (field in identity_fields) {
    if (!identical(as.character(disk_manifest[[field]]), as.character(run$manifest[[field]]))) {
      rlang::abort(paste0('JSON manifest disagrees with the QC run for `', field, '`.'))
    }
  }
  if (as.integer(disk_manifest$run_schema_version) != .qc_run_schema_version ||
      as.integer(disk_manifest$manifest_schema_version) != .qc_manifest_schema_version) {
    rlang::abort('JSON manifest has an unsupported schema version.')
  }
  run
}
