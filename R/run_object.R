.qc_run_schema_version <- 2L
.qc_manifest_schema_version <- 2L

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

# Identifies the implementation that produced a run. The package version alone
# cannot: a development tree and a release share it. `implementation_hash`
# digests every function in the package namespace, so any code change gives a
# different value; `source_revision` is the commit recorded in the installed
# DESCRIPTION when there is one (NA otherwise); the dependency versions are
# those of the packages that carry the computation.
.dependency_packages <- c('mgcv', 'dplyr', 'tidyr', 'lubridate', 'rlang', 'digest')

.build_identity <- function() {
  ns <- asNamespace('routineqc')
  objects <- sort(ls(ns, all.names = TRUE))
  functions <- Filter(function(nm) is.function(get(nm, envir = ns)), objects)
  implementation_hash <- digest::digest(
    lapply(functions, function(nm) {
      fn <- get(nm, envir = ns)
      list(nm, deparse(formals(fn)), deparse(body(fn)))
    }),
    algo = 'sha256', serialize = TRUE
  )
  description <- utils::packageDescription('routineqc')
  revision <- NA_character_
  for (field in c('RemoteSha', 'GithubSHA1', 'git_revision')) {
    value <- description[[field]]
    if (!is.null(value) && !is.na(value)) {
      revision <- as.character(value)
      break
    }
  }
  versions <- vapply(.dependency_packages, function(pkg) {
    tryCatch(as.character(utils::packageVersion(pkg)), error = function(e) NA_character_)
  }, character(1))
  list(
    implementation_hash = implementation_hash,
    source_revision = revision,
    r_version = paste(R.version$major, R.version$minor, sep = '.'),
    dependency_versions = as.list(versions)
  )
}

.config_fingerprint <- function(config) {
  digest::digest(unclass(config), algo = 'sha256', serialize = TRUE)
}

# `implementation_hash` is NULL for runs saved before it was recorded; leaving
# it out of the digest keeps their analysis identity unchanged.
.analysis_id <- function(run_schema_version, package_version, input_fingerprint,
                         config_fingerprint, implementation_hash = NULL) {
  parts <- list(
    run_schema_version = run_schema_version,
    package_version = package_version,
    input_fingerprint = input_fingerprint,
    config_fingerprint = config_fingerprint
  )
  if (!is.null(implementation_hash)) parts$implementation_hash <- implementation_hash
  digest::digest(parts, algo = 'sha256', serialize = TRUE)
}

.new_qc_run <- function(data_flagged, model, summaries, config, input_data, provenance = list()) {
  validate_qc_config(config)
  provenance <- .validate_provenance(provenance)
  input_fingerprint <- fingerprint_qc_input(input_data)
  package_version <- .package_version()
  build <- .build_identity()
  config_fingerprint <- .config_fingerprint(config)
  analysis_id <- .analysis_id(
    .qc_run_schema_version, package_version, input_fingerprint, config_fingerprint,
    build$implementation_hash
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
    build = build,
    count_translation = .count_translation_summary(data_flagged),
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
    prevalence_model_available = !is.null(model),
    model_eligible_rows = sum(data_flagged$model_assessment_eligible),
    model_assessed_rows = sum(data_flagged$model_assessed),
    model_prediction_coverage = if (any(data_flagged$model_assessment_eligible)) {
      sum(data_flagged$model_assessed) / sum(data_flagged$model_assessment_eligible)
    } else {
      NA_real_
    },
    prediction_status_counts = as.list(table(data_flagged$prediction_status, useNA = 'ifany')),
    prevalence_dispersion = if ('resid_dispersion' %in% names(data_flagged)) {
      data_flagged$resid_dispersion[[1]]
    } else {
      NA_real_
    },
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
  .validate_qc_config(run$config, stored = TRUE)
  manifest <- run$manifest
  manifest_required <- c(
    'manifest_schema_version', 'run_schema_version', 'analysis_id',
    'execution_id', 'input_fingerprint', 'config_fingerprint', 'input_rows',
    'prevalence_model_available', 'model_eligible_rows',
    'model_assessed_rows', 'model_prediction_coverage', 'prediction_status_counts'
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
  prediction_columns <- c(
    'model_assessment_eligible', 'model_assessed', 'prediction_status'
  )
  if (!all(prediction_columns %in% names(run$data_flagged))) {
    rlang::abort('QC run data is missing model-assessment fields.')
  }
  expected_eligible <- sum(run$data_flagged$model_assessment_eligible)
  expected_assessed <- sum(run$data_flagged$model_assessed)
  expected_coverage <- if (expected_eligible > 0L) {
    expected_assessed / expected_eligible
  } else {
    NA_real_
  }
  expected_status_counts <- as.list(table(run$data_flagged$prediction_status, useNA = 'ifany'))
  model_manifest_matches <-
    identical(manifest$prevalence_model_available, !is.null(run$model)) &&
    identical(as.integer(manifest$model_eligible_rows), as.integer(expected_eligible)) &&
    identical(as.integer(manifest$model_assessed_rows), as.integer(expected_assessed)) &&
    isTRUE(all.equal(manifest$model_prediction_coverage, expected_coverage)) &&
    identical(manifest$prediction_status_counts, expected_status_counts)
  if (!model_manifest_matches) {
    rlang::abort('QC run model-assessment results do not match its manifest.')
  }
  expected_analysis_id <- .analysis_id(
    manifest$run_schema_version, manifest$package_version,
    manifest$input_fingerprint, manifest$config_fingerprint,
    manifest$build$implementation_hash
  )
  if (!identical(manifest$analysis_id, expected_analysis_id)) {
    rlang::abort('QC run analysis identity does not match its manifest components.')
  }
  .validate_run_consistency(run)
  invisible(run)
}

# Internal consistency of the stored results. The action, exclusion and review
# columns must agree with each other, and, for runs that record their
# implementation (older runs do not, and their summaries were computed by
# earlier rules), the summaries must equal a recomputation from the data.
# It does not re-run the model or the rules, so it detects tampering or
# mixed-up pieces, not a different but self-consistent analysis.
.validate_run_consistency <- function(run) {
  data <- run$data_flagged
  action_columns <- c(
    'qc_action', 'flag_exclude_authorized', 'flag_review_recommended', 'flag_any_qc_issue'
  )
  if (all(action_columns %in% names(data))) {
    action <- as.character(data$qc_action)
    consistent <-
      !anyNA(action) &&
      identical(as.logical(data$flag_exclude_authorized), action == 'exclude_core_invalid') &&
      identical(as.logical(data$flag_review_recommended), startsWith(action, 'review_')) &&
      identical(as.logical(data$flag_any_qc_issue), action != 'retain')
    if (!consistent) {
      rlang::abort('QC run actions, exclusion flags and review flags are inconsistent.')
    }
    if ('flag_core_invalid' %in% names(data) &&
        any(data$flag_exclude_authorized & !dplyr::coalesce(data$flag_core_invalid, FALSE))) {
      rlang::abort('QC run authorizes exclusion for a row without invalid core counts.')
    }
  }
  if (is.null(run$manifest$build$implementation_hash) || !is.list(run$summaries)) {
    return(invisible(run))
  }
  recompute <- list(
    by_region = summarise_qc_by_region, by_month = summarise_qc_by_month,
    by_facility = summarise_qc_by_facility, by_council = summarise_qc_by_council,
    by_district = summarise_qc_by_district
  )
  for (nm in intersect(names(recompute), names(run$summaries))) {
    expected <- recompute[[nm]](data)
    if (!isTRUE(all.equal(as.data.frame(run$summaries[[nm]]), as.data.frame(expected),
                          check.attributes = FALSE))) {
      rlang::abort(paste0('QC run summary `', nm, '` does not match its data.'))
    }
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
