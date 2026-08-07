#!/usr/bin/env Rscript

args <- commandArgs(trailingOnly = TRUE)
launch <- '--launch' %in% args
paths <- args[args != '--launch']
output_dir <- if (length(paths) > 0L) {
  paths[[1]]
} else {
  file.path(getwd(), 'qc-runs', 'smoke')
}

suppressPackageStartupMessages(library(routineqc))

cat('Creating synthetic source data...\n')
source_data <- simulate_qc_data(n_facilities = 12, n_months = 18, seed = 42)
source_data$record_id <- sprintf('synthetic-%04d', seq_len(nrow(source_data)))

cat('Adapting source columns to the canonical contract...\n')
adapted <- adapt_qc_data(
  source_data,
  mapping = list(
    facility_id = 'facility',
    month_date = 'month',
    region = 'region',
    district = 'district',
    council = 'council',
    tested = 'tested',
    positive = 'positive',
    source_record_id = 'record_id'
  ),
  identity_strategy = 'synthetic stable facility code',
  adapter = 'package_smoke_test',
  adapter_version = '1.0.0',
  dataset_id = 'synthetic-smoke-seed-42',
  source_system = 'routineqc-simulator'
)

print(adapted$diagnostics)
stopifnot(adapted$ready, nrow(adapted$data) == nrow(source_data))

cat('Running the operational QC profile...\n')
qc_run <- suppressWarnings(run_qc_adapter(
  adapted,
  config = qc_config('operational'),
  nthreads = 1
))
validate_qc_run(qc_run)

dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
stamp <- format(Sys.time(), '%Y%m%d-%H%M%S')
run_path <- file.path(output_dir, paste0('synthetic-smoke-', stamp, '.rds'))
written <- write_qc_run(qc_run, run_path)

cat('\nQC smoke test passed.\n')
cat('Rows:', nrow(qc_run$data_flagged), '\n')
cat('Authorized exclusions:', sum(qc_run$data_flagged$flag_exclude_authorized), '\n')
cat('Review recommended:', sum(qc_run$data_flagged$flag_review_recommended), '\n')
cat('Model coverage:', qc_run$manifest$model_prediction_coverage, '\n')
cat('Run artifact:', written$run, '\n')
cat('JSON manifest:', written$manifest, '\n\n')
print(list_qc_runs(output_dir))

if (launch) {
  cat('\nOpening the local Run Explorer in your default browser...\n')
  cat('Keep this terminal running while using the app; press Ctrl+C to stop it.\n')
  launch_qc_app(output_dir, launch_browser = TRUE)
} else {
  cat('\nTo inspect this run interactively:\n')
  cat('  launch_qc_app(', dQuote(normalizePath(output_dir, winslash = '/')), ')\n', sep = '')
}
