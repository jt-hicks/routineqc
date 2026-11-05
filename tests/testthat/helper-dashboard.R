# Flagged synthetic data with a region > council > facility hierarchy, shared
# by the dashboard tests. Built once per test run.
dashboard_fixture <- local({
  cache <- NULL
  function() {
    if (is.null(cache)) {
      sim <- routineqc::simulate_qc_truth(
        n_regions = 2, councils_per_region = 3, facilities_per_council = 4, n_months = 30,
        missing = c('isolated', 'consecutive'), error_rate = 0.05, seed = 21
      )
      qc <- suppressWarnings(routineqc::run_routine_qc(
        sim$observed, facility_var = 'facility', region_var = 'region', month_var = 'month',
        tested_var = 'tested', positive_var = 'positive', attending_var = 'attending',
        attendance_definition = 'all outpatient attendances', council_var = 'council', nthreads = 1
      ))
      cache <<- list(sim = sim, qc = qc, data = qc$data_flagged)
    }
    cache
  }
})
