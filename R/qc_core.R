#' routineqc: Reusable QC Tools For Routine Surveillance Data
#'
#' The package provides modular functions for preparing routine facility-month
#' data, fitting prevalence models, flagging potential anomalies, and producing
#' summaries and plots for QC review.
#'
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#'
#' @keywords internal
"_PACKAGE"

.resolve_col <- function(x) {
  if (is.null(x)) {
    return(NULL)
  }

  if (rlang::is_symbol(x)) {
    return(rlang::as_string(x))
  }

  if (is.character(x) && length(x) == 1L) {
    return(x)
  }

  if (rlang::is_quosure(x)) {
    return(rlang::as_name(rlang::get_expr(x)))
  }

  rlang::abort("Column mappings must be a single string or bare column name.")
}

.coerce_month_date <- function(x) {
  if (inherits(x, "Date")) {
    return(lubridate::floor_date(x, unit = "month"))
  }

  if (inherits(x, "POSIXt")) {
    return(lubridate::floor_date(as.Date(x), unit = "month"))
  }

  if (is.numeric(x)) {
    x <- as.character(x)
  }

  x_chr <- as.character(x)
  parsed <- suppressWarnings(lubridate::parse_date_time(
    x_chr,
    orders = c("ymd", "Ymd", "Y-m", "Y/m", "m/Y", "b Y", "B Y", "Y b", "Y B"),
    truncated = 2
  ))

  out <- as.Date(parsed)
  out <- lubridate::floor_date(out, unit = "month")
  out
}

.safe_divide <- function(num, den) {
  dplyr::if_else(is.na(den) | den == 0, NA_real_, as.numeric(num) / as.numeric(den))
}

.validate_required_columns <- function(data, cols) {
  missing_cols <- setdiff(cols, names(data))
  if (length(missing_cols) > 0) {
    rlang::abort(paste0("Missing required columns: ", paste(missing_cols, collapse = ", ")))
  }
}

#' Prepare Routine QC Data
#'
#' Standardises user-specified columns into a common schema used across QC
#' functions and appends derived fields without dropping rows.
#'
#' @param data A data frame with raw routine surveillance records.
#' @param facility_var Facility identifier column.
#' @param region_var Region column.
#' @param month_var Month/date column.
#' @param tested_var Tested count column.
#' @param positive_var Positive count column.
#' @param council_var Optional council column.
#' @param district_var Optional district column.
#'
#' @return A tibble with standardised QC fields appended.
#' @export
prepare_qc_data <- function(data,
                            facility_var,
                            region_var,
                            month_var,
                            tested_var,
                            positive_var,
                            council_var = NULL,
                            district_var = NULL) {
  facility_var <- .resolve_col(facility_var)
  region_var <- .resolve_col(region_var)
  month_var <- .resolve_col(month_var)
  tested_var <- .resolve_col(tested_var)
  positive_var <- .resolve_col(positive_var)
  council_var <- .resolve_col(council_var)
  district_var <- .resolve_col(district_var)

  required <- c(facility_var, region_var, month_var, tested_var, positive_var)
  .validate_required_columns(data, required)

  out <- dplyr::as_tibble(data) %>%
    dplyr::mutate(
      facility_id = as.character(.data[[facility_var]]),
      region = as.character(.data[[region_var]]),
      month_date = .coerce_month_date(.data[[month_var]]),
      tested = suppressWarnings(as.numeric(.data[[tested_var]])),
      positive = suppressWarnings(as.numeric(.data[[positive_var]]))
    )

  if (!is.null(council_var) && council_var %in% names(out)) {
    out <- dplyr::mutate(out, council = as.character(.data[[council_var]]))
  }

  if (!is.null(district_var) && district_var %in% names(out)) {
    out <- dplyr::mutate(out, district = as.character(.data[[district_var]]))
  }

  unique_months <- sort(unique(out$month_date))

  out %>%
    dplyr::mutate(
      year = lubridate::year(month_date),
      month_num = lubridate::month(month_date),
      time_index = match(month_date, unique_months),
      prevalence = .safe_divide(positive, tested)
    )
}

#' Flag Logical Data Errors
#'
#' Adds flags for impossible or invalid tested/positive combinations.
#'
#' @param data A prepared QC data frame.
#'
#' @return The input data with logical error flags appended.
#' @export
flag_logical_errors <- function(data) {
  .validate_required_columns(data, c("tested", "positive"))

  dplyr::as_tibble(data) %>%
    dplyr::mutate(
      flag_positive_gt_tested = positive > tested,
      flag_tested_negative = tested < 0,
      flag_positive_negative = positive < 0,
      flag_zero_tested_positive = tested == 0 & positive > 0,
      flag_missing_tested_or_positive = is.na(tested) | is.na(positive),
      flag_invalid_logical = (
        flag_positive_gt_tested |
          flag_tested_negative |
          flag_positive_negative |
          flag_zero_tested_positive |
          flag_missing_tested_or_positive
      ) %>%
        dplyr::coalesce(FALSE)
    )
}

#' Fit Prevalence GAM
#'
#' Fits a binomial GAM using logically valid rows with tested > 0.
#'
#' @param data A prepared and logically flagged data frame.
#' @param nthreads Number of threads to pass to [mgcv::bam()].
#'
#' @return A fitted `mgcv` model, or `NULL` if insufficient data.
#' @export
fit_prevalence_gam <- function(data, nthreads = 1) {
  .validate_required_columns(
    data,
    c("facility_id", "region", "month_num", "time_index", "tested", "positive")
  )

  train <- dplyr::as_tibble(data)
  if (!"flag_invalid_logical" %in% names(train)) {
    train <- flag_logical_errors(train)
  }

  train <- train %>%
    dplyr::filter(!flag_invalid_logical, tested > 0, !is.na(month_num), !is.na(time_index)) %>%
    dplyr::mutate(
      facility_id = as.factor(facility_id),
      region = as.factor(region)
    )

  if (nrow(train) < 50 || dplyr::n_distinct(train$facility_id) < 2 || dplyr::n_distinct(train$region) < 1) {
    warning("Insufficient valid rows to fit prevalence model; returning NULL.", call. = FALSE)
    return(NULL)
  }

  fit_once <- function(k_month = 12, k_time = 40) {
    mgcv::bam(
      cbind(positive, tested - positive) ~
        s(month_num, by = region, bs = "cc", k = k_month) +
        s(time_index, k = k_time) +
        s(facility_id, bs = "re"),
      data = train,
      family = stats::binomial(),
      method = "fREML",
      discrete = TRUE,
      nthreads = nthreads,
      knots = list(month_num = c(0.5, 12.5))
    )
  }

  fit <- tryCatch(
    fit_once(k_month = 12, k_time = 40),
    error = function(e) {
      month_support <- train %>%
        dplyr::group_by(region) %>%
        dplyr::summarise(n_month = dplyr::n_distinct(month_num), .groups = "drop") %>%
        dplyr::pull(n_month)

      k_month_fallback <- max(3, min(11, min(month_support, na.rm = TRUE) - 1))
      k_time_fallback <- max(5, min(40, dplyr::n_distinct(train$time_index) - 1))

      warning(
        paste0(
          "Default GAM basis dimensions were too large for the available data; ",
          "refitting with k_month=", k_month_fallback,
          " and k_time=", k_time_fallback, "."
        ),
        call. = FALSE
      )

      fit_once(k_month = k_month_fallback, k_time = k_time_fallback)
    }
  )

  fit
}

#' Add Prevalence Predictions
#'
#' Appends fitted prevalence and residual diagnostics from a fitted GAM.
#'
#' @param data A prepared QC data frame.
#' @param model A model returned by [fit_prevalence_gam()].
#'
#' @return The input data with prediction columns appended.
#' @export
add_prevalence_predictions <- function(data, model) {
  out <- dplyr::as_tibble(data)
  .validate_required_columns(out, c("tested", "positive", "facility_id", "region", "month_num", "time_index"))

  if (is.null(model)) {
    return(out %>% dplyr::mutate(
      p_hat = NA_real_,
      expected_positive = NA_real_,
      pearson_resid = NA_real_
    ))
  }

  pred <- rep(NA_real_, nrow(out))
  valid_idx <- which(!is.na(out$tested) & !is.na(out$month_num) & !is.na(out$time_index))

  if (length(valid_idx) > 0) {
    newdata <- out[valid_idx, , drop = FALSE]
    newdata$facility_id <- as.factor(newdata$facility_id)
    newdata$region <- as.factor(newdata$region)

    pred_vals <- suppressWarnings(stats::predict(model, newdata = newdata, type = "response"))
    pred[valid_idx] <- as.numeric(pred_vals)
  }

  out <- out %>%
    dplyr::mutate(
      p_hat = pmin(1 - 1e-6, pmax(1e-6, pred)),
      expected_positive = tested * p_hat,
      pearson_resid = dplyr::if_else(
        is.na(tested) | is.na(positive) | is.na(p_hat) | tested <= 0,
        NA_real_,
        (positive - expected_positive) / sqrt(tested * p_hat * (1 - p_hat))
      )
    )

  out
}

#' Add Prevalence QC Flags
#'
#' Adds prevalence-based anomaly flags using residuals and simple temporal rules.
#'
#' @param data A data frame with prevalence and prediction columns.
#' @param resid_threshold Absolute Pearson residual threshold.
#' @param large_n_threshold Minimum tested count for all-negative/all-positive rules.
#' @param large_change_threshold Absolute month-to-month prevalence change threshold.
#' @param prevalence_low_extreme Lower bound for extreme observed prevalence.
#' @param prevalence_high_extreme Upper bound for extreme observed prevalence.
#'
#' @return Data with prevalence QC flags appended.
#' @export
add_prevalence_qc_flags <- function(data,
                                    resid_threshold = 4,
                                    large_n_threshold = 30,
                                    large_change_threshold = 0.35,
                                    prevalence_low_extreme = 0.001,
                                    prevalence_high_extreme = 0.999) {
  .validate_required_columns(data, c("facility_id", "month_date", "tested", "positive", "prevalence", "pearson_resid"))

  out <- dplyr::as_tibble(data) %>%
    dplyr::group_by(facility_id) %>%
    dplyr::arrange(month_date, .by_group = TRUE) %>%
    dplyr::mutate(
      prev_lag = dplyr::lag(prevalence),
      monthly_prev_change = abs(prevalence - prev_lag),
      flag_resid_extreme = abs(pearson_resid) >= resid_threshold,
      flag_all_negative_large_n = tested >= large_n_threshold & positive == 0,
      flag_all_positive_large_n = tested >= large_n_threshold & positive == tested,
      flag_large_monthly_prevalence_change = !is.na(monthly_prev_change) & monthly_prev_change >= large_change_threshold,
      flag_prevalence_extreme = (
        (!is.na(prevalence) & (prevalence <= prevalence_low_extreme | prevalence >= prevalence_high_extreme)) |
          flag_resid_extreme |
          flag_all_negative_large_n |
          flag_all_positive_large_n
      ) %>% dplyr::coalesce(FALSE)
    ) %>%
    dplyr::ungroup() %>%
    dplyr::select(-prev_lag, -monthly_prev_change)

  out
}

#' Add Tested Volume QC Flags
#'
#' Adds facility-level tested count anomaly flags using rolling median and MAD.
#'
#' @param data A prepared data frame.
#' @param min_roll_n Minimum previous observations required for rolling stats.
#' @param tested_z_threshold Absolute robust z-score threshold.
#' @param tested_high_ratio High tested-to-median ratio threshold.
#' @param tested_low_ratio Low tested-to-median ratio threshold.
#' @param tested_jump_ratio Current-to-previous tested jump ratio threshold.
#' @param tested_drop_ratio Current-to-previous tested drop ratio threshold.
#'
#' @return Data with tested-count rolling metrics and QC flags appended.
#' @export
add_tested_volume_qc <- function(data,
                                 min_roll_n = 6,
                                 tested_z_threshold = 5,
                                 tested_high_ratio = 3,
                                 tested_low_ratio = 0.25,
                                 tested_jump_ratio = 5,
                                 tested_drop_ratio = 0.2) {
  .validate_required_columns(data, c("facility_id", "month_date", "tested"))

  dplyr::as_tibble(data) %>%
    dplyr::group_by(facility_id) %>%
    dplyr::arrange(month_date, .by_group = TRUE) %>%
    dplyr::mutate(
      tested_prev = dplyr::lag(tested),
      tested_roll_median = slider::slide_dbl(
        tested_prev,
        ~ stats::median(.x, na.rm = TRUE),
        .before = min_roll_n - 1,
        .complete = TRUE
      ),
      tested_roll_mad = slider::slide_dbl(
        tested_prev,
        ~ stats::mad(.x, center = stats::median(.x, na.rm = TRUE), constant = 1, na.rm = TRUE),
        .before = min_roll_n - 1,
        .complete = TRUE
      ),
      tested_robust_z = dplyr::if_else(
        is.na(tested_roll_mad) | tested_roll_mad == 0,
        NA_real_,
        (tested - tested_roll_median) / (tested_roll_mad * 1.4826)
      ),
      tested_to_roll_ratio = .safe_divide(tested, tested_roll_median),
      tested_to_prev_ratio = .safe_divide(tested, tested_prev),
      flag_tested_extreme_high = (
        (!is.na(tested_robust_z) & tested_robust_z >= tested_z_threshold) |
          (!is.na(tested_to_roll_ratio) & tested_to_roll_ratio >= tested_high_ratio)
      ) %>% dplyr::coalesce(FALSE),
      flag_tested_extreme_low = (
        (!is.na(tested_robust_z) & tested_robust_z <= -tested_z_threshold) |
          (!is.na(tested_to_roll_ratio) & tested_to_roll_ratio <= tested_low_ratio)
      ) %>% dplyr::coalesce(FALSE),
      flag_tested_zero_unusual = (tested == 0 & !is.na(tested_roll_median) & tested_roll_median > 0) %>%
        dplyr::coalesce(FALSE),
      flag_tested_large_jump = (!is.na(tested_to_prev_ratio) & tested_prev > 0 & tested_to_prev_ratio >= tested_jump_ratio) %>%
        dplyr::coalesce(FALSE),
      flag_tested_large_drop = (!is.na(tested_to_prev_ratio) & tested_prev > 0 & tested_to_prev_ratio <= tested_drop_ratio) %>%
        dplyr::coalesce(FALSE),
      flag_tested_volume_extreme = (
        flag_tested_extreme_high |
          flag_tested_extreme_low |
          flag_tested_zero_unusual |
          flag_tested_large_jump |
          flag_tested_large_drop
      ) %>% dplyr::coalesce(FALSE)
    ) %>%
    dplyr::ungroup() %>%
    dplyr::select(-tested_prev, -tested_to_roll_ratio, -tested_to_prev_ratio)
}

#' Add Temporal QC Flags
#'
#' Identifies isolated extreme prevalence points within each facility time series.
#'
#' @param data A data frame with prevalence QC flags.
#'
#' @return Data with `flag_isolated_extreme` appended.
#' @export
add_temporal_qc_flags <- function(data) {
  .validate_required_columns(data, c("facility_id", "month_date", "flag_prevalence_extreme"))

  dplyr::as_tibble(data) %>%
    dplyr::group_by(facility_id) %>%
    dplyr::arrange(month_date, .by_group = TRUE) %>%
    dplyr::mutate(
      prev_extreme = dplyr::lag(flag_prevalence_extreme, default = FALSE),
      next_extreme = dplyr::lead(flag_prevalence_extreme, default = FALSE),
      flag_isolated_extreme = flag_prevalence_extreme & !prev_extreme & !next_extreme
    ) %>%
    dplyr::ungroup() %>%
    dplyr::select(-prev_extreme, -next_extreme)
}

#' Assign QC Action
#'
#' Combines QC flags into hierarchical action recommendations.
#'
#' @param data A data frame with logical, prevalence, and tested-volume flags.
#'
#' @return Data with QC action and combined issue flags appended.
#' @export
assign_qc_action <- function(data) {
  out <- dplyr::as_tibble(data)

  required_optional <- c(
    "flag_invalid_logical",
    "flag_resid_extreme",
    "flag_all_negative_large_n",
    "flag_all_positive_large_n",
    "flag_large_monthly_prevalence_change",
    "flag_prevalence_extreme",
    "flag_tested_volume_extreme",
    "flag_isolated_extreme"
  )

  for (nm in required_optional) {
    if (!nm %in% names(out)) {
      out[[nm]] <- FALSE
    }
  }

  out <- out %>%
    dplyr::mutate(
      prevalence_issue = (
        flag_resid_extreme |
          flag_all_negative_large_n |
          flag_all_positive_large_n |
          flag_large_monthly_prevalence_change |
          flag_prevalence_extreme |
          flag_isolated_extreme
      ) %>% dplyr::coalesce(FALSE),
      tested_issue = flag_tested_volume_extreme %>% dplyr::coalesce(FALSE),
      plausibility_issue = !flag_invalid_logical & !prevalence_issue & !tested_issue & tested == 0 & positive == 0,
      qc_action = dplyr::case_when(
        flag_invalid_logical ~ "remove_invalid_logical",
        flag_all_negative_large_n | flag_all_positive_large_n | flag_resid_extreme | flag_isolated_extreme ~ "review_or_remove_high_confidence",
        prevalence_issue ~ "review_prevalence_extreme",
        tested_issue ~ "review_tested_volume",
        plausibility_issue ~ "review_plausibility",
        TRUE ~ "retain"
      ),
      qc_reason = dplyr::case_when(
        qc_action == "remove_invalid_logical" ~ "Logical inconsistency in tested/positive values",
        qc_action == "review_or_remove_high_confidence" ~ "High-confidence prevalence anomaly",
        qc_action == "review_prevalence_extreme" ~ "Potential prevalence anomaly",
        qc_action == "review_tested_volume" ~ "Potential tested volume anomaly",
        qc_action == "review_plausibility" ~ "Requires plausibility review",
        TRUE ~ "No QC issue detected"
      ),
      flag_remove_primary = qc_action %in% c("remove_invalid_logical", "review_or_remove_high_confidence"),
      flag_sensitivity = qc_action %in% c("review_prevalence_extreme", "review_tested_volume", "review_or_remove_high_confidence"),
      flag_any_qc_issue = qc_action != "retain",
      flag_tested_only_issue = tested_issue & !prevalence_issue & !flag_invalid_logical,
      flag_prevalence_only_issue = prevalence_issue & !tested_issue & !flag_invalid_logical,
      flag_combined_tested_prevalence_issue = tested_issue & prevalence_issue & !flag_invalid_logical
    ) %>%
    dplyr::select(-prevalence_issue, -tested_issue, -plausibility_issue)

  out
}

.summarise_qc_group <- function(data, group_vars = character(0)) {
  dat <- dplyr::as_tibble(data)

  needed <- c(
    "tested", "positive", "flag_any_qc_issue", "flag_invalid_logical",
    "flag_prevalence_extreme", "flag_tested_volume_extreme", "flag_tested_only_issue",
    "flag_prevalence_only_issue", "flag_combined_tested_prevalence_issue",
    "flag_remove_primary", "flag_sensitivity"
  )

  for (nm in needed) {
    if (!nm %in% names(dat)) {
      dat[[nm]] <- if (nm %in% c("tested", "positive")) NA_real_ else FALSE
    }
  }

  group_syms <- rlang::syms(group_vars)

  dat %>%
    dplyr::group_by(!!!group_syms) %>%
    dplyr::summarise(
      total_rows = dplyr::n(),
      n_any_qc_issue = sum(flag_any_qc_issue, na.rm = TRUE),
      pct_any_qc_issue = 100 * n_any_qc_issue / total_rows,
      invalid_logical_rows = sum(flag_invalid_logical, na.rm = TRUE),
      prevalence_extreme_rows = sum(flag_prevalence_extreme, na.rm = TRUE),
      tested_volume_extreme_rows = sum(flag_tested_volume_extreme, na.rm = TRUE),
      tested_only_issues = sum(flag_tested_only_issue, na.rm = TRUE),
      prevalence_only_issues = sum(flag_prevalence_only_issue, na.rm = TRUE),
      combined_tested_prevalence_issues = sum(flag_combined_tested_prevalence_issue, na.rm = TRUE),
      recommended_primary_removals = sum(flag_remove_primary, na.rm = TRUE),
      sensitivity_flags = sum(flag_sensitivity, na.rm = TRUE),
      total_tested_before_qc = sum(tested, na.rm = TRUE),
      total_positive_before_qc = sum(positive, na.rm = TRUE),
      total_tested_after_primary_qc = sum(dplyr::if_else(flag_remove_primary, 0, tested), na.rm = TRUE),
      total_positive_after_primary_qc = sum(dplyr::if_else(flag_remove_primary, 0, positive), na.rm = TRUE),
      prevalence_before_qc = .safe_divide(total_positive_before_qc, total_tested_before_qc),
      prevalence_after_primary_qc = .safe_divide(total_positive_after_primary_qc, total_tested_after_primary_qc),
      .groups = "drop"
    )
}

#' Summarise QC By Region
#'
#' @param data A QC-flagged data frame.
#'
#' @return Region-level QC summary table.
#' @export
summarise_qc_by_region <- function(data) {
  .validate_required_columns(data, "region")
  .summarise_qc_group(data, group_vars = c("region"))
}

#' Summarise QC By Month
#'
#' @param data A QC-flagged data frame.
#'
#' @return Month-level QC summary table.
#' @export
summarise_qc_by_month <- function(data) {
  .validate_required_columns(data, "month_date")
  .summarise_qc_group(data, group_vars = c("month_date"))
}

#' Summarise QC By Facility
#'
#' @param data A QC-flagged data frame.
#'
#' @return Facility-level QC summary table.
#' @export
summarise_qc_by_facility <- function(data) {
  .validate_required_columns(data, "facility_id")
  .summarise_qc_group(data, group_vars = c("facility_id"))
}

#' Summarise QC By Council
#'
#' @param data A QC-flagged data frame containing `council`.
#'
#' @return Council-level QC summary table.
#' @export
summarise_qc_by_council <- function(data) {
  .validate_required_columns(data, "council")
  .summarise_qc_group(data, group_vars = c("council"))
}

#' Summarise QC By District
#'
#' @param data A QC-flagged data frame containing `district`.
#'
#' @return District-level QC summary table.
#' @export
summarise_qc_by_district <- function(data) {
  .validate_required_columns(data, "district")
  .summarise_qc_group(data, group_vars = c("district"))
}

#' Summarise Before/After QC
#'
#' Summarises prevalence and volume before and after primary QC removals.
#'
#' @param data A QC-flagged data frame.
#' @param by One of `overall`, `region`, `council`, `district`, `month`.
#'
#' @return A summary tibble.
#' @export
summarise_before_after_qc <- function(data, by = c("overall", "region", "council", "district", "month")) {
  by <- match.arg(by)

  if (by == "overall") {
    return(.summarise_qc_group(data, group_vars = character(0)))
  }

  group_var <- switch(
    by,
    region = "region",
    council = "council",
    district = "district",
    month = "month_date"
  )

  .validate_required_columns(data, group_var)
  .summarise_qc_group(data, group_vars = group_var)
}

#' Run Threshold Sensitivity
#'
#' Re-runs selected prevalence and tested QC thresholds over a grid and returns
#' grouped summaries.
#'
#' @param data A data frame with prepared/predicted columns.
#' @param resid_thresholds Vector of residual thresholds.
#' @param tested_z_thresholds Vector of tested robust-z thresholds.
#' @param by One of `region`, `council`, `district`, or `month`.
#'
#' @return A tibble with threshold combinations and summary metrics.
#' @export
run_threshold_sensitivity <- function(data,
                                      resid_thresholds = c(3, 4, 5),
                                      tested_z_thresholds = c(4, 5, 6),
                                      by = c("region", "council", "district", "month")) {
  by <- match.arg(by)

  grid <- tidyr::crossing(
    resid_threshold = resid_thresholds,
    tested_z_threshold = tested_z_thresholds
  )

  purrr::map2_dfr(
    grid$resid_threshold,
    grid$tested_z_threshold,
    function(r_thr, z_thr) {
      tmp <- data %>%
        add_prevalence_qc_flags(resid_threshold = r_thr) %>%
        add_tested_volume_qc(tested_z_threshold = z_thr) %>%
        add_temporal_qc_flags() %>%
        assign_qc_action()

      summarise_before_after_qc(tmp, by = by) %>%
        dplyr::mutate(
          resid_threshold = r_thr,
          tested_z_threshold = z_thr,
          .before = 1
        )
    }
  )
}

#' Simulate QC Data
#'
#' Generates synthetic facility-month ANC malaria testing data with injected
#' anomalies for testing and examples.
#'
#' @param n_facilities Number of facilities.
#' @param n_months Number of months.
#' @param start_month Start month as date-like string.
#' @param seed Random seed.
#'
#' @return A tibble with simulated routine data and truth labels.
#' @export
simulate_qc_data <- function(n_facilities = 40,
                             n_months = 24,
                             start_month = "2022-01-01",
                             seed = 123) {
  set.seed(seed)

  facilities <- paste0("F", sprintf("%03d", seq_len(n_facilities)))
  months <- seq.Date(as.Date(start_month), by = "month", length.out = n_months)

  base <- tidyr::crossing(
    facility = facilities,
    month = months
  ) %>%
    dplyr::mutate(
      region = sample(paste0("Region_", LETTERS[1:4]), dplyr::n(), replace = TRUE),
      district = sample(paste0("District_", seq_len(8)), dplyr::n(), replace = TRUE),
      council = sample(paste0("Council_", seq_len(12)), dplyr::n(), replace = TRUE)
    )

  facility_effect <- stats::rnorm(n_facilities, 0, 0.4)
  names(facility_effect) <- facilities

  dat <- base %>%
    dplyr::mutate(
      month_num = lubridate::month(month),
      season = sin(2 * pi * month_num / 12),
      tested = pmax(0, round(stats::rnorm(dplyr::n(), mean = 120 + 20 * season, sd = 25))),
      lp = -1.2 + 0.4 * season + facility_effect[facility],
      p = stats::plogis(lp),
      positive = stats::rbinom(dplyr::n(), size = pmax(tested, 0), prob = p),
      injected_error = FALSE
    ) %>%
    dplyr::select(facility, region, district, council, month, tested, positive, injected_error)

  n <- nrow(dat)
  idx1 <- sample(seq_len(n), size = max(5, round(0.01 * n)))
  idx2 <- sample(setdiff(seq_len(n), idx1), size = max(5, round(0.01 * n)))
  idx3 <- sample(setdiff(seq_len(n), c(idx1, idx2)), size = max(5, round(0.01 * n)))

  dat$positive[idx1] <- dat$tested[idx1] + sample(1:20, length(idx1), replace = TRUE)
  dat$tested[idx2] <- dat$tested[idx2] * sample(c(5, 6), length(idx2), replace = TRUE)
  dat$positive[idx3] <- 0

  dat$injected_error[c(idx1, idx2, idx3)] <- TRUE
  dplyr::as_tibble(dat)
}

#' Evaluate QC Against Truth
#'
#' Computes simple classification metrics comparing QC flags against known truth.
#'
#' @param data Data containing truth and QC flag columns.
#' @param truth_var Name of binary truth column.
#' @param flag_var Name of binary QC flag column.
#'
#' @return A one-row tibble with confusion matrix counts and metrics.
#' @export
evaluate_qc_against_truth <- function(data,
                                      truth_var = "injected_error",
                                      flag_var = "flag_any_qc_issue") {
  .validate_required_columns(data, c(truth_var, flag_var))

  truth <- as.logical(data[[truth_var]])
  flag <- as.logical(data[[flag_var]])

  tp <- sum(flag & truth, na.rm = TRUE)
  fp <- sum(flag & !truth, na.rm = TRUE)
  fn <- sum(!flag & truth, na.rm = TRUE)
  tn <- sum(!flag & !truth, na.rm = TRUE)

  precision <- .safe_divide(tp, tp + fp)
  recall <- .safe_divide(tp, tp + fn)
  specificity <- .safe_divide(tn, tn + fp)
  f1 <- .safe_divide(2 * precision * recall, precision + recall)

  dplyr::tibble(
    tp = tp,
    fp = fp,
    fn = fn,
    tn = tn,
    precision = precision,
    recall = recall,
    specificity = specificity,
    f1 = f1
  )
}

#' Plot Prevalence Residuals
#'
#' @param data A QC data frame with `pearson_resid` and `month_date`.
#'
#' @return A ggplot object.
#' @export
plot_prevalence_residuals <- function(data) {
  .validate_required_columns(data, c("month_date", "pearson_resid"))

  ggplot2::ggplot(data, ggplot2::aes(x = month_date, y = pearson_resid)) +
    ggplot2::geom_hline(yintercept = c(-4, 4), linetype = "dashed", color = "red") +
    ggplot2::geom_point(alpha = 0.5) +
    ggplot2::theme_minimal() +
    ggplot2::labs(x = "Month", y = "Pearson residual", title = "Prevalence residual diagnostics")
}

#' Plot Tested Robust Z
#'
#' @param data A QC data frame with `tested_robust_z` and `month_date`.
#'
#' @return A ggplot object.
#' @export
plot_tested_robust_z <- function(data) {
  .validate_required_columns(data, c("month_date", "tested_robust_z"))

  ggplot2::ggplot(data, ggplot2::aes(x = month_date, y = tested_robust_z)) +
    ggplot2::geom_hline(yintercept = c(-5, 5), linetype = "dashed", color = "red") +
    ggplot2::geom_point(alpha = 0.5) +
    ggplot2::theme_minimal() +
    ggplot2::labs(x = "Month", y = "Tested robust z", title = "Tested count robust-z diagnostics")
}

#' Plot Facility Time Series
#'
#' @param data A QC data frame.
#' @param facility_id Facility identifier value to filter.
#'
#' @return A ggplot object.
#' @export
plot_facility_timeseries <- function(data, facility_id) {
  .validate_required_columns(data, c("facility_id", "month_date", "prevalence"))

  dat <- dplyr::as_tibble(data) %>%
    dplyr::filter(.data$facility_id == facility_id)

  if (!"flag_any_qc_issue" %in% names(dat)) {
    dat$flag_any_qc_issue <- FALSE
  }

  ggplot2::ggplot(dat, ggplot2::aes(x = month_date, y = prevalence)) +
    ggplot2::geom_line(color = "#1f78b4") +
    ggplot2::geom_point(ggplot2::aes(color = flag_any_qc_issue)) +
    ggplot2::theme_minimal() +
    ggplot2::scale_color_manual(values = c("FALSE" = "#1f78b4", "TRUE" = "#e31a1c"), guide = "none") +
    ggplot2::labs(x = "Month", y = "Prevalence", title = paste("Facility", facility_id, "prevalence over time"))
}

#' Plot Before/After Prevalence
#'
#' @param data A QC data frame with `flag_remove_primary`.
#'
#' @return A ggplot object.
#' @export
plot_before_after_prevalence <- function(data) {
  .validate_required_columns(data, c("tested", "positive", "flag_remove_primary"))

  before <- .safe_divide(sum(data$positive, na.rm = TRUE), sum(data$tested, na.rm = TRUE))

  keep <- dplyr::as_tibble(data) %>%
    dplyr::filter(!flag_remove_primary)

  after <- .safe_divide(sum(keep$positive, na.rm = TRUE), sum(keep$tested, na.rm = TRUE))

  plot_dat <- dplyr::tibble(
    state = c("Before QC", "After primary QC"),
    prevalence = c(before, after)
  )

  ggplot2::ggplot(plot_dat, ggplot2::aes(x = state, y = prevalence, fill = state)) +
    ggplot2::geom_col(width = 0.6) +
    ggplot2::theme_minimal() +
    ggplot2::guides(fill = "none") +
    ggplot2::labs(x = NULL, y = "Prevalence", title = "Overall prevalence before and after primary QC")
}

#' Plot QC Summary By Region
#'
#' @param summary_data Output from [summarise_qc_by_region()].
#'
#' @return A ggplot object.
#' @export
plot_qc_summary_by_region <- function(summary_data) {
  .validate_required_columns(summary_data, c("region", "pct_any_qc_issue"))

  ggplot2::ggplot(summary_data, ggplot2::aes(x = stats::reorder(region, pct_any_qc_issue), y = pct_any_qc_issue)) +
    ggplot2::geom_col(fill = "#33a02c") +
    ggplot2::coord_flip() +
    ggplot2::theme_minimal() +
    ggplot2::labs(x = "Region", y = "% rows with any QC issue", title = "QC burden by region")
}

#' Run Routine QC Pipeline
#'
#' Main user-facing wrapper that prepares data, fits prevalence model, applies QC
#' flags, and returns flagged data plus standard summaries.
#'
#' @param raw_data Raw routine surveillance data.
#' @param facility_var Facility identifier column.
#' @param region_var Region column.
#' @param month_var Month/date column.
#' @param tested_var Tested count column.
#' @param positive_var Positive count column.
#' @param council_var Optional council column.
#' @param district_var Optional district column.
#' @param nthreads Threads passed to [fit_prevalence_gam()].
#' @param ... Additional arguments reserved for future extensions.
#'
#' @return A list with `data_flagged`, `model`, and `summaries`.
#' @export
run_routine_qc <- function(raw_data,
                           facility_var,
                           region_var,
                           month_var,
                           tested_var,
                           positive_var,
                           council_var = NULL,
                           district_var = NULL,
                           nthreads = 1,
                           ...) {
  prepared <- prepare_qc_data(
    data = raw_data,
    facility_var = facility_var,
    region_var = region_var,
    month_var = month_var,
    tested_var = tested_var,
    positive_var = positive_var,
    council_var = council_var,
    district_var = district_var
  )

  validate_qc_input(prepared)

  dat <- prepared %>%
    flag_logical_errors()

  model <- fit_prevalence_gam(dat, nthreads = nthreads)

  dat <- dat %>%
    add_prevalence_predictions(model = model) %>%
    add_prevalence_qc_flags() %>%
    add_tested_volume_qc() %>%
    add_temporal_qc_flags() %>%
    assign_qc_action()

  summaries <- list(by_region = summarise_qc_by_region(dat))

  if ("council" %in% names(dat)) {
    summaries$by_council <- summarise_qc_by_council(dat)
  }

  if ("district" %in% names(dat)) {
    summaries$by_district <- summarise_qc_by_district(dat)
  }

  summaries$by_month <- summarise_qc_by_month(dat)
  summaries$by_facility <- summarise_qc_by_facility(dat)

  list(
    data_flagged = dat,
    model = model,
    summaries = summaries
  )
}
