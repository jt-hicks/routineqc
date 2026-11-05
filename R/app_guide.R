# The QC guide: every flag, in plain language, with the thresholds actually
# used by the selected run. Definitions are generated from the run's
# configuration so that the guide cannot drift from what was applied.

.guide_config <- function(config) {
  defaults <- .qc_profile_defaults('recommended')
  defaulted <- character()
  for (section in c('prevalence', 'tested_volume', 'attendance', 'priority')) {
    have <- config[[section]]
    if (is.null(have)) {
      config[[section]] <- defaults[[section]]
      defaulted <- c(defaulted, section)
    } else if (section != 'priority') {
      missing <- setdiff(names(defaults[[section]]), names(have))
      if (length(missing) > 0L) {
        config[[section]] <- utils::modifyList(defaults[[section]], have)
        defaulted <- c(defaulted, section)
      }
    }
  }
  attr(config, 'defaulted_sections') <- defaulted
  config
}

.g <- function(x) format(x, trim = TRUE, digits = 4, scientific = FALSE)

.qc_flag_catalog <- function(config) {
  cfg <- .guide_config(config)
  p <- cfg$prevalence
  v <- cfg$tested_volume
  a <- cfg$attendance
  pr <- cfg$priority
  resid_rule <- if (!is.null(p$resid_threshold)) {
    paste0('|Pearson residual| above ', .g(p$resid_threshold))
  } else {
    paste0('|Pearson residual| above ', .g(p$resid_threshold_large), ' when tested is at least ',
           .g(p$resid_large_n), ', otherwise above ', .g(p$resid_threshold_small))
  }
  both <- if (isTRUE(v$require_both_extreme)) 'and' else 'or'
  row <- function(key, domain, label, definition, needs = 'Both counts and a fitted model') {
    dplyr::tibble(key = key, flag = paste0('flag_', key), domain = domain, label = label,
                  definition = definition, needs = needs)
  }
  catalog <- dplyr::bind_rows(
    row('positive_gt_tested', 'Count consistency', 'Positives exceed tested',
        'Positive count is larger than the tested count. Impossible.', 'Both counts'),
    row('zero_tested_positive', 'Count consistency', 'Positives with zero tested',
        'No one was tested but positives are reported. Impossible.', 'Both counts'),
    row('tested_negative', 'Count consistency', 'Negative tested count',
        'Tested count is below zero. Impossible.', 'Tested count'),
    row('positive_negative', 'Count consistency', 'Negative positive count',
        'Positive count is below zero. Impossible.', 'Positive count'),
    row('count_invalid_value', 'Count consistency', 'Invalid count value',
        paste('A count was entered as text that is not a number, as infinity, or as a fraction.',
              'The value is kept for review and takes no part in calculations.'), 'Count as entered'),
    row('resid_extreme', 'Prevalence', 'Extreme residual',
        paste0('Positives differ from the model expectation: ', resid_rule, '.'),
        'Both counts, a fitted model and enough history'),
    row('all_negative_large_n', 'Prevalence', 'All negative, large sample',
        paste0('No positives from at least ', .g(p$all_negative_min_tested),
               ' tests, when the model gives this a chance below ', .g(p$all_negative_max_probability),
               ' (a dispersion-adjusted heuristic pending calibration).'), 'Both counts and a fitted model'),
    row('all_positive_large_n', 'Prevalence', 'All positive, large sample',
        paste0('Every test positive from at least ', .g(p$all_positive_min_tested), ' tests.'),
        'Both counts'),
    row('large_monthly_prevalence_change', 'Prevalence', 'Large month-to-month change',
        paste0('Positivity moved by more than ', .g(100 * p$large_change_threshold),
               ' percentage points from the previous month, with at least ',
               .g(p$large_change_min_tested), ' tested in both months',
               if (isTRUE(p$large_change_require_adjacent)) ' and consecutive calendar months' else '',
               '. Not flagged when the only reason is that the previous month was an outlier.'),
        'Two adjacent months of both counts'),
    row('tested_extreme_high', 'Tested volume', 'Unusually high testing',
        paste0('At least ', .g(v$tested_high_min), ' tested, more than ', .g(v$tested_high_ratio),
               ' times the trailing ', .g(v$baseline_window_months), '-month median ', both,
               ' a robust z-score above ', .g(v$tested_z_threshold), '.'),
        'A trailing baseline of at least the minimum months'),
    row('tested_extreme_low', 'Tested volume', 'Unusually low testing',
        paste0('Trailing median of at least ', .g(v$baseline_low_min), ', and testing below ',
               .g(v$tested_low_ratio), ' times that median ', both, ' a robust z-score below -',
               .g(v$tested_z_threshold), '.'), 'A trailing baseline'),
    row('tested_zero_unusual', 'Tested volume', 'Zero tested after steady testing',
        paste0('Zero tested when the trailing median is at least ', .g(v$baseline_zero_min), '.'),
        'A trailing baseline'),
    row('tested_large_jump', 'Tested volume', 'Large jump from previous month',
        paste0('Tested at least ', .g(v$tested_jump_min), ' and more than ', .g(v$tested_jump_ratio),
               ' times the previous month, which was at least ', .g(v$previous_jump_min), '.'),
        'The previous month'),
    row('tested_large_drop', 'Tested volume', 'Large drop from previous month',
        paste0('Previous month at least ', .g(v$previous_drop_min), ' and testing below ',
               .g(v$tested_drop_ratio), ' times it.'), 'The previous month'),
    row('tested_gt_attending', 'Attendance', 'Tested exceeds attendance',
        'More people tested than attended. The attendance figure may be the problem.',
        'Tested count and attendance'),
    row('tested_gt_attending_major', 'Attendance', 'Tested far exceeds attendance',
        paste0('Tested is more than ', .g(a$excess_ratio), ' times attendance and at least ',
               .g(a$excess_min), ' above it.'), 'Tested count and attendance'),
    row('attending_negative', 'Attendance', 'Negative attendance',
        'Attendance is below zero.', 'Attendance'),
    row('coverage_low', 'Testing coverage', 'Low testing coverage',
        paste0('Tested is below ', .g(100 * a$coverage_low), '% of attendance, where attendance is at least ',
               .g(a$coverage_min_attending),
               if (!is.null(a$coverage_from)) paste0(' and from ', a$coverage_from) else '', '.'),
        'Tested count and attendance'),
    row('coverage_very_low', 'Testing coverage', 'Very low testing coverage',
        paste0('Tested is below ', .g(100 * a$coverage_very_low), '% of attendance, where attendance is at least ',
               .g(a$coverage_min_attending), '.'), 'Tested count and attendance')
  )
  catalog$priority <- vapply(catalog$key, function(k) {
    if (k %in% .qc_always_critical || k == 'count_invalid_value') 'critical'
    else if (!is.null(pr[[k]])) pr[[k]] else 'low'
  }, character(1))
  catalog$priority_fixed <- catalog$key %in% c(.qc_always_critical, 'count_invalid_value')
  catalog$counts_in_doubt <- vapply(catalog$key, function(k) {
    t <- .qc_flag_targets[[k]]
    if (is.null(t)) 'the invalid count only' else paste(t, collapse = ' and ')
  }, character(1))
  catalog$authorizes_exclusion <- catalog$key %in% .qc_always_critical
  catalog$domain <- factor(catalog$domain, levels = c('Count consistency', 'Prevalence', 'Tested volume',
                                                       'Attendance', 'Testing coverage'))
  attr(catalog, 'defaulted_sections') <- attr(cfg, 'defaulted_sections')
  catalog
}

.qc_modifier_notes <- function() {
  dplyr::tibble(
    flag = c('flag_isolated_extreme', 'flag_temporal_context_insufficient'),
    label = c('Isolated extreme', 'Not enough temporal context'),
    definition = c(
      'An extreme residual whose neighbouring months are both present, consecutive and not extreme.',
      'An extreme residual that lacks consecutive months on one side, so it cannot be called isolated.'
    ),
    effect = 'Context for reviewers. It does not change priority or the action.'
  )
}

.qc_flag_vs_action_text <- function() {
  list(
    flag = 'A flag is one rule firing on one record. A record can carry several flags.',
    priority = paste('Priority orders review. A record takes the highest priority among its flags,',
                     'and at least the multiple-signals priority when two or more domains are involved.',
                     'Priority never authorizes exclusion.'),
    action = paste('The action names where the evidence is: prevalence, tested volume, attendance,',
                   'coverage, several of them, or impossible counts. Only impossible counts',
                   '(critical, count-consistency flags) are authorized for exclusion; everything',
                   'else is review-only.'),
    unassessed = paste('A record a check could not assess (missing counts, too little history, no model)',
                       'is not a record that passed. The overview reports these separately.')
  )
}

.record_flags <- function(row, catalog) {
  active <- vapply(catalog$flag, function(f) {
    f %in% names(row) && isTRUE(as.logical(row[[f]][[1]]))
  }, logical(1))
  catalog[active, , drop = FALSE]
}

# A plain-language explanation of why one record was flagged, with the counts
# involved and the thresholds from the run.
.explain_record <- function(row, catalog) {
  row <- dplyr::as_tibble(row)[1, , drop = FALSE]
  fired <- .record_flags(row, catalog)
  get <- function(x) if (x %in% names(row)) row[[x]][[1]] else NA
  facts <- c(
    paste0('Facility ', get('facility_id'), ', ', format(get('month_date'), '%B %Y')),
    paste0('Tested ', get('tested'), ', positive ', get('positive'),
           if (!is.na(get('attending'))) paste0(', attendance ', get('attending')) else '')
  )
  action <- as.character(get('qc_action'))
  priority <- as.character(get('review_priority'))
  status <- as.character(get('prediction_status'))
  lines <- facts
  if (nrow(fired) == 0L) {
    lines <- c(lines, 'No flag fired on this record.')
    if (!is.na(status) && status != 'assessed') {
      lines <- c(lines, paste0('The model check did not assess it (', status,
                               '), so this is not evidence that the record is sound.'))
    }
  } else {
    for (i in seq_len(nrow(fired))) {
      lines <- c(lines, paste0(fired$label[[i]], ' (', fired$domain[[i]], ', ', fired$priority[[i]],
                               '): ', fired$definition[[i]],
                               ' Counts in doubt: ', fired$counts_in_doubt[[i]], '.'))
    }
    lines <- c(lines, paste0('Action: ', gsub('_', ' ', action), '. Review priority: ', priority, '.',
                             if (isTRUE(get('flag_exclude_authorized'))) {
                               ' Authorized for exclusion because the counts are impossible.'
                             } else ' Review only: nothing is excluded.'))
  }
  lines
}
