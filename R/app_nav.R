# Navigation and shared presentation helpers for the dashboard. Everything
# here is a pure function of data and a selection, so the cascading behaviour
# is testable without a running Shiny session.
#
# A selection is list(region, mid, facility): the geographical path from the
# national level down. `mid` is the intermediate administrative level between
# region and facility: the `council` field when the run has one, otherwise the
# `district` field. The two are never merged, and a run with neither goes
# straight from region to facility.

.nav_empty <- function() list(region = NULL, mid = NULL, facility = NULL)

.nav_clean <- function(x) {
  x <- as.character(x)
  x[!is.na(x) & trimws(x) != '']
}

.nav_mid_field <- function(data) {
  for (field in c('council', 'district')) {
    if (field %in% names(data) && length(.nav_clean(data[[field]])) > 0L) return(field)
  }
  NULL
}

.nav_mid_label <- function(field) {
  if (is.null(field)) NA_character_ else switch(field, council = 'Council', district = 'District', field)
}

.nav_hierarchy <- function(data) {
  mid_field <- .nav_mid_field(data)
  out <- dplyr::tibble(
    region = if ('region' %in% names(data)) as.character(data$region) else NA_character_,
    mid = if (is.null(mid_field)) NA_character_ else as.character(data[[mid_field]]),
    facility = as.character(data$facility_id)
  )
  out$mid[trimws(out$mid) == ''] <- NA_character_
  # A facility sits in one place; if the data disagree, the most recent row decides.
  out <- out[order(data$month_date), ]
  out <- out[!duplicated(out$facility, fromLast = TRUE), ]
  out[order(out$region, out$mid, out$facility), ]
}

.nav_level <- function(selection) {
  if (!is.null(selection$facility)) 'facility'
  else if (!is.null(selection$mid)) 'mid'
  else if (!is.null(selection$region)) 'region'
  else 'national'
}

.nav_normalise <- function(selection) {
  one <- function(x) {
    x <- .nav_clean(x)
    if (length(x) == 0L) NULL else x[[1]]
  }
  list(region = one(selection$region), mid = one(selection$mid), facility = one(selection$facility))
}

# Choices at each level that are consistent with the selection above it.
.nav_choices <- function(data, selection) {
  hier <- .nav_hierarchy(data)
  selection <- .nav_normalise(selection)
  in_region <- if (is.null(selection$region)) rep(TRUE, nrow(hier)) else hier$region %in% selection$region
  in_mid <- if (is.null(selection$mid)) rep(TRUE, nrow(hier)) else hier$mid %in% selection$mid
  list(
    region = sort(unique(.nav_clean(hier$region))),
    mid = sort(unique(.nav_clean(hier$mid[in_region]))),
    facility = sort(unique(hier$facility[in_region & in_mid]))
  )
}

# Sets one level. Choosing a level clears everything below it, and choosing a
# lower level fills in the levels above it from the hierarchy, so a facility
# picked from a long list always sits under the right council and region.
.nav_set <- function(data, selection, level = c('region', 'mid', 'facility'), value = NULL) {
  level <- match.arg(level)
  hier <- .nav_hierarchy(data)
  selection <- .nav_normalise(selection)
  value <- .nav_clean(value)
  value <- if (length(value) == 0L) NULL else value[[1]]
  selection[[level]] <- value
  if (level == 'region') {
    selection$mid <- NULL
    selection$facility <- NULL
  } else if (level == 'mid') {
    selection$facility <- NULL
    if (!is.null(value)) {
      parent <- unique(.nav_clean(hier$region[hier$mid %in% value]))
      if (length(parent) >= 1L) selection$region <- parent[[1]]
    }
  } else if (!is.null(value)) {
    row <- hier[hier$facility %in% value, , drop = FALSE]
    if (nrow(row) > 0L) {
      selection$mid <- if (is.na(row$mid[[1]])) NULL else row$mid[[1]]
      selection$region <- if (is.na(row$region[[1]])) NULL else row$region[[1]]
    }
  }
  .nav_validate(data, selection)
}

# Drops any part of a selection the data no longer support (for example after
# loading a different run), together with everything beneath it.
.nav_validate <- function(data, selection) {
  hier <- .nav_hierarchy(data)
  selection <- .nav_normalise(selection)
  if (!is.null(selection$region) && !selection$region %in% hier$region) selection <- .nav_empty()
  if (!is.null(selection$mid) &&
      !selection$mid %in% hier$mid[hier$region %in% selection$region | is.null(selection$region)]) {
    selection$mid <- NULL
    selection$facility <- NULL
  }
  if (!is.null(selection$facility)) {
    ok <- hier$facility %in% selection$facility
    if (!is.null(selection$region)) ok <- ok & hier$region %in% selection$region
    if (!is.null(selection$mid)) ok <- ok & hier$mid %in% selection$mid
    if (!any(ok)) selection$facility <- NULL
  }
  selection
}

.nav_subset <- function(data, selection) {
  selection <- .nav_normalise(selection)
  mid_field <- .nav_mid_field(data)
  keep <- rep(TRUE, nrow(data))
  if (!is.null(selection$region)) keep <- keep & as.character(data$region) %in% selection$region
  if (!is.null(selection$mid) && !is.null(mid_field)) {
    keep <- keep & as.character(data[[mid_field]]) %in% selection$mid
  }
  if (!is.null(selection$facility)) keep <- keep & as.character(data$facility_id) %in% selection$facility
  data[keep, , drop = FALSE]
}

.nav_breadcrumb <- function(selection, mid_field = NULL) {
  selection <- .nav_normalise(selection)
  parts <- c('National', selection$region, selection$mid, selection$facility)
  paste(parts, collapse = ' > ')
}

# The unit type shown one step down from the current selection.
.nav_child_level <- function(selection, mid_field = NULL) {
  switch(
    .nav_level(selection),
    national = 'region',
    region = if (is.null(mid_field)) 'facility' else 'mid',
    mid = 'facility',
    facility = NA_character_
  )
}

# --- shared presentation ------------------------------------------------------

.qc_priority_palette <- c(critical = '#b2182b', high = '#e66101', medium = '#d4a017',
                          low = '#5e81ac', none = '#9aa0a6')

.explore_metrics <- list(
  positivity = list(label = 'Positivity among those tested', short = 'Positivity',
                    column = 'positivity', percent = TRUE,
                    help = 'Positive tests as a share of tests, over records with both counts valid.'),
  tested = list(label = 'Number tested', short = 'Tested', column = 'tested_total', percent = FALSE,
                help = 'Sum of valid tested counts.'),
  attendance = list(label = 'Attendance (denominator)', short = 'Attendance',
                    column = 'attendance_total', percent = FALSE,
                    help = 'Sum of attendance counts that have an approved definition.'),
  coverage = list(label = 'Testing coverage', short = 'Coverage', column = 'coverage', percent = TRUE,
                  help = 'Tested over attendance, over records where both are usable.')
)

.explore_metric_labels <- function() {
  stats::setNames(names(.explore_metrics), vapply(.explore_metrics, `[[`, character(1), 'label'))
}
