# QC details: what the checks found, how often, where and when. Everything on
# this tab describes the run as it was flagged. Nothing here depends on a filter
# strategy; what a strategy does with these flags belongs to Filter impact.
#
# Two units are used and never mixed. A *flag* is one rule firing on one
# record; a *record* is one facility-month. The flow from flags to priorities
# counts flags (a record with two flags contributes two). Everything else
# counts records: a record counts once towards a flag however it is combined.
#
# The page filters work on flags: a flag type, the flag's own priority and the
# priority of the record it is on. A record is in the selection when at least
# one of its flags passes every filter.

.details_priority_labels <- c(critical = 'Critical', high = 'High', medium = 'Medium', low = 'Low',
                              none = 'No priority')

.details_flag_template <- function() {
  dplyr::tibble(.source = integer(), key = character(), label = character(), domain = character(),
                flag_priority = character(), record_priority = character(), month_date = as.Date(character()),
                facility_id = character(), unit = character())
}

# One row per flag that fired: the record it fired on, the flag's configured
# priority and the priority its record ended up with after combining flags.
.details_flag_long <- function(data, catalog) {
  field <- .unit_field(data)
  rows <- lapply(seq_len(nrow(catalog)), function(i) {
    col <- catalog$flag[[i]]
    if (!col %in% names(data)) return(NULL)
    hit <- which(dplyr::coalesce(as.logical(data[[col]]), FALSE))
    if (length(hit) == 0L) return(NULL)
    dplyr::tibble(.source = hit, key = catalog$key[[i]], label = catalog$label[[i]],
                  domain = as.character(catalog$domain[[i]]), flag_priority = catalog$priority[[i]])
  })
  long <- dplyr::bind_rows(c(list(.details_flag_template()[0, 1:5]), rows))
  record_priority <- if ('review_priority' %in% names(data)) as.character(data$review_priority) else
    rep(NA_character_, nrow(data))
  record_priority[!record_priority %in% .qc_priority_levels] <- 'none'
  long$record_priority <- record_priority[long$.source]
  long$month_date <- data$month_date[long$.source]
  long$facility_id <- as.character(data$facility_id)[long$.source]
  long$unit <- as.character(data[[field]])[long$.source]
  attr(long, 'unit_field') <- field
  long
}

# Records carrying the selected flag, or any flag when none is selected.
.details_sources <- function(long, key = NULL) {
  if (is.null(key)) unique(long$.source) else unique(long$.source[long$key == key])
}

# Flags passing every filter that is set. NULL or '' means no filter.
.details_filter_mask <- function(long, key = NULL, flag_priority = NULL, record_priority = NULL) {
  set <- function(x) length(x) == 1L && !is.na(x) && nzchar(x)
  keep <- rep(TRUE, nrow(long))
  if (set(key)) keep <- keep & long$key == key
  if (set(flag_priority)) keep <- keep & long$flag_priority == flag_priority
  if (set(record_priority)) keep <- keep & long$record_priority == record_priority
  keep
}

.details_filter_long <- function(long, key = NULL, flag_priority = NULL, record_priority = NULL) {
  out <- long[.details_filter_mask(long, key, flag_priority, record_priority), , drop = FALSE]
  attr(out, 'unit_field') <- attr(long, 'unit_field')
  out
}

# How the current selection reads in a sentence: 'any flag', 'the flag "X"',
# 'a medium-priority flag on a high-priority record', and so on.
.details_phrase <- function(label = NULL, flag_priority = NULL, record_priority = NULL) {
  set <- function(x) length(x) == 1L && !is.na(x) && nzchar(x)
  lower <- function(p) tolower(.details_priority_labels[[p]])
  out <- if (set(label)) paste0('the flag "', label, '"') else
    if (set(flag_priority)) paste0('a ', lower(flag_priority), '-priority flag') else 'any flag'
  if (set(record_priority)) out <- paste0(out, ' on a ', lower(record_priority), '-priority record')
  out
}

.details_reason_summary <- function(long, n_records) {
  if (nrow(long) == 0L) {
    return(dplyr::tibble(key = character(), label = character(), flag_priority = character(),
                         records = integer(), share = double()))
  }
  long %>%
    dplyr::distinct(.source, key, label, flag_priority) %>%
    dplyr::count(key, label, flag_priority, name = 'records') %>%
    dplyr::mutate(share = .safe_divide(records, rep(n_records, dplyr::n()))) %>%
    dplyr::arrange(dplyr::desc(records), label)
}

# Run facts for the summary strip, as label = value.
.details_run_facts <- function(run) {
  data <- run$data_flagged
  field <- .unit_field(data)
  unit_label <- if (field == 'region') 'Regions' else paste0(.nav_mid_label(field), 's')
  units <- .nav_clean(data[[field]])
  lg <- function(col) sum(.lg(data, col))
  coverage <- run$manifest$model_prediction_coverage
  dataset <- tryCatch(.manifest_value(run$manifest$provenance, 'dataset_id'), error = function(e) NA_character_)
  facts <- c(
    Dataset = if (is.null(dataset) || is.na(dataset)) 'Not recorded' else as.character(dataset),
    `Reporting period` = paste(format(min(data$month_date), '%b %Y'), 'to', format(max(data$month_date), '%b %Y')),
    Records = format(nrow(data), big.mark = ','),
    Facilities = format(dplyr::n_distinct(data$facility_id), big.mark = ','),
    stats::setNames(format(length(unique(units)), big.mark = ','), unit_label),
    `Flagged for review` = sprintf('%s (%.1f%%)', format(lg('flag_review_recommended'), big.mark = ','),
                                   100 * .safe_divide(lg('flag_review_recommended'), nrow(data))),
    `Impossible values excluded` = format(lg('flag_exclude_authorized'), big.mark = ','),
    `Model coverage` = if (is.null(coverage) || is.na(coverage)) 'Not recorded' else sprintf('%.1f%%', 100 * coverage),
    Configuration = paste(c(run$manifest$config_profile, run$manifest$action_policy), collapse = ', ')
  )
  facts[nzchar(facts)]
}

.details_monthly <- function(data, long, key = NULL) {
  months <- .explore_months(data)
  levels <- as.character(months)
  src <- .details_sources(long, key)
  dplyr::tibble(
    month_date = months,
    records = as.integer(table(factor(as.character(data$month_date), levels = levels))),
    flagged = as.integer(table(factor(as.character(data$month_date[src]), levels = levels)))
  ) %>%
    dplyr::mutate(share = .safe_divide(flagged, records))
}

# Unit by month: the share of each unit's records carrying the flag. Units are
# ordered by how many flagged records they have; only the top ones are kept.
.details_unit_month <- function(data, long, key = NULL, max_units = 25L) {
  field <- .unit_field(data)
  unit <- as.character(data[[field]])
  keep <- !is.na(unit) & trimws(unit) != ''
  months <- .explore_months(data)
  units <- sort(unique(unit[keep]))
  flagged <- seq_len(nrow(data)) %in% .details_sources(long, key)
  u <- factor(unit[keep], levels = units)
  m <- factor(as.character(data$month_date[keep]), levels = as.character(months))
  records <- unclass(table(u, m))
  hits <- unclass(table(u[flagged[keep]], m[flagged[keep]]))
  totals <- rowSums(hits)
  keep_units <- order(-totals, units)
  keep_units <- utils::head(keep_units[totals[keep_units] > 0], max_units)
  records <- records[keep_units, , drop = FALSE]
  hits <- hits[keep_units, , drop = FALSE]
  share <- ifelse(records > 0, hits / pmax(records, 1), NA_real_)
  list(units = units[keep_units], months = months, records = records, flagged = hits, share = share,
       unit_field = field, units_total = length(units))
}

# How flagged records are spread across facilities: the share of them that
# the most-affected facilities account for. Facilities with none count too.
.details_concentration <- function(data, long, key = NULL, top = 0.05) {
  facilities <- unique(as.character(data$facility_id))
  src <- .details_sources(long, key)
  n <- sort(as.integer(table(factor(as.character(data$facility_id[src]), levels = facilities))),
            decreasing = TRUE)
  total <- sum(n)
  cum <- if (total > 0) cumsum(n) / total else rep(0, length(n))
  curve <- dplyr::tibble(share_facilities = c(0, seq_along(n) / length(n)), share_flagged = c(0, cum))
  # Read off the curve at exactly `top`, so the sentence matches the chart.
  top_share <- if (total > 0) stats::approx(curve$share_facilities, curve$share_flagged, xout = top)$y else NA_real_
  list(curve = curve, top = top, top_share = top_share,
       flagged_records = total, facilities = length(n), facilities_flagged = sum(n > 0))
}

# `focus` marks the flags in the current selection; the rest are drawn faded.
.details_flow <- function(long, focus = rep(TRUE, nrow(long))) {
  if (nrow(long) == 0L) {
    return(dplyr::tibble(label = character(), flag_priority = character(), record_priority = character(),
                         focus = logical(), flags = integer()))
  }
  long$focus <- focus
  dplyr::count(long, label, flag_priority, record_priority, focus, name = 'flags')
}

# Share of each unit's records in the selection, for the map.
.details_unit_share <- function(data, long, field) {
  unit <- as.character(data[[field]])
  keep <- !is.na(unit) & trimws(unit) != ''
  units <- sort(unique(unit[keep]))
  u <- factor(ifelse(keep, unit, NA_character_), levels = units)
  flagged <- seq_len(nrow(data)) %in% .details_sources(long)
  out <- dplyr::tibble(unit = units, records = as.integer(table(u)),
                       flagged_records = as.integer(tapply(flagged, u, sum)))
  out$share_flagged <- .safe_divide(out$flagged_records, out$records)
  out
}

.details_ranked_units <- function(data, long, key = NULL, persistent_months = 6L) {
  field <- .unit_field(data)
  unit <- as.character(data[[field]])
  keep <- !is.na(unit) & trimws(unit) != ''
  units <- sort(unique(unit[keep]))
  u <- factor(ifelse(keep, unit, NA_character_), levels = units)
  flagged <- seq_len(nrow(data)) %in% .details_sources(long, key)
  priority <- if ('review_priority' %in% names(data)) as.character(data$review_priority) else
    rep(NA_character_, nrow(data))
  severe <- flagged & priority %in% c('critical', 'high')
  per_unit <- function(x) as.integer(tapply(x, u, sum))
  out <- dplyr::tibble(unit = units)
  if (field != 'region' && 'region' %in% names(data)) {
    out$region <- as.character(tapply(as.character(data$region), u, function(x) {
      x <- .nav_clean(x)
      if (length(x) == 0L) NA_character_ else names(sort(table(x), decreasing = TRUE))[[1]]
    }))
  }
  out$facilities <- as.integer(tapply(as.character(data$facility_id), u, dplyr::n_distinct))
  out$records <- as.integer(table(u))
  out$flagged_records <- per_unit(flagged)
  out$share_flagged <- .safe_divide(out$flagged_records, out$records)
  out$critical_high_records <- per_unit(severe)
  # Persistence: facilities flagged in at least `persistent_months` distinct months.
  facility_months <- dplyr::distinct(dplyr::tibble(unit = unit, facility = as.character(data$facility_id),
                                                   month = data$month_date)[flagged & keep, ])
  persistent <- dplyr::count(facility_months, unit, facility, name = 'flagged_months') %>%
    dplyr::filter(flagged_months >= persistent_months) %>%
    dplyr::count(unit, name = 'persistent')
  out$persistent_facilities <- dplyr::coalesce(persistent$persistent[match(out$unit, persistent$unit)], 0L)
  attr(out, 'unit_field') <- field
  attr(out, 'persistent_months') <- persistent_months
  out[order(-out$flagged_records, -dplyr::coalesce(out$share_flagged, 0), out$unit), ]
}

# --- plots --------------------------------------------------------------------

.hex_alpha <- function(hex, alpha) {
  hex <- sub('^#', '', hex)
  rgb <- vapply(c(1L, 3L, 5L), function(i) strtoi(substr(hex, i, i + 1L), 16L), integer(1))
  sprintf('rgba(%d,%d,%d,%.2f)', rgb[[1]], rgb[[2]], rgb[[3]], alpha)
}

.details_empty_plot <- function(message) {
  plotly::plotly_empty(type = 'scatter', mode = 'markers') %>%
    plotly::layout(annotations = list(list(text = message, showarrow = FALSE, xref = 'paper', yref = 'paper',
                                           x = 0.5, y = 0.5)))
}

.plot_details_reasons <- function(summary, selected = NULL) {
  if (nrow(summary) == 0L) return(.details_empty_plot('No flags fired in this run.'))
  s <- summary[order(summary$records, decreasing = FALSE), ]
  plot <- plotly::plot_ly(source = 'details_reasons')
  for (p in .qc_priority_levels) {
    d <- s[s$flag_priority == p, , drop = FALSE]
    if (nrow(d) == 0L) next
    opacity <- if (is.null(selected)) 1 else ifelse(d$key == selected, 1, 0.3)
    plot <- plotly::add_bars(
      plot, x = d$records, y = d$label, key = d$key, orientation = 'h', name = .details_priority_labels[[p]],
      marker = list(color = .qc_priority_palette[[p]], opacity = opacity),
      text = paste0(d$label, '<br>', format(d$records, big.mark = ','), ' records (',
                    sprintf('%.1f%%', 100 * d$share), ' of all records)<br>Flag priority: ',
                    .details_priority_labels[[p]]),
      hoverinfo = 'text', textposition = 'none'
    )
  }
  plotly::layout(plot, barmode = 'overlay',
                 xaxis = list(title = 'Records with this flag', separatethousands = TRUE),
                 yaxis = list(title = '', categoryorder = 'array', categoryarray = s$label),
                 legend = list(orientation = 'h', x = 0, y = 1.02, yanchor = 'bottom',
                               title = list(text = 'Flag priority ')),
                 margin = list(l = 10, r = 10, t = 30)) %>%
    plotly::config(displayModeBar = FALSE)
}

.plot_details_monthly <- function(monthly, color = '#5f5e5a') {
  plotly::plot_ly() %>%
    plotly::add_bars(
      x = monthly$month_date, y = monthly$share, marker = list(color = color),
      text = paste0(format(monthly$month_date, '%b %Y'), '<br>', format(monthly$flagged, big.mark = ','), ' of ',
                    format(monthly$records, big.mark = ','), ' records (',
                    ifelse(is.na(monthly$share), 'NA', sprintf('%.1f%%', 100 * monthly$share)), ')'),
      hoverinfo = 'text', textposition = 'none'
    ) %>%
    plotly::layout(xaxis = list(title = ''), yaxis = list(title = 'Share of records', tickformat = '.1%',
                                                          rangemode = 'tozero'),
                   bargap = 0.15, showlegend = FALSE, margin = list(t = 10))
}

.plot_details_unit_month <- function(um) {
  if (length(um$units) == 0L) return(.details_empty_plot('No records with this flag.'))
  label <- .if_null(.nav_mid_label(if (um$unit_field == 'region') NULL else um$unit_field), 'Region')
  text <- matrix(paste0(
    rep(um$units, times = length(um$months)), ', ', rep(format(um$months, '%b %Y'), each = length(um$units)),
    '<br>', ifelse(um$records == 0L, 'No records',
                   paste0(um$flagged, ' of ', um$records, ' records flagged (',
                          sprintf('%.1f%%', 100 * um$share), ')'))
  ), nrow = length(um$units))
  plotly::plot_ly(
    x = um$months, y = um$units, z = um$share, text = text, type = 'heatmap', hoverinfo = 'text',
    colorscale = list(c(0, '#f7fbff'), c(1, '#b2182b')), zmin = 0, xgap = 1, ygap = 1,
    colorbar = list(title = list(text = 'Flagged'), tickformat = '.0%', thickness = 10)
  ) %>%
    plotly::layout(xaxis = list(title = ''),
                   yaxis = list(title = label, autorange = 'reversed', type = 'category'),
                   margin = list(t = 10))
}

.plot_details_concentration <- function(conc) {
  if (conc$flagged_records == 0L) return(.details_empty_plot('No records with this flag.'))
  c <- conc$curve
  plotly::plot_ly() %>%
    plotly::add_lines(x = c(0, 1), y = c(0, 1), line = list(color = '#bdbdbd', dash = 'dot', width = 1),
                      hoverinfo = 'skip', name = 'Even spread') %>%
    plotly::add_lines(
      x = c$share_facilities, y = c$share_flagged, line = list(color = '#185FA5', width = 2),
      name = 'Flagged records',
      text = sprintf('Top %.0f%% of facilities<br>%.0f%% of flagged records',
                     100 * c$share_facilities, 100 * c$share_flagged),
      hoverinfo = 'text'
    ) %>%
    plotly::layout(xaxis = list(title = 'Ranked proportion of facilities', tickformat = '.0%', range = c(0, 1)),
                   yaxis = list(title = 'Share of flagged records', tickformat = '.0%', range = c(0, 1.02)),
                   showlegend = FALSE, margin = list(t = 10))
}

# Flag -> the flag's configured priority -> the priority of the record it is
# on. The second step is where a record takes the highest priority among its
# flags and is raised when several domains fire together.
.plot_details_flow <- function(flow) {
  if (nrow(flow) == 0L) return(.details_empty_plot('No flags fired in this run.'))
  totals <- tapply(flow$flags, flow$label, sum)
  labels <- names(sort(totals, decreasing = TRUE))
  fp <- intersect(.qc_priority_levels, flow$flag_priority)
  rp <- intersect(c(.qc_priority_levels, 'none'), flow$record_priority)
  n_l <- length(labels)
  nodes <- c(labels, .details_priority_labels[fp], .details_priority_labels[rp])
  node_colors <- c(rep('#888780', n_l), unname(.qc_priority_palette[fp]), unname(.qc_priority_palette[rp]))
  id_fp <- function(p) n_l + match(p, fp) - 1L
  id_rp <- function(p) n_l + length(fp) + match(p, rp) - 1L
  if (!'focus' %in% names(flow)) flow$focus <- TRUE
  alpha <- function(on) ifelse(on, 0.45, 0.08)
  first <- stats::aggregate(flags ~ label + flag_priority + focus, data = flow, FUN = sum)
  second <- stats::aggregate(flags ~ flag_priority + record_priority + focus, data = flow, FUN = sum)
  source <- c(match(first$label, labels) - 1L, id_fp(second$flag_priority))
  target <- c(id_fp(first$flag_priority), id_rp(second$record_priority))
  value <- c(first$flags, second$flags)
  link_color <- c(
    vapply(seq_len(nrow(first)), function(i) .hex_alpha(.qc_priority_palette[[first$flag_priority[[i]]]],
                                                         alpha(first$focus[[i]])), character(1)),
    vapply(seq_len(nrow(second)), function(i) .hex_alpha(.qc_priority_palette[[second$record_priority[[i]]]],
                                                          alpha(second$focus[[i]])), character(1))
  )
  header <- function(x, text, anchor) list(x = x, y = 1.07, xref = 'paper', yref = 'paper', text = text,
                                           showarrow = FALSE, xanchor = anchor, font = list(size = 12))
  plotly::plot_ly(
    type = 'sankey', arrangement = 'snap',
    node = list(label = nodes, color = node_colors, pad = 10, thickness = 14, line = list(width = 0),
                hovertemplate = '%{label}<br>%{value:,} flags<extra></extra>'),
    link = list(source = source, target = target, value = value, color = link_color,
                hovertemplate = '%{source.label} to %{target.label}<br>%{value:,} flags<extra></extra>')
  ) %>%
    plotly::layout(margin = list(l = 10, r = 10, t = 40, b = 10),
                   annotations = list(header(0, 'Flag', 'left'), header(0.5, 'Flag priority', 'center'),
                                      header(1, 'Record priority', 'right')))
}

# --- map ----------------------------------------------------------------------

# Placeholder polygons until real boundary files are supplied (see
# `.dummy_region_sf()`); one rectangle per unit, in a grid.
.dummy_unit_sf <- function(units) {
  shp <- .dummy_region_sf(units)
  names(shp)[names(shp) == 'region'] <- 'unit'
  shp
}

.details_map <- function(shares) {
  shp <- merge(.dummy_unit_sf(shares$unit), shares, by = 'unit', all.x = TRUE)
  if (nrow(shp) == 0L) return(leaflet::leaflet())
  shp$pct <- 100 * dplyr::coalesce(shp$share_flagged, 0)
  pal <- leaflet::colorNumeric(c('#fff7bc', '#fe9929', '#d95f0e', '#993404'),
                               domain = c(0, max(1, shp$pct)), na.color = '#cccccc')
  bb <- sf::st_bbox(shp)
  leaflet::leaflet(shp, options = leaflet::leafletOptions(zoomControl = TRUE, attributionControl = FALSE)) %>%
    leaflet::fitBounds(bb[['xmin']], bb[['ymin']], bb[['xmax']], bb[['ymax']]) %>%
    leaflet::addPolygons(
      fillColor = ~pal(pct), fillOpacity = 0.85, color = '#ffffff', weight = 1.5,
      highlightOptions = leaflet::highlightOptions(color = '#17363c', weight = 3, bringToFront = TRUE),
      label = ~sprintf('%s: %s of %s records (%.1f%%)', unit, format(flagged_records, big.mark = ','),
                       format(records, big.mark = ','), pct)
    ) %>%
    leaflet::addLegend(pal = pal, values = ~pct, opacity = 0.9, title = '% of records', position = 'bottomright',
                       labFormat = leaflet::labelFormat(suffix = '%'))
}

# --- UI -----------------------------------------------------------------------

.qc_card <- function(title, ..., sub = NULL) {
  shiny::tags$section(
    class = 'qc-landing-card',
    if (inherits(title, 'shiny.tag')) title else shiny::tags$h4(title),
    if (!is.null(sub)) shiny::tags$p(class = 'qc-card-sub', sub),
    ...
  )
}

.details_ui <- function() {
  priorities <- c('All' = '', stats::setNames(.qc_priority_levels, .details_priority_labels[.qc_priority_levels]))
  shiny::tagList(
    shiny::tags$style(shiny::HTML('
      .qc-card-sub { font-size: 12px; color: #5f6b6e; margin: -6px 0 8px; }
      .qc-run-strip { display: flex; flex-wrap: wrap; gap: 4px 18px; font-size: 12px; color: #485b62;
        background: #f3f6f7; border-radius: 6px; padding: 7px 12px; margin: 12px 0 4px; }
      .qc-run-strip b { font-weight: 600; color: #17363c; }
      .qc-details-filters { display: flex; flex-wrap: wrap; align-items: flex-end; gap: 0 16px; margin-top: 12px; }
      .qc-details-filters .form-group { margin-bottom: 4px; min-width: 200px; }
      .qc-details-filters a { margin-bottom: 12px; font-size: 13px; }
      details.qc-landing-card > summary { cursor: pointer; font-size: 16px; color: #17363c; }
    ')),
    shiny::tags$div(
      class = 'qc-details-filters',
      shiny::selectInput('dt_flag', 'Flag', choices = c('All flags' = ''), width = '280px'),
      shiny::selectInput('dt_flag_priority', 'Flag priority', choices = priorities, width = '160px'),
      shiny::selectInput('dt_record_priority', 'Record priority', choices = priorities, width = '160px'),
      shiny::actionLink('dt_clear', 'Clear filters')
    ),
    shiny::fluidRow(
      shiny::column(7, .qc_card(
        'Most common QC flags',
        sub = 'Click a bar to focus the page on that flag.',
        plotly::plotlyOutput('dt_reasons', height = '380px')
      )),
      shiny::column(5, .qc_card(
        'Flagged records over time',
        plotly::plotlyOutput('dt_when', height = '400px')
      ))
    ),
    shiny::fluidRow(
      shiny::column(7, .qc_card(
        'How flags become priorities',
        sub = paste('A record can have multiple flags. A record\'s review priority is based on the highest',
                    'priority among its flags, but can be escalated if certain problems occur together.'),
        plotly::plotlyOutput('dt_flow', height = '420px')
      )),
      shiny::column(5, .qc_card(
        'Flagged records by area',
        sub = shiny::textOutput('dt_map_sub', inline = TRUE),
        shiny::uiOutput('dt_map_level_ui'),
        leaflet::leafletOutput('dt_map', height = '360px')
      ))
    ),
    shiny::fluidRow(
      shiny::column(8, .qc_card(
        'Where and when',
        sub = shiny::textOutput('dt_where_sub', inline = TRUE),
        plotly::plotlyOutput('dt_unit_month', height = '480px')
      )),
      shiny::column(4, .qc_card(
        'How concentrated',
        sub = shiny::textOutput('dt_concentration_sub', inline = TRUE),
        plotly::plotlyOutput('dt_concentration', height = '400px')
      ))
    ),
    .qc_card(
      shiny::tags$h4(shiny::textOutput('dt_ranked_title', inline = TRUE)),
      sub = shiny::textOutput('dt_ranked_sub', inline = TRUE),
      DT::DTOutput('dt_ranked')
    ),
    shiny::tags$details(
      class = 'qc-landing-card',
      # Shiny suspends outputs it cannot see; tell it when the section opens.
      ontoggle = "if (this.open) { $(this).trigger('shown'); window.dispatchEvent(new Event('resize')); }",
      shiny::tags$summary('What each check could assess'),
      shiny::tags$p(class = 'qc-card-sub', style = 'margin-top: 8px;',
                    'A record a check could not assess (missing counts, too little history, no model) is not a record that passed it.'),
      plotly::plotlyOutput('dt_assessment', height = '300px')
    ),
    shiny::uiOutput('dt_run_strip')
  )
}

# --- server -------------------------------------------------------------------

# `open_unit(field, unit)` moves Explore data to a unit and shows it.
.details_server <- function(input, output, session, selected_run, data_r, catalog_r, open_unit = NULL) {
  one <- function(x) if (length(x) == 0L || is.na(x[[1]]) || !nzchar(x[[1]])) NULL else x[[1]]
  long_r <- shiny::reactive(.details_flag_long(data_r(), catalog_r()))
  key_r <- shiny::reactive(one(input$dt_flag))
  fp_r <- shiny::reactive(one(input$dt_flag_priority))
  rp_r <- shiny::reactive(one(input$dt_record_priority))
  mask_r <- shiny::reactive(.details_filter_mask(long_r(), key_r(), fp_r(), rp_r()))
  filtered_r <- shiny::reactive({
    out <- long_r()[mask_r(), , drop = FALSE]
    attr(out, 'unit_field') <- attr(long_r(), 'unit_field')
    out
  })
  label_r <- shiny::reactive({
    k <- key_r()
    if (is.null(k)) NULL else one(long_r()$label[match(k, long_r()$key)])
  })
  filtered_any_r <- shiny::reactive(!is.null(key_r()) || !is.null(fp_r()) || !is.null(rp_r()))
  phrase_r <- shiny::reactive(.details_phrase(label_r(), fp_r(), rp_r()))
  unit_word_r <- shiny::reactive({
    field <- .unit_field(data_r())
    if (field == 'region') 'region' else tolower(.nav_mid_label(field))
  })

  # New run: offer its flags and clear every filter.
  shiny::observeEvent(long_r(), {
    rs <- .details_reason_summary(long_r(), nrow(data_r()))
    shiny::updateSelectInput(session, 'dt_flag', choices = c('All flags' = '', stats::setNames(rs$key, rs$label)),
                             selected = '')
    shiny::updateSelectInput(session, 'dt_flag_priority', selected = '')
    shiny::updateSelectInput(session, 'dt_record_priority', selected = '')
  })
  shiny::observeEvent(input$dt_clear, {
    for (id in c('dt_flag', 'dt_flag_priority', 'dt_record_priority')) shiny::updateSelectInput(session, id, selected = '')
  })

  # The flag chart follows the priority filters; the chosen flag is highlighted.
  output$dt_reasons <- plotly::renderPlotly({
    shown <- .details_filter_long(long_r(), NULL, fp_r(), rp_r())
    p <- .plot_details_reasons(.details_reason_summary(shown, nrow(data_r())), key_r())
    plotly::event_register(p, 'plotly_click')
  })
  shiny::observeEvent(plotly::event_data('plotly_click', source = 'details_reasons', priority = 'event'), {
    click <- plotly::event_data('plotly_click', source = 'details_reasons', priority = 'event')
    key <- if (!is.null(click$key)) as.character(click$key[[1]]) else NULL
    if (is.null(key) || !nzchar(key)) return()
    shiny::updateSelectInput(session, 'dt_flag', selected = if (identical(key_r(), key)) '' else key)
  })

  output$dt_when <- plotly::renderPlotly({
    color <- if (!is.null(key_r())) .qc_priority_palette[[long_r()$flag_priority[match(key_r(), long_r()$key)]]]
      else if (!is.null(fp_r())) .qc_priority_palette[[fp_r()]]
      else if (!is.null(rp_r())) .qc_priority_palette[[rp_r()]]
      else '#5f5e5a'
    .plot_details_monthly(.details_monthly(data_r(), filtered_r()), color)
  })

  output$dt_flow <- plotly::renderPlotly(.plot_details_flow(.details_flow(long_r(), mask_r())))

  map_field_r <- shiny::reactive({
    mid <- .nav_mid_field(data_r())
    if (identical(input$dt_map_level, 'mid') && !is.null(mid)) mid else 'region'
  })
  output$dt_map_level_ui <- shiny::renderUI({
    mid <- .nav_mid_field(data_r())
    if (is.null(mid) || !'region' %in% names(data_r())) return(NULL)
    shiny::radioButtons('dt_map_level', NULL, inline = TRUE, selected = shiny::isolate(input$dt_map_level),
                        choices = stats::setNames(c('region', 'mid'), c('Region', .nav_mid_label(mid))))
  })
  output$dt_map_sub <- shiny::renderText(paste0('Share of records with ', phrase_r(), '.'))
  output$dt_map <- leaflet::renderLeaflet(.details_map(.details_unit_share(data_r(), filtered_r(), map_field_r())))

  unit_month_r <- shiny::reactive(.details_unit_month(data_r(), filtered_r()))
  output$dt_where_sub <- shiny::renderText(
    paste0('Share of records in each ', unit_word_r(), ' with ', phrase_r(), ', by month.')
  )
  output$dt_unit_month <- plotly::renderPlotly(.plot_details_unit_month(unit_month_r()))

  concentration_r <- shiny::reactive(.details_concentration(data_r(), filtered_r()))
  output$dt_concentration_sub <- shiny::renderText({
    c <- concentration_r()
    if (c$flagged_records == 0L) return(paste0('No records with ', phrase_r(), '.'))
    sprintf('The top %.0f%% of facilities with the most records with %s account for %.0f%% of %s.',
            100 * c$top, phrase_r(), 100 * c$top_share,
            if (filtered_any_r()) 'those records' else 'all flagged records')
  })
  output$dt_concentration <- plotly::renderPlotly(.plot_details_concentration(concentration_r()))

  ranked_r <- shiny::reactive(.details_ranked_units(data_r(), filtered_r()))
  output$dt_ranked_title <- shiny::renderText({
    field <- attr(ranked_r(), 'unit_field')
    paste(if (field == 'region') 'Regions' else paste0(.nav_mid_label(field), 's'), 'ranked by flagged records')
  })
  output$dt_ranked_sub <- shiny::renderText(paste0(
    'Records with ', phrase_r(), '. Persistent facilities had such a record in at least ',
    attr(ranked_r(), 'persistent_months'), ' different months. ',
    if (!is.null(open_unit)) paste0('Select a row to open that ', unit_word_r(), ' in Explore data.') else ''
  ))
  output$dt_ranked <- DT::renderDT({
    r <- ranked_r()
    field <- attr(r, 'unit_field')
    names(r)[names(r) == 'unit'] <- if (field == 'region') 'Region' else .nav_mid_label(field)
    show <- dplyr::rename(
      r, dplyr::any_of(c(Region = 'region')), Facilities = facilities, Records = records,
      `Flagged records` = flagged_records, `Share flagged` = share_flagged,
      `Critical / high records` = critical_high_records, `Persistent facilities` = persistent_facilities
    )
    DT::datatable(show, rownames = FALSE, selection = 'single',
                  options = list(pageLength = 10, order = list())) %>%
      DT::formatPercentage('Share flagged', 1) %>%
      DT::formatRound(c('Records', 'Flagged records'), 0)
  })
  shiny::observeEvent(input$dt_ranked_rows_selected, {
    r <- ranked_r()
    i <- input$dt_ranked_rows_selected
    if (is.null(open_unit) || length(i) != 1L || i > nrow(r)) return()
    open_unit(attr(r, 'unit_field'), r$unit[[i]])
  })

  output$dt_assessment <- plotly::renderPlotly(.plot_assessment(.assessment_summary(data_r())))

  output$dt_run_strip <- shiny::renderUI({
    facts <- .details_run_facts(selected_run())
    shiny::tags$div(class = 'qc-run-strip', lapply(names(facts), function(k) {
      shiny::tags$span(paste0(k, ': '), shiny::tags$b(facts[[k]]))
    }))
  })

  invisible(NULL)
}
