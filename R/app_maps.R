.validate_qc_boundaries <- function(boundaries) {
  if (is.null(boundaries)) return(list())
  fields <- c('region', 'district', 'council')
  if (!is.list(boundaries) || inherits(boundaries, 'sf') ||
      is.null(names(boundaries)) || any(!names(boundaries) %in% fields) ||
      anyDuplicated(names(boundaries))) {
    rlang::abort('`boundaries` must be a named list of region, district and/or council sf layers.')
  }
  for (field in names(boundaries)) {
    layer <- boundaries[[field]]
    if (!inherits(layer, 'sf') || nrow(layer) == 0L || !field %in% names(layer)) {
      rlang::abort(paste0('Boundary layer "', field, '" must be a non-empty sf layer with a "', field, '" column.'))
    }
    keys <- trimws(as.character(layer[[field]]))
    if (anyNA(keys) || any(!nzchar(keys)) || anyDuplicated(keys)) {
      rlang::abort(paste0('Boundary layer "', field, '" must have unique, non-missing unit names.'))
    }
    if (is.na(sf::st_crs(layer)) || any(sf::st_is_empty(layer)) ||
        any(!sf::st_geometry_type(layer) %in% c('POLYGON', 'MULTIPOLYGON')) ||
        !all(sf::st_is_valid(layer) %in% TRUE)) {
      rlang::abort(paste0('Boundary layer "', field, '" requires a defined CRS and valid, non-empty polygons.'))
    }
    layer[[field]] <- keys
    boundaries[[field]] <- sf::st_transform(layer, 4326)
  }
  boundaries
}

.qc_map_data <- function(boundaries, field, shares) {
  layer <- boundaries[[field]]
  if (is.null(layer)) return(NULL)
  layer$unit <- as.character(layer[[field]])
  at <- match(layer$unit, shares$unit)
  layer$records <- shares$records[at]
  layer$flagged_records <- shares$flagged_records[at]
  layer$pct <- 100 * shares$share_flagged[at]
  attr(layer, 'unmatched_units') <- setdiff(shares$unit, layer$unit)
  layer
}

.plot_qc_map <- function(layer, field, measure) {
  options <- leaflet::leafletOptions(zoomControl = TRUE, attributionControl = FALSE)
  if (is.null(layer)) {
    return(leaflet::addControl(
      leaflet::leaflet(options = options),
      as.character(shiny::tags$div(paste0('Boundary data not supplied for ', field, '.'))),
      position = 'topleft'
    ))
  }
  pal <- leaflet::colorNumeric(c('#fff7bc', '#fe9929', '#d95f0e', '#993404'),
                               domain = c(0, 100), na.color = '#cccccc')
  bb <- sf::st_bbox(layer)
  labels <- ifelse(
    is.na(layer$records),
    paste0(layer$unit, ': No observations'),
    sprintf('%s: %s of %s records %s (%.1f%%)', layer$unit,
            format(layer$flagged_records, big.mark = ','), format(layer$records, big.mark = ','),
            measure, layer$pct)
  )
  map <- leaflet::leaflet(layer, options = options) %>%
    leaflet::fitBounds(bb[['xmin']], bb[['ymin']], bb[['xmax']], bb[['ymax']]) %>%
    leaflet::addPolygons(
      fillColor = pal(layer$pct), fillOpacity = 0.85, color = '#ffffff', weight = 1,
      layerId = layer$unit, label = labels,
      highlightOptions = leaflet::highlightOptions(color = '#17363c', weight = 3, bringToFront = TRUE)
    ) %>%
    leaflet::addLegend(pal = pal, values = c(0, 100, layer$pct), opacity = 0.9, title = '% of records',
                       position = 'bottomright', na.label = 'No observations',
                       className = 'info legend qc-map-legend',
                       labFormat = leaflet::labelFormat(suffix = '%'))
  unmatched <- attr(layer, 'unmatched_units')
  if (length(unmatched) > 0L) {
    map <- leaflet::addControl(
      map, as.character(shiny::tags$div(paste0('No matching boundary: ', paste(unmatched, collapse = ', ')))),
      position = 'bottomleft'
    )
  }
  map
}
