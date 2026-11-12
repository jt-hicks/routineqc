map_test_boundaries <- function(field = 'region', units = c('R1', 'R2', 'R3')) {
  polygons <- lapply(seq_along(units), function(i) {
    sf::st_polygon(list(matrix(c(30 + i, -8, 30.5 + i, -8, 30.5 + i, -7.5,
                                30 + i, -7.5, 30 + i, -8), ncol = 2, byrow = TRUE)))
  })
  layer <- sf::st_sf(unit = units, geometry = sf::st_sfc(polygons, crs = 4326))
  names(layer)[1] <- field
  stats::setNames(list(layer), field)
}

test_that('boundary layers validate identities and geometry and transform projected coordinates', {
  source <- map_test_boundaries()
  projected <- lapply(source, sf::st_transform, crs = 3857)
  result <- .validate_qc_boundaries(projected)
  expect_equal(sf::st_coordinates(result$region), sf::st_coordinates(source$region), tolerance = 1e-7)
  expect_error(.validate_qc_boundaries(source$region), 'named list')
  bad <- source
  bad$region$region <- c('R1', ' R1 ', 'R3')
  expect_error(.validate_qc_boundaries(bad), 'unique, non-missing')
  bad$region$region[2] <- NA
  expect_error(.validate_qc_boundaries(bad), 'unique, non-missing')
  bad <- source
  sf::st_crs(bad$region) <- NA
  expect_error(.validate_qc_boundaries(bad), 'defined CRS')
  bad <- source
  sf::st_geometry(bad$region)[[1]] <- sf::st_polygon(list(
    matrix(c(31, -8, 32, -7, 32, -8, 31, -7, 31, -8), ncol = 2, byrow = TRUE)
  ))
  expect_error(.validate_qc_boundaries(bad), 'valid, non-empty polygons')
})

test_that('maps distinguish zero flags from no observations and identify unmatched units', {
  boundaries <- .validate_qc_boundaries(map_test_boundaries())
  shares <- tibble::tibble(unit = c('R1', 'R2', 'Unmatched'), records = 10L,
                           flagged_records = c(0L, 5L, 1L), share_flagged = c(0, 0.5, 0.1))
  layer <- .qc_map_data(boundaries, 'region', shares)
  expect_equal(layer$pct, c(0, 50, NA))
  expect_equal(layer$records, c(10L, 10L, NA_integer_))
  expect_identical(attr(layer, 'unmatched_units'), 'Unmatched')
  map <- .plot_qc_map(layer, 'region', 'flagged')
  serialized <- jsonlite::toJSON(map$x$calls, auto_unbox = TRUE)
  expect_match(serialized, 'R3: No observations', fixed = TRUE)
  expect_match(serialized, 'No matching boundary: Unmatched', fixed = TRUE)
  expect_match(serialized, '#cccccc', fixed = TRUE)
  legend <- Filter(function(x) x$method == 'addLegend', map$x$calls)[[1]]$args[[1]]
  expect_equal(range(as.numeric(sub('%', '', legend$labels, fixed = TRUE))), c(0, 100))
  missing <- .details_map(shares)
  expect_match(jsonlite::toJSON(missing$x$calls), 'Boundary data not supplied')
  expect_false(any(vapply(missing$x$calls, function(x) x$method == 'addPolygons', logical(1))))
})

test_that('QC details uses the supplied layer for the selected geographic level', {
  boundaries <- map_test_boundaries('district', c('D1', 'D2'))
  shares <- tibble::tibble(unit = 'D1', records = 10L, flagged_records = 2L, share_flagged = 0.2)
  expect_match(jsonlite::toJSON(.details_map(shares, boundaries, 'district')$x$calls), 'D2: No observations')
  expect_match(jsonlite::toJSON(.details_map(shares, boundaries, 'region')$x$calls), 'Boundary data not supplied')
})
