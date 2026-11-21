# Classify already-noded line pieces without intersecting them back with their
# source linework. Exact line-on-line overlays can lose interpolated segments
# through floating-point collinearity differences.
classify_network_sources <- function(noded_sfc, source_lines_sf, tolerance = 1e-5) {
  if (!requireNamespace("sf", quietly = TRUE)) stop("Install the sf package")
  if (length(tolerance) != 1L || !is.finite(tolerance) || tolerance < 0) {
    stop("Source classification tolerance must be one finite, nonnegative number")
  }
  geometry <- sf::st_geometry(noded_sfc)
  answer <- data.frame(source_class = rep(NA_character_, length(geometry)),
    measure_habitat = rep(FALSE, length(geometry)),
    bridge_id = rep(NA_character_, length(geometry)), stringsAsFactors = FALSE)
  if (!length(geometry)) return(answer)
  if (!inherits(source_lines_sf, "sf") || !"source_class" %in% names(source_lines_sf)) {
    stop("source_lines_sf must be sf with a source_class column")
  }
  if (is.na(sf::st_crs(geometry)) || isTRUE(sf::st_is_longlat(geometry)) ||
      !sf::st_crs(geometry)$units_gdal %in% c("metre", "meter", "m")) {
    stop("Source classification requires a projected CRS with meter units")
  }
  if (is.na(sf::st_crs(source_lines_sf))) stop("Source lines must have a CRS")
  if (any(sf::st_geometry_type(geometry) != "LINESTRING") ||
      any(sf::st_is_empty(geometry))) {
    stop("noded_sfc must contain nonempty LINESTRING pieces")
  }
  sources <- sf::st_transform(source_lines_sf, sf::st_crs(geometry))
  sources <- sources[!sf::st_is_empty(sources), ]
  if (!nrow(sources) || any(!sf::st_geometry_type(sources) %in%
                            c("LINESTRING", "MULTILINESTRING"))) {
    stop("Source lines must contain nonempty line geometries")
  }
  priority <- c("prediction", "swifd_connector", "alignment_link")
  if (anyNA(sources$source_class) || any(!sources$source_class %in% priority)) {
    stop("Unsupported source_class; use prediction, swifd_connector, or alignment_link")
  }
  if (!"bridge_id" %in% names(sources)) sources$bridge_id <- NA_character_
  sources$bridge_id <- as.character(sources$bridge_id)

  # Each noded edge has uniform source support. Use its half-length point,
  # rather than a centroid, which can lie outside a curved stream line.
  midpoints <- lapply(geometry, function(line) {
    xy <- unclass(line)[, 1:2, drop = FALSE]
    segment_length <- sqrt(rowSums((xy[-1L, , drop = FALSE] -
      xy[-nrow(xy), , drop = FALSE])^2))
    total <- sum(segment_length)
    if (!is.finite(total) || total <= 0) stop("Noded pieces must have positive finite length")
    cumulative <- cumsum(segment_length)
    segment <- which(cumulative >= total / 2)[1L]
    preceding <- if (segment == 1L) 0 else cumulative[segment - 1L]
    fraction <- (total / 2 - preceding) / segment_length[segment]
    sf::st_point(unname(xy[segment, ] +
      fraction * (xy[segment + 1L, ] - xy[segment, ])))
  })
  points <- sf::st_sfc(midpoints, crs = sf::st_crs(geometry))
  minimum_distance <- rep(Inf, length(geometry))
  for (kind in priority) {
    pending <- which(is.na(answer$source_class))
    source_rows <- which(sources$source_class == kind)
    if (!length(pending) || !length(source_rows)) next
    support <- sf::st_geometry(sources)[source_rows]
    nearest <- sf::st_nearest_feature(points[pending], support)
    distance <- numeric(length(pending))
    # Bound copies of long source polylines in by-element GEOS distance calls.
    batches <- split(seq_along(pending), ceiling(seq_along(pending) / 2000L))
    for (batch in batches) {
      distance[batch] <- as.numeric(sf::st_distance(
        points[pending[batch]], support[nearest[batch]], by_element = TRUE))
    }
    minimum_distance[pending] <- pmin(minimum_distance[pending], distance)
    matched <- which(is.finite(distance) & distance <= tolerance)
    if (!length(matched)) next
    rows <- pending[matched]
    answer$source_class[rows] <- kind
    answer$measure_habitat[rows] <- kind != "alignment_link"
    if (kind != "prediction") {
      answer$bridge_id[rows] <- sources$bridge_id[source_rows[nearest[matched]]]
    }
  }
  unsupported <- which(is.na(answer$source_class))
  if (length(unsupported)) {
    first <- head(unsupported, 6L)
    stop("Cannot classify ", length(unsupported), " noded piece(s) within ",
      format(tolerance), " m; pieces ", paste(first, collapse = ", "),
      "; nearest support distances ",
      paste(format(minimum_distance[first], digits = 6), collapse = ", "), " m")
  }
  answer
}
