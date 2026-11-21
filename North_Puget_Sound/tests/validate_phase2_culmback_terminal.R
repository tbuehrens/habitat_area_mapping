# Read-only proof for the one explicitly authorized modeled-channel terminal.
# No generic barrier is allowed to inherit this exception.
source("R/project_paths.R")
suppressPackageStartupMessages(library(sf))
assert <- function(ok, message) if (!isTRUE(ok)) stop(message, call. = FALSE)
key <- "user_override:Culmback_Dam"
file <- "outputs/phase2_reviewed_barriers/North_Puget_Sound_Steelhead_phase2.gpkg"
read_layer <- function(layer) st_read(file, layer = layer, quiet = TRUE)
has_key <- function(values) vapply(strsplit(ifelse(is.na(values), "", values), ";", fixed = TRUE),
  function(parts) key %in% parts, logical(1))
length_m <- function(x) {
  geometry <- if (inherits(x, "sf")) st_geometry(x) else x
  if (!length(geometry) || all(st_is_empty(geometry))) return(0)
  sum(as.numeric(st_length(st_union(geometry))))
}
barriers <- read_layer("barrier_characteristics")
point <- barriers[barriers$review_record_key == key, ]
masks <- read_layer("upstream_basin_masks")
mask <- masks[masks$review_record_key == key, ]
assert(nrow(point) == 1L && point$review_number == 912L && point$phase2_applied &&
  point$override_future_cut && nrow(mask) == 1L && mask$terminal_dam_validated,
  "The terminal-only user authorization or applied basin is missing")
edges <- st_read("outputs/review_swifd_connections/North_Puget_Sound_Steelhead.gpkg",
  layer = "stream_matching_edges", quiet = TRUE)
edge <- edges[edges$graph_edge_id == point$graph_edge_id, ]
assert(nrow(edge) == 1L && as.character(point$matched_llid) == "1218192478598" &&
  point$snap_distance_m <= 50 && point$matching_channel_count == 1L &&
  !point$junction_ambiguous, "Culmback is not uniquely matched to the original physical Sultan channel")
xy <- unclass(st_geometry(edge)[[1L]])[, 1:2, drop = FALSE]
at <- st_coordinates(point)[1L, 1:2]
distances <- sqrt(rowSums(sweep(rbind(xy[1L, ], xy[nrow(xy), ]), 2, at)^2))
terminal <- which.min(distances)
assert(distances[terminal] < 1e-5 && c(edge$from_degree, edge$to_degree)[terminal] == 1L,
  "Culmback's original channel match is not an exact unbranched terminal")

records <- nps_read_cache_json("spatial_data/phase2/dam_tolt_basins/manifest.json", simplifyVector = FALSE)
record <- Filter(function(x) identical(x$review_record_key, key), records)
assert(length(record) == 1L, "Missing unique Culmback source manifest")
record <- record[[1L]]
assert(as.character(record$comid) == "23963709" && record$approved &&
  identical(tolower(unname(tools::md5sum(record$basin_path))), tolower(record$basin_md5)) &&
  identical(tolower(mask$source_md5), tolower(record$basin_md5)), "Culmback full basin provenance changed")
raw <- jsonlite::fromJSON(record$basin_path, simplifyVector = FALSE)
full <- Filter(function(x) identical(x$id, "drainageBasin"), raw$features)
assert(length(full) == 1L, "A local split catchment was substituted for the complete Culmback basin")
source <- st_transform(st_read(jsonlite::toJSON(list(type = "FeatureCollection", features = full),
  auto_unbox = TRUE, digits = NA), quiet = TRUE), st_crs(mask))
source <- st_make_valid(source)
source_area <- sum(as.numeric(st_area(source)))
source <- suppressWarnings(st_collection_extract(source, "POLYGON"))
source <- source[!st_is_empty(source) & as.numeric(st_area(source)) > 0, ]
assert(nrow(source) > 0L && abs(sum(as.numeric(st_area(source))) - source_area) < .01,
  "Polygon extraction discarded positive-area Culmback drainage coverage")
assert(sum(as.numeric(st_area(suppressWarnings(st_sym_difference(
  st_union(st_geometry(mask)), st_union(st_geometry(st_make_valid(source)))))))) < .01,
  "Applied Culmback mask differs from the full source drainage basin")
area_sqmi <- as.numeric(st_area(mask)) / 2589988.110336
assert(abs(area_sqmi / 68.3 - 1) < .05, "Culmback basin conflicts with the independently published 68.3-square-mile drainage")

# Independently interpolate the first 50 m from the physical terminal and
# project it onto the published named route in digitized downstream order.
raw_route <- jsonlite::fromJSON(file.path(record$cache_folder, "named_flowline.json"), simplifyVector = FALSE)
route_feature <- Filter(function(x) as.character(x$properties$comid) == "23963709", raw_route$features)
assert(length(route_feature) == 1L &&
  tolower(route_feature[[1L]]$properties$flowdir) == "with digitized" &&
  route_feature[[1L]]$properties$gnis_name == "Sultan River", "Published route lacks explicit Sultan downstream direction")
route <- suppressWarnings(st_cast(st_transform(st_read(jsonlite::toJSON(
  list(type = "FeatureCollection", features = route_feature), auto_unbox = TRUE, digits = NA),
  quiet = TRUE), st_crs(edge)), "LINESTRING"))
assert(nrow(route) == 1L, "Published Sultan reference is ambiguous")
rxy <- unclass(st_geometry(route)[[1L]])[, 1:2, drop = FALSE]
line_positions <- function(coords) c(0, cumsum(sqrt(rowSums(diff(coords)^2))))
positions <- line_positions(xy)
assert(tail(positions, 1L) >= 50, "Insufficient physical downstream support at the dam")
samples <- if (terminal == 1L) seq(0, 50, length.out = 7L) else
  seq(tail(positions, 1L), tail(positions, 1L) - 50, length.out = 7L)
sample_xy <- cbind(approx(positions, xy[, 1L], samples)$y, approx(positions, xy[, 2L], samples)$y)
route_positions <- line_positions(rxy)
project <- function(p) {
  starts <- rxy[-nrow(rxy), , drop = FALSE]
  vectors <- diff(rxy)
  squared <- rowSums(vectors^2)
  fraction <- rowSums(sweep(starts, 2, p, function(a, b) b - a) * vectors) / squared
  fraction[!is.finite(fraction)] <- 0
  fraction <- pmax(0, pmin(1, fraction))
  projected <- starts + vectors * fraction
  distance <- sqrt(rowSums(sweep(projected, 2, p)^2))
  j <- which.min(distance)
  c(distance = distance[j], measure = route_positions[j] + sqrt(squared[j]) * fraction[j])
}
projected <- t(apply(sample_xy, 1L, project))
assert(all(projected[, "distance"] <= 25) && all(diff(projected[, "measure"]) > .25),
  "The physical terminal-to-interior direction is not independently proven downstream")

extent <- read_layer("phase2_extent_status")
extent <- extent[extent$measure_habitat, ]
tagged <- extent[has_key(extent$downstream_of_barrier), ]
off_edge <- suppressWarnings(st_difference(st_union(st_geometry(tagged)),
  st_buffer(st_union(st_geometry(edge)), 1e-5)))
assert(nrow(tagged) > 0L && length_m(off_edge) < .001,
  "Terminal-dam downstream authority was extended to another physical channel")
protected <- extent[has_key(extent$watershed_downstream_protected_by_barrier), ]
protected_m <- length_m(protected)
assert(protected_m > 23 && protected_m < 24 &&
  length_m(suppressWarnings(st_difference(st_union(st_geometry(protected)),
    st_buffer(st_union(st_geometry(edge)), 1e-5)))) < .001,
  "Culmback downstream precision protection is not confined to the proven exact physical edge")
coords <- st_coordinates(protected)[, 1:2, drop = FALSE]
assert(max(sqrt(rowSums(sweep(coords, 2, at)^2))) < 100,
  "Culmback local precision protection extends beyond the 100 m bound")
remaining <- extent[!extent$removed, ]
inside <- suppressWarnings(st_intersection(remaining, st_geometry(mask)))
assert(length_m(inside[!has_key(inside$watershed_downstream_protected_by_barrier), ]) < .001,
  "Unproven upstream Culmback distribution remains inside the full basin")
cat(sprintf("PASS: Culmback terminal-only proof; exact complete %.6f mi2 basin; seven published downstream projections; %.6f m protection confined to original Sultan edge.\n",
  area_sqmi, protected_m))
