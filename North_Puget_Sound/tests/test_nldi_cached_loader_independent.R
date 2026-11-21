# No network requests: synthetic cached USGS responses exercise loader gates.
source(file.path("R", "barrier_stream_identity.R"))
source(file.path("R", "stream_edge_identity.R"))
source(file.path("R", "nldi_delineation.R"))
suppressPackageStartupMessages(library(sf))
check <- function(x, message) if (!isTRUE(x)) stop(message, call. = FALSE)
offset <- c(500000, 5400000)
xy <- function(x) sweep(x, 2, offset, "+")
line <- function(x) st_linestring(xy(x))
to_geo <- function(x) {
  s <- st_sfc(st_linestring(xy(x)), crs = 26910)
  z <- st_coordinates(st_transform(s, 4326))[, 1:2, drop = FALSE]
  unname(lapply(seq_len(nrow(z)), function(i) unname(z[i, ])))
}
network <- st_sf(source_class = "prediction", measure_habitat = TRUE,
  geometry = st_sfc(line(rbind(c(0, 0), c(1000, 0))), crs = 26910))
reference <- st_sf(LLID = "fixture", LLID_STRM_NAME = "Main River", geometry = st_geometry(network))
edges <- build_stream_identity_index(network, reference)$edges
barrier <- st_sf(review_record_key = "6:fixture", review_number = 903L, SiteId = "fixture",
  StreamName = "Main River", graph_edge_id = edges$graph_edge_id[1],
  geometry = st_sfc(st_point(c(500, 0) + offset), crs = 26910))
at <- st_coordinates(st_transform(barrier, 4326))[1, 1:2]
directory <- tempfile("cached_nldi_independent_")
dir.create(directory)
record <- list(review_record_key = "6:fixture", review_number = 999L,
  status = "approved_name_and_course", approved = TRUE, reference_stream_name = "Main River",
  comid = "123", indexed_longitude = unname(at[1]), indexed_latitude = unname(at[2]),
  indexed_shift_m = 0, route_support_coverage = 1, retrieved_utc = "fixture",
  cache_folder = directory, basin_path = file.path(directory, "basin.json"))
basin <- list(type = "FeatureCollection", features = list(list(type = "Feature", id = "drainageBasin",
  properties = list(), geometry = list(type = "Polygon", coordinates = list(to_geo(rbind(
    c(-100, -100), c(500, -100), c(500, 100), c(-100, 100), c(-100, -100))))))))
jsonlite::write_json(basin, record$basin_path, auto_unbox = TRUE, digits = NA)
record$basin_md5 <- unname(tools::md5sum(record$basin_path))
write_case <- function(value = record, route = rbind(c(0, 0), c(1000, 0)), flowdir = "With Digitized", reference_name = "Main River") {
  feature <- list(type = "Feature", properties = list(comid = "123", flowdir = flowdir, gnis_name = reference_name),
    geometry = list(type = "LineString", coordinates = to_geo(route)))
  jsonlite::write_json(list(type = "FeatureCollection", features = list(feature)),
    file.path(directory, "named_flowline.json"), auto_unbox = TRUE, digits = NA)
  jsonlite::write_json(list(value), file.path(directory, "manifest.json"), auto_unbox = TRUE, digits = NA)
}
write_case()
good <- load_nldi_delineations(barrier, edges, directory)
check(nrow(good$candidate_masks) == 1L && !good$candidate_masks$local_outlet_precision_validated,
  "A cache loader must leave outlet precision unapproved until post-graph validation")
check(good$audit$review_number == 903L && good$candidate_masks$review_record_key == "6:fixture",
  "Cache envelope numbering replaced the source review number or key")
check(nrow(good$downstream_anchors) == 1L &&
  good$downstream_anchors$graph_edge_id == barrier$graph_edge_id &&
  good$downstream_anchors$direction_evidence_valid &&
  grepl("Published WithDigitized", good$downstream_anchors$direction_basis, fixed = TRUE),
  "A downstream anchor needs an explicit declared edge and published direction evidence")
check(as.numeric(st_distance(good$downstream_anchors, edges, by_element = TRUE)) < 1e-5,
  "The cached direction anchor moved off its declared physical edge")

for (name in c("unnamed", "Other River")) {
  wrong <- record
  wrong$reference_stream_name <- name
  write_case(wrong)
  rejected <- load_nldi_delineations(barrier, edges, directory)
  check(nrow(rejected$candidate_masks) == 0L, "An unknown or different NLDI stream was accepted")
}
far <- record
far_point <- st_transform(st_sfc(st_point(c(500, 80) + offset), crs = 26910), 4326)
far$indexed_longitude <- st_coordinates(far_point)[1, 1]
far$indexed_latitude <- st_coordinates(far_point)[1, 2]
write_case(far)
check(nrow(load_nldi_delineations(barrier, edges, directory)$candidate_masks) == 0L,
  "A NLDI indexed location beyond50m was accepted")

write_case(record, reference_name = "Other River")
check(nrow(load_nldi_delineations(barrier, edges, directory)$candidate_masks) == 0L,
  "The cached route's actual stream name disagrees with the approved manifest")

# A stale approved manifest must not overrule the actual cached line geometry.
# A same-named crossing near the point is not the barrier's physical course.
write_case(record, route = rbind(c(500, -300), c(500, 300)))
crossed <- load_nldi_delineations(barrier, edges, directory)
check(nrow(crossed$candidate_masks) == 0L,
  "A single-point crossing was accepted as line-level NLDI identity")
write_case(record, route = rbind(c(0, 100), c(1000, 100)))
parallel <- load_nldi_delineations(barrier, edges, directory)
check(nrow(parallel$candidate_masks) == 0L,
  "A distant parallel cached course inherited a stale approved flag")

cat("PASS: cache identity/distance/course gates, unchanged source numbering, unapproved raw outlet precision, and published same-edge direction anchors.\n")
