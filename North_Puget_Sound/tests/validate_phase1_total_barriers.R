# Read-only real-output validation. No downloads, exports, or cache updates.
source("R/project_paths.R")
library(sf)
source("tests/upstream_basin_validation_helpers.R")
source("R/barrier_stream_identity.R")
source("R/stream_edge_identity.R")
args <- commandArgs(trailingOnly = TRUE)
out <- if (length(args)) args[1] else "outputs/phase1_total_barriers"
baseline <- "outputs/review_swifd_connections"
assert <- function(x, message) if (!isTRUE(x)) stop(message, call. = FALSE)
yes <- function(x) x %in% TRUE
close <- function(a, b, tolerance = 1e-6) length(a) == length(b) &&
  all(is.finite(a)) && all(is.finite(b)) && all(abs(a - b) <= tolerance)
slash <- function(x) gsub("\\\\", "/", x)

# Use an independently captured pre-Phase1 snapshot, not only the exporter's
# own manifest. Include the file set so added/removed baseline files also fail.
snapshot <- read.csv("spatial_data/phase1_validation/review_file_hashes_before.csv", stringsAsFactors = FALSE)
current_files <- list.files(baseline, recursive = TRUE, all.files = TRUE, no.. = TRUE)
assert(setequal(slash(snapshot$RelativePath), slash(current_files)), "The preserved review file set changed")
paths <- file.path(baseline, slash(snapshot$RelativePath))
assert(all(file.info(paths)$size == snapshot$Length), "A preserved review file changed length")
assert(identical(tolower(unname(tools::md5sum(paths))), tolower(snapshot$MD5)),
  "A preserved review file changed content")
output_folders <- basename(list.dirs("outputs", recursive = FALSE))
preserved_folders <- c("review_swifd_connections", "phase1_total_barriers", "phase2_reviewed_barriers", "legacy_nooksack")
assert(all(preserved_folders %in% output_folders) &&
  all(output_folders %in% c(preserved_folders, "phase3_tidal_barriers")), "Unexpected extra output folders")

needed <- c("population_phase1_miles.csv", "phase1_barrier_decisions.csv", "barrier_characteristics.csv",
  "phase1_cut_barriers.csv", "phase1_graph_cut_audit.csv", "downstream_direction_audit.csv",
  "upstream_basin_audit.csv", "upstream_basin_validation.csv", "nldi_delineation_audit.csv",
  "basin_completeness_audit.csv",
  "upstream_coverage_audit.csv", "upstream_navigation_audit.csv",
  "baseline_file_manifest.csv", "barrier_field_inventory.csv", "barrier_code_inventory.csv",
  "barrier_metadata_summary.csv", "report_fetch_audit.csv", "run_metadata.json",
  "North_Puget_Sound_Steelhead_map.png", "Nooksack_Steelhead_map.png",
  "North_Puget_Sound_Steelhead_population_maps.pdf", "Skagit_before_after.png")
assert(all(file.exists(file.path(out, needed))), "Phase1 products are incomplete")
assert(length(list.files(file.path(out, "maps"), pattern = "\\.png$")) == 16L,
  "Phase1 must contain 16 population maps")
manifest <- read.csv(file.path(out, "baseline_file_manifest.csv"), stringsAsFactors = FALSE)
manifest <- manifest[match(slash(snapshot$RelativePath), slash(manifest$baseline_file)), ]
assert(nrow(manifest) == nrow(snapshot) && !anyNA(manifest$baseline_file) &&
  identical(tolower(manifest$md5), tolower(snapshot$MD5)), "Exporter baseline manifest differs from the independent snapshot")

table <- read.csv(file.path(out, "population_phase1_miles.csv"))
decisions <- read.csv(file.path(out, "phase1_barrier_decisions.csv"), stringsAsFactors = FALSE)
characteristics <- read.csv(file.path(out, "barrier_characteristics.csv"), stringsAsFactors = FALSE)
applied <- read.csv(file.path(out, "phase1_cut_barriers.csv"), stringsAsFactors = FALSE)
audit <- read.csv(file.path(out, "phase1_graph_cut_audit.csv"), stringsAsFactors = FALSE)
basin_audit <- read.csv(file.path(out, "upstream_basin_audit.csv"), stringsAsFactors = FALSE)
nldi_audit <- read.csv(file.path(out, "nldi_delineation_audit.csv"), stringsAsFactors = FALSE)
coverage_audit <- read.csv(file.path(out, "upstream_coverage_audit.csv"), stringsAsFactors = FALSE)
completeness_audit <- read.csv(file.path(out, "basin_completeness_audit.csv"), stringsAsFactors = FALSE)
navigation_audit <- nps_read_cache_csv(file.path(out, "upstream_navigation_audit.csv"), stringsAsFactors = FALSE)
metadata <- jsonlite::fromJSON(file.path(out, "run_metadata.json"))
controls <- metadata$nldi_course_controls
assert(is.list(controls) && controls$heading_halfspan_m == 25 && controls$sample_count == 7L &&
  controls$local_half_length_m == 50 && controls$max_match_distance_m == 25 &&
  controls$max_heading_degrees == 35 && controls$min_coverage == .8,
  "Phase1 metadata does not record the approved scale-aware NLDI course controls")
assert("heading_halfspan_m" %in% names(nldi_audit) && all(nldi_audit$heading_halfspan_m == 25),
  "NLDI audit contains inconsistent or undocumented tangent scales")
review <- read.csv(file.path(baseline, "barrier_review.csv"), stringsAsFactors = FALSE)
before_table <- read.csv(file.path(baseline, "population_distribution_miles.csv"))
gpkg <- file.path(out, "North_Puget_Sound_Steelhead_phase1.gpkg")
layers <- c("noaa_populations", "phase1_extent_status", "phase1_remaining", "phase1_removed_upstream",
  "phase1_unresolved_direction", "barrier_characteristics", "applied_barriers", "downstream_anchors", "upstream_basin_masks")
assert(all(layers %in% st_layers(gpkg)$name), "Phase1 GIS layers are incomplete")
pops <- st_read(gpkg, layer = "noaa_populations", quiet = TRUE)
all_lines <- st_read(gpkg, layer = "phase1_extent_status", quiet = TRUE)
remaining <- st_read(gpkg, layer = "phase1_remaining", quiet = TRUE)
removed <- st_read(gpkg, layer = "phase1_removed_upstream", quiet = TRUE)
unresolved <- st_read(gpkg, layer = "phase1_unresolved_direction", quiet = TRUE)
anchors <- st_read(gpkg, layer = "downstream_anchors", quiet = TRUE)
masks <- nps_resolve_record_paths(st_read(gpkg, layer = "upstream_basin_masks", quiet = TRUE))
barrier_points <- st_read(gpkg, layer = "barrier_characteristics", quiet = TRUE)
before <- st_read(file.path(baseline, "North_Puget_Sound_Steelhead.gpkg"),
  layer = "steelhead_patched_untruncated", quiet = TRUE)
matching_edges <- st_read(file.path(baseline, "North_Puget_Sound_Steelhead.gpkg"),
  layer = "stream_matching_edges", quiet = TRUE)
matching_coordinates <- lapply(st_geometry(matching_edges), function(x) unclass(x)[, 1:2, drop = FALSE])
matching_adjacency <- lapply(seq_len(max(c(matching_edges$from_node, matching_edges$to_node))),
  function(node) c(which(matching_edges$from_node == node), which(matching_edges$to_node == node)))
matching_index <- list(edges = matching_edges, coordinates = matching_coordinates,
  adjacency = matching_adjacency, nodes = data.frame(degree = lengths(matching_adjacency)))

# Independently compute positions along a cached published flowline. Its vertex
# order establishes direction only when the source explicitly says WithDigitized.
project_part <- function(xy, point) {
  delta <- xy[-1L, , drop = FALSE] - xy[-nrow(xy), , drop = FALSE]
  squared <- rowSums(delta ^ 2)
  relative <- matrix(point, nrow(delta), 2L, byrow = TRUE) - xy[-nrow(xy), , drop = FALSE]
  fraction <- pmax(0, pmin(1, rowSums(relative * delta) / ifelse(squared > 0, squared, 1)))
  projected <- xy[-nrow(xy), , drop = FALSE] + delta * fraction
  nearest <- which.min(rowSums(sweep(projected, 2L, point) ^ 2))
  segment_lengths <- sqrt(squared)
  list(distance = sqrt(sum((projected[nearest, ] - point) ^ 2)),
    measure = sum(segment_lengths[seq_len(nearest - 1L)]) + fraction[nearest] * segment_lengths[nearest])
}
read_cached_reference <- function(record, point) {
  path <- file.path(record$cache_folder, "named_flowline.json")
  assert(file.exists(path), "Coverage claim has no cached named flowline")
  raw <- jsonlite::fromJSON(path, simplifyVector = FALSE)
  features <- Filter(function(x) identical(as.character(x$properties$comid), as.character(record$comid)), raw$features)
  assert(length(features) == 1L, "Coverage reference COMID is absent or ambiguous")
  feature <- features[[1L]]
  assert(stream_identity_names_match(point$StreamName, feature$properties$gnis_name),
    "Coverage reference does not match the barrier's own stream")
  route <- st_read(jsonlite::toJSON(list(type = "FeatureCollection", features = features),
    auto_unbox = TRUE, digits = NA), quiet = TRUE)
  route <- suppressWarnings(st_cast(st_transform(route, st_crs(point)), "LINESTRING"))
  list(parts = lapply(st_geometry(route), function(x) unclass(x)[, 1:2, drop = FALSE]),
    flowdir = tolower(trimws(as.character(feature$properties$flowdir))),
    md5 = unname(tools::md5sum(path)))
}
check_cached_course <- function(record, point, reference) {
  edge <- match(point$graph_edge_id, matching_edges$graph_edge_id)
  assert(!is.na(edge), "Covered barrier's exact physical edge is missing")
  projection <- project_part(matching_coordinates[[edge]], st_coordinates(point)[1L, 1:2])
  assert(projection$distance <= 1e-5, "Covered point is off its declared physical edge")
  support <- stream_identity_local_support(matching_index, edge, projection$measure, 50)
  assert(support$length >= 40 && !support$topology_ambiguous && support$junction_distance > 25,
    "Covered point lacks an unambiguous local channel course")
  positions <- seq(0, support$length, length.out = 7L)
  supported <- vapply(positions, function(position) {
    sample <- stream_identity_at(support$xy, position)
    projections <- lapply(reference$parts, project_part, point = sample)
    part <- which.min(vapply(projections, `[[`, numeric(1), "distance"))
    on <- projections[[part]]
    # The NHDPlusV2 routing reference is generalized at1:100k. Its approved
    # tangent window is25m on each side; primary WDFW identity checks stay finer.
    own_heading <- stream_identity_at(support$xy, min(support$length, position + 25)) -
      stream_identity_at(support$xy, max(0, position - 25))
    xy <- reference$parts[[part]]
    length <- sum(sqrt(rowSums(diff(xy) ^ 2)))
    reference_heading <- stream_identity_at(xy, min(length, on$measure + 25)) -
      stream_identity_at(xy, max(0, on$measure - 25))
    norm <- sqrt(sum(own_heading ^ 2) * sum(reference_heading ^ 2))
    angle <- if (norm > 1e-12) acos(min(1, abs(sum(own_heading * reference_heading)) / norm)) * 180 / pi else 90
    on$distance <= 25 + 1e-8 && angle <= 35 + 1e-8
  }, logical(1))
  assert(mean(supported) >= .8, "Covered point's cached line does not support its actual channel course")
  indexed <- st_transform(st_sfc(st_point(c(record$indexed_longitude, record$indexed_latitude)), crs = 4326), st_crs(point))
  indexed_xy <- st_coordinates(indexed)[1L, 1:2]
  assert(as.numeric(st_distance(point, indexed)) <= 50 + 1e-5 &&
    min(vapply(lapply(reference$parts, project_part, point = indexed_xy), `[[`, numeric(1), "distance")) <= 25 + 1e-8,
    "Coverage indexed point is off the named reference or beyond the model snap tolerance")
}

assert(nrow(pops) == 16L && nrow(table) == 16L && setequal(pops$pop_id, table$pop_id),
  "Phase1 populations differ from the 16-population baseline")
assert(setequal(decisions$review_record_key, review$review_record_key) &&
  !anyDuplicated(decisions$review_record_key), "A numbered baseline barrier disappeared or was duplicated")
prior_index <- match(review$review_record_key, decisions$review_record_key)
assert(identical(as.integer(decisions$review_number[prior_index]), as.integer(review$review_number)),
  "Phase1 changed existing review numbers")
assert(identical(decisions, characteristics), "Decision and characteristics CSVs disagree")

# Identity alone is not passage evidence. Explicit natural Totals from either
# verification layer, or the specifically authorized Gorge override, qualify.
override <- decisions$review_record_key == "user_override:Gorge_Dam" & yes(decisions$override_future_cut)
natural_inventory <- decisions$source_layer %in% c("5", "6")
explicit <- yes(decisions$source_explicit_total) |
  (yes(decisions$report_identity_verified) & yes(decisions$report_explicit_total))
strict <- yes(decisions$snap_accepted) & yes(decisions$stream_identity_confirmed) &
  yes(decisions$identity_valid_for_cut) & !yes(decisions$junction_ambiguous) &
  decisions$matching_channel_count == 1L & decisions$snap_distance_m <= 50 + 1e-8
inside <- !is.na(decisions$population_ids) & nzchar(trimws(decisions$population_ids))
allowed <- inside & strict & !yes(decisions$total_evidence_conflict) & (override | (natural_inventory & explicit))
allowed[is.na(allowed)] <- FALSE
assert(identical(yes(decisions$phase1_selected), allowed), "Phase1 selected a barrier outside its explicit-Total/Gorge policy")
assert(!any(yes(decisions$phase1_applied) & !allowed), "A nonqualifying barrier caused a Phase1 cut")
report_required <- yes(decisions$phase1_selected) & !override & !yes(decisions$source_explicit_total)
assert(all(yes(decisions$report_identity_verified[report_required])) &&
  all(yes(decisions$report_explicit_total[report_required])) &&
  all(!is.na(decisions$report_url[report_required]) & nzchar(decisions$report_url[report_required])) &&
  all(grepl("^[[:xdigit:]]{32}$", decisions$report_source_md5[report_required])),
  "A report-backed natural Total lacks site identity or source provenance")
assert(setequal(audit$review_record_key, decisions$review_record_key[allowed]),
  "Graph audit does not match selected barriers")
assert(all(yes(decisions$phase1_applied) == (yes(decisions$phase1_selected) & yes(decisions$upstream_basin_approved))) &&
  setequal(applied$review_record_key, decisions$review_record_key[yes(decisions$phase1_applied)]) &&
  setequal(applied$review_record_key, masks$review_record_key) &&
  setequal(masks$review_record_key, basin_audit$review_record_key),
  "Applied-cut tables disagree")
assert(all(masks$review_record_key %in% audit$review_record_key[yes(audit$applied)]),
  "An approved basin lacks the independently proven local graph direction required for its outlet check")
assert(setequal(nldi_audit$review_record_key, decisions$review_record_key[allowed]),
  "NLDI audit does not account for every selected Total-barrier request")
assert(setequal(coverage_audit$review_record_key, decisions$review_record_key[allowed]) &&
  !anyDuplicated(coverage_audit$review_record_key), "Coverage audit must account for each selected Total exactly once")
cov_index <- match(coverage_audit$review_record_key, decisions$review_record_key)
assert(all(yes(coverage_audit$own_cut_applied) == yes(decisions$upstream_basin_approved[cov_index])) &&
  all(yes(coverage_audit$complete_upstream_coverage) ==
    (yes(coverage_audit$own_cut_applied) | yes(coverage_audit$covered_by_downstream_basin))) &&
  all(yes(decisions$complete_upstream_coverage[cov_index]) == yes(coverage_audit$complete_upstream_coverage)),
  "Complete coverage was confused with applying an upper barrier's own cut")
assert(all(c("review_record_key", "review_number", "phase1_applied", "complete_upstream_coverage",
  "unexplained_upstream_remaining_m") %in% names(completeness_audit)) &&
  setequal(completeness_audit$review_record_key, decisions$review_record_key[allowed]) &&
  !anyDuplicated(completeness_audit$review_record_key),
  "Consolidated basin completeness audit does not account for every selected Total exactly once")
complete_index <- match(completeness_audit$review_record_key, decisions$review_record_key)
assert(identical(as.integer(completeness_audit$review_number), as.integer(decisions$review_number[complete_index])) &&
  all(yes(completeness_audit$phase1_applied) == yes(decisions$phase1_applied[complete_index])) &&
  all(yes(completeness_audit$complete_upstream_coverage) == yes(decisions$complete_upstream_coverage[complete_index])),
  "Consolidated completeness audit contradicts source numbering or cut/coverage decisions")
own <- yes(completeness_audit$phase1_applied)
assert(all(is.finite(completeness_audit$unexplained_upstream_remaining_m[own])) &&
  all(completeness_audit$unexplained_upstream_remaining_m[own] <= .001),
  "An applied cut has no quantitative proof of zero unexplained upstream residual")
pending <- !yes(completeness_audit$complete_upstream_coverage)
assert(all(is.na(completeness_audit$unexplained_upstream_remaining_m[pending])),
  "A pending basin is being reported as zero residual/passed without complete upstream proof")
raw_requests <- nps_read_cache_json("spatial_data/upstream_basins/batch/manifest.json", simplifyVector = FALSE)
raw_requests <- lapply(raw_requests, function(record) {
  path <- file.path(record$cache_folder, "manifest.json")
  if (file.exists(path)) {
    latest <- nps_read_cache_json(path, simplifyVector = FALSE)
    if (identical(latest$review_record_key, record$review_record_key) &&
        identical(latest$request_sha256, record$request_sha256)) return(latest)
  }
  record
})
request_keys <- vapply(raw_requests, function(x) as.character(x$review_record_key), character(1))

# A local splitCatchment is not a full upstream basin unless the published
# network independently proves this is a true, single-catchment headwater.
# Check source features and VAAs directly, rather than trusting coverage flags.
for (i in seq_len(nrow(masks))) {
  key <- masks$review_record_key[i]
  is_gorge <- identical(key, "user_override:Gorge_Dam")
  record_row <- match(key, request_keys)
  record <- if (is.na(record_row)) NULL else raw_requests[[record_row]]
  if (is_gorge) {
    path <- as.character(masks$mask_cache_file[i])
    expected_md5 <- as.character(masks$mask_cache_md5[i])
  } else {
    assert(!is.null(record), "Approved mask has no cached request record")
    path <- as.character(record$basin_path)
    expected_md5 <- as.character(record$basin_md5)
  }
  assert(length(path) == 1L && !is.na(path) && file.exists(path) &&
    identical(tolower(unname(tools::md5sum(path))), tolower(expected_md5)),
    paste("Approved mask lacks its exact source basin/cache hash:", key))
  raw <- jsonlite::fromJSON(path, simplifyVector = FALSE)
  full <- Filter(function(x) identical(x$id, "drainageBasin"), raw$features)
  split <- Filter(function(x) identical(x$id, "splitCatchment"), raw$features)
  if (is_gorge || ("completeness_method" %in% names(masks) &&
      identical(as.character(masks$completeness_method[i]), "full_drainageBasin"))) {
    assert(length(full) == 1L, paste("Full drainage mask actually lacks drainageBasin:", key))
    chosen <- full
  } else {
    assert("completeness_method" %in% names(masks) &&
      identical(as.character(masks$completeness_method[i]), "published_headwater_single_catchment_split") &&
      length(full) == 0L && length(split) == 1L,
      paste("A local split catchment is being treated as complete without headwater proof:", key))
    source <- jsonlite::fromJSON(file.path(record$cache_folder, "named_flowline.json"), simplifyVector = FALSE)
    feature <- Filter(function(x) identical(as.character(x$properties$comid), as.character(record$comid)), source$features)
    assert(length(feature) == 1L, "Headwater network source COMID is missing or ambiguous")
    published <- feature[[1L]]$properties
    assert(all(c("startflag", "uphydroseq", "streamorde", "areasqkm", "totdasqkm") %in% names(published)),
      "Headwater source lacks required published topology/drainage attributes")
    assert(as.numeric(published$startflag) == 1 && as.numeric(published$uphydroseq) == 0 &&
      as.numeric(published$streamorde) == 1 && as.numeric(published$areasqkm) > 0 &&
      abs(as.numeric(published$areasqkm) - as.numeric(published$totdasqkm)) <= 1e-6,
      paste("Split-only basin has published upstream reaches or additional drainage:", key))
    nav_path <- as.character(masks$headwater_navigation_cache_file[i])
    nav_md5 <- as.character(masks$headwater_navigation_md5[i])
    assert(length(nav_path) == 1L && !is.na(nav_path) && file.exists(nav_path) &&
      identical(tolower(unname(tools::md5sum(nav_path))), tolower(nav_md5)),
      paste("Headwater claim lacks independently cached upstream navigation:", key))
    nav <- jsonlite::fromJSON(nav_path, simplifyVector = FALSE)
    assert(identical(nav$type, "FeatureCollection"), "Headwater navigation is not a valid feature collection")
    nav_ids <- unique(vapply(nav$features, function(x) as.character(x$properties$nhdplus_comid), character(1)))
    assert(length(nav_ids) == 1L && identical(nav_ids, as.character(record$comid)) &&
      masks$headwater_upstream_reach_count[i] == 1L,
      paste("Headwater navigation includes other upstream reaches:", key))
    chosen <- split
  }
  original_polygon <- st_read(jsonlite::toJSON(list(type = "FeatureCollection", features = chosen),
    auto_unbox = TRUE, digits = NA), quiet = TRUE)
  original_polygon <- st_make_valid(st_transform(original_polygon, st_crs(masks)))
  if (any(st_geometry_type(original_polygon) == "GEOMETRYCOLLECTION"))
    original_polygon <- suppressWarnings(st_collection_extract(original_polygon, "POLYGON"))
  difference <- suppressWarnings(st_sym_difference(st_union(st_geometry(original_polygon)), st_geometry(masks)[i]))
  tolerance_sq_m <- max(.001, sum(as.numeric(st_area(original_polygon))) * 1e-8)
  assert(sum(as.numeric(st_area(difference))) <= tolerance_sq_m,
    paste("Exported upstream mask is not its complete approved source polygon:", key))
  if (!is_gorge) {
    source <- jsonlite::fromJSON(file.path(record$cache_folder, "named_flowline.json"), simplifyVector = FALSE)
    feature <- Filter(function(x) identical(as.character(x$properties$comid), as.character(record$comid)), source$features)
    assert(length(feature) == 1L, "Approved basin's published drainage-area reference is missing or ambiguous")
    published <- feature[[1L]]$properties
    local_area <- suppressWarnings(as.numeric(published$areasqkm))
    total_area <- suppressWarnings(as.numeric(published$totdasqkm))
    if (length(local_area) == 1L && length(total_area) == 1L && is.finite(local_area) &&
        is.finite(total_area) && local_area >= 0 && total_area > 0 && total_area >= local_area) {
      actual_area <- sum(as.numeric(st_area(original_polygon))) / 1e6
      area_tolerance <- max(.01, .05 * total_area)
      assert(actual_area >= max(0, total_area - local_area) - area_tolerance &&
        actual_area <= total_area + area_tolerance &&
        masks$published_area_check[i] == "passes_published_plausible_range",
        paste("Complete mask area conflicts with its published total/upstream drainage area:", key))
    }
  }
}

for (i in which(yes(coverage_audit$covered_by_downstream_basin))) {
  parent_key <- coverage_audit$covering_basin_key[i]
  parent_mask <- masks[masks$review_record_key == parent_key, ]
  nav <- navigation_audit[navigation_audit$review_record_key == parent_key, ]
  assert(nrow(parent_mask) == 1L, "Covered-by-downstream claim lacks an approved basin")
  point <- barrier_points[match(coverage_audit$review_record_key[i], barrier_points$review_record_key), ]
  raw_record <- raw_requests[[match(coverage_audit$review_record_key[i], request_keys)]]
  reference <- read_cached_reference(raw_record, point)
  check_cached_course(raw_record, point, reference)
  assert(yes(coverage_audit$cached_course_revalidated[i]) &&
    coverage_audit$cached_course_coverage[i] >= .8 &&
    coverage_audit$cached_course_tangent_halfspan_m[i] == 25 &&
    identical(tolower(coverage_audit$cached_reference_md5[i]), tolower(reference$md5)),
    "Covered-by-downstream audit lacks the independently checked cached-course proof")
  indexed <- st_transform(st_sfc(st_point(c(raw_record$indexed_longitude,
    raw_record$indexed_latitude)), crs = 4326), st_crs(point))
  assert(as.numeric(st_distance(point, indexed)) <= 50 + 1e-5 &&
    as.character(raw_record$comid) == coverage_audit$upstream_comid[i],
    "Downstream coverage lacks a positively validated own NLDI channel")
  assert(lengths(st_within(point, parent_mask)) > 0L &&
    as.numeric(st_distance(point, st_boundary(st_geometry(parent_mask)))) > 100 &&
    lengths(st_within(indexed, parent_mask)) > 0L &&
    as.numeric(st_distance(indexed, st_boundary(st_geometry(parent_mask)))) > 100 &&
    coverage_audit$indexed_boundary_distance_m[i] > 100,
    "Coverage rests on a boundary-adjacent point rather than a confirmed upstream interior location")
  if (coverage_audit$upstream_comid[i] == coverage_audit$covering_outlet_comid[i]) {
    lower_point <- barrier_points[match(parent_key, barrier_points$review_record_key), ]
    lower_record <- raw_requests[[match(parent_key, request_keys)]]
    lower_reference <- read_cached_reference(lower_record, lower_point)
    check_cached_course(lower_record, lower_point, lower_reference)
    assert(reference$flowdir == "with digitized" && lower_reference$flowdir == "with digitized" &&
      coverage_audit$published_flowdir[i] == "with digitized" &&
      as.character(lower_record$comid) == coverage_audit$covering_outlet_comid[i],
      "Same-COMID coverage lacks published, consistent with-digitized flow direction")
    lower_indexed <- st_transform(st_sfc(st_point(c(lower_record$indexed_longitude,
      lower_record$indexed_latitude)), crs = 4326), st_crs(point))
    assert(as.numeric(st_distance(lower_point, lower_indexed)) <= 50 + 1e-5,
      "Covering barrier's indexed pourpoint is beyond the snap tolerance")
    upper_xy <- st_coordinates(indexed)[1L, 1:2]
    lower_xy <- st_coordinates(lower_indexed)[1L, 1:2]
    for (parts in list(reference$parts, lower_reference$parts)) {
      upper <- lapply(parts, project_part, point = upper_xy)
      lower <- lapply(parts, project_part, point = lower_xy)
      a <- which(vapply(upper, `[[`, numeric(1), "distance") <= 25)
      b <- which(vapply(lower, `[[`, numeric(1), "distance") <= 25)
      assert(length(a) == 1L && length(b) == 1L && a == b &&
        lower[[b]]$measure - upper[[a]]$measure > 100 &&
        coverage_audit$same_comid_upstream_gap_m[i] > 100,
        "Same-COMID coverage does not prove more than100m of upstream ordering on one published flowline part")
    }
  } else {
    assert(nrow(nav) == 1L && nav$status == "positive_upstream_comid_membership_available" && file.exists(nav$cache_file),
      "Distinct-COMID coverage lacks cached positive navigation evidence")
    raw_nav <- jsonlite::fromJSON(nav$cache_file, simplifyVector = FALSE)
    nav_ids <- unique(vapply(raw_nav$features, function(x) as.character(x$properties$nhdplus_comid), character(1)))
    assert(coverage_audit$upstream_comid[i] %in% nav_ids &&
      coverage_audit$covering_outlet_comid[i] %in% nav_ids,
      "A claimed upper barrier is not in the published upstream network of its covering basin")
  }
}
assert(all(audit$cut_status[yes(audit$applied)] == "applied_proven_component_local_upstream"),
  "An applied cut lacks proven component-local direction")
gorge <- decisions[decisions$review_record_key == "user_override:Gorge_Dam", ]
assert(nrow(gorge) == 1L && gorge$review_number == 903L && yes(gorge$phase1_applied),
  "The authorized Gorge903 scenario is missing")

assert(all(c("removed", "unresolved_direction", "original_component", "removed_by_barrier",
  "unresolved_by_barrier", "measure_habitat", "source_class") %in% names(all_lines)),
  "Phase1 line provenance/status fields are incomplete")
assert(!any(yes(all_lines$removed) & yes(all_lines$unresolved_direction)),
  "Removed and unresolved mileage bins overlap")
assert(all(!yes(remaining$removed)) && all(yes(removed$removed)) &&
  all(yes(unresolved$unresolved_direction)), "GIS status subsets contain incorrect pieces")
keys <- function(x) if (is.na(x) || !nzchar(x)) character() else strsplit(x, ";", fixed = TRUE)[[1]]
for (i in which(yes(all_lines$removed))) {
  causes <- keys(all_lines$removed_by_barrier[i])
  rows <- match(causes, decisions$review_record_key)
  assert(length(causes) > 0 && !anyNA(rows) && all(yes(decisions$phase1_applied[rows])) && all(allowed[rows]),
    "A removed piece has no applied, approved Total-barrier cause")
}

# The remaining and unresolved lengths are intentionally distinct: unresolved
# pieces are visible, but counted only once in the three-way partition.
mi <- function(lines, id, selection = rep(TRUE, nrow(lines))) {
  z <- lines[lines$pop_id == id & selection, ]
  if (!nrow(z)) return(0)
  sum(as.numeric(st_length(st_union(st_geometry(z))))) / 1609.344
}
table <- table[match(pops$pop_id, table$pop_id), ]
before_table <- before_table[match(pops$pop_id, before_table$pop_id), ]
measure <- yes(all_lines$measure_habitat)
resolved <- measure & !yes(all_lines$removed) & !yes(all_lines$unresolved_direction)
uncertain <- measure & yes(all_lines$unresolved_direction)
excluded <- measure & yes(all_lines$removed)
for (i in seq_len(nrow(pops))) {
  id <- pops$pop_id[i]
  base <- mi(before, id, yes(before$measure_habitat))
  got <- c(mi(all_lines, id, resolved), mi(all_lines, id, uncertain), mi(all_lines, id, excluded))
  csv <- unlist(table[i, c("remaining_direction_resolved_miles", "remaining_direction_unresolved_miles", "removed_upstream_miles")])
  assert(close(base, sum(got)) && close(got, csv) &&
    close(table$baseline_untruncated_miles[i], base) && close(base, before_table$patched_untruncated_miles[i]),
    paste("Phase1 three-way mileage partition failed for population", id))
  assert(close(table$phase1_remaining_visible_miles[i], sum(got[1:2])) &&
    close(mi(remaining, id, yes(remaining$measure_habitat)), sum(got[1:2])) &&
    close(mi(removed, id, yes(removed$measure_habitat)), got[3]) &&
    close(mi(unresolved, id, yes(unresolved$measure_habitat)), got[2]),
    paste("Phase1 GIS subsets/remaining-mileage columns disagree for population", id))
  for (source in c("prediction", "swifd_connector", "alignment_link")) {
    assert(close(mi(all_lines, id, all_lines$source_class == source), mi(before, id, before$source_class == source)),
      paste("Phase1 changed source geometry length for", source, "in population", id))
  }
  assert(close(table$phase1_remaining_model_miles[i], mi(remaining, id,
    yes(remaining$measure_habitat) & remaining$source_class == "prediction")) &&
    close(table$phase1_remaining_swifd_miles[i], mi(remaining, id,
      yes(remaining$measure_habitat) & remaining$source_class == "swifd_connector")),
    paste("Remaining source-class CSV mileage differs from GIS for population", id))
}
canyon <- table[table$pop_id == 269, ]
canyon_removed_model <- mi(all_lines, 269, excluded & all_lines$source_class == "prediction")
assert(close(canyon$phase1_remaining_model_miles + canyon_removed_model, 11.927758921744),
  "Canyon269 source mileage was lost instead of partitioned by documented Total causes")
assert(isFALSE(metadata$graph_settings$maritime_connectivity_filter) &&
  metadata$graph_diagnostic_summary$source_partition_error_m <= 1e-5 &&
  metadata$watershed_summary$source_partition_error_m <= 1e-5 &&
  metadata$approved_upstream_basins == nrow(masks) &&
  metadata$pending_upstream_basins == sum(allowed & !yes(decisions$upstream_basin_approved)) &&
  metadata$selected_with_complete_upstream_coverage == sum(yes(coverage_audit$complete_upstream_coverage)) &&
  metadata$selected_covered_by_downstream_basin == sum(yes(coverage_audit$covered_by_downstream_basin)) &&
  metadata$selected_pending_upstream_coverage == sum(!yes(coverage_audit$complete_upstream_coverage)),
  "Scenario metadata contradicts complete-basin approval or source preservation")
for (key in audit$review_record_key[yes(audit$applied)]) {
  a <- anchors[anchors$review_record_key == key & yes(anchors$used), ]
  assert(nrow(a) == 1L && yes(a$direction_evidence_valid) && yes(a$anchor_reviewed) &&
    is.finite(a$snap_distance_m) && a$snap_distance_m <= a$max_snap_distance_m + 1e-8 &&
    !is.na(a$direction_basis) && nzchar(a$direction_basis) &&
    !is.na(a$direction_source_url) && grepl("^https://", a$direction_source_url),
    paste("An applied barrier lacks a unique reviewed downstream anchor:", key))
}

# This is the completeness test that the graph-only scenario could not pass:
# disconnected upstream model and SWIFD linework must also be absent.
coverage <- validate_upstream_basin_completeness(remaining, removed, masks, barrier_points)
assert(all(coverage$unproven_upstream_residual_m <= .001), "Unproven upstream residual remains")
assert(all(basin_audit$remaining_inside_m <= basin_audit$protected_local_downstream_m + .001),
  "Mask audit reports upstream geometry remaining outside proven local protection")

cat(sprintf("PASS: all%d baseline review files unchanged; all16 population/source partitions balance.\n", nrow(snapshot)))
cat(sprintf("PASS: %d selected, %d applied; only explicit Totals (either verification layer) or Gorge903.\n",
  sum(yes(decisions$phase1_selected)), nrow(applied)))
cat("Applied barriers by verification status:\n")
print(table(applied$verification, useNA = "ifany"))
cat(sprintf("Canyon269: %.10f model miles remaining, %.10f removed; source total preserved.\n",
  canyon$phase1_remaining_model_miles, canyon_removed_model))
cat(sprintf("BASIN COVERAGE: %d/%d approved; %d pending. Every approved mask has zero unproven upstream residual.\n",
  nrow(masks), sum(allowed), sum(allowed & !yes(decisions$upstream_basin_approved))))
cat(sprintf("TOTAL-REQUEST COVERAGE: %d/%d complete through own or proven downstream basin; %d still pending.\n",
  sum(yes(coverage_audit$complete_upstream_coverage)), nrow(coverage_audit),
  sum(!yes(coverage_audit$complete_upstream_coverage))))
cat(sprintf("Gorge watershed: %.3fmi2 total, %.3fmi2 in Canada; all captured isolated upstream source geometry removed.\n",
  attr(coverage, "gorge_total_sqmi"), attr(coverage, "gorge_canadian_sqmi")))
cat("Applied review numbers:", paste(applied$review_number, collapse = ", "), "\n")
