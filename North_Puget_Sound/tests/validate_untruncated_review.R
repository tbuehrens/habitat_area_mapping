# Validate the real exported review against the cached raw model and full
# SWIFD augmentation. This performs no downloads, exports, or cache mutations.
# Rscript tests/validate_untruncated_review.R [output_directory]
library(sf)
arguments <- commandArgs(trailingOnly = TRUE)
output_dir <- if (length(arguments)) arguments[1L] else "outputs/review_swifd_connections"
assert <- function(value, message) if (!isTRUE(value)) stop(message, call. = FALSE)
near <- function(a, b, tolerance = 1e-6) {
  length(a) == length(b) && all(is.finite(a)) && all(is.finite(b)) &&
    all(abs(a - b) <= tolerance)
}
source("R/swifd_sources.R")
bridges <- readRDS("spatial_data/north_puget_swifd_bridges.rds")$bridges
raw <- st_transform(readRDS("spatial_data/north_puget_steelhead.rds"), 26910)
swifd <- readRDS("spatial_data/north_puget_swifd_steelhead.rds")
files <- c("population_distribution_miles.csv", "barrier_review.csv",
           "barrier_snap_rejections.csv", "barrier_match_candidates.csv", "run_metadata.json")
assert(all(file.exists(file.path(output_dir, files))), "Untruncated review products are missing")
gpkg_candidates <- list.files(output_dir, pattern = "\\.gpkg$", full.names = TRUE)
assert(length(gpkg_candidates) == 1L, "Expected exactly one review GeoPackage")
gpkg <- gpkg_candidates[1L]
required_layers <- c("noaa_populations", "raw_occupancy_model", "steelhead_patched_untruncated",
  "swifd_gap_routes", "endpoint_alignment_links", "barriers_snapped", "barriers_original",
  "gap_endpoint_audit", "barrier_match_candidates", "stream_matching_edges")
assert(all(required_layers %in% st_layers(gpkg)$name), "Review GeoPackage layers are incomplete")
assert(!any(c("predicted_extent_status", "natural_barriers_audit") %in% st_layers(gpkg)$name),
       "Obsolete barrier-cut layers remain in the review GeoPackage")
population <- st_read(gpkg, layer = "noaa_populations", quiet = TRUE)
raw_export <- st_read(gpkg, layer = "raw_occupancy_model", quiet = TRUE)
patched <- st_read(gpkg, layer = "steelhead_patched_untruncated", quiet = TRUE)
routes <- st_read(gpkg, layer = "swifd_gap_routes", quiet = TRUE)
alignments <- st_read(gpkg, layer = "endpoint_alignment_links", quiet = TRUE)
snapped <- st_read(gpkg, layer = "barriers_snapped", quiet = TRUE)
original_barriers <- st_read(gpkg, layer = "barriers_original", quiet = TRUE)
candidate_matches <- st_read(gpkg, layer = "barrier_match_candidates", quiet = TRUE)
stream_edges <- st_read(gpkg, layer = "stream_matching_edges", quiet = TRUE)
table <- read.csv(file.path(output_dir, "population_distribution_miles.csv"))
barrier_table <- read.csv(file.path(output_dir, "barrier_review.csv"))
metadata <- jsonlite::fromJSON(file.path(output_dir, "run_metadata.json"))

assert(isFALSE(metadata$barrier_truncation_applied), "Review metadata must explicitly disable barrier truncation")
assert(metadata$barrier_summary$actual_cuts == 0, "Metadata records an actual barrier cut")
assert("barrier_truncation_applied" %in% names(table) && all(!table$barrier_truncation_applied),
       "Population mileage must explicitly state that no barrier truncation was applied")
assert(nrow(population) == 16L && nrow(table) == 16L,
       "Expected all 16 NOAA population polygons and mileage rows")
assert(setequal(population$pop_id, table$pop_id), "Mileage population IDs differ from NOAA polygons")
maps <- list.files(file.path(output_dir, "maps"), pattern = "\\.png$", full.names = TRUE)
assert(length(maps) == 16L, "Expected exactly 16 population maps")
canyon_map <- maps[grepl("^269_", basename(maps))]
assert(length(canyon_map) == 1L && file.info(canyon_map)$size > 0,
       "Canyon Creek map is missing or empty")

for (name in c("raw_model_miles", "swifd_added_miles", "patched_untruncated_miles",
               "alignment_link_miles_not_counted")) {
  assert(name %in% names(table) && all(is.finite(table[[name]])) && all(table[[name]] >= 0),
         paste("Invalid mileage column:", name))
}
assert(near(table$patched_untruncated_miles, table$raw_model_miles + table$swifd_added_miles),
       "Patched mileage must equal the complete raw model plus SWIFD routes, without alignment mileage")
for (geometry in list(raw_export, patched, routes, alignments)) {
  if ("removed" %in% names(geometry)) assert(!any(geometry$removed), "Review exports contain removed stream pieces")
  if ("status" %in% names(geometry)) assert(!any(grepl("removed_above_barrier", geometry$status)),
    "Review stream status still contains barrier truncation")
}

length_by_population <- function(lines, measured_only = FALSE) {
  if (measured_only && "measure_habitat" %in% names(lines)) lines <- lines[as.logical(lines$measure_habitat), ]
  if (!nrow(lines)) return(rep(0, nrow(population)))
  clipped <- if ("pop_id" %in% names(lines)) lines else
    suppressWarnings(st_intersection(lines, population["pop_id"]))
  clipped <- clipped[!st_is_empty(clipped) & st_dimension(clipped) == 1, ]
  vapply(population$pop_id, function(id) {
    geometry <- st_geometry(clipped[clipped$pop_id == id, ])
    if (!length(geometry)) return(0)
    sum(as.numeric(st_length(st_union(geometry)))) / 1609.344
  }, numeric(1))
}
raw_expected <- length_by_population(raw)
route_expected <- length_by_population(bridges$connectors)
alignment_expected <- length_by_population(bridges$alignment_links)
table <- table[match(population$pop_id, table$pop_id), ]
assert(near(length_by_population(raw_export), raw_expected), "Exported raw model lost or added source linework")
assert(near(length_by_population(routes), route_expected), "Exported SWIFD gap routes were truncated or changed")
assert(near(length_by_population(alignments), alignment_expected), "Exported alignment links differ from the uncut graph")
assert(near(length_by_population(patched, measured_only = TRUE), raw_expected + route_expected),
       "The patched review is not the full untruncated raw-plus-SWIFD network")
assert(near(table$raw_model_miles, raw_expected), "Raw-model CSV mileage differs from the source snapshot")
assert(near(table$swifd_added_miles, route_expected), "SWIFD CSV mileage differs from the full uncut routes")
assert(near(table$alignment_link_miles_not_counted, alignment_expected), "Alignment audit mileage differs from the graph")
canyon <- table[table$pop_id == 269, ]
assert(near(canyon$raw_model_miles, 11.927758921744), "Canyon Creek raw model must retain all 11.9277589 miles")

assert(nrow(eligible_steelhead_swifd(swifd)) == nrow(swifd), "Cached SWIFD contains ineligible categories")
assert(!any(grepl("^Gradient", swifd$DISTTYPE_DESC, ignore.case = TRUE), na.rm = TRUE) &&
       !any(grepl("^Gradient", swifd$USETYPE_DESC, ignore.case = TRUE), na.rm = TRUE),
       "Gradient distribution must be excluded from routing")
assert(all(c("Presence", "Rearing", "Spawning") %in% swifd$USETYPE_DESC),
       "Presence, rearing, and spawning records must remain eligible")
assert(any(grepl("^Artificial", swifd$DISTTYPE_DESC)) &&
       any(grepl("^Transported", swifd$DISTTYPE_DESC)) &&
       any(grepl("^Presumed", swifd$DISTTYPE_DESC)),
       "Artificial, transported, and presumed distribution were unexpectedly excluded")
assert(all(bridges$bridge_audit$from_snap_m <= 100 + 1e-8) &&
       all(bridges$bridge_audit$to_snap_m <= 100 + 1e-8) &&
       all(bridges$bridge_audit$routed_swifd_m > 0), "A connector violates the SWIFD endpoint/path criteria")

barrier_fields <- c("review_record_key", "review_number", "snap_distance_m", "future_cut_candidate",
  "truncation_applied", "explicit_total_passage", "override_future_cut", "future_cut_requested",
  "stream_identity_confirmed", "junction_ambiguous", "eligible_for_future_cut",
  "identity_review_status", "candidate_count", "own_stream_candidate_count",
  "snap_accepted", "display_location_available")
assert(all(barrier_fields %in% names(barrier_table)), "Barrier review columns are incomplete")
assert(all(!barrier_table$truncation_applied) && !anyNA(barrier_table$truncation_applied),
       "Barrier review must not apply any truncation")
if ("applied" %in% names(barrier_table)) {
  assert(all(!barrier_table$applied) && !anyNA(barrier_table$applied), "A barrier cut was marked applied")
}
assert(all(barrier_table$future_cut_requested ==
  (barrier_table$explicit_total_passage | barrier_table$override_future_cut)),
  "Cut requests must have explicit total-passage evidence or an explicit user override")
assert(all(barrier_table$future_cut_candidate == barrier_table$eligible_for_future_cut) &&
  all(barrier_table$eligible_for_future_cut ==
    (barrier_table$future_cut_requested & barrier_table$snap_accepted)),
  "Future-cut eligibility must require both a cut request and strict accepted stream identity")
assert(all(!barrier_table$snap_accepted |
    (barrier_table$stream_identity_confirmed & !barrier_table$junction_ambiguous)),
  "An unconfirmed identity or ambiguous junction was accepted for a future cut")
assert(!any(barrier_table$eligible_for_future_cut[
  !barrier_table$stream_identity_confirmed | barrier_table$junction_ambiguous]),
  "Unknown, mismatched, ambiguous, or junction-blocked locations must not be eligible for future cuts")
if ("allow_cut" %in% names(barrier_table)) {
  assert(all(!barrier_table$allow_cut |
    (barrier_table$stream_identity_confirmed & !barrier_table$junction_ambiguous)),
    "A legacy allow_cut flag contradicts the strict identity/junction decision")
}
unknown <- !barrier_table$explicit_total_passage & !barrier_table$override_future_cut
assert(!any(barrier_table$future_cut_candidate[unknown]),
       "Unknown passability was incorrectly treated as total blockage")
if ("barrier_key" %in% names(barrier_table)) {
  gorge <- barrier_table[barrier_table$barrier_key == "user_override:Gorge_Dam", ]
  assert(nrow(gorge) == 1L && isTRUE(gorge$future_cut_requested) &&
         isTRUE(gorge$override_future_cut) && !isTRUE(gorge$explicit_total_passage),
         "Gorge Dam must remain an explicitly labeled request, without fabricated passability or identity")
}
assert(!anyNA(barrier_table$review_number) && !anyDuplicated(barrier_table$review_number),
       "Numbered barrier labels must be present and unique")
assert(all(is.finite(barrier_table$snap_distance_m)) && all(barrier_table$snap_distance_m <= 50 + 1e-8),
       "Numbered barrier points must be within the 50-meter snapping tolerance")
assert(all(barrier_table$display_location_available),
       "Numbered rows must identify display locations, including strict-identity failures")

# Existing numbers belong to source records, not to a changing subset of
# confirmed matches. Preserve the actual 903-row pre-strict-review snapshot.
prior_path <- if (length(arguments) >= 2L) arguments[2L] else
  "spatial_data/strict_identity_review/barrier_review_before_strict.csv"
if (file.exists(prior_path)) {
  prior <- read.csv(prior_path, stringsAsFactors = FALSE)
  assert(!anyDuplicated(prior$review_record_key), "Prior numbering keys are not unique")
  current_index <- match(prior$review_record_key, barrier_table$review_record_key)
  assert(!anyNA(current_index), "A previously numbered location-review record disappeared")
  assert(identical(as.integer(barrier_table$review_number[current_index]), as.integer(prior$review_number)),
         "Existing source-record review numbers changed")
  new <- !barrier_table$review_record_key %in% prior$review_record_key
  assert(all(barrier_table$review_number[new] > max(prior$review_number)),
         "New record numbers were inserted into the existing numbering")
}
assert("review_number" %in% names(snapped) && setequal(snapped$review_number, barrier_table$review_number),
       "Numbered barrier CSV and map/GIS points differ")
physical <- bridges$network[bridges$network$measure_habitat, ]
if (nrow(snapped)) {
  nearest <- st_nearest_feature(snapped, physical)
  distance <- as.numeric(st_distance(snapped, physical[nearest, ], by_element = TRUE))
  assert(all(distance <= 1e-5), "A snapped barrier is not on a physical model/SWIFD stream")
  if (all("barrier_key" %in% names(snapped), "barrier_key" %in% names(original_barriers))) {
    original <- original_barriers[match(snapped$barrier_key, original_barriers$barrier_key), ]
    displacement <- as.numeric(st_distance(snapped, original, by_element = TRUE))
    assert(all(is.finite(displacement)) && all(displacement <= 50 + 1e-8),
           "A numbered barrier was moved more than 50 meters from its original location")
  }
}

# Independently enumerate every original-point/physical-edge pair within 50 m.
# This catches nearest-only searches, skipped unnamed candidates, and candidate
# audits that silently omit an alternative stream. No positive identity is
# inferred from this location test.
assert(all(c("review_record_key", "graph_edge_id") %in% names(candidate_matches)) &&
  "graph_edge_id" %in% names(stream_edges), "Candidate audit lacks record/physical-edge identifiers")
assert(!anyDuplicated(stream_edges$graph_edge_id), "Physical matching edge IDs are not unique")
nearby_edges <- st_is_within_distance(st_transform(original_barriers, st_crs(stream_edges)),
  stream_edges, dist = 50 + 1e-8)
expected_pairs <- unlist(lapply(seq_along(nearby_edges), function(i) {
  if (!length(nearby_edges[[i]])) return(character())
  paste(original_barriers$review_record_key[i], stream_edges$graph_edge_id[nearby_edges[[i]]], sep = "|")
}), use.names = FALSE)
actual_pairs <- paste(candidate_matches$review_record_key, candidate_matches$graph_edge_id, sep = "|")
assert(!anyDuplicated(actual_pairs) && setequal(actual_pairs, expected_pairs),
       "Candidate audit must include every physical edge within 50 m, exactly once")
expected_count <- lengths(nearby_edges)[match(barrier_table$review_record_key, original_barriers$review_record_key)]
assert(identical(as.integer(barrier_table$candidate_count), as.integer(expected_count)),
       "Barrier candidate counts differ from independent spatial enumeration")
in_radius <- original_barriers$review_record_key[lengths(nearby_edges) > 0]
assert(setequal(in_radius, barrier_table$review_record_key),
       "An in-radius location was dropped instead of retained for numbered review")
if (nrow(candidate_matches)) {
  assigned <- match(candidate_matches$graph_edge_id, stream_edges$graph_edge_id)
  assert(!anyNA(assigned), "A candidate refers to a missing physical edge")
  assigned_distance <- as.numeric(st_distance(candidate_matches, stream_edges[assigned, ], by_element = TRUE))
  assert(all(assigned_distance <= 1e-5), "Candidate snapped geometry is not on its assigned physical edge")
  assert("selected_for_location" %in% names(candidate_matches), "Candidate audit lacks the location-selection flag")
  selected <- candidate_matches[as.logical(candidate_matches$selected_for_location), ]
  assert(!anyDuplicated(selected$review_record_key) &&
    setequal(selected$review_record_key, barrier_table$review_record_key),
    "Every numbered record needs exactly one selected candidate location")
  selected <- selected[match(barrier_table$review_record_key, selected$review_record_key), ]
  mapped <- snapped[match(barrier_table$review_record_key, snapped$review_record_key), ]
  assert(all(as.numeric(st_distance(selected, mapped, by_element = TRUE)) <= 1e-5),
    "Mapped review locations differ from the selected candidate geometry")
  accepted <- barrier_table$snap_accepted
  assert(all(selected$stream_identity_valid[accepted]) &&
    all(selected$own_stream_match[accepted]) && !any(selected$junction_ambiguous[accepted]),
    "An accepted barrier lacks positive own-stream evidence on its selected physical candidate")
}

cat("PASS: all 16 populations retain the complete raw model plus eligible SWIFD patches; no barrier truncation.\n")
cat(sprintf("Canyon Creek: %.10f raw model miles; model/route/alignment tables match GIS and cached sources.\n",
            canyon$raw_model_miles))
cat("PASS: connector endpoints <=100m, gradient excluded, requested SWIFD categories included, numbered barriers <=50m/on physical streams.\n")
cat("PASS: all in-radius candidate pairs retained; strict identity/junction eligibility; existing source-record review numbers preserved.\n")
