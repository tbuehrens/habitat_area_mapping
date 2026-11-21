# Independent geometry fixtures for strict line-level barrier identity.
# No downloads, production exports, or barrier cuts are performed.
source(file.path("R", "barrier_stream_identity.R"))
source(file.path("R", "stream_edge_identity.R"))
suppressPackageStartupMessages(library(sf))

strict_assert <- function(value, message) if (!isTRUE(value)) stop(message, call. = FALSE)
offset <- c(500000, 5400000)
xy <- function(x) sweep(x, 2, offset, "+")
line <- function(x) st_linestring(xy(x))
point <- function(x) st_point(x + offset)
physical <- function(...) st_sf(source_class = "prediction", measure_habitat = TRUE,
  geometry = st_sfc(..., crs = 26910))
reference <- function(geometry, name = "Main River", llid = "main") {
  st_sf(LLID = llid, LLID_STRM_NAME = name, geometry = geometry)
}
main <- physical(line(rbind(c(0, 0), c(1000, 0))))
main_reference <- reference(st_geometry(main))
main_wkb <- st_as_binary(st_geometry(main))

candidate <- function(index, location, edge_id = NULL) {
  result <- st_sf(geometry = st_sfc(point(location), crs = 26910))
  if (is.null(edge_id)) edge_id <- index$edges$graph_edge_id[st_nearest_feature(result, index$edges)]
  result$graph_edge_id <- edge_id
  result
}
assess <- function(index, location = c(500, 0), own = "Main R", receiving = "Other R", edge_id = NULL, ...) {
  assess_stream_edge_identity(index, candidate(index, location, edge_id), own, receiving, ...)
}

# A name is supported along the assigned physical edge, with explicit evidence.
index <- build_stream_identity_index(main, main_reference)
good <- assess(index)
strict_assert(good$own_stream_match && good$stream_identity_valid && good$identity_valid_for_cut,
  "A unique aligned own-stream reference should establish line identity")
strict_assert(!good$junction_ambiguous && good$support_length_m >= 40 && good$support_coverage >= .8,
  "Positive line identity needs sufficient support and no junction ambiguity")
strict_assert(good$matched_llid == "main" && good$matched_stream_name == "Main River",
  "Positive identity must identify its named reference")
strict_assert(inherits(attr(good, "sample_points"), "sf"), "Sample evidence must be available for audit")
strict_assert(identical(main_wkb, st_as_binary(st_geometry(main))), "Identity assessment modified source geometry")

# Nearby at a single point is insufficient: the named reference crosses the
# physical edge at right angles and does not identify the horizontal waterway.
cross_reference <- reference(st_sfc(line(rbind(c(500, -300), c(500, 300))), crs = 26910),
  name = "Side Creek", llid = "side_crossing")
cross_index <- build_stream_identity_index(main, cross_reference)
cross <- assess(cross_index, own = "Side Cr")
strict_assert(!cross$stream_identity_valid && !cross$identity_valid_for_cut,
  "A crossing reference near the snapped point must not name the physical edge")

# Unknown barrier identity, receiving-stream identity, unknown reference names,
# and missing references all block eligibility; none falls back to nearest line.
unnamed <- assess(index, own = "unnamed", receiving = "Main R")
mismatch <- assess(index, own = "Little Cr", receiving = "Other R")
receiving <- assess(index, own = "Little Cr", receiving = "Main R")
missing_index <- build_stream_identity_index(main, main_reference[0, ])
missing <- assess(missing_index)
unknown_reference <- main_reference
unknown_reference$LLID_STRM_NAME <- "Unnamed Creek"
unknown_index <- build_stream_identity_index(main, unknown_reference)
unknown <- assess(unknown_index)
for (item in list(unnamed, mismatch, receiving, missing, unknown)) {
  strict_assert(!item$stream_identity_valid && !item$identity_valid_for_cut,
    "An unnamed, mismatched, or unsupported identity was allowed for a future cut")
}
strict_assert(receiving$receiving_stream_conflict, "Receiving-stream confusion must be explicit")
same_receiving <- assess(index, own = "Main River", receiving = "Main R")
strict_assert(same_receiving$own_stream_match && !same_receiving$receiving_stream_conflict,
  "A receiving field repeating the own name must not reject a valid own-stream match")

# Distinct competing LLIDs with the same own name are still ambiguous. A
# deterministic nearest-reference choice cannot turn them into confirmation.
parallel_reference <- st_sf(LLID = c("own_1", "own_2"), LLID_STRM_NAME = "Main River",
  geometry = st_sfc(line(rbind(c(0, 0), c(1000, 0))),
    line(rbind(c(0, 3), c(1000, 3))), crs = 26910))
parallel_index <- build_stream_identity_index(main, parallel_reference)
parallel <- assess(parallel_index)
strict_assert(!parallel$stream_identity_valid && !parallel$identity_valid_for_cut,
  "Competing supported own-stream reference IDs must block confirmation")
reverse_index <- build_stream_identity_index(main, parallel_reference[2:1, ])
reverse <- assess(reverse_index)
strict_assert(identical(parallel$stream_identity_valid, reverse$stream_identity_valid) &&
  identical(parallel$identity_valid_for_cut, reverse$identity_valid_for_cut),
  "Reference input order changed an ambiguity decision")

# A true physical junction is a separate blocker even when the specified
# mainstem edge has strong own-name support. Edge assignment stays explicit.
tee <- physical(line(rbind(c(0, 0), c(1000, 0))), line(rbind(c(500, 0), c(500, 200))))
tee_index <- build_stream_identity_index(tee, main_reference)
left_id <- candidate(tee_index, c(450, 0))$graph_edge_id
near_junction <- assess(tee_index, c(490, 0), edge_id = left_id)
strict_assert(near_junction$junction_ambiguous && !near_junction$identity_valid_for_cut,
  "A barrier near a physical junction must be blocked from future cuts")
strict_assert(identical(as.character(near_junction$graph_edge_id), as.character(left_id)),
  "Junction assessment silently reassigned the specified physical edge")
away <- assess(tee_index, c(400, 0), edge_id = left_id)
strict_assert(!away$junction_ambiguous && away$identity_valid_for_cut,
  "An aligned mainstem point away from a junction should remain confirmable")

# A very short supported reach is not enough evidence for the default minimum
# supported line length, even though its nearest reference distance is zero.
short <- physical(line(rbind(c(0, 0), c(20, 0))))
short_index <- build_stream_identity_index(short, reference(st_geometry(short)))
short_result <- assess(short_index, c(10, 0))
strict_assert(!short_result$identity_valid_for_cut,
  "A short coincident edge bypassed the minimum line-support requirement")

cat("PASS: strict line-level identity distinguishes aligned, crossing, unknown, competing, and junction cases; no source geometry changes.\n")
