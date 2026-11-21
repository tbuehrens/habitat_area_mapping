# Independent topology fixtures. Evidence/field-verification selection belongs
# to the Phase1 export policy; this tests the conservative cut mechanism.
source(file.path("R", "source_classification.R"))
source(file.path("R", "network.R"))
source(file.path("R", "barrier_stream_identity.R"))
source(file.path("R", "stream_edge_identity.R"))
source(file.path("R", "phase_barrier_cuts.R"))
suppressPackageStartupMessages(library(sf))
check <- function(value, message) if (!isTRUE(value)) stop(message, call. = FALSE)
offset <- c(500000, 5400000)
line <- function(x) st_linestring(sweep(x, 2, offset, "+"))
point <- function(x) st_point(x + offset)
network <- st_sf(source_class = c("prediction", "swifd_connector", "prediction",
    "prediction", "prediction", "alignment_link"),
  measure_habitat = c(rep(TRUE, 5), FALSE),
  bridge_id = c(NA, "patch", NA, NA, NA, "patch"),
  geometry = st_sfc(line(rbind(c(0, 0), c(100, 0))),
    line(rbind(c(100, 0), c(200, 0))), line(rbind(c(200, 0), c(400, 0))),
    line(rbind(c(350, 0), c(350, 100))), line(rbind(c(1000, 100), c(1100, 100))),
    line(rbind(c(100, 0), c(100, 10))), crs = 26910))
reference <- st_sf(LLID = "main", LLID_STRM_NAME = "Main River",
  geometry = st_sfc(line(rbind(c(0, 0), c(400, 0))), crs = 26910))
edges <- build_stream_identity_index(network, reference)$edges
point_edge <- function(location, table = edges) table$graph_edge_id[
  st_nearest_feature(st_sfc(point(location), crs = 26910), table)]
barrier <- st_sf(review_record_key = "verified_total_fixture", review_number = 42L,
  graph_edge_id = point_edge(c(300, 0)), snap_accepted = TRUE,
  stream_identity_confirmed = TRUE, identity_valid_for_cut = TRUE,
  junction_ambiguous = FALSE, matching_channel_count = 1L, snap_distance_m = 0,
  explicit_total_passage = TRUE, override_future_cut = FALSE, matched_llid = "main",
  geometry = st_sfc(point(c(300, 0)), crs = 26910))
anchor <- st_sf(anchor_id = "reviewed_downstream", review_record_key = barrier$review_record_key,
  graph_edge_id = barrier$graph_edge_id, direction_evidence_valid = TRUE,
  anchor_reviewed = TRUE, max_snap_distance_m = 1, expected_llid = "main",
  direction_basis = "Fixture's explicitly reviewed downstream anchor", direction_source_url = "fixture",
  geometry = st_sfc(point(c(250, 0)), crs = 26910))
physical_length <- function(x) sum(as.numeric(st_length(x[x$measure_habitat, ])))
near <- function(a, b) abs(a - b) < 1e-7
result <- cut_reviewed_barriers(network, barrier, edges, anchor)
check(result$barrier_audit$applied && near(physical_length(result$removed), 200) &&
  near(physical_length(result$retained), 400),
  "The cut must remove only the proven upstream 100 m mainstem and 100 m tributary")
check(near(sum(as.numeric(st_length(result$removed[result$removed$source_class == "prediction", ]))), 200) &&
  !any(result$removed$source_class %in% c("swifd_connector", "alignment_link")),
  "Source provenance or alignment exclusion changed during the cut")
unrelated <- result$network[lengths(st_intersects(result$network,
  st_sfc(point(c(1050, 100)), crs = 26910))) > 0, ]
check(nrow(unrelated) == 1L && unrelated$retained && !unrelated$removed && !unrelated$unresolved_direction &&
  unrelated$cut_status == "retained_unaffected", "An unrelated disconnected component was filtered out")

# Green stream identity is insufficient without a total-evidence request.
green_only <- barrier
green_only$explicit_total_passage <- FALSE
uncut <- cut_reviewed_barriers(network, green_only, edges, anchor)
check(!any(uncut$network$removed) && !any(uncut$network$unresolved_direction) &&
  !uncut$barrier_audit$applied, "Stream identity alone caused a barrier cut")

# Missing direction retains every piece visibly. Its unresolved subset is
# separate from resolved retention for the three-way mileage equation.
unknown_direction <- cut_reviewed_barriers(network, barrier, edges, anchor[0, ])
check(!any(unknown_direction$network$removed) &&
  near(physical_length(unknown_direction$unresolved), 500) &&
  near(physical_length(unknown_direction$network[
    !unknown_direction$network$removed & !unknown_direction$network$unresolved_direction, ]), 100),
  "Unknown upstream direction must remain visible and leave unrelated components resolved")

# The graph independently checks an actual junction even if its input flag was
# erroneously false. A requested cut does not authorize severing a confluence.
at_junction <- barrier
st_geometry(at_junction) <- st_sfc(point(c(350, 0)), crs = 26910)
junction_result <- cut_reviewed_barriers(network, at_junction, edges, anchor)
check(!any(junction_result$network$removed) && !junction_result$barrier_audit$applied,
  "A physical confluence was cut despite the junction safeguard")

# Reverse every source line. The explicit downstream anchor, rather than
# vertex order, must determine the exact same removed geometry.
reversed <- network
st_geometry(reversed) <- st_sfc(lapply(st_geometry(network), function(x)
  st_linestring(unclass(x)[nrow(x):1, , drop = FALSE])), crs = 26910)
backward <- cut_reviewed_barriers(reversed, barrier, edges, anchor)
difference <- st_sym_difference(st_union(st_geometry(result$removed)),
  st_union(st_geometry(backward$removed)))
check(near(sum(as.numeric(st_length(difference))), 0), "Vertex order changed the upstream cut")

# A bypass around the barrier proves that cutting a single vertex has not
# isolated an upstream side; retain the component instead of assuming a cut.
loop <- st_sf(source_class = "prediction", measure_habitat = TRUE, bridge_id = NA_character_,
  geometry = st_sfc(line(rbind(c(0, 0), c(400, 0))),
    line(rbind(c(250, 0), c(250, -100), c(400, -100), c(400, 0))), crs = 26910))
loop_edges <- build_stream_identity_index(loop, reference)$edges
loop_barrier <- barrier
loop_barrier$graph_edge_id <- point_edge(c(300, 0), loop_edges)
loop_anchor <- anchor
loop_anchor$graph_edge_id <- loop_barrier$graph_edge_id
st_geometry(loop_anchor) <- st_sfc(point(c(270, 0)), crs = 26910)
bypass <- cut_reviewed_barriers(loop, loop_barrier, loop_edges, loop_anchor)
check(!any(bypass$network$removed) && !bypass$barrier_audit$applied &&
  bypass$barrier_audit$cut_status == "unresolved_alternative_path_around_barrier",
  "An alternative channel was ignored when inferring an upstream side")

for (x in list(result, uncut, unknown_direction, junction_result, backward)) {
  check(near(physical_length(x$network), 600) && x$summary$source_partition_error_m < 1e-7,
    "A cut changed total source geometry before its retained/removed partition")
}
cat("PASS: partial-edge component-local cuts, upstream tributaries, unrelated disconnected retention, green-only no cut, unknown direction visible, junction and bypass blocking, reversed orientation, source-length balance.\n")
