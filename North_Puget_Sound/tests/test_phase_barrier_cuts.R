source("R/network.R")
source("R/source_classification.R")
source("R/barrier_stream_identity.R")
source("R/stream_edge_identity.R")
source("R/phase_barrier_cuts.R")
library(sf)

model_lines <- function(...) st_sf(source_class = "prediction", measure_habitat = TRUE,
  bridge_id = NA_character_, geometry = st_sfc(lapply(list(...), st_linestring), crs = 26910))
strict_edges <- function(network) {
  g <- suppressWarnings(st_cast(st_union(st_geometry(network[network$measure_habitat, ])), "LINESTRING"))
  xy <- lapply(g, function(z) unclass(z)[, 1:2, drop = FALSE])
  endpoints <- do.call(rbind, lapply(xy, function(z) z[c(1L, nrow(z)), , drop = FALSE]))
  group <- network_endpoint_groups(endpoints, .25)
  from <- group[seq.int(1L, length(group), 2L)]
  to <- group[seq.int(2L, length(group), 2L)]
  degree <- tabulate(c(from, to), max(group))
  st_sf(graph_edge_id = seq_along(g), channel_chain_id = seq_along(g),
    from_degree = degree[from], to_degree = degree[to], geometry = g)
}
barrier_at <- function(point, edges, key = "test:total") st_sf(
  review_record_key = key, review_number = 1L,
  graph_edge_id = edges$graph_edge_id[st_nearest_feature(st_sfc(st_point(point), crs = 26910), edges)],
  explicit_total_passage = TRUE, override_future_cut = FALSE,
  snap_accepted = TRUE, stream_identity_confirmed = TRUE, identity_valid_for_cut = TRUE,
  junction_ambiguous = FALSE, matching_channel_count = 1L, snap_distance_m = 0,
  geometry = st_sfc(st_point(point), crs = 26910))
anchor_at <- function(point, barrier, target = barrier$graph_edge_id) st_sf(
  anchor_id = "reviewed_downstream", review_record_key = barrier$review_record_key,
  graph_edge_id = target, direction_evidence_valid = TRUE, anchor_reviewed = TRUE,
  max_snap_distance_m = 1, direction_basis = "Independent synthetic downstream evidence",
  direction_source_url = "synthetic-test", geometry = st_sfc(st_point(point), crs = 26910))
near <- function(a, b) stopifnot(isTRUE(all.equal(a, b, tolerance = 1e-7)))

# All pre-existing disconnected model components survive. Even a segment drawn
# geographically above the cut is retained when connectivity cannot prove it.
n <- model_lines(rbind(c(0, 0), c(0, 100)), rbind(c(5, 70), c(5, 100)),
  rbind(c(100, 0), c(100, 100)))
e <- strict_edges(n); b <- barrier_at(c(0, 50), e); a <- anchor_at(c(0, 10), b)
r <- cut_reviewed_barriers(n, b, e, a)
near(sum(r$removed$length_m), 50)
near(sum(r$retained$length_m), 180)
stopifnot(r$barrier_audit$applied, nrow(r$unresolved) == 0,
  length(unique(r$retained$original_component)) == 3L,
  all(r$removed$removed_by_barrier == "test:total"))
stopifnot(all(st_coordinates(r$removed)[, 2] >= 50))

# Vertex order cannot determine upstream or change the answer.
reverse <- n
st_geometry(reverse)[1L] <- st_sfc(st_linestring(rbind(c(0, 100), c(0, 0))), crs = 26910)
rr <- cut_reviewed_barriers(reverse, b, strict_edges(reverse), a)
near(sum(rr$removed$length_m), 50)
stopifnot(all(st_coordinates(rr$removed)[, 2] >= 50))

# Missing direction evidence does not delete a disconnected component.
unresolved <- cut_reviewed_barriers(n, b, e, a[FALSE, ])
near(sum(unresolved$retained$length_m), 230)
stopifnot(!nrow(unresolved$removed), !unresolved$barrier_audit$applied,
  unresolved$barrier_audit$cut_status == "unresolved_downstream_anchor_missing")
wrong_component <- a
wrong_component$graph_edge_id <- st_nearest_feature(st_sfc(st_point(c(100, 10)), crs = 26910), e)
st_geometry(wrong_component) <- st_sfc(st_point(c(100, 10)), crs = 26910)
unconnected <- cut_reviewed_barriers(n, b, e, wrong_component)
stopifnot(!nrow(unconnected$removed), unconnected$barrier_audit$cut_status == "unresolved_anchor_in_different_component")

# A display point, copied eligible flag, or geometry on a different selected
# stream cannot authorize any cut.
unknown <- b; unknown$explicit_total_passage <- FALSE; unknown$eligible_for_future_cut <- TRUE
stopifnot(!nrow(cut_reviewed_barriers(n, unknown, e, a)$removed))
ambiguous <- b; ambiguous$matching_channel_count <- 2L
stopifnot(!nrow(cut_reviewed_barriers(n, ambiguous, e, a)$removed))
wrong_edge <- b; wrong_edge$graph_edge_id <- wrong_component$graph_edge_id
bad <- cut_reviewed_barriers(n, wrong_edge, e, a)
stopifnot(!nrow(bad$removed), bad$barrier_audit$cut_status == "rejected_not_on_selected_edge")

# Near a fork, do not sever an arbitrary arm. An alternative physical route
# around a barrier also leaves direction unresolved and retains all geometry.
fork <- model_lines(rbind(c(0, 0), c(0, 100)), rbind(c(0, 50), c(50, 50)))
fe <- strict_edges(fork); fb <- barrier_at(c(0, 35), fe); fa <- anchor_at(c(0, 10), fb)
fr <- cut_reviewed_barriers(fork, fb, fe, fa)
stopifnot(!nrow(fr$removed), fr$barrier_audit$cut_status == "rejected_junction_proximity")
loop <- model_lines(rbind(c(0, 0), c(0, 100), c(100, 100), c(100, 0), c(0, 0)))
le <- strict_edges(loop); lb <- barrier_at(c(0, 50), le); la <- anchor_at(c(0, 10), lb)
lr <- cut_reviewed_barriers(loop, lb, le, la)
stopifnot(!nrow(lr$removed), lr$barrier_audit$cut_status == "unresolved_alternative_path_around_barrier")

# Source accounting survives a cut through a model+alignment+SWIFD chain;
# alignment lengths remain excluded from physical mileage.
chain <- model_lines(rbind(c(0, 0), c(0, 100)), rbind(c(0, 100), c(0, 110)),
  rbind(c(0, 110), c(0, 200)), rbind(c(200, 0), c(200, 100)))
chain$source_class[2:3] <- c("alignment_link", "swifd_connector")
chain$measure_habitat[2L] <- FALSE
chain$bridge_id[2:3] <- "bridge_1"
ce <- strict_edges(chain); cb <- barrier_at(c(0, 50), ce); ca <- anchor_at(c(0, 10), cb)
cr <- cut_reviewed_barriers(chain, cb, ce, ca)
near(cr$barrier_audit$removed_model_m, 50)
near(cr$barrier_audit$removed_swifd_m, 90)
near(cr$barrier_audit$removed_alignment_m, 10)
near(cr$summary$removed_physical_m, 140)
near(sum(cr$retained$length_m) + sum(cr$removed$length_m), sum(as.numeric(st_length(chain))))

# A named receiving-stream confluence independently roots the reference. The
# nearby anchor follows reference topology and is invariant to vertex order.
orient <- model_lines(rbind(c(0, 0), c(0, 1000)))
oe <- strict_edges(orient); ob <- barrier_at(c(0, 500), oe)
ob$matched_llid <- "1230000480000"; ob$matched_stream_name <- "Test Creek"
ob$TributaryToName <- "Receiving R"
reference <- st_sf(LLID = c("1230000480000", "1230000481000"),
  LLID_STRM_NAME = c("Test Creek", "Receiving River"), geometry = st_sfc(
    st_linestring(rbind(c(0, 0), c(0, 1000))),
    st_linestring(rbind(c(-1000, 0), c(1000, 0))), crs = 26910))
oa <- derive_downstream_anchors(ob, oe, reference)
stopifnot(nrow(oa$anchors) == 1L, oa$audit$direction_status == "validated_named_reference_downstream_anchor",
  oa$audit$root_validation_method == "unique_named_receiving_stream_confluence")
near(unname(st_coordinates(oa$anchors)[1L, 2L]), 400)
reference_reverse <- reference
st_geometry(reference_reverse)[1L] <- st_sfc(st_linestring(rbind(c(0, 1000), c(0, 0))), crs = 26910)
oar <- derive_downstream_anchors(ob, oe, reference_reverse)
near(unname(st_coordinates(oar$anchors)[1L, 2L]), 400)
# Two equally plausible receiving-water confluences cannot establish a root.
two_roots <- reference
st_geometry(two_roots)[1L] <- st_sfc(st_linestring(rbind(c(0, 0), c(0, 1000), c(500, 1000), c(500, 0))), crs = 26910)
oa_bad <- derive_downstream_anchors(ob, oe, two_roots)
stopifnot(!nrow(oa_bad$anchors), oa_bad$audit$direction_status == "unresolved_multiple_receiving_stream_confluences")

cat("Component-local total barrier cuts, disconnected preservation, independent named-reference direction, exact edge gates, cycles and source partitions passed.\n")
