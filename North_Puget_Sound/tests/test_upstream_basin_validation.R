source("R/upstream_basins.R")
library(sf)
offset <- c(500000, 5400000)
xy <- function(x) sweep(x, 2, offset, "+")
key <- "test:total"
graph <- st_sf(source_class = "prediction", measure_habitat = TRUE, bridge_id = NA_character_,
  removed = c(FALSE, TRUE), removed_by_barrier = c("", key), downstream_of_barrier = c(key, ""),
  geometry = st_sfc(st_linestring(xy(rbind(c(-1000, 0), c(0, 0)))),
    st_linestring(xy(rbind(c(0, 0), c(1000, 0)))), crs = 26910))
barrier <- st_sf(review_record_key = key, graph_edge_id = 1L, explicit_total_passage = TRUE,
  snap_accepted = TRUE, stream_identity_confirmed = TRUE, identity_valid_for_cut = TRUE,
  matching_channel_count = 1L, junction_ambiguous = FALSE,
  geometry = st_sfc(st_point(offset), crs = 26910))
mask <- function(start) st_sf(review_record_key = key, graph_edge_id = 1L,
  watershed_validated = TRUE, stream_identity_validated = TRUE, total_barrier_approved = TRUE,
  coverage_complete = TRUE, local_outlet_precision_validated = FALSE,
  geometry = st_sfc(st_polygon(list(xy(rbind(c(start, -100), c(2000, -100),
    c(2000, 100), c(start, 100), c(start, -100))))), crs = 26910))

exact <- validate_nldi_basin_masks(mask(0), graph, barrier)
stopifnot(nrow(exact$masks) == 1L, exact$audit$mask_approved)
collection <- mask(0)
st_geometry(collection) <- st_sfc(st_geometrycollection(list(st_geometry(collection)[[1L]],
  st_linestring(xy(rbind(c(3000, 0), c(3100, 0)))))), crs = 26910)
normalized <- validate_nldi_basin_masks(collection, graph, barrier)
stopifnot(nrow(normalized$masks) == 1L, normalized$audit$basin_polygon_extracted_from_collection)
local <- validate_nldi_basin_masks(mask(-30), graph, barrier)
stopifnot(nrow(local$masks) == 1L, abs(local$audit$proven_downstream_inside_mask_m - 30) < 1e-7)
partition <- apply_upstream_basin_masks(graph, local$masks, barrier)
stopifnot(abs(partition$barrier_audit$remaining_inside_m - 30) < 1e-7,
  abs(partition$barrier_audit$protected_local_downstream_m - 30) < 1e-7)
bad_upstream <- validate_nldi_basin_masks(mask(200), graph, barrier)
stopifnot(!nrow(bad_upstream$masks), grepl("known_upstream", bad_upstream$audit$approval_reason))
bad_downstream <- validate_nldi_basin_masks(mask(-200), graph, barrier)
stopifnot(!nrow(bad_downstream$masks), grepl("known_downstream", bad_downstream$audit$approval_reason))
no_direction <- graph; no_direction$downstream_of_barrier <- ""
held <- validate_nldi_basin_masks(mask(0), no_direction, barrier)
stopifnot(!nrow(held$masks), grepl("direction_missing", held$audit$approval_reason))
stopifnot(!nrow(validate_nldi_basin_masks(mask(0)[FALSE, ], graph, barrier)$masks))
empty <- apply_upstream_basin_masks(graph, exact$masks[FALSE, ], barrier)
stopifnot(nrow(empty$network) == nrow(graph), !empty$summary$applied_masks)
cat("Basin validation requires distinct exact graph direction and bounds both upstream/downstream disagreements; local protected stubs and empty batches pass.\n")
