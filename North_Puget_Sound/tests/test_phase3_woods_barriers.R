# Read-only source/geometry regression for the two individually reviewed Woods
# sites and the exact cross-divide source edge. No production exports run.
for (file in c("project_paths.R", "source_classification.R", "barrier_stream_identity.R",
    "stream_edge_identity.R", "network.R", "barrier_metadata.R", "phase_barrier_cuts.R",
    "upstream_basins.R", "nldi_delineation.R", "phase3_woods_barriers.R")) source(file.path("R", file))
suppressPackageStartupMessages(library(sf))
x <- load_phase3_woods_barriers()
stopifnot(identical(as.integer(x$barriers$review_number), c(10L, 914L)),
  identical(as.character(x$barriers$SiteId), c("891970", "891169")),
  all(x$barriers$report_blockage == "Total"), x$barriers$reported_height_m[1L] == 18.3,
  x$barriers$verification[1L] == "Verified", x$barriers$verification[2L] == "Not field verified",
  identical(x$barriers$phase3_selected, c(TRUE, FALSE)), !x$barriers$snap_accepted[2L],
  x$barriers$snap_distance_m[2L] > 394, x$barriers$snap_distance_m[2L] < 395,
  nrow(x$masks) == 1L, x$masks$review_record_key == "5:44379",
  x$masks$completeness_method == "full_drainageBasin")
gpkg <- "outputs/review_swifd_connections/North_Puget_Sound_Steelhead.gpkg"
edges <- st_read(gpkg, layer = "stream_matching_edges", quiet = TRUE)
network <- readRDS("spatial_data/north_puget_swifd_bridges.rds")$bridges$network
selected <- x$barriers[x$barriers$phase3_selected, ]
cuts <- cut_reviewed_barriers(network, selected, edges, x$anchors)
rows <- which(cuts$network$graph_edge_id == 4574L)
stopifnot(length(rows) == 1L)
# A separate barrier's evidence must survive the scoped Woods correction.
cuts$network$downstream_of_barrier[rows] <- upstream_append_key(
  cuts$network$downstream_of_barrier[rows], "fixture:other_barrier")
wkb <- st_as_binary(st_geometry(cuts$network))
review <- prepare_phase3_woods_graph(cuts$network, selected, edges)
stopifnot(identical(wkb, st_as_binary(st_geometry(review$network))),
  !upstream_has_key(review$network$downstream_of_barrier[rows], "5:44379"),
  upstream_has_key(review$network$downstream_of_barrier[rows], "fixture:other_barrier"),
  upstream_has_key(review$network$unresolved_by_barrier[rows], "5:44379"),
  all(review$network$downstream_of_barrier[-rows] == cuts$network$downstream_of_barrier[-rows]),
  nrow(review$audit) == 1L, review$audit$graph_edge_id == 4574L,
  review$audit$upstream_reference_comid == "23963905", nrow(review$course_checks) == 21L)
bad <- selected
bad$report_blockage <- "Partial"
rejected <- tryCatch({prepare_phase3_woods_graph(cuts$network, bad, edges); FALSE}, error = function(e) TRUE)
stopifnot(rejected)
bad <- selected
bad$snap_distance_m <- 51
rejected <- tryCatch({prepare_phase3_woods_graph(cuts$network, bad, edges); FALSE}, error = function(e) TRUE)
stopifnot(rejected)
selected$phase1_selected <- TRUE
checked <- validate_nldi_basin_masks(x$masks, review$network, selected)
stopifnot(nrow(checked$masks) == 1L, checked$audit$proven_upstream_outside_mask_m < 1e-6,
  checked$audit$proven_downstream_inside_mask_m > 22,
  checked$audit$proven_downstream_inside_mask_m < 24,
  checked$audit$max_local_disagreement_distance_m < 23)
applied <- apply_upstream_basin_masks(review$network, checked$masks, selected)
remote <- applied$network[applied$network$graph_edge_id %in% 4574L, ]
stopifnot(sum(remote$length_m[remote$removed]) > 2258,
  sum(remote$length_m[remote$retained]) > 2943,
  applied$barrier_audit$remaining_inside_m > 22,
  applied$barrier_audit$remaining_inside_m < 24,
  applied$summary$source_partition_error_m < 1e-5)
cat("PASS: Woods10 report/strict channel/full basin;914held beyond50m; exact scoped graph correction preserves all geometry/other keys; remote upstream removed and local downstream protected.\n")
