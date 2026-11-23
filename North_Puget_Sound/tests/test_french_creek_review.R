# Real-source regression for the newly reviewed lower French Creek cut. This
# reads caches and exercises source/channel/full-basin gates without outputs.
for (module in c("project_paths", "source_classification", "barrier_stream_identity", "stream_edge_identity",
    "network", "barrier_metadata", "phase_barrier_cuts", "upstream_basins", "nldi_delineation", "french_creek_review"))
  source(file.path("R", paste0(module, ".R")))
bundle <- load_french_creek_review()
stopifnot(identical(bundle$barriers$review_number, c(563L, 581L, 915L)),
  identical(bundle$barriers$report_blockage, c("Total", "Total", "Partial")),
  identical(bundle$barriers$reported_feature_type, c("Waterfall", "Gradient", "Waterfall")),
  all(is.na(bundle$barriers$reported_height_m)),
  bundle$audit$published_downstream_measure_m[1L] < bundle$audit$published_downstream_measure_m[2L],
  bundle$audit$published_downstream_measure_m[2L] < bundle$audit$published_downstream_measure_m[3L],
  abs(bundle$audit$published_distance_between_barriers_m[1L] - 2024.526572) < .001,
  bundle$barriers$snap_distance_m[3L] > 85, !bundle$barriers$snap_accepted[3L],
  !bundle$barriers$explicit_total_passage[3L], !bundle$barriers$french_review_selected[3L])
mask <- bundle$masks
stopifnot(nrow(mask) == 1L, mask$review_number == 581L, mask$review_record_key == "6:52652",
  mask$completeness_method == "full_drainageBasin", mask$full_basin_area_km2 > 9,
  mask$full_basin_area_km2 < 10, mask$indexed_shift_m < 50,
  mask$local_outlet_precision_validated, mask$coverage_complete)
for (field in c("source_split_basin_cache_file", "source_flowtrace_cache_file", "source_flowtrace_confirmation_file", "source_navigation_cache_file")) {
  hash_field <- sub("_cache_file$", "_md5", field)
  if (field == "source_flowtrace_confirmation_file") hash_field <- "source_flowtrace_confirmation_md5"
  stopifnot(identical(unname(tools::md5sum(mask[[field]])), mask[[hash_field]]))
}
gpkg <- "outputs/review_swifd_connections/North_Puget_Sound_Steelhead.gpkg"
edges <- sf::st_read(gpkg, layer = "stream_matching_edges", quiet = TRUE)
selected <- bundle$barriers[bundle$barriers$french_review_selected, ]
base <- readRDS("spatial_data/north_puget_swifd_bridges.rds")$bridges$network
cuts <- cut_reviewed_barriers(base, selected, edges, bundle$anchors)
checked <- validate_nldi_basin_masks(mask, cuts$network, selected)
stopifnot(nrow(checked$masks) == 1L,
  checked$audit$proven_upstream_outside_mask_m < 1e-6,
  checked$audit$max_local_disagreement_distance_m < 37)
applied <- apply_upstream_basin_masks(cuts$network, checked$masks, selected)
stopifnot(applied$summary$source_partition_error_m < 1e-5,
  applied$barrier_audit$remaining_inside_m < 37,
  abs(applied$barrier_audit$remaining_inside_m - applied$barrier_audit$protected_local_downstream_m) < 1e-6)
reject <- function(point) inherits(try(french_creek_recover_mask(point,
  list(downstream_anchors = bundle$anchors)), silent = TRUE), "try-error")
partial <- selected
partial$report_explicit_total <- FALSE
too_far <- selected
too_far$snap_distance_m <- 50.01
stopifnot(reject(partial), reject(too_far))
cat("French review PASS: lower581 Total complete basin; downstream915 Partial and >50m held; no source geometry added\n")
