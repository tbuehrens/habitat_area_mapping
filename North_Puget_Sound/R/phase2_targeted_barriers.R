# Two explicitly requested Phase 2 sites. Existing source classifications and
# review numbers remain visible; these exceptions never change Phase 1 files.
if (!exists("nps_read_cache_json", mode = "function")) source(file.path("R", "project_paths.R"))
phase2_targeted_barrier_inputs <- function(
    baseline_dir = "outputs/review_swifd_connections",
    phase1_dir = "outputs/phase1_total_barriers",
    cache_dir = "spatial_data/phase2") {
  gpkg <- file.path(baseline_dir, "North_Puget_Sound_Steelhead.gpkg")
  points <- sf::st_read(gpkg, layer = "barriers_snapped", quiet = TRUE)
  points <- points[match(c(663L, 344L), points$review_number), ]
  if (anyNA(points$review_number) ||
      !identical(as.character(points$review_record_key), c("6:54918", "6:44226")) ||
      !identical(as.character(points$SiteId), c("890769", "891098")))
    stop("The two targeted sites no longer match their established review identities")
  metadata <- utils::read.csv(file.path(phase1_dir, "phase1_barrier_decisions.csv"),
    stringsAsFactors = FALSE, check.names = FALSE)
  metadata <- metadata[match(points$review_record_key, metadata$review_record_key), ]
  for (field in names(metadata)) points[[field]] <- metadata[[field]]
  points$phase2_original_stream_name <- as.character(points$StreamName)
  points$phase2_original_identity_status <- as.character(points$stream_identity_status)
  points$phase2_original_phase1_status <- as.character(points$phase1_cut_status)
  points$phase2_target <- c("Deer Creek above Higgins Creek", "West Cady Creek above mouth")
  points$phase2_selection_basis <- c(
    "Explicit user full-passage-barrier override; WDFW Partial classification retained",
    "Requested lowest West Cady barrier; newly retrieved WDFW report explicitly Waterfall / Total")
  points$phase2_passability_source <- c("User observation / explicit scenario override", "WDFW FPDSI report page 3")
  points$phase2_stream_alias_review <- c("Existing positively confirmed Deer Creek channel",
    "Exact geographic alias: W Cady Cr = Cady Creek, West = West Cady Creek; same LLID 1213182478994")

  # This is an exact, geographically checked site-specific alias, not a relaxed
  # name-matching rule. The existing local geometry independently supports one
  # physical channel with 100% coverage and <3.30m maximum reference distance.
  west <- 2L
  if (points$phase2_original_stream_name[west] != "W Cady Cr" ||
      points$matched_stream_name[west] != "Cady Creek, West" ||
      as.character(points$matched_llid[west]) != "1213182478994" ||
      points$candidate_count[west] != 1L || points$support_coverage[west] < 1 - 1e-8 ||
      points$max_reference_distance_m[west] > 3.30 || points$median_heading_degrees[west] > 3 ||
      points$snap_distance_m[west] > 50 || isTRUE(points$junction_ambiguous[west]))
    stop("West Cady exact-alias/own-channel geometry checks failed")
  if (points$SiteLongitude[west] < -121.307 || points$SiteLongitude[west] > -121.304 ||
      points$SiteLatitude[west] < 47.896 || points$SiteLatitude[west] > 47.899)
    stop("West Cady mouth target moved outside its reviewed location")
  points$StreamName[west] <- "West Cady Creek"
  points$barrier_stream[west] <- "West Cady Creek"
  for (field in c("stream_identity_confirmed", "snap_accepted", "own_stream_match",
      "stream_identity_valid", "identity_valid_for_cut", "allow_cut", "identity_correction_applied"))
    points[[field]][west] <- TRUE
  points$own_stream_candidate_count[west] <- 1L
  points$matching_channel_count[west] <- 1L
  points$stream_identity_status[west] <- "own_stream_confirmed_exact_geographic_alias"
  points$identity_status[west] <- "own_stream_confirmed_exact_geographic_alias"
  points$identity_review_status[west] <- "phase2_exact_west_cady_alias_and_unique_local_channel"
  points$TributaryToName[west] <- "North Fork Skykomish River"

  for (i in seq_len(nrow(points))) {
    path <- file.path(cache_dir, "reports", paste0(points$SiteId[i], "_Report.text.json"))
    if (!file.exists(path) && i != west) path <- "spatial_data/barrier_reports/890769_Report.text.json"
    if (!file.exists(path)) stop("Targeted report must be fetched first: ", path)
    report <- nps_read_cache_json(path, simplifyVector = FALSE)
    parsed <- parse_natural_barrier_report(unlist(report$pages), as.character(points$SiteId[i]),
      report$report_url, report$report_retrieved_utc, report$report_source_md5)
    if (!isTRUE(parsed$report_identity_verified)) stop("Targeted report identity failed")
    for (field in names(parsed)) {
      if (!field %in% names(points)) points[[field]] <- rep(parsed[[field]][NA_integer_], nrow(points))
      points[[field]][i] <- parsed[[field]][1L]
    }
    points$reported_feature_type[i] <- parsed$report_natural_type
    points$reported_height_m[i] <- parsed$report_waterfall_height_m
    points$report_pdf[i] <- report$report_pdf
    points$report_assessment_pages[i] <- parsed$report_assessment_pages
  }
  if (!identical(as.character(points$report_blockage), c("Partial", "Total")) ||
      points$report_natural_type[west] != "Waterfall")
    stop("Targeted source classifications changed; review before cutting")
  points$phase2_original_explicit_total <- points$explicit_total_passage
  points$public_explicit_total_passage <- points$explicit_total_passage
  points$explicit_total_passage <- c(FALSE, TRUE)
  points$confirmed_total_for_scenario <- c(FALSE, TRUE)
  points$override_future_cut <- c(TRUE, FALSE)
  points$override_reason <- c("User explicitly identified full passage barrier on Deer Creek immediately above Higgins confluence", NA_character_)
  points$total_passage_evidence <- points$phase2_selection_basis
  points$total_evidence_basis <- points$phase2_selection_basis
  points$reported_height_quality <- points$report_waterfall_height_quality
  points$phase2_review_action <- c("Apply user-observed full passage barrier; retain source Partial",
    "Apply newly report-confirmed Total waterfall on exact geographic alias")
  points$eligible_for_future_cut <- TRUE
  points$phase2_user_requested <- TRUE
  points$phase2_selected <- TRUE
  points$phase2_report_source_classification_retained <- TRUE
  points
}

build_phase2_targeted_barriers <- function(cache_dir = "spatial_data/phase2", prepare_only = FALSE) {
  dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)
  points <- phase2_targeted_barrier_inputs(cache_dir = cache_dir)
  gpkg <- "outputs/review_swifd_connections/North_Puget_Sound_Steelhead.gpkg"
  edges <- sf::st_read(gpkg, layer = "stream_matching_edges", quiet = TRUE)
  request_points <- points
  # The generic request preparation reserves override_future_cut for Gorge;
  # the Deer exception still needs its own ordinary complete-basin request.
  request_points$override_future_cut <- FALSE
  batch_dir <- file.path(cache_dir, "targeted_basins")
  prepared <- prepare_phase1_basin_requests(request_points, edges, cache_dir = batch_dir)
  if (isTRUE(prepare_only)) return(invisible(list(barriers = points, requests = prepared)))
  delineations <- load_nldi_delineations(request_points, edges, cache_dir = batch_dir)
  # The vector-projected West Cady point produces an incomplete split. Official
  # elevation flowtrace independently locates the same stream within 50m, and
  # the subsequent split response supplies a genuine full drainageBasin.
  west_row <- which(points$review_number == 344L)
  if (!points$review_record_key[west_row] %in% delineations$candidate_masks$review_record_key) {
    recovered <- phase2_recover_west_cady_mask(points[west_row, ], delineations,
      cache_dir = cache_dir)
    delineations$candidate_masks <- dplyr::bind_rows(delineations$candidate_masks, recovered$mask)
    row <- match(points$review_record_key[west_row], delineations$audit$review_record_key)
    for (field in intersect(names(delineations$audit), names(recovered$mask)))
      delineations$audit[[field]][row] <- recovered$mask[[field]][1L]
    delineations$audit$status[row] <- "full_basin_at_same_channel_official_flowtrace_intersection"
    delineations$audit$completeness_method[row] <- recovered$mask$completeness_method
    delineations$audit$full_basin_area_km2[row] <- recovered$mask$full_basin_area_km2
    delineations$audit$published_area_check[row] <- recovered$mask$published_area_check
  }
  if (nrow(delineations$candidate_masks) != 2L)
    stop("Both targeted sites require complete validated upstream basins; inspect targeted_basins/manifest.json")
  named <- load_stream_identity_reference()
  direction <- derive_downstream_anchors(points, edges, named,
    marine_geometry = readRDS("spatial_data/marine_north.rds"))
  anchors <- dplyr::bind_rows(direction$anchors,
    delineations$downstream_anchors[!delineations$downstream_anchors$review_record_key %in%
      direction$anchors$review_record_key, ])
  bridges <- readRDS("spatial_data/north_puget_swifd_bridges.rds")$bridges
  cuts <- cut_reviewed_barriers(bridges$network, points, edges, anchors)
  validation_points <- points
  # The shared validator's current-phase gate is named phase1_selected; set it
  # only on this local validation copy, preserving original Phase 1 membership.
  validation_points$phase1_selected <- validation_points$phase2_selected
  checked <- validate_nldi_basin_masks(delineations$candidate_masks, cuts$network, validation_points)
  if (nrow(checked$masks) != 2L) stop("A targeted basin failed exact graph/outlet validation")
  checked$masks$phase2_selection_basis <- points$phase2_selection_basis[
    match(checked$masks$review_record_key, points$review_record_key)]
  checked$masks$source_report_blockage <- points$report_blockage[
    match(checked$masks$review_record_key, points$review_record_key)]
  audit <- dplyr::left_join(delineations$audit,
    sf::st_drop_geometry(points)[, c("review_record_key", "SiteId", "StreamName",
      "phase2_original_stream_name", "phase2_selection_basis", "report_blockage",
      "report_natural_type", "reported_height_m", "phase2_original_phase1_status")],
    by = "review_record_key")
  result <- list(barriers = points, masks = checked$masks, anchors = anchors, audit = audit,
    basin_validation = checked$audit, graph_audit = cuts$barrier_audit,
    notes = c("Targets preserve existing review numbers 663 and 344 and source record keys.",
      "Deer 663 is an explicit user override of WDFW Partial; source was not relabeled Total.",
      "West Cady 344 is source-reported Waterfall/Total; height is unknown (negative source sentinel).",
      "Both are unfield-verified inventory sites. Neither report independently verifies steelhead-run-specific blockage.",
      "Only the two requested rows are changed. Phase 1 and the original SWIFD review remain read-only."))
  saveRDS(result, file.path(cache_dir, "targeted_barriers.rds"))
  utils::write.csv(audit, file.path(cache_dir, "targeted_barrier_audit.csv"), row.names = FALSE)
  invisible(result)
}

phase2_recover_west_cady_mask <- function(point, delineations, cache_dir = "spatial_data/phase2") {
  if (nrow(point) != 1L || point$review_record_key != "6:44226" || point$review_number != 344L)
    stop("This recovery is restricted to the individually reviewed West Cady mouth barrier")
  records <- nps_read_cache_json(file.path(cache_dir, "targeted_basins", "manifest.json"), simplifyVector = FALSE)
  record <- Filter(function(x) x$review_record_key == "6:44226", records)[[1L]]
  if (!isTRUE(record$approved) || record$comid != "23963587" ||
      record$reference_stream_name != "West Cady Creek" || record$route_support_coverage < 1 - 1e-8 ||
      record$indexed_shift_m > 50) stop("West Cady indexed channel is unconfirmed")
  crs <- sf::st_crs(point)
  recovery <- file.path(cache_dir, "west_cady_recovery")
  basin_file <- file.path(recovery, "flowtraced_split_basin.json")
  flowtrace_file <- file.path(recovery, "physical_flowtrace.json")
  confirmation_file <- file.path(recovery, "indexed_flowtrace.json")
  nav_file <- file.path(recovery, "upstream_flowlines.json")
  if (!all(file.exists(c(basin_file, flowtrace_file, confirmation_file, nav_file))))
    stop("Fetch the official West Cady flowtrace and complete split basin first")
  navigation <- sf::st_transform(sf::st_read(nav_file, quiet = TRUE), crs)
  route_json <- nps_read_cache_json(file.path(record$cache_folder, "named_flowline.json"), simplifyVector = FALSE)
  properties <- Filter(function(x) as.character(x$properties$comid) == "23963587", route_json$features)[[1L]]$properties
  trace_properties <- function(path) {
    raw <- nps_read_cache_json(path, simplifyVector = FALSE)
    features <- Filter(function(x) identical(x$id, "nhdFlowline"), raw$features)
    if (length(features) != 1L) stop("Official flowtrace lacks unique named flowline")
    features[[1L]]$properties
  }
  trace <- trace_properties(flowtrace_file)
  confirmation <- trace_properties(confirmation_file)
  if (!"nhdplus_comid" %in% names(navigation) ||
      !"23963587" %in% as.character(navigation$nhdplus_comid) || nrow(navigation) != 15L ||
      isTRUE(properties$startflag == 1) || properties$totdasqkm <= properties$areasqkm ||
      as.character(trace$comid) != "23963587" || trace$gnis_name != "West Cady Creek" ||
      as.character(confirmation$comid) != "23963587" ||
      !isTRUE(all.equal(unlist(trace$intersection_point), unlist(confirmation$intersection_point))))
    stop("West Cady positive published upstream-network/catchment identity checks failed")
  intersection <- sf::st_transform(sf::st_sfc(sf::st_point(as.numeric(unlist(trace$intersection_point))), crs = 4326), crs)
  shift <- as.numeric(sf::st_distance(intersection, point))
  if (!is.finite(shift) || shift > 50 || trace$raindrop_pathDist > 50)
    stop("West Cady official flowtrace intersection exceeds the approved 50m barrier tolerance")
  raw <- nps_read_cache_json(basin_file, simplifyVector = FALSE)
  complete <- nldi_validate_basin_completeness(raw, properties, record, crs)
  if (!isTRUE(complete$valid) || complete$diagnostics$completeness_method != "full_drainageBasin")
    stop("Official flowtraced West Cady response lacks a complete upstream basin")
  basin <- sf::st_read(jsonlite::toJSON(list(type = "FeatureCollection", features = complete$feature),
    auto_unbox = TRUE, digits = NA), quiet = TRUE)
  geometry <- sf::st_union(sf::st_geometry(sf::st_transform(sf::st_make_valid(basin), crs)))
  # Both original indexed anchors survive even when the loader correctly rejects
  # split-only basin completeness. They were independently oriented by published
  # FlowDir, seven monotonic projections and the same physical channel.
  if (!point$review_record_key %in% delineations$downstream_anchors$review_record_key)
    stop("West Cady exact physical-channel downstream anchor is unavailable")
  mask <- sf::st_sf(review_record_key = as.character(point$review_record_key),
    review_number = point$review_number, graph_edge_id = point$graph_edge_id,
    watershed_validated = TRUE, stream_identity_validated = TRUE,
    total_barrier_approved = TRUE, coverage_complete = TRUE,
    local_outlet_precision_validated = FALSE, protect_local_graph_downstream = TRUE,
    barrier_x = sf::st_coordinates(point)[1L, 1L], barrier_y = sf::st_coordinates(point)[1L, 2L],
    local_correction_limit_m = 100, nldi_comid = "23963587", indexed_shift_m = shift,
    indexing_method = "official_elevation_flowtrace_same_COMID_intersection_confirmed_from_two_starting_points",
    source_url = "https://api.water.usgs.gov/nldi/pygeoapi/processes/nldi-splitcatchment/execution?f=json",
    source_md5 = unname(tools::md5sum(basin_file)), source_retrieved_utc =
      format(file.info(basin_file)$mtime, tz = "UTC", format = "%Y-%m-%dT%H:%M:%SZ"),
    heading_halfspan_m = nldi_course_controls()$heading_halfspan_m,
    source_split_basin_cache_file = basin_file, source_split_basin_md5 = unname(tools::md5sum(basin_file)),
    source_flowtrace_cache_file = flowtrace_file, source_flowtrace_md5 = unname(tools::md5sum(flowtrace_file)),
    source_flowtrace_confirmation_file = confirmation_file,
    source_flowtrace_confirmation_md5 = unname(tools::md5sum(confirmation_file)),
    source_flowtrace_url = "https://api.water.usgs.gov/nldi/pygeoapi/processes/nldi-flowtrace/execution?f=json",
    flowtrace_longitude = as.numeric(trace$intersection_point[[1L]]),
    flowtrace_latitude = as.numeric(trace$intersection_point[[2L]]),
    flowtrace_path_distance_m = trace$raindrop_pathDist,
    source_navigation_cache_file = nav_file, source_navigation_md5 = unname(tools::md5sum(nav_file)),
    source_navigation_url = "https://api.water.usgs.gov/nldi/linked-data/comid/23963587/navigation/UT/flowlines?f=json&distance=1000",
    source_upstream_reach_count = nrow(navigation),
    geometry = geometry)
  for (field in names(complete$diagnostics)) mask[[field]] <- complete$diagnostics[[field]]
  provenance <- sf::st_drop_geometry(mask)
  jsonlite::write_json(provenance, file.path(recovery, "recovery_provenance.json"), pretty = TRUE,
    auto_unbox = TRUE, na = "null", digits = NA)
  list(mask = mask)
}

load_phase2_targeted_barriers <- function(cache_dir = "spatial_data/phase2") {
  if (!requireNamespace("sf", quietly = TRUE)) stop("Install the project sf dependency")
  path <- file.path(cache_dir, "targeted_barriers.rds")
  if (!file.exists(path)) stop("Build the reviewed two-site Phase 2 bundle first: ", path)
  nps_read_cache_rds(path)
}
