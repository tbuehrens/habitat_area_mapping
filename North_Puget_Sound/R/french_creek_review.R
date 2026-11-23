# Isolated, source-confirmed lower French Creek Total barrier review. Source
# lines and earlier scenarios remain unchanged; the saved full basin also cuts
# disconnected upstream model pieces. No geometry is inferred from stream names.
if (!exists("nps_read_cache_json", mode = "function")) source(file.path("R", "project_paths.R"))

french_parse_report <- function(report) {
  # PDF readers differ in whether a label and its value occupy separate lines.
  # Join only nonempty label/value pairs; leave blank fields and raw cached
  # text/PDF unchanged. Classification still requires the exact printed value.
  normalized <- vapply(unlist(report$pages), function(page) {
    lines <- strsplit(page, "\n", fixed = TRUE)[[1L]]
    for (i in seq_len(max(0L, length(lines) - 1L))) {
      if (grepl(":\\s*$", lines[i], perl = TRUE) && nzchar(trimws(lines[i + 1L])) &&
          !grepl(":", lines[i + 1L], fixed = TRUE)) {
        lines[i] <- paste(lines[i], trimws(lines[i + 1L]))
        lines[i + 1L] <- ""
      }
    }
    paste(lines, collapse = "\n")
  }, character(1))
  parse_natural_barrier_report(normalized, as.character(report$SiteId), report$report_url,
    report$report_retrieved_utc, report$report_source_md5)
}

french_creek_source_inventory <- function(cache_dir = "spatial_data/stillaguamish_review") {
  records <- nps_read_cache_json(file.path(cache_dir, "french", "source_queries", "query_manifest.json"),
    simplifyVector = FALSE)
  if (!setequal(vapply(records, function(x) as.integer(x$layer), integer(1)), 0:8))
    stop("French review requires all nine public fish-passage layers")
  inventory <- list()
  for (record in records) {
    if (!identical(unname(tools::md5sum(record$cache_file)), record$md5)) stop("French source-query snapshot hash changed")
    raw <- nps_read_cache_json(record$cache_file, simplifyVector = FALSE)
    if (!identical(raw$type, "FeatureCollection") || isTRUE(raw$exceededTransferLimit) ||
        length(raw$features) != record$feature_count) stop("French source inventory is incomplete")
    for (feature in raw$features) {
      value <- feature$properties
      direct <- identical(value$StreamName, "French Cr")
      disposition <- if (!direct) "Different named tributary; no French mainstem cut" else
        if (value$SiteId == "890651") "581: lower Total; full basin approved" else
        if (value$SiteId == "890795") "563: upstream Total; retain source history" else
        if (value$SiteId == "890155") "915: Partial and farther than50m from physical model; held" else
        if (value$SiteId == "XID4015" && value$FishPassageBarrierStatusCode == 20 && value$PercentFishPassableCode == 40)
          "Current source explicitly not a barrier / fully passable" else stop("New or changed French mainstem source needs individual review")
      inventory[[length(inventory) + 1L]] <- data.frame(source_layer = record$layer,
        SiteId = as.character(value$SiteId), StreamName = value$StreamName,
        FeatureType = value$FeatureType, directly_named_French = direct,
        disposition = disposition, source_url = record$source_url, source_md5 = record$md5,
        stringsAsFactors = FALSE)
    }
  }
  result <- do.call(rbind, inventory)
  if (!setequal(result$SiteId[result$directly_named_French], c("890651", "890795", "890155", "XID4015")))
    stop("The complete French mainstem source inventory changed")
  result
}

french_creek_review_inputs <- function(cache_dir = "spatial_data/stillaguamish_review") {
  gpkg <- "outputs/review_swifd_connections/North_Puget_Sound_Steelhead.gpkg"
  original <- sf::st_read(gpkg, layer = "barriers_snapped", quiet = TRUE)
  points <- original[match(c(563L, 581L), original$review_number), ]
  if (nrow(points) != 2L || anyNA(points$review_number) ||
      !identical(as.character(points$SiteId), c("890795", "890651")) ||
      !identical(as.character(points$review_record_key), c("6:52351", "6:52652")))
    stop("The two existing French Creek source identities changed")
  metadata <- nps_read_cache_csv("outputs/phase3_tidal_barriers/phase3_barrier_decisions.csv",
    stringsAsFactors = FALSE, check.names = FALSE)
  for (field in names(metadata)) points[[field]] <- metadata[[field]][match(points$review_number, metadata$review_number)]
  fresh <- sf::st_read(file.path(cache_dir, "french", "source_queries", "layer_6.geojson"), quiet = TRUE)
  fresh <- sf::st_transform(fresh, sf::st_crs(points))
  held_source <- fresh[as.character(fresh$SiteId) == "890155", ]
  if (nrow(held_source) != 1L) stop("The additional downstream French source must remain explicitly inventoried")
  held <- points[NA_integer_, ]
  sf::st_geometry(held) <- sf::st_geometry(held_source)
  for (field in intersect(names(held_source), names(held))) {
    if (field != attr(held, "sf_column")) held[[field]] <- held_source[[field]]
  }
  held$review_number <- 915L
  held$review_record_key <- held$barrier_key <- "6:52710"
  held$source_layer <- "6"
  held$verification <- "Not field verified"
  held$source_url <- "https://geodataservices.wdfw.wa.gov/arcgis/rest/services/ApplicationServices/FP_Sites/MapServer/6/query"
  source_xy <- sf::st_coordinates(held_source)[1L, 1:2]
  geographic <- sf::st_coordinates(sf::st_transform(held_source, 4326))[1L, 1:2]
  held$original_source_x <- held$original_x <- source_xy[1L]
  held$original_source_y <- held$original_y <- source_xy[2L]
  held$original_source_crs <- "EPSG:26910"
  held$original_longitude <- geographic[1L]
  held$original_latitude <- geographic[2L]
  held$original_geometry_wkt <- sf::st_as_text(sf::st_geometry(held_source))
  edges <- sf::st_read(gpkg, layer = "stream_matching_edges", quiet = TRUE)
  distances <- as.numeric(sf::st_distance(held_source, edges))
  nearest <- which.min(distances)
  held$snap_distance_m <- held$initial_snap_distance_m <- distances[nearest]
  held$nearest_physical_line <- held$graph_edge_id <- edges$graph_edge_id[nearest]
  held$channel_chain_id <- edges$channel_chain_id[nearest]
  held$candidate_count <- sum(distances <= 50)
  held$own_stream_candidate_count <- held$matching_channel_count <- 0L
  held$stream_identity_status <- held$identity_status <- "outside_50m_physical_channel_tolerance"
  held$identity_review_status <- "held_original_source_point_85m_from_model_no_snap"
  held$display_location_available <- TRUE
  held$snapped_x <- held$snapped_y <- NA_real_
  held$population_ids <- "296"
  held$population_names <- "Stillaguamish River (winter)"
  for (field in intersect(names(points), names(held))) {
    if (is.character(points[[field]]) || is.character(held[[field]])) {
      points[[field]] <- as.character(points[[field]])
      held[[field]] <- as.character(held[[field]])
    }
  }
  points <- dplyr::bind_rows(points, held)
  for (i in seq_len(nrow(points))) {
    source <- fresh[as.character(fresh$SiteId) == as.character(points$SiteId[i]), ]
    original_source <- sf::st_sfc(sf::st_point(c(points$original_source_x[i], points$original_source_y[i])),
      crs = sf::st_crs(points))
    if (nrow(source) != 1L || source$StreamName != "French Cr" ||
        source$FeatureType != "Natural Barrier" ||
        as.numeric(sf::st_distance(source, original_source)) > 1)
      stop("A current French source identity, type, name or location changed; review before reuse")
    report <- nps_read_cache_json(file.path(cache_dir, "french", "reports",
      paste0(points$SiteId[i], "_Report.text.json")), simplifyVector = FALSE)
    parsed <- french_parse_report(report)
    if (!isTRUE(parsed$report_identity_verified) ||
        (points$review_number[i] != 915L && !isTRUE(parsed$report_explicit_total)) ||
        parsed$report_stream_name != "French Cr")
      stop("French source reports must identify their sites and explicitTotal for selected sources")
    for (field in names(parsed)) {
      if (!field %in% names(points)) points[[field]] <- rep(parsed[[field]][NA_integer_], nrow(points))
      points[[field]][i] <- parsed[[field]][1L]
    }
    points$reported_feature_type[i] <- parsed$report_natural_type
    points$reported_height_m[i] <- parsed$report_waterfall_height_m
    points$reported_height_quality[i] <- parsed$report_waterfall_height_quality
    points$report_pdf[i] <- report$report_pdf
  }
  active <- points$review_number != 915L
  if (!identical(points$reported_feature_type[active], c("Waterfall", "Gradient")) ||
      any(points$snap_distance_m[active] > 50) || any(points$graph_edge_id[active] != 4017L) ||
      any(points$matching_channel_count[active] != 1L) || any(points$candidate_count[active] != 1L) ||
      any(!points$snap_accepted[active]) || any(!points$stream_identity_confirmed[active]) || any(!points$identity_valid_for_cut[active]) ||
      points$snap_distance_m[!active] <= 50)
    stop("French cut requires unchanged unique own-channel identity within50m")
  if (points$report_blockage[!active] != "Partial") stop("The held downstream French source classification changed")
  points$explicit_total_passage <- points$confirmed_total_for_scenario <- points$report_explicit_total
  points$phase1_selected <- active
  points$french_review_selected <- c(FALSE, TRUE, FALSE)
  points$french_review_action <- c("Retain upstream Total source record; lower581 governs accessible French Creek extent",
    "Apply lower source-reported unverifiedTotalGradient581 using independently recovered complete upstream basin",
    "Hold farther-downstream source915: reportPartial and original point exceeds50m physical-channel tolerance; no snap or cut")
  for (field in c("snap_accepted", "stream_identity_confirmed", "stream_identity_valid", "identity_valid_for_cut",
      "allow_cut", "own_stream_match", "phase1_applied", "phase2_selected", "phase2_applied", "phase3_selected",
      "phase3_applied", "applied", "truncation_applied", "override_future_cut")) points[[field]][!active] <- FALSE
  rownames(points) <- NULL
  points
}

french_creek_recover_mask <- function(point, delineations,
    cache_dir = "spatial_data/stillaguamish_review") {
  if (nrow(point) != 1L || point$review_number != 581L || point$review_record_key != "6:52652" ||
      point$SiteId != "890651" || !isTRUE(point$report_explicit_total) || point$snap_distance_m > 50)
    stop("The recovery is restricted to the reviewed lower French Total barrier581")
  records <- nps_read_cache_json("spatial_data/upstream_basins/batch/manifest.json", simplifyVector = FALSE)
  record <- Filter(function(x) x$review_number == 581L, records)[[1L]]
  if (!isTRUE(record$approved) || record$comid != "24274901" ||
      record$reference_stream_name != "French Creek" || record$route_support_coverage < 1 - 1e-8 ||
      record$indexed_shift_m > 50)
    stop("The existing independently matched French reference course changed")
  recovery <- file.path(cache_dir, "french", "recovery")
  basin_file <- file.path(recovery, "flowtraced_split_basin.json")
  trace_file <- file.path(recovery, "physical_flowtrace.json")
  confirm_file <- file.path(recovery, "indexed_flowtrace.json")
  nav_file <- file.path(recovery, "upstream_flowlines.json")
  properties <- function(file) {
    raw <- nps_read_cache_json(file, simplifyVector = FALSE)
    matches <- Filter(function(f) identical(f$id, "nhdFlowline"), raw$features)
    if (length(matches) != 1L) stop("Official French flowtrace lacks a unique flowline")
    matches[[1L]]$properties
  }
  trace <- properties(trace_file)
  confirm <- properties(confirm_file)
  if (as.character(trace$comid) != "24274901" || as.character(confirm$comid) != "24274901" ||
      trace$gnis_name != "French Creek" || confirm$gnis_name != "French Creek" ||
      !isTRUE(all.equal(unlist(trace$intersection_point), unlist(confirm$intersection_point))) ||
      trace$raindrop_pathDist > 50 || confirm$raindrop_pathDist > 50)
    stop("French flowtrace must identify the exact same named channel from both original starting points within50m")
  crs <- sf::st_crs(point)
  intersection <- sf::st_transform(sf::st_sfc(sf::st_point(as.numeric(unlist(trace$intersection_point))), crs = 4326), crs)
  shift <- as.numeric(sf::st_distance(intersection, point))
  if (!is.finite(shift) || shift > 50) stop("Recovered French outlet exceeds the unchanged50m indexing tolerance")
  route_file <- file.path(record$cache_folder, "named_flowline.json")
  route_json <- nps_read_cache_json(route_file, simplifyVector = FALSE)
  routes <- Filter(function(f) as.character(f$properties$comid) == "24274901", route_json$features)
  if (length(routes) != 1L || routes[[1L]]$properties$gnis_name != "French Creek" ||
      tolower(routes[[1L]]$properties$flowdir) != "with digitized")
    stop("Published French course and flow direction must remain independently confirmed")
  navigation <- nps_read_cache_json(nav_file, simplifyVector = FALSE)
  ids <- vapply(navigation$features, function(f) as.character(f$properties$nhdplus_comid), character(1))
  published <- routes[[1L]]$properties
  if (!identical(unique(ids), "24274901") || published$startflag != 1 || published$uphydroseq != 0 ||
      published$areasqkm != published$totdasqkm)
    stop("Published headwater attributes and exhaustive1000km upstream navigation must agree")
  complete <- nldi_validate_basin_completeness(nps_read_cache_json(basin_file, simplifyVector = FALSE),
    published, record, crs)
  if (!isTRUE(complete$valid) || complete$diagnostics$completeness_method != "full_drainageBasin")
    stop("Recovered French response must provide a unique complete drainageBasin")
  if (!point$review_record_key %in% delineations$downstream_anchors$review_record_key)
    stop("The independently oriented exact physical-channel French downstream anchor is missing")
  raw <- sf::st_read(jsonlite::toJSON(list(type = "FeatureCollection", features = complete$feature),
    auto_unbox = TRUE, digits = NA), quiet = TRUE)
  mask <- sf::st_sf(review_record_key = point$review_record_key, review_number = point$review_number,
    graph_edge_id = point$graph_edge_id, watershed_validated = TRUE, stream_identity_validated = TRUE,
    total_barrier_approved = TRUE, coverage_complete = TRUE, local_outlet_precision_validated = FALSE,
    protect_local_graph_downstream = TRUE, local_correction_limit_m = 100,
    barrier_x = sf::st_coordinates(point)[1L, 1L], barrier_y = sf::st_coordinates(point)[1L, 2L],
    nldi_comid = "24274901", indexed_shift_m = shift,
    indexing_method = "official_elevation_flowtrace_same_COMID_intersection_confirmed_from_two_starting_points",
    source_url = "https://api.water.usgs.gov/nldi/pygeoapi/processes/nldi-splitcatchment/execution?f=json",
    source_md5 = unname(tools::md5sum(basin_file)),
    source_retrieved_utc = format(file.info(basin_file)$mtime, tz = "UTC", format = "%Y-%m-%dT%H:%M:%SZ"),
    heading_halfspan_m = nldi_course_controls()$heading_halfspan_m,
    source_split_basin_cache_file = basin_file, source_split_basin_md5 = unname(tools::md5sum(basin_file)),
    source_flowtrace_cache_file = trace_file, source_flowtrace_md5 = unname(tools::md5sum(trace_file)),
    source_flowtrace_confirmation_file = confirm_file, source_flowtrace_confirmation_md5 = unname(tools::md5sum(confirm_file)),
    source_flowtrace_url = "https://api.water.usgs.gov/nldi/pygeoapi/processes/nldi-flowtrace/execution?f=json",
    flowtrace_longitude = trace$intersection_point[[1L]], flowtrace_latitude = trace$intersection_point[[2L]],
    flowtrace_path_distance_m = trace$raindrop_pathDist,
    source_navigation_cache_file = nav_file, source_navigation_md5 = unname(tools::md5sum(nav_file)),
    source_navigation_url = "https://api.water.usgs.gov/nldi/linked-data/comid/24274901/navigation/UT/flowlines?f=json&distance=1000",
    source_upstream_reach_count = length(unique(ids)),
    geometry = sf::st_union(sf::st_geometry(sf::st_make_valid(sf::st_transform(raw, crs)))))
  for (field in names(complete$diagnostics)) mask[[field]] <- complete$diagnostics[[field]]
  mask
}

build_french_creek_review <- function(cache_dir = "spatial_data/stillaguamish_review") {
  inventory <- french_creek_source_inventory(cache_dir)
  points <- french_creek_review_inputs(cache_dir)
  gpkg <- "outputs/review_swifd_connections/North_Puget_Sound_Steelhead.gpkg"
  edges <- sf::st_read(gpkg, layer = "stream_matching_edges", quiet = TRUE)
  delineations <- load_nldi_delineations(points[points$review_number != 915L, ], edges)
  if (any(delineations$audit$route_support_coverage != 1) ||
      any(delineations$audit$nldi_comid != "24274901"))
    stop("Both French sites need independent same-channel course support")
  records <- nps_read_cache_json("spatial_data/upstream_basins/batch/manifest.json", simplifyVector = FALSE)
  record <- Filter(function(x) x$review_number == 581L, records)[[1L]]
  route <- suppressWarnings(sf::st_cast(sf::st_transform(sf::st_read(file.path(record$cache_folder,
    "named_flowline.json"), quiet = TRUE), sf::st_crs(points)), "LINESTRING"))
  if (nrow(route) != 1L) stop("French ordering requires the single published reference reach")
  xy <- unclass(sf::st_geometry(route)[[1L]])[, 1:2, drop = FALSE]
  measure <- vapply(seq_len(nrow(points)), function(i)
    stream_identity_project(xy, sf::st_coordinates(points[i, ])[1L, 1:2])$measure, numeric(1))
  if (measure[2L] <= measure[1L] + 1000 || measure[3L] <= measure[2L])
    stop("Published downstream course no longer proves563,581,915 ordering")
  selected <- points[points$french_review_selected, ]
  mask <- french_creek_recover_mask(selected, delineations, cache_dir)
  anchors <- delineations$downstream_anchors[delineations$downstream_anchors$review_record_key %in% selected$review_record_key, ]
  base <- readRDS("spatial_data/north_puget_swifd_bridges.rds")$bridges$network
  cuts <- cut_reviewed_barriers(base, selected, edges, anchors)
  checked <- validate_nldi_basin_masks(mask, cuts$network, selected)
  if (nrow(checked$masks) != 1L) {
    print(checked$audit[, c("review_number", "approval_reason")])
    stop("French recovered full basin failed independent graph/outlet validation")
  }
  applied <- apply_upstream_basin_masks(cuts$network, checked$masks)
  key <- selected$review_record_key
  inside <- upstream_line_parts(suppressWarnings(sf::st_intersection(applied$network,
    sf::st_union(checked$masks))))
  remaining <- inside[inside$measure_habitat & !inside$removed, ]
  protection <- upstream_local_geometry_check(remaining, selected, 100)
  if (!protection$valid) stop("French complete mask left unexplained remote upstream habitat")
  audit <- sf::st_drop_geometry(points)[, c("review_number", "review_record_key", "SiteId", "StreamName",
    "verification", "report_blockage", "reported_feature_type", "reported_height_m", "reported_height_quality",
    "snap_distance_m", "graph_edge_id", "french_review_selected", "french_review_action", "report_url", "report_source_md5")]
  audit$published_downstream_measure_m <- measure
  audit$published_lower_site <- c(FALSE, FALSE, TRUE)
  audit$lowest_approvable_within_50m <- c(FALSE, TRUE, FALSE)
  audit$published_distance_between_barriers_m <- measure[2L] - measure[1L]
  audit$source_classification_preserved <- TRUE
  audit$complete_upstream_mask_approved <- audit$review_record_key %in% checked$masks$review_record_key
  audit$full_basin_area_km2 <- c(NA_real_, checked$masks$full_basin_area_km2, NA_real_)
  audit$remaining_inside_complete_mask_m <- c(NA_real_, protection$length_m, NA_real_)
  audit$max_protected_downstream_distance_m <- c(NA_real_, protection$max_distance_m, NA_real_)
  bundle <- list(barriers = points, masks = checked$masks, anchors = anchors, audit = audit,
    all_reviewed_barriers = points, basin_audit = delineations$audit, basin_validation = checked$audit,
    graph_audit = cuts$barrier_audit, application_audit = applied$barrier_audit,
    application_summary = applied$summary,
    source_inventory = inventory,
    notes = c("Existing581/site890651/6:52652 is the lower FrenchCreek Total barrier, about2.025km downstream of563 along published USGS FlowDir=WithDigitized.",
      "581 is an unverified source-reported Total Gradient barrier. Height,gradient,assessment date and field crew are unavailable; printed run labels are not independent run-specific proof.",
      "Strict existing unique same-channel edge4017 match retained: source-to-model snap10.5868m, primaryWDFW coverage100%, independentUSGS coverage100%.",
      "Original581 request returned a0.00449km2 split sliver and was correctly held by graph-agreement checks. OfficialDEMflowtrace from model and indexed points identifies the sameFrenchCOMID and identical pourpoint within50m; recovered source has a unique full drainageBasin.",
      "The complete watershed removes disconnected upstream model/SWIFD habitat too; only graph-proven local downstream overlap within100m may survive at the outlet.",
      "New915/site890155 is fartherdownstream in publishedrecords but source-reportedPartial and85m from the nearest physicalmodel channel; held at its originalsourcecoordinate,never snapped orcut.581 is the lowest publishedTotal found and meets the unchanged50m rule.",
      "Earlier source geometry, barrier numbers and scenario outputs are unchanged.563 remains in source audit;581 sets the lower effective French cut."))
  dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)
  saveRDS(bundle, file.path(cache_dir, "french_creek.rds"))
  utils::write.csv(audit, file.path(cache_dir, "french_creek_audit.csv"), row.names = FALSE)
  utils::write.csv(inventory, file.path(cache_dir, "french_source_inventory.csv"), row.names = FALSE)
  invisible(bundle)
}

load_french_creek_review <- function(cache_dir = "spatial_data/stillaguamish_review") {
  file <- file.path(cache_dir, "french_creek.rds")
  if (!file.exists(file)) stop("Prepare French sources with python scripts/prepare_french_creek_review.py")
  bundle <- nps_read_cache_rds(file)
  selected <- bundle$barriers[bundle$barriers$french_review_selected, ]
  held <- bundle$barriers[bundle$barriers$review_number == 915L, ]
  if (!identical(bundle$barriers$review_number, c(563L, 581L, 915L)) || nrow(selected) != 1L || selected$review_number != 581L ||
      selected$report_blockage != "Total" || selected$snap_distance_m > 50 ||
      nrow(held) != 1L || held$snap_distance_m <= 50 || held$snap_accepted || held$french_review_selected ||
      nrow(bundle$masks) != 1L || !isTRUE(bundle$masks$coverage_complete) ||
      bundle$masks$completeness_method != "full_drainageBasin")
    stop("French cached source identity and complete-basin safeguards failed")
  bundle
}
