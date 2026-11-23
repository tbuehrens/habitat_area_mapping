# Woods Creek's two source-reported Total natural barriers. Only an independently
# confirmed physical stream assignment within50m can produce a Phase3 cut.
if (!exists("nps_read_cache_json", mode = "function")) source(file.path("R", "project_paths.R"))

phase3_woods_inputs <- function(cache_dir = "spatial_data/phase3") {
  woods <- file.path(cache_dir, "woods")
  gpkg <- "outputs/review_swifd_connections/North_Puget_Sound_Steelhead.gpkg"
  original <- sf::st_read(gpkg, layer = "barriers_snapped", quiet = TRUE)
  existing <- original[original$review_number == 10L, ]
  stopifnot(nrow(existing) == 1L, existing$SiteId == "891970", existing$review_record_key == "5:44379")
  metadata <- nps_read_cache_csv("outputs/phase2_reviewed_barriers/phase2_barrier_decisions.csv",
    stringsAsFactors = FALSE, check.names = FALSE)
  metadata <- metadata[metadata$review_number == 10L, ]
  for (field in names(metadata)) existing[[field]] <- metadata[[field]]
  source <- sf::st_read(file.path(woods, "source_queries", "layer_6.geojson"), quiet = TRUE)
  source <- source[as.character(source$SiteId) == "891169", ]
  if (nrow(source) != 1L) stop("The reviewed West Fork Woods natural source is missing or duplicated")
  source <- sf::st_transform(source, sf::st_crs(existing))
  new <- existing[NA_integer_, ]
  sf::st_geometry(new) <- sf::st_geometry(source)
  for (field in intersect(names(source), names(new))) {
    if (field != attr(new, "sf_column")) new[[field]] <- source[[field]]
  }
  new$review_number <- 914L
  new$review_record_key <- new$barrier_key <- paste0("6:", source$OBJECTID)
  new$verification <- "Not field verified"
  new$source_layer <- "6"
  new$source_url <- paste0("https://geodataservices.wdfw.wa.gov/arcgis/rest/services/",
    "ApplicationServices/FP_Sites/MapServer/6/query")
  original_xy <- sf::st_coordinates(source)[1L, 1:2]
  geographic <- sf::st_coordinates(sf::st_transform(source, 4326))[1L, 1:2]
  new$original_source_x <- new$original_x <- original_xy[1L]
  new$original_source_y <- new$original_y <- original_xy[2L]
  new$original_source_crs <- "EPSG:26910"
  new$original_longitude <- geographic[1L]
  new$original_latitude <- geographic[2L]
  new$original_geometry_wkt <- sf::st_as_text(sf::st_geometry(source))
  edges <- sf::st_read(gpkg, layer = "stream_matching_edges", quiet = TRUE)
  distances <- as.numeric(sf::st_distance(source, edges))
  nearest <- which.min(distances)
  new$initial_snap_distance_m <- new$snap_distance_m <- distances[nearest]
  new$nearest_physical_line <- new$graph_edge_id <- edges$graph_edge_id[nearest]
  new$channel_chain_id <- edges$channel_chain_id[nearest]
  new$candidate_count <- sum(distances <= 50)
  new$own_stream_candidate_count <- new$matching_channel_count <- 0L
  new$display_location_available <- TRUE
  new$stream_identity_status <- new$identity_status <- "outside_50m_physical_channel_tolerance"
  new$identity_review_status <- "held_source_point_394m_from_model_no_snap"
  new$snapped_x <- new$snapped_y <- NA_real_
  new$population_ids <- "290"
  new$population_names <- "Snohomish/Skykomish Rivers (winter)"
  for (field in intersect(names(existing), names(new))) {
    if (is.character(existing[[field]]) || is.character(new[[field]])) {
      existing[[field]] <- as.character(existing[[field]])
      new[[field]] <- as.character(new[[field]])
    }
  }
  points <- dplyr::bind_rows(existing, new)
  for (field in c("phase1_selected", "phase1_applied", "phase2_selected", "phase2_applied",
      "override_future_cut", "applied", "truncation_applied", "snap_accepted", "stream_identity_confirmed",
      "stream_identity_valid", "identity_valid_for_cut", "allow_cut", "own_stream_match",
      "passability_evidence_conflict", "total_evidence_conflict", "report_blockage_conflict",
      "junction_ambiguous", "identity_conflict_flag")) points[[field]] <- FALSE
  points$phase3_original_identity_status <- c(existing$stream_identity_status, "new_unmatched_source_record")
  points$phase3_original_wdfw_support_coverage <- c(existing$support_coverage, NA_real_)
  points$phase3_original_wdfw_max_reference_distance_m <- c(existing$max_reference_distance_m, NA_real_)
  points$phase3_selected <- FALSE
  points$phase3_review_action <- c("Pending independent USGS same-stream identity and complete upstream basin",
    "Hold reportedTotal: source point farther than50m from any model/SWIFD physical stream")
  for (i in seq_len(nrow(points))) {
    file <- file.path(woods, "reports", paste0(points$SiteId[i], "_Report.text.json"))
    report <- nps_read_cache_json(file, simplifyVector = FALSE)
    parsed <- parse_natural_barrier_report(unlist(report$pages), as.character(points$SiteId[i]),
      report$report_url, report$report_retrieved_utc, report$report_source_md5)
    if (!isTRUE(parsed$report_identity_verified) || !isTRUE(parsed$report_explicit_total))
      stop("Both Woods sources require independently identified explicitTotal report evidence")
    for (field in names(parsed)) {
      if (!field %in% names(points)) points[[field]] <- rep(parsed[[field]][NA_integer_], nrow(points))
      points[[field]][i] <- parsed[[field]][1L]
    }
    points$reported_feature_type[i] <- parsed$report_natural_type
    points$reported_height_m[i] <- parsed$report_waterfall_height_m
    points$reported_height_quality[i] <- parsed$report_waterfall_height_quality
    points$report_pdf[i] <- report$report_pdf
  }
  stopifnot(points$report_natural_type[1L] == "Waterfall", points$reported_height_m[1L] == 18.3,
    points$report_natural_type[2L] == "Gradient", points$snap_distance_m[2L] > 50)
  points$explicit_total_passage <- points$confirmed_total_for_scenario <- TRUE
  points$source_explicit_total <- FALSE
  points$total_evidence_basis <- points$total_passage_evidence <- "WDFW FPDSI natural barrier assessment report page3: BlockageTotal"
  points$phase3_identity_reference <- c("Pending same-stream USGS NHDPlus course validation", "No physical candidate within50m")
  points
}

build_phase3_woods_barriers <- function(cache_dir = "spatial_data/phase3", prepare_only = FALSE) {
  points <- phase3_woods_inputs(cache_dir)
  gpkg <- "outputs/review_swifd_connections/North_Puget_Sound_Steelhead.gpkg"
  edges <- sf::st_read(gpkg, layer = "stream_matching_edges", quiet = TRUE)
  batch <- file.path(cache_dir, "woods", "basins")
  request <- points[points$review_number == 10L, ]
  prepared <- prepare_phase1_basin_requests(request, edges, cache_dir = batch)
  if (isTRUE(prepare_only)) return(invisible(prepared))
  delineations <- load_nldi_delineations(request, edges, cache_dir = batch)
  audit <- delineations$audit
  records <- nps_read_cache_json(file.path(batch, "manifest.json"), simplifyVector = FALSE)
  record <- Filter(function(x) x$review_record_key == "5:44379", records)[[1L]]
  if (nrow(audit) != 1L || nrow(delineations$candidate_masks) != 1L ||
      nrow(delineations$downstream_anchors) != 1L || audit$nldi_comid != "23963775" ||
      audit$nldi_reference_stream != "Woods Creek" || audit$route_support_coverage != 1 ||
      audit$indexed_shift_m > 50 || audit$completeness_method != "full_drainageBasin" ||
      request$candidate_count != 1L || request$snap_distance_m > 50 || request$graph_edge_id != 4767L)
    stop("Woods waterfall lacks strict independent same-stream/full-basin evidence")
  row <- which(points$review_number == 10L)
  for (field in c("snap_accepted", "stream_identity_confirmed", "stream_identity_valid", "identity_valid_for_cut",
      "allow_cut", "own_stream_match", "identity_correction_applied", "eligible_for_future_cut", "phase3_selected"))
    points[[field]][row] <- TRUE
  points$own_stream_candidate_count[row] <- points$matching_channel_count[row] <- 1L
  points$matched_stream_name[row] <- "Woods Creek"
  points$matched_llid[row] <- "1219691478469"
  points$matched_route_key[row] <- "USGS_COMID_23963775|woods_creek"
  points$stream_identity_status[row] <- points$identity_status[row] <- "own_stream_confirmed_independent_published_USGS_course"
  points$identity_review_status[row] <- "confirmed_Woods_east_fork_physical_channel_by_USGS_course_and_source_waterfall"
  points$phase3_identity_reference[row] <- "Published USGS WoodsCreek COMID23963775: seven physical-course samples,100% within25m/35deg; uniquephysicaledge4767 within50m"
  points$phase3_review_action[row] <- "Apply newly report-confirmed field-verified18.3mTotal waterfall; independent same-fork channel evidence"
  points$phase3_nldi_comid <- c("23963775", NA_character_)
  points$phase3_nldi_course_coverage <- c(audit$route_support_coverage, NA_real_)
  points$phase3_nldi_max_reference_distance_m <- c(record$route_max_distance_m, NA_real_)
  points$phase3_nldi_max_heading_degrees <- c(record$route_max_heading_degrees, NA_real_)
  selected <- points[points$phase3_selected, ]
  network <- readRDS("spatial_data/north_puget_swifd_bridges.rds")$bridges$network
  cuts <- cut_reviewed_barriers(network, selected, edges, delineations$downstream_anchors)
  graph_review <- prepare_phase3_woods_graph(cuts$network, selected, edges, cache_dir)
  cuts$network <- graph_review$network
  validation_points <- selected
  validation_points$phase1_selected <- TRUE
  checked <- validate_nldi_basin_masks(delineations$candidate_masks, cuts$network, validation_points)
  if (nrow(checked$masks) != 1L) {
    print(checked$audit[, c("review_number", "approval_reason")])
    stop("Woods full basin failed independently proven graph/outlet agreement")
  }
  report_audit <- sf::st_drop_geometry(points)[, c("review_number", "review_record_key", "SiteId", "StreamName",
    "verification", "SiteLatitude", "SiteLongitude", "snap_distance_m", "graph_edge_id", "report_blockage",
    "reported_feature_type", "reported_height_m", "phase3_selected", "phase3_review_action", "phase3_identity_reference")]
  report_audit$complete_upstream_mask_approved <- report_audit$review_record_key %in% checked$masks$review_record_key
  bundle <- list(barriers = points, masks = checked$masks, anchors = delineations$downstream_anchors,
    audit = report_audit, basin_audit = audit, basin_validation = checked$audit, graph_audit = cuts$barrier_audit,
    graph_direction_correction_audit = graph_review$audit, graph_direction_course_checks = graph_review$course_checks,
    notes = c("Existing10/site891970/5:44379 retained; verifiedTotal waterfall18.3m, survey2010-01-01,Physicalcriterion.",
      "East/main Woods fork identity independently confirmed by named USGS course23963775 with unchanged25m/35degree/80% gates (actual100%).",
      "WestFork914/site891169 is source-reportedTotalGradient,unverified,heightunknown; held394.6m from nearest physicalmodel edge,never snapped/truncated.",
      "Queries cover all9WDFW fish-passage layers. Total-coded separate tributaries and WoodsLake/Sultan drainage are excluded from fork cuts.",
      "Ecology WRIA7Plan2024 identifies EastFork natural waterfall preventing anadromous passage; WFCarpenter and eastfork join downstream.",
      "No printed species/run label is treated as independent steelhead-run-specific blockage proof."))
  dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)
  saveRDS(bundle, file.path(cache_dir, "woods_barriers.rds"))
  utils::write.csv(report_audit, file.path(cache_dir, "woods_barrier_audit.csv"), row.names = FALSE)
  utils::write.csv(graph_review$audit, file.path(cache_dir, "woods_graph_direction_correction.csv"), row.names = FALSE)
  utils::write.csv(graph_review$course_checks, file.path(cache_dir, "woods_graph_course_checks.csv"), row.names = FALSE)
  invisible(bundle)
}

prepare_phase3_woods_graph <- function(network, barrier_points, stream_edges,
    cache_dir = "spatial_data/phase3") {
  key <- "5:44379"
  point <- barrier_points[barrier_points$review_record_key == key, ]
  empty <- list(network = network, audit = data.frame(), course_checks = data.frame())
  if (!nrow(point)) return(empty)
  if (nrow(point) != 1L || point$review_number != 10L || point$SiteId != "891970" ||
      point$report_blockage != "Total" || point$reported_height_m != 18.3 ||
      !isTRUE(point$snap_accepted) || !isTRUE(point$identity_valid_for_cut) || point$snap_distance_m > 50)
    stop("The Woods graph correction is restricted to the reviewed18.3mTotal waterfall10")
  rows <- which(network$graph_edge_id == 4574L & network$source_class == "prediction" &
    upstream_has_key(network$downstream_of_barrier, key))
  if (!length(rows)) return(empty)
  reference_edge <- stream_edges[stream_edges$graph_edge_id == 4574L, ]
  if (nrow(reference_edge) != 1L ||
      !length(sf::st_equals(sf::st_union(sf::st_geometry(network[network$graph_edge_id %in% 4574L, ])),
        sf::st_union(sf::st_geometry(reference_edge)))[[1L]]))
    stop("The reviewed northern Woods prediction edge changed geometry")
  woods <- file.path(cache_dir, "woods")
  records <- nps_read_cache_json(file.path(woods, "basins", "manifest.json"), simplifyVector = FALSE)
  record <- Filter(function(x) x$review_record_key == key, records)[[1L]]
  if (record$comid != "23963775" || !identical(unname(tools::md5sum(record$basin_path)), record$basin_md5))
    stop("Woods correction lacks its exact unchanged complete-basin snapshot")
  basin <- sf::st_read(record$basin_path, quiet = TRUE)
  basin <- sf::st_transform(basin[basin$id == "drainageBasin", ], sf::st_crs(network))
  if (nrow(basin) != 1L) stop("Woods correction requires a unique full drainageBasin")
  nav_file <- file.path(woods, "upstream_navigation.json")
  route_file <- file.path(woods, "northern_upstream_reference.json")
  navigation <- nps_read_cache_json(nav_file, simplifyVector = FALSE)
  ids <- vapply(navigation$features, function(f) as.character(f$properties$nhdplus_comid), character(1))
  if (!all(c("23963775", "23963905") %in% ids))
    stop("The northern reference reach is not positively upstream of Woods waterfall in the official UT network")
  raw_route <- nps_read_cache_json(route_file, simplifyVector = FALSE)
  properties <- raw_route$features[[1L]]$properties
  if (length(raw_route$features) != 1L || as.character(properties$comid) != "23963905" ||
      properties$gnis_name != "Woods Creek" || tolower(properties$flowdir) != "with digitized")
    stop("Northern Woods reference name, identity or published flow direction changed")
  route <- suppressWarnings(sf::st_cast(sf::st_transform(sf::st_read(route_file, quiet = TRUE),
    sf::st_crs(network)), "LINESTRING"))
  if (nrow(route) != 1L) stop("Northern Woods course proof requires the single reviewed source reference line")
  inside <- upstream_line_parts(suppressWarnings(sf::st_intersection(network[rows, ], sf::st_union(basin))))
  outside <- upstream_line_parts(suppressWarnings(sf::st_difference(network[rows, ], sf::st_union(basin))))
  if (nrow(inside) != 1L || !nrow(outside) ||
      as.numeric(sf::st_distance(inside, point)) <= 1000)
    stop("The reviewed remote Woods source must cross the watershed divide, independently of the local outlet zone")
  xy <- unclass(sf::st_geometry(inside)[[1L]])[, 1:2, drop = FALSE]
  length_m <- as.numeric(sf::st_length(inside))
  rxy <- unclass(sf::st_geometry(route)[[1L]])[, 1:2, drop = FALSE]
  reference_length <- as.numeric(sf::st_length(route))
  # Review three fixed, evenly spaced interior course windows. This audits the
  # valley assignment, not a new barrier snap or a blanket direction override.
  # Report the middle window's imperfect support too; at least two windows must
  # meet the unchanged25m/35degree/80% local-reference controls.
  checks <- lapply(c(.25, .5, .75), function(fraction) {
    support <- stream_identity_slice(xy, length_m * fraction - 50, length_m * fraction + 50)
    do.call(rbind, lapply(seq(0, 100, length.out = 7), function(m) {
      sample <- stream_identity_at(support, m)
      projected <- stream_identity_project(rxy, sample)
      physical_heading <- stream_identity_at(support, min(100, m + 25)) -
        stream_identity_at(support, max(0, m - 25))
      reference_heading <- stream_identity_at(rxy, min(reference_length, projected$measure + 25)) -
        stream_identity_at(rxy, max(0, projected$measure - 25))
      angle <- acos(min(1, abs(sum(physical_heading * reference_heading)) /
        sqrt(sum(physical_heading^2) * sum(reference_heading^2)))) * 180 / pi
      data.frame(review_number = 10L, graph_edge_id = 4574L, fraction = fraction,
        sample_m = m, distance_m = projected$distance, heading_degrees = angle,
        supports_course = projected$distance <= 25 && angle <= 35)
    }))
  })
  checks <- do.call(rbind, checks)
  coverage <- tapply(checks$supports_course, checks$fraction, mean)
  if (sum(coverage >= .8) < 2L) stop("The northern Woods source valley lacks independent physical-course support")
  original_wkb <- sf::st_as_binary(sf::st_geometry(network))
  remove_key <- function(x) vapply(strsplit(ifelse(is.na(x), "", x), ";", fixed = TRUE),
    function(values) paste(values[nzchar(values) & values != key], collapse = ";"), character(1))
  network$downstream_of_barrier[rows] <- remove_key(network$downstream_of_barrier[rows])
  network$unresolved_by_barrier[rows] <- upstream_append_key(network$unresolved_by_barrier[rows], key)
  network$unresolved_direction[rows] <- TRUE
  unchanged_status <- !(network$removed[rows] %in% TRUE)
  network$cut_status[rows[unchanged_status]] <- "unresolved_Woods_cross_divide_prediction_connectivity"
  stopifnot(identical(original_wkb, sf::st_as_binary(sf::st_geometry(network))))
  audit <- data.frame(review_number = 10L, review_record_key = key, graph_edge_id = 4574L,
    source_class = "prediction", action = "Clear only Woods10 downstream certainty; mark unresolved; complete basin decides inside portion",
    changed_rows = length(rows), source_geometry_unchanged = TRUE,
    inside_basin_m = sum(as.numeric(sf::st_length(inside))),
    outside_basin_m = sum(as.numeric(sf::st_length(outside))),
    upstream_reference_comid = "23963905", waterfall_comid = "23963775",
    course_coverage_quarter = unname(coverage[1L]), course_coverage_middle = unname(coverage[2L]),
    course_coverage_three_quarters = unname(coverage[3L]),
    navigation_file = nav_file, navigation_md5 = unname(tools::md5sum(nav_file)),
    reference_file = route_file, reference_md5 = unname(tools::md5sum(route_file)),
    basin_file = record$basin_path, basin_md5 = record$basin_md5,
    navigation_source_url = "https://api.water.usgs.gov/nldi/linked-data/comid/23963775/navigation/UT/flowlines?f=json&distance=1000",
    reference_source_url = "https://api.water.usgs.gov/fabric/pygeoapi/collections/nhdflowline_network/items?comid=23963905&f=json",
    stringsAsFactors = FALSE)
  list(network = network, audit = audit, course_checks = checks)
}

load_phase3_woods_barriers <- function(cache_dir = "spatial_data/phase3") {
  path <- file.path(cache_dir, "woods_barriers.rds")
  if (!file.exists(path)) stop("Prepare reviewed Woods barrier sources first with scripts/prepare_phase3_woods_barriers.py")
  bundle <- nps_read_cache_rds(path)
  if (!identical(as.integer(bundle$barriers$review_number), c(10L, 914L)) ||
      nrow(bundle$masks) != 1L || bundle$masks$review_record_key != "5:44379" ||
      bundle$barriers$phase3_selected[2L] || bundle$barriers$snap_accepted[2L])
    stop("Woods bundle no longer preserves reviewed source identities/held WestFork location")
  bundle
}
