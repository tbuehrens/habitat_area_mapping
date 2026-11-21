# Drainage masks classify existing habitat, including disconnected prediction
# components. They never add reference hydrography to habitat linework.
if (!exists("nps_read_cache_json", mode = "function")) source(file.path("R", "project_paths.R"))

upstream_has_key <- function(values, key) vapply(strsplit(as.character(ifelse(is.na(values), "", values)), ";", fixed = TRUE),
  function(x) key %in% x, logical(1))

upstream_append_key <- function(values, key) vapply(strsplit(as.character(ifelse(is.na(values), "", values)), ";", fixed = TRUE),
  function(x) paste(unique(c(x[nzchar(x)], key)), collapse = ";"), character(1))

upstream_line_parts <- function(x) {
  if (!nrow(x)) return(x)
  x <- x[!sf::st_is_empty(x), , drop = FALSE]
  if (!nrow(x)) return(x)
  x <- x[sf::st_geometry_type(x) %in% c("LINESTRING", "MULTILINESTRING", "GEOMETRYCOLLECTION"), , drop = FALSE]
  if (!nrow(x)) return(x)
  x <- suppressWarnings(sf::st_collection_extract(x, "LINESTRING"))
  x <- suppressWarnings(sf::st_cast(x, "LINESTRING"))
  x[as.numeric(sf::st_length(x)) > 1e-8, , drop = FALSE]
}

upstream_local_geometry_check <- function(lines, point, limit_m) {
  if (!nrow(lines)) return(list(length_m = 0, max_distance_m = 0, valid = TRUE))
  distance <- sqrt(rowSums(sweep(sf::st_coordinates(lines)[, 1:2, drop = FALSE],
    2L, sf::st_coordinates(point)[1L, 1:2])^2))
  length_m <- sum(as.numeric(sf::st_length(lines)))
  list(length_m = length_m, max_distance_m = max(distance),
    valid = length_m <= limit_m && max(distance) <= limit_m)
}

load_gorge_upstream_basin <- function(gorge_review_row, graph_network,
    cache_dir = "spatial_data/upstream_basins", local_correction_limit_m = 100) {
  if (length(local_correction_limit_m) != 1L || !is.finite(local_correction_limit_m) ||
      local_correction_limit_m < 0 || local_correction_limit_m > 100)
    stop("The approved local downstream correction is bounded at 100 meters")
  if (!inherits(gorge_review_row, "sf") || nrow(gorge_review_row) != 1L ||
      gorge_review_row$review_record_key != "user_override:Gorge_Dam") stop("Supply the uniquely reviewed Gorge Dam row")
  required <- c("snap_accepted", "stream_identity_confirmed", "identity_valid_for_cut", "override_future_cut")
  if (!all(required %in% names(gorge_review_row)) ||
      !all(vapply(required, function(n) isTRUE(gorge_review_row[[n]][1L]), logical(1))) ||
      isTRUE(gorge_review_row$junction_ambiguous[1L]) ||
      !identical(as.character(gorge_review_row$matched_llid[1L]), "1223661483874"))
    stop("Gorge basin requires the positively matched Skagit River override")
  if (!all(c("downstream_of_barrier", "removed_by_barrier") %in% names(graph_network)))
    stop("Supply the exact component-local graph result to validate the basin's local outlet")
  files <- file.path(cache_dir, c("gorge_split_basin.json", "gorge_hydrolocation.json", "gorge_site.json"))
  if (!all(file.exists(files)) || any(file.info(files)$size == 0))
    stop("Fetch the precise USGS split-catchment response and hydrolocation first; no whole-catchment fallback")
  basin <- sf::st_read(files[1L], quiet = TRUE)
  if (!"id" %in% names(basin) || sum(basin$id == "drainageBasin") != 1L)
    stop("USGS split response has no unique full drainageBasin geometry")
  basin <- basin[basin$id == "drainageBasin", ]
  hydro <- sf::st_read(files[2L], quiet = TRUE)
  hydro <- hydro[hydro$source == "indexed", ]
  site <- sf::st_read(files[3L], quiet = TRUE)
  if (nrow(hydro) != 1L || nrow(site) != 1L || as.character(hydro$comid) != "24255191" ||
      as.character(site$identifier) != "USGS-12177700" || as.character(site$comid) != "24255191")
    stop("USGS Gorge site and precise hydrolocation disagree")
  crs <- sf::st_crs(graph_network)
  basin <- sf::st_transform(sf::st_make_valid(basin), crs)
  dam <- sf::st_transform(gorge_review_row, crs)
  hydro <- sf::st_transform(hydro, crs)
  snap_m <- as.numeric(sf::st_distance(dam, hydro))
  if (!is.finite(snap_m) || snap_m > 50) stop("USGS precise outlet is beyond the approved local channel tolerance")
  area <- sum(as.numeric(sf::st_area(basin))) / 2589988.110336
  bounds <- sf::st_bbox(sf::st_transform(basin, 4326))
  if (abs(area / 1159 - 1) > .05 || bounds["ymax"] <= 49.1)
    stop("Gorge basin fails the independent USGS drainage-area / Canadian headwater benchmark")
  # Densify the international latitude boundary before projecting it so that a
  # straight projected chord does not materially bias the Canadian-area check.
  north_ring <- rbind(cbind(seq(bounds["xmin"] - .1, bounds["xmax"] + .1, length.out = 101L), 49),
    c(bounds["xmax"] + .1, bounds["ymax"] + .1), c(bounds["xmin"] - .1, bounds["ymax"] + .1),
    c(bounds["xmin"] - .1, 49))
  north <- sf::st_transform(sf::st_sfc(sf::st_polygon(list(unname(north_ring))), crs = 4326), crs)
  canadian_area <- sum(as.numeric(sf::st_area(suppressWarnings(sf::st_intersection(basin, north))))) / 2589988.110336
  if (abs(canadian_area / 400 - 1) > .1) stop("Gorge basin fails the independent USGS Canadian drainage-area benchmark")
  key <- as.character(dam$review_record_key)
  geometry <- sf::st_union(sf::st_geometry(basin))
  proven_downstream <- graph_network[upstream_has_key(graph_network$downstream_of_barrier, key) &
    graph_network$measure_habitat, ]
  overlap <- upstream_line_parts(suppressWarnings(sf::st_intersection(proven_downstream, geometry)))
  local_downstream <- upstream_local_geometry_check(overlap, dam, local_correction_limit_m)
  proven_upstream <- graph_network[upstream_has_key(graph_network$removed_by_barrier, key) &
    graph_network$measure_habitat, ]
  missing <- upstream_line_parts(suppressWarnings(sf::st_difference(proven_upstream, geometry)))
  local_upstream <- upstream_local_geometry_check(missing, dam, local_correction_limit_m)
  if (!local_downstream$valid || !local_upstream$valid)
    stop("Basin disagrees with exact graph direction beyond a bounded local outlet correction")
  sf::st_sf(review_record_key = key,
    review_number = if ("review_number" %in% names(dam)) dam$review_number else NA_integer_,
    graph_edge_id = dam$graph_edge_id, watershed_validated = TRUE,
    stream_identity_validated = TRUE, total_barrier_approved = TRUE,
    coverage_complete = TRUE, local_outlet_precision_validated = TRUE,
    protect_local_graph_downstream = TRUE,
    local_correction_limit_m = local_correction_limit_m,
    barrier_x = sf::st_coordinates(dam)[1L, 1L], barrier_y = sf::st_coordinates(dam)[1L, 2L],
    area_sqmi = area, official_area_sqmi = 1159,
    official_area_in_canada_sqmi = 400, area_in_canada_sqmi = canadian_area,
    northern_extent_latitude = unname(bounds["ymax"]),
    canadian_extent_validated = TRUE,
    outlet_comid = "24255191", outlet_reachcode = as.character(hydro$reachcode),
    outlet_measure = as.numeric(hydro$measure), outlet_to_model_barrier_m = snap_m,
    local_downstream_protected_m = local_downstream$length_m,
    local_upstream_outside_mask_m = local_upstream$length_m,
    mask_method = "USGS_precise_splitcatchment_full_upstream_basin_with_bounded_exact_graph_outlet_correction",
    mask_source_url = "https://api.water.usgs.gov/nldi/pygeoapi/processes/nldi-splitcatchment/execution?f=json",
    validation_source_url = "https://waterdata.usgs.gov/nwis/uv?legacy=1&site_no=12177700",
    mask_cache_file = normalizePath(files[1L], winslash = "/"),
    mask_cache_md5 = unname(tools::md5sum(files[1L])),
    basin_resolution_note = "NHDPlusV2/30m elevation delineation; exact graph direction protects the short offset at the approved model barrier",
    geometry = geometry)
}

validate_nldi_basin_masks <- function(candidate_masks, graph_network, barrier_points,
    local_correction_limit_m = 100) {
  if (length(local_correction_limit_m) != 1L || !is.finite(local_correction_limit_m) ||
      local_correction_limit_m < 0 || local_correction_limit_m > 100)
    stop("Local basin correction must be between zero and 100 meters")
  if (!inherits(candidate_masks, "sf") || !inherits(graph_network, "sf") || !inherits(barrier_points, "sf"))
    stop("Supply candidate drainage polygons, the exact graph partition, and reviewed barrier points")
  required <- c("review_record_key", "graph_edge_id", "watershed_validated", "stream_identity_validated",
    "total_barrier_approved", "coverage_complete")
  if (!all(required %in% names(candidate_masks))) stop("Candidate masks lack channel/source/total validation fields")
  if (!all(c("removed_by_barrier", "downstream_of_barrier", "measure_habitat") %in% names(graph_network)))
    stop("Graph partition lacks explicit upstream/downstream barrier causes")
  crs <- sf::st_crs(graph_network)
  masks <- sf::st_transform(candidate_masks, crs)
  points <- sf::st_transform(barrier_points, crs)
  audit <- sf::st_drop_geometry(masks)
  audit$mask_approved <- rep(FALSE, nrow(masks))
  audit$approval_status <- rep("pending_not_evaluated", nrow(masks))
  audit$approval_reason <- rep("", nrow(masks))
  audit$local_outlet_precision_validated <- rep(FALSE, nrow(masks))
  audit$basin_polygon_extracted_from_collection <- rep(FALSE, nrow(masks))
  for (field in c("graph_proven_upstream_physical_m", "graph_proven_downstream_physical_m",
      "proven_upstream_outside_mask_m", "proven_downstream_inside_mask_m",
      "max_local_disagreement_distance_m")) audit[[field]] <- rep(NA_real_, nrow(masks))
  masks$local_outlet_precision_validated <- rep(FALSE, nrow(masks))
  masks$protect_local_graph_downstream <- rep(TRUE, nrow(masks))
  masks$local_correction_limit_m <- rep(local_correction_limit_m, nrow(masks))
  masks$barrier_x <- masks$barrier_y <- rep(NA_real_, nrow(masks))
  for (i in seq_len(nrow(masks))) {
    key <- as.character(masks$review_record_key[i])
    checked <- tryCatch({
      positive <- vapply(required[3:6], function(field) isTRUE(masks[[field]][i]), logical(1))
      if (!all(positive)) stop("candidate_validation_missing:", paste(required[3:6][!positive], collapse = ","))
      row <- which(as.character(points$review_record_key) == key)
      if (length(row) != 1L) stop("reviewed_barrier_not_unique")
      point <- points[row, ]
      true <- function(field) field %in% names(point) && isTRUE(point[[field]][1L])
      strict <- all(vapply(c("snap_accepted", "stream_identity_confirmed", "identity_valid_for_cut"), true, logical(1)))
      strict <- strict && !true("junction_ambiguous") &&
        ("matching_channel_count" %in% names(point) && isTRUE(point$matching_channel_count == 1L))
      if (!strict || !isTRUE(point$graph_edge_id == masks$graph_edge_id[i])) stop("strict_selected_stream_identity_not_confirmed")
      total <- true("explicit_total_passage") || true("override_future_cut") || true("confirmed_total_for_scenario")
      if (!total || true("total_evidence_conflict") || true("passability_evidence_conflict") ||
          true("report_blockage_conflict")) stop("explicit_total_barrier_evidence_not_confirmed")
      if ("phase1_selected" %in% names(point) && !true("phase1_selected")) stop("barrier_not_selected_for_current_phase")
      if (!sf::st_geometry_type(masks)[i] %in% c("POLYGON", "MULTIPOLYGON", "GEOMETRYCOLLECTION") || sf::st_is_empty(masks[i, ]))
        stop("candidate_basin_geometry_invalid")
      valid <- sf::st_make_valid(sf::st_geometry(masks)[i])
      if (sf::st_geometry_type(valid) == "GEOMETRYCOLLECTION") {
        valid <- sf::st_union(suppressWarnings(sf::st_collection_extract(valid, "POLYGON")))
        audit$basin_polygon_extracted_from_collection[i] <- TRUE
      }
      if (sf::st_is_empty(valid) || !sf::st_geometry_type(valid) %in% c("POLYGON", "MULTIPOLYGON"))
        stop("candidate_basin_has_no_positive_polygon")
      if (as.numeric(sf::st_area(valid)) <= 0) stop("candidate_basin_area_not_positive")
      sf::st_geometry(masks)[i] <- valid
      upstream <- graph_network[upstream_has_key(graph_network$removed_by_barrier, key), ]
      downstream <- graph_network[upstream_has_key(graph_network$downstream_of_barrier, key), ]
      up_m <- sum(as.numeric(sf::st_length(upstream[upstream$measure_habitat, ])))
      down_m <- sum(as.numeric(sf::st_length(downstream[downstream$measure_habitat, ])))
      audit$graph_proven_upstream_physical_m[i] <- up_m
      audit$graph_proven_downstream_physical_m[i] <- down_m
      if (up_m <= 1e-8 || down_m <= 1e-8) stop("distinct_graph_upstream_downstream_direction_missing")
      outside <- upstream_line_parts(suppressWarnings(sf::st_difference(upstream, valid)))
      overlap <- upstream_line_parts(suppressWarnings(sf::st_intersection(downstream, valid)))
      up <- upstream_local_geometry_check(outside, point, local_correction_limit_m)
      down <- upstream_local_geometry_check(overlap, point, local_correction_limit_m)
      audit$proven_upstream_outside_mask_m[i] <- up$length_m
      audit$proven_downstream_inside_mask_m[i] <- down$length_m
      audit$max_local_disagreement_distance_m[i] <- max(up$max_distance_m, down$max_distance_m)
      if (!up$valid) stop("known_upstream_geometry_outside_basin_beyond_local_limit")
      if (!down$valid) stop("known_downstream_geometry_inside_basin_beyond_local_limit")
      masks$barrier_x[i] <- sf::st_coordinates(point)[1L, 1L]
      masks$barrier_y[i] <- sf::st_coordinates(point)[1L, 2L]
      TRUE
    }, error = function(e) conditionMessage(e))
    if (isTRUE(checked)) {
      masks$local_outlet_precision_validated[i] <- TRUE
      audit$mask_approved[i] <- audit$local_outlet_precision_validated[i] <- TRUE
      audit$approval_status[i] <- "approved_precise_basin_with_bounded_graph_outlet_check"
    } else {
      audit$approval_status[i] <- "pending_watershed_validation"
      audit$approval_reason[i] <- as.character(checked)
    }
  }
  list(masks = masks[audit$mask_approved, ], audit = audit,
    settings = list(local_correction_limit_m = local_correction_limit_m,
      independent_local_graph_direction_required = TRUE,
      missing_direction_or_distant_disagreement = "held; no watershed removal applied"))
}

load_nldi_upstream_membership <- function(approved_masks,
    cache_dir = "spatial_data/upstream_basins/navigation", refresh = FALSE, download = TRUE,
    barrier_points = NULL) {
  masks <- approved_masks
  audit <- data.frame(review_record_key = as.character(masks$review_record_key),
    outlet_comid = rep(NA_character_, nrow(masks)), status = rep("unavailable", nrow(masks)),
    source_url = rep(NA_character_, nrow(masks)), cache_file = rep(NA_character_, nrow(masks)),
    error = rep(NA_character_, nrow(masks)), stringsAsFactors = FALSE)
  membership <- setNames(vector("list", nrow(masks)), as.character(masks$review_record_key))
  pending <- if (!is.null(barrier_points)) sf::st_transform(barrier_points[
    !barrier_points$review_record_key %in% masks$review_record_key, ], sf::st_crs(masks)) else NULL
  dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)
  for (i in seq_len(nrow(masks))) {
    if (!is.null(pending)) {
      rows <- which(lengths(sf::st_within(pending, masks[i, ])) > 0L)
      deep <- length(rows) && any(as.numeric(sf::st_distance(pending[rows, ], sf::st_boundary(masks[i, ]))) > 100)
      if (!deep) { audit$status[i] <- "not_needed_no_pending_deep_interior_site"; next }
    }
    id <- if ("outlet_comid" %in% names(masks)) as.character(masks$outlet_comid[i]) else NA_character_
    if ((is.na(id) || !nzchar(id)) && "nldi_comid" %in% names(masks)) id <- as.character(masks$nldi_comid[i])
    if (is.na(id) || !grepl("^[0-9]+$", id)) { audit$error[i] <- "Approved basin has no NLDI outlet COMID"; next }
    url <- paste0("https://api.water.usgs.gov/nldi/linked-data/comid/", id,
      "/navigation/UT/flowlines?f=json&distance=1000")
    path <- file.path(cache_dir, paste0("comid_", id, "_UT_1000.json"))
    audit$outlet_comid[i] <- id; audit$source_url[i] <- url; audit$cache_file[i] <- path
    found <- tryCatch({
      if (isTRUE(refresh) || !file.exists(path)) {
        if (!isTRUE(download)) stop("Upstream navigation cache has not been fetched")
        utils::download.file(url, path, quiet = TRUE, mode = "wb",
          method = if (.Platform$OS.type == "windows") "wininet" else "libcurl")
      }
      raw <- nps_read_cache_json(path, simplifyVector = FALSE)
      if (!identical(raw$type, "FeatureCollection")) stop("Navigation response is not a FeatureCollection")
      ids <- unique(vapply(raw$features, function(x) as.character(x$properties$nhdplus_comid), character(1)))
      if (!id %in% ids) stop("Navigation response does not contain its starting COMID")
      ids
    }, error = function(e) e)
    if (inherits(found, "error")) audit$error[i] <- conditionMessage(found)
    else { membership[[i]] <- found; audit$status[i] <- "positive_upstream_comid_membership_available" }
  }
  list(membership = membership, audit = audit,
    settings = list(navigation_mode = "UT", search_distance_km = 1000,
      negative_membership_is_not_exhaustive_proof = TRUE))
}

upstream_verify_cached_course <- function(record, point, stream_edges, support_index) {
  # Re-read geometry and published attributes; an earlier approval boolean or
  # a cached coverage statistic cannot substitute for current physical support.
  record <- nps_resolve_record_paths(record)
  tryCatch({
    for (field in c("snap_accepted", "stream_identity_confirmed", "identity_valid_for_cut"))
      if (!field %in% names(point) || !isTRUE(point[[field]][1L])) stop("strict_current_channel_identity_unconfirmed")
    if (isTRUE(point$junction_ambiguous[1L]) || !isTRUE(point$matching_channel_count[1L] == 1L))
      stop("strict_current_channel_is_ambiguous")
    if (is.null(record$cache_folder) || is.null(record$comid) ||
        is.null(record$indexed_longitude) || is.null(record$indexed_latitude)) stop("missing_cached_channel_evidence")
    path <- file.path(record$cache_folder, "named_flowline.json")
    if (!file.exists(path)) stop("cached_reference_geometry_missing")
    raw <- nps_read_cache_json(path, simplifyVector = FALSE)
    feature <- Filter(function(x) identical(as.character(x$properties$comid), as.character(record$comid)), raw$features)
    if (length(feature) != 1L) stop("cached_reference_comid_not_unique")
    feature <- feature[[1L]]
    if (!stream_identity_names_match(point$StreamName, feature$properties$gnis_name)) stop("cached_reference_name_unconfirmed")
    if (!is.null(record$graph_edge_id) && !isTRUE(as.integer(record$graph_edge_id) == point$graph_edge_id))
      stop("cached_request_graph_edge_changed")
    crs <- sf::st_crs(stream_edges)
    route <- sf::st_read(jsonlite::toJSON(list(type = "FeatureCollection", features = list(feature)),
      auto_unbox = TRUE, digits = NA), quiet = TRUE)
    route <- suppressWarnings(sf::st_cast(sf::st_transform(route, crs), "LINESTRING"))
    indexed <- sf::st_transform(sf::st_sfc(sf::st_point(c(record$indexed_longitude,
      record$indexed_latitude)), crs = 4326), crs)
    if (as.numeric(sf::st_distance(indexed, point)) > 50) stop("current_index_shift_beyond_50m")
    edge <- match(point$graph_edge_id, stream_edges$graph_edge_id)
    if (is.na(edge)) stop("current_selected_graph_edge_missing")
    point_xy <- sf::st_coordinates(point)[1L, 1:2]
    projection <- stream_identity_project(support_index$coordinates[[edge]], point_xy)
    if (projection$distance > 1e-5) stop("point_not_on_current_selected_graph_edge")
    support <- stream_identity_local_support(support_index, edge, projection$measure, 50)
    if (support$length < 40 || support$topology_ambiguous || support$junction_distance <= 25)
      stop("current_course_support_or_junction_ambiguous")
    positions <- seq(0, support$length, length.out = 7L)
    samples <- t(vapply(positions, function(m) stream_identity_at(support$xy, m), numeric(2)))
    reference_xy <- lapply(sf::st_geometry(route), function(x) unclass(x)[, 1:2, drop = FALSE])
    reference_lengths <- as.numeric(sf::st_length(route))
    tangent_halfspan <- if (exists("nldi_course_controls", mode = "function"))
      nldi_course_controls()$heading_halfspan_m else 25
    distance <- heading <- numeric(7L)
    for (k in seq_len(7L)) {
      projected <- lapply(reference_xy, stream_identity_project, point = samples[k, ])
      part <- which.min(vapply(projected, `[[`, numeric(1), "distance"))
      at <- projected[[part]]
      own_tangent <- stream_identity_at(support$xy, min(support$length, positions[k] + tangent_halfspan)) -
        stream_identity_at(support$xy, max(0, positions[k] - tangent_halfspan))
      ref_tangent <- stream_identity_at(reference_xy[[part]], min(reference_lengths[part], at$measure + tangent_halfspan)) -
        stream_identity_at(reference_xy[[part]], max(0, at$measure - tangent_halfspan))
      norm <- sqrt(sum(own_tangent^2) * sum(ref_tangent^2))
      distance[k] <- at$distance
      heading[k] <- if (norm > 1e-12) acos(min(1, abs(sum(own_tangent * ref_tangent)) / norm)) * 180 / pi else 90
    }
    coverage <- mean(distance <= 25 + 1e-8 & heading <= 35 + 1e-8)
    if (coverage < .8) stop("cached_reference_current_course_unconfirmed")
    indexed_xy <- sf::st_coordinates(indexed)[1L, 1:2]
    indexed_projection <- lapply(reference_xy, stream_identity_project, point = indexed_xy)
    if (min(vapply(indexed_projection, `[[`, numeric(1), "distance")) > 25)
      stop("indexed_point_not_supported_by_cached_reference")
    list(valid = TRUE, indexed = indexed, reference_xy = reference_xy,
      flowdir = tolower(trimws(as.character(feature$properties$flowdir))),
      course_coverage = coverage, tangent_halfspan_m = tangent_halfspan,
      source_md5 = unname(tools::md5sum(path)))
  }, error = function(e) list(valid = FALSE, reason = conditionMessage(e)))
}

assess_downstream_basin_coverage <- function(barrier_points, approved_masks, nldi_records,
    upstream_membership, interior_margin_m = 100, stream_edges = NULL) {
  if (!is.finite(interior_margin_m) || interior_margin_m < 100) stop("Coverage requires at least 100 meters of interior separation")
  for (field in c("watershed_validated", "stream_identity_validated", "total_barrier_approved",
      "coverage_complete", "local_outlet_precision_validated"))
    if (!field %in% names(approved_masks) || any(!approved_masks[[field]] %in% TRUE))
      stop("Coverage requires fully approved downstream basins: ", field)
  crs <- sf::st_crs(approved_masks)
  points <- sf::st_transform(barrier_points, crs)
  records <- nps_resolve_record_paths(nldi_records)
  if (is.character(records) && length(records) == 1L)
    records <- nps_read_cache_json(records, simplifyVector = FALSE)
  if (is.data.frame(records)) stop("Supply raw per-request manifest records, preserving named-course validation and indexed coordinates")
  records <- lapply(records, function(record) {
    latest_path <- if (!is.null(record$cache_folder)) file.path(record$cache_folder, "manifest.json") else ""
    if (nzchar(latest_path) && file.exists(latest_path)) {
      latest <- tryCatch(nps_read_cache_json(latest_path, simplifyVector = FALSE), error = function(e) NULL)
      if (!is.null(latest) && identical(latest$review_record_key, record$review_record_key) &&
          identical(latest$request_sha256, record$request_sha256)) return(latest)
    }
    record
  })
  keys <- vapply(records, function(x) as.character(x$review_record_key), character(1))
  audit <- data.frame(review_record_key = as.character(points$review_record_key),
    own_cut_applied = points$review_record_key %in% approved_masks$review_record_key,
    covered_by_downstream_basin = FALSE, covering_basin_key = NA_character_,
    coverage_status = "pending_no_independent_downstream_basin_proof",
    upstream_comid = NA_character_, covering_outlet_comid = NA_character_,
    model_boundary_distance_m = NA_real_, indexed_boundary_distance_m = NA_real_,
    proof_method = NA_character_, stringsAsFactors = FALSE)
  audit$cached_course_revalidated <- FALSE
  audit$cached_course_tangent_halfspan_m <- NA_real_
  audit$cached_course_coverage <- audit$same_comid_upstream_gap_m <- NA_real_
  audit$cached_reference_md5 <- audit$published_flowdir <- NA_character_
  audit$coverage_status[audit$own_cut_applied] <- "own_complete_basin_approved"
  support_index <- NULL
  if (!is.null(stream_edges)) {
    stream_edges <- sf::st_transform(stream_edges, crs)
    coordinates <- lapply(sf::st_geometry(stream_edges), function(x) unclass(x)[, 1:2, drop = FALSE])
    adjacency <- lapply(seq_len(max(c(stream_edges$from_node, stream_edges$to_node))),
      function(node) c(which(stream_edges$from_node == node), which(stream_edges$to_node == node)))
    support_index <- list(edges = stream_edges, coordinates = coordinates, adjacency = adjacency,
      nodes = data.frame(degree = lengths(adjacency)))
  }
  proof_cache <- new.env(parent = emptyenv())
  proof <- function(key) {
    if (exists(key, envir = proof_cache, inherits = FALSE)) return(get(key, envir = proof_cache))
    row <- match(key, points$review_record_key); record_row <- match(key, keys)
    checked <- if (is.null(support_index) || is.na(row) || is.na(record_row))
      list(valid = FALSE, reason = "current_physical_course_or_cache_missing") else
        upstream_verify_cached_course(records[[record_row]], points[row, ], stream_edges, support_index)
    assign(key, checked, envir = proof_cache)
    checked
  }
  for (i in which(!audit$own_cut_applied)) {
    record_row <- match(points$review_record_key[i], keys)
    record <- if (is.na(record_row)) NULL else records[[record_row]]
    checked <- proof(as.character(points$review_record_key[i]))
    if (is.null(record) || !isTRUE(checked$valid)) { audit$coverage_status[i] <- "pending_own_cached_nldi_course_not_revalidated"; next }
    audit$cached_course_revalidated[i] <- TRUE
    audit$cached_course_tangent_halfspan_m[i] <- checked$tangent_halfspan_m
    audit$cached_course_coverage[i] <- checked$course_coverage
    audit$cached_reference_md5[i] <- checked$source_md5
    audit$published_flowdir[i] <- checked$flowdir
    id <- as.character(record$comid)
    audit$upstream_comid[i] <- id
    indexed <- checked$indexed
    for (j in seq_len(nrow(approved_masks))) {
      key <- as.character(approved_masks$review_record_key[j])
      ids <- upstream_membership[[key]]
      outlet <- if ("outlet_comid" %in% names(approved_masks)) as.character(approved_masks$outlet_comid[j]) else NA_character_
      if ((is.na(outlet) || !nzchar(outlet)) && "nldi_comid" %in% names(approved_masks)) outlet <- as.character(approved_masks$nldi_comid[j])
      same_reach <- identical(id, outlet)
      if (!same_reach && (!length(ids) || !id %in% ids)) next
      mask <- sf::st_geometry(approved_masks)[j]
      if (!length(sf::st_within(points[i, ], mask)[[1L]]) || !length(sf::st_within(indexed, mask)[[1L]])) next
      model_distance <- as.numeric(sf::st_distance(points[i, ], sf::st_boundary(mask)))
      indexed_distance <- as.numeric(sf::st_distance(indexed, sf::st_boundary(mask)))
      if (model_distance <= interior_margin_m || indexed_distance <= interior_margin_m) next
      if (same_reach) {
        # A single published WithDigitized reference establishes downstream
        # order. Project both actual delineation pourpoints onto that one part;
        # cached NHD measure conventions or model vertex order are not assumed.
        lower <- proof(key)
        lower_record_row <- match(key, keys)
        if (is.na(lower_record_row) || !identical(as.character(records[[lower_record_row]]$comid), outlet)) next
        if (!identical(checked$flowdir, "with digitized") || !isTRUE(lower$valid) ||
            !identical(lower$flowdir, "with digitized")) next
        upper_xy <- sf::st_coordinates(indexed)[1L, 1:2]
        lower_xy <- sf::st_coordinates(lower$indexed)[1L, 1:2]
        upper_at <- lapply(checked$reference_xy, stream_identity_project, point = upper_xy)
        lower_at <- lapply(checked$reference_xy, stream_identity_project, point = lower_xy)
        upper_parts <- which(vapply(upper_at, `[[`, numeric(1), "distance") <= 25)
        lower_parts <- which(vapply(lower_at, `[[`, numeric(1), "distance") <= 25)
        if (length(upper_parts) != 1L || length(lower_parts) != 1L || upper_parts != lower_parts) next
        gap <- lower_at[[lower_parts]]$measure - upper_at[[upper_parts]]$measure
        if (!is.finite(gap) || gap <= interior_margin_m) next
        # The lower barrier's independently read cache must agree in direction;
        # a stale/reversed source snapshot cannot silently supply its ordering.
        upper_again <- lapply(lower$reference_xy, stream_identity_project, point = upper_xy)
        lower_again <- lapply(lower$reference_xy, stream_identity_project, point = lower_xy)
        ua <- which(vapply(upper_again, `[[`, numeric(1), "distance") <= 25)
        la <- which(vapply(lower_again, `[[`, numeric(1), "distance") <= 25)
        if (length(ua) != 1L || length(la) != 1L || ua != la ||
            lower_again[[la]]$measure - upper_again[[ua]]$measure <= interior_margin_m) next
        audit$same_comid_upstream_gap_m[i] <- gap
      }
      audit$covered_by_downstream_basin[i] <- TRUE
      audit$covering_basin_key[i] <- key
      audit$covering_outlet_comid[i] <- outlet
      audit$model_boundary_distance_m[i] <- model_distance
      audit$indexed_boundary_distance_m[i] <- indexed_distance
      audit$proof_method[i] <- if (same_reach)
        "same_COMID_published_WithDigitized_order_over_100m_both_cached_courses_revalidated_and_both_points_deep_inside" else
        "distinct_COMID_in_USGS_upstream_tributary_navigation_current_cached_course_revalidated_and_both_points_deep_inside"
      audit$coverage_status[i] <- "covered_by_downstream_approved_basin_own_cut_not_applied"
      break
    }
  }
  audit$complete_upstream_coverage <- audit$own_cut_applied | audit$covered_by_downstream_basin
  list(audit = audit, settings = list(interior_margin_m = interior_margin_m,
    published_upstream_network_membership_required = TRUE,
    same_comid_ordering = "published_WithDigitized_single_part_both_indexed_pourpoints_more_than_100m_apart",
    current_cached_course_revalidation_required = TRUE,
    own_cut_claimed_for_covered_barriers = FALSE))
}

apply_upstream_basin_masks <- function(network, basin_masks, barrier_points = NULL, length_tolerance_m = 1e-5) {
  if (!inherits(network, "sf") || !inherits(basin_masks, "sf") || is.na(sf::st_crs(network)) ||
      isTRUE(sf::st_is_longlat(network))) stop("Supply projected source-tagged network and basin polygons")
  required <- c("review_record_key", "watershed_validated", "stream_identity_validated", "total_barrier_approved",
    "coverage_complete", "local_outlet_precision_validated")
  if (!all(required %in% names(basin_masks))) stop("Basin masks need explicit source, channel, total-barrier and outlet validation")
  if (!all(c("source_class", "measure_habitat") %in% names(network))) stop("Network source attribution is required")
  if (anyDuplicated(basin_masks$review_record_key)) stop("Use one validated drainage mask per barrier")
  for (field in required[-1L]) if (any(!basin_masks[[field]] %in% TRUE))
    stop("Unvalidated mask rejected: ", field)
  crs <- sf::st_crs(network)
  if (!crs$units_gdal %in% c("metre", "meter", "m")) stop("Use a network CRS in meters")
  masks <- sf::st_transform(sf::st_make_valid(basin_masks), crs)
  if (!"protect_local_graph_downstream" %in% names(masks)) masks$protect_local_graph_downstream <- rep(TRUE, nrow(masks))
  if (!"local_correction_limit_m" %in% names(masks)) masks$local_correction_limit_m <- rep(100, nrow(masks))
  if (any(!is.finite(masks$local_correction_limit_m)) || any(masks$local_correction_limit_m < 0) ||
      any(masks$local_correction_limit_m > 100)) stop("Local downstream protection may not exceed 100 meters")
  if (!is.null(barrier_points)) {
    barrier_points <- sf::st_transform(barrier_points, crs)
    matched <- match(masks$review_record_key, barrier_points$review_record_key)
    if (anyNA(matched)) stop("Every approved basin needs its reviewed barrier location")
    at <- sf::st_coordinates(barrier_points)[matched, 1:2, drop = FALSE]
    masks$barrier_x <- at[, 1L]; masks$barrier_y <- at[, 2L]
  }
  if (any(!sf::st_geometry_type(masks) %in% c("POLYGON", "MULTIPOLYGON"))) stop("Masks must be drainage polygons")
  lines <- network
  if (!"input_piece_id" %in% names(lines)) lines$input_piece_id <- seq_len(nrow(lines))
  if (!"removed" %in% names(lines)) lines$removed <- FALSE
  if (!"removed_by_barrier" %in% names(lines)) lines$removed_by_barrier <- ""
  if (!"downstream_of_barrier" %in% names(lines)) lines$downstream_of_barrier <- ""
  if (!"unresolved_direction" %in% names(lines)) lines$unresolved_direction <- FALSE
  if (!"cut_status" %in% names(lines)) lines$cut_status <- "retained_unaffected"
  lines$watershed_removed_by_barrier <- ""
  lines$watershed_downstream_protected_by_barrier <- ""
  initial <- tapply(as.numeric(sf::st_length(lines)), lines$source_class, sum)
  audit <- sf::st_drop_geometry(masks)
  for (field in c("inside_physical_m", "newly_removed_physical_m", "already_removed_physical_m",
      "protected_downstream_physical_m", "inside_alignment_m", "raw_inside_m", "remaining_inside_m",
      "protected_local_downstream_m", "max_protection_distance_m")) audit[[field]] <- rep(0, nrow(masks))
  audit$watershed_applied <- rep(FALSE, nrow(masks))
  audit$approval_status <- rep("approved_validated_watershed", nrow(masks))
  for (i in seq_len(nrow(masks))) {
    key <- as.character(masks$review_record_key[i])
    nearby <- which(lengths(sf::st_intersects(lines, masks[i, ])) > 0L)
    if (!length(nearby)) next
    selected <- lines[nearby, ]
    inside <- upstream_line_parts(suppressWarnings(sf::st_intersection(selected, sf::st_geometry(masks)[i])))
    outside <- upstream_line_parts(suppressWarnings(sf::st_difference(selected, sf::st_geometry(masks)[i])))
    if (!nrow(inside)) next
    inside_length <- as.numeric(sf::st_length(inside))
    protect <- rep(FALSE, nrow(inside))
    if ("protect_local_graph_downstream" %in% names(masks) && isTRUE(masks$protect_local_graph_downstream[i])) {
      protect <- upstream_has_key(inside$downstream_of_barrier, key)
      if (any(protect)) {
        needed <- c("local_correction_limit_m", "barrier_x", "barrier_y")
        if (!all(needed %in% names(masks))) stop("Local graph correction lacks a bounded barrier location")
        point <- sf::st_sfc(sf::st_point(c(masks$barrier_x[i], masks$barrier_y[i])), crs = crs)
        check <- upstream_local_geometry_check(inside[protect, ], point,
          masks$local_correction_limit_m[i])
        if (!check$valid) stop("Known downstream geometry inside mask exceeds approved local correction: ", key)
        audit$max_protection_distance_m[i] <- check$max_distance_m
        inside$watershed_downstream_protected_by_barrier[protect] <- upstream_append_key(
          inside$watershed_downstream_protected_by_barrier[protect], key)
      }
    }
    physical <- inside$measure_habitat & !protect
    audit$inside_physical_m[i] <- sum(inside_length[physical])
    audit$raw_inside_m[i] <- sum(inside_length[inside$measure_habitat])
    audit$newly_removed_physical_m[i] <- sum(inside_length[physical & !inside$removed])
    audit$already_removed_physical_m[i] <- sum(inside_length[physical & inside$removed])
    audit$protected_downstream_physical_m[i] <- sum(inside_length[protect & inside$measure_habitat])
    audit$inside_alignment_m[i] <- sum(inside_length[!inside$measure_habitat & !protect])
    inside$removed[!protect] <- TRUE
    inside$removed_by_barrier[!protect] <- upstream_append_key(inside$removed_by_barrier[!protect], key)
    inside$watershed_removed_by_barrier[!protect] <- upstream_append_key(inside$watershed_removed_by_barrier[!protect], key)
    inside$cut_status[!protect] <- "removed_upstream_basin"
    audit$remaining_inside_m[i] <- sum(inside_length[inside$measure_habitat & !inside$removed])
    audit$protected_local_downstream_m[i] <- sum(inside_length[protect & inside$measure_habitat & !inside$removed])
    lines <- rbind(lines[-nearby, , drop = FALSE], outside, inside)
    audit$watershed_applied[i] <- TRUE
    audit$approval_status[i] <- "applied_validated_watershed"
  }
  lines$piece_id <- seq_len(nrow(lines))
  lines$length_m <- as.numeric(sf::st_length(lines))
  lines$retained <- !lines$removed
  lines$unresolved_direction <- lines$unresolved_direction & lines$retained
  # Later, overlapping upstream masks can remove an earlier protected piece.
  # Report final residuals, rather than the intermediate state of each pass.
  retained_physical <- lines[lines$retained & lines$measure_habitat, ]
  for (i in seq_len(nrow(masks))) {
    key <- as.character(masks$review_record_key[i])
    near <- which(lengths(sf::st_intersects(retained_physical, masks[i, ])) > 0L)
    residual <- upstream_line_parts(suppressWarnings(sf::st_intersection(
      retained_physical[near, ], sf::st_geometry(masks)[i])))
    audit$remaining_inside_m[i] <- sum(as.numeric(sf::st_length(residual)))
    protected <- upstream_has_key(residual$watershed_downstream_protected_by_barrier, key) &
      upstream_has_key(residual$downstream_of_barrier, key)
    if (any(!protected)) stop("Unexplained retained habitat remains inside approved upstream basin: ", key)
    audit$protected_local_downstream_m[i] <- audit$remaining_inside_m[i]
    audit$max_protection_distance_m[i] <- 0
    if (nrow(residual)) {
      point <- sf::st_sfc(sf::st_point(c(masks$barrier_x[i], masks$barrier_y[i])), crs = crs)
      check <- upstream_local_geometry_check(residual, point, masks$local_correction_limit_m[i])
      if (!check$valid) stop("Final local downstream protection exceeds its approved bound: ", key)
      audit$max_protection_distance_m[i] <- check$max_distance_m
    }
  }
  final <- tapply(lines$length_m, lines$source_class, sum)
  error <- max(abs(initial - final[names(initial)]))
  if (!is.finite(error) || error > max(length_tolerance_m, sum(initial) * 1e-10))
    stop("Watershed splitting failed source-length conservation: ", error, "m")
  list(network = lines, retained = lines[lines$retained, ], removed = lines[lines$removed, ],
    unresolved = lines[lines$unresolved_direction, ], barrier_audit = audit,
    masks = masks, summary = list(applied_masks = sum(audit$watershed_applied),
      retained_physical_m = sum(lines$length_m[lines$retained & lines$measure_habitat]),
      removed_physical_m = sum(lines$length_m[lines$removed & lines$measure_habitat]),
      source_partition_error_m = error),
    settings = list(method = "validated_upstream_drainage_polygon_intersection",
      habitat_added = FALSE, geographic_bbox_or_longitude_filter = FALSE))
}
