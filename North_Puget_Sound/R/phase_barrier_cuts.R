# Component-local cuts at positively identified total barriers. This module never
# filters a network by coastal/outlet connectivity. A disconnected component is
# retained unless an explicit downstream anchor proves which side of its own
# matched barrier is upstream.

gorge_downstream_anchor <- function(crs = 26910) {
  sf::st_transform(sf::st_sf(
    anchor_id = "USGS_12178000_Newhalem",
    review_record_key = "user_override:Gorge_Dam",
    graph_edge_id = NA_integer_,
    projection_rule = "same_selected_barrier_graph_edge",
    max_snap_distance_m = 50,
    direction_evidence_valid = TRUE,
    anchor_reviewed = TRUE,
    expected_stream_name = "Skagit River",
    expected_llid = "1223661483874",
    coordinate_accuracy_note = "USGS map-interpolated monitoring location; reported accuracy one arc-second",
    direction_basis = paste("Seattle City Light 2021 Revised Study Plan identifies Gorge Dam at USGS river mile 96.6",
      "and the downstream Newhalem gage at river mile 93.7. Direction is independent of line vertex order."),
    coordinate_source_url = "https://waterdata.usgs.gov/monitoring-location/USGS-12178000/",
    direction_source_url = "https://www.seattle.gov/Documents/Departments/CityLight/CurrentProjects/P-553_04_Skagit_RSP_20210407.pdf",
    geometry = sf::st_sfc(sf::st_point(c(-121.246234788845, 48.6717922878038)), crs = 4326)), crs)
}

# Orient a short anchor on the exact selected physical edge using an independent
# named-reference tree. Its root must be a uniquely validated receiving-stream
# confluence, marine contact, or a close corroborated LLID mouth. An LLID is a
# persistent identifier, so its decoded coordinates alone never establish flow.
derive_downstream_anchors <- function(barriers, stream_edges, named_reference,
    marine_geometry = NULL, confluence_tolerance = 25, mouth_tolerance = 100,
    marine_tolerance = 250, local_anchor_distance = 100,
    reference_tolerance = 25, node_tolerance = 0.25, anchor_reviewed = TRUE) {
  if (!exists("stream_identity_names_match", mode = "function"))
    stop("Source R/barrier_stream_identity.R and R/stream_edge_identity.R first")
  crs <- sf::st_crs(stream_edges)
  barriers <- sf::st_transform(barriers, crs)
  reference <- sf::st_transform(named_reference, crs)
  if (!is.null(marine_geometry)) marine_geometry <- sf::st_transform(marine_geometry, crs)
  name_field <- intersect(c("reference_stream_name", "LLID_STRM_NAME", "LLID_STREAMNAME"), names(reference))[1L]
  if (is.na(name_field) || !"LLID" %in% names(reference)) stop("Reference needs LLID and stream names")
  template <- gorge_downstream_anchor(crs)
  for (field in c("reference_root_distance_m", "reference_root_cycle_rank", "reference_minus_distance_m",
      "reference_plus_distance_m", "local_reference_max_distance_m", "local_reference_max_heading_degrees"))
    template[[field]] <- NA_real_
  template$root_validation_method <- NA_character_
  anchors <- list()
  audit <- data.frame(review_record_key = as.character(barriers$review_record_key),
    graph_edge_id = barriers$graph_edge_id, matched_llid = as.character(barriers$matched_llid),
    direction_status = "unresolved_reference_not_evaluated", root_validation_method = NA_character_,
    root_distance_m = NA_real_, root_cycle_rank = NA_integer_,
    minus_root_distance_m = NA_real_, plus_root_distance_m = NA_real_,
    local_reference_max_distance_m = NA_real_, local_reference_max_heading_degrees = NA_real_,
    stringsAsFactors = FALSE)
  point_at <- function(xy, measure) network_slice_line(xy, measure,
    sum(sqrt(rowSums(diff(xy)^2))))[1L, ]
  for (i in seq_len(nrow(barriers))) {
    id <- as.character(barriers$matched_llid[i])
    edge <- match(barriers$graph_edge_id[i], stream_edges$graph_edge_id)
    if (is.na(id) || !grepl("^[0-9]{13}$", id) || is.na(edge)) {
      audit$direction_status[i] <- "unresolved_reference_id_or_selected_edge_missing"; next
    }
    own <- which(!is.na(reference$LLID) & as.character(reference$LLID) == id)
    if (!length(own)) { audit$direction_status[i] <- "unresolved_reference_route_missing"; next }
    geometry <- suppressWarnings(sf::st_cast(sf::st_union(sf::st_geometry(reference)[own]), "LINESTRING"))
    geometry <- geometry[as.numeric(sf::st_length(geometry)) > 1e-8]
    xy <- lapply(geometry, function(z) unclass(z)[, 1:2, drop = FALSE])
    ep <- do.call(rbind, lapply(xy, function(z) z[c(1L, nrow(z)), , drop = FALSE]))
    groups <- network_endpoint_groups(ep, node_tolerance)
    from <- groups[seq.int(1L, length(groups), 2L)]
    to <- groups[seq.int(2L, length(groups), 2L)]
    nxy <- ep[!duplicated(groups), , drop = FALSE]
    graph <- igraph::graph_from_edgelist(cbind(as.character(from), as.character(to)), directed = FALSE)
    lengths <- as.numeric(sf::st_length(geometry))
    igraph::E(graph)$weight <- lengths
    terminal <- as.integer(names(igraph::degree(graph))[igraph::degree(graph) == 1L])
    if (!length(terminal)) { audit$direction_status[i] <- "unresolved_reference_has_no_terminal"; next }
    terminal_geometry <- sf::st_sfc(lapply(terminal, function(j) sf::st_point(nxy[j, ])), crs = crs)
    roots <- integer(); root_method <- NA_character_; root_distance <- NA_real_
    receiving <- if ("TributaryToName" %in% names(barriers)) as.character(barriers$TributaryToName[i]) else NA_character_
    receiving_rows <- if (!is.na(receiving)) which(stream_identity_names_match(reference[[name_field]], rep(receiving, nrow(reference))) &
      !is.na(reference$LLID) & as.character(reference$LLID) != id) else integer()
    if (length(receiving_rows)) {
      distance <- as.numeric(sf::st_distance(terminal_geometry, sf::st_union(sf::st_geometry(reference)[receiving_rows])))
      candidates <- which(distance <= confluence_tolerance)
      if (length(candidates) == 1L) {
        roots <- terminal[candidates]; root_method <- "unique_named_receiving_stream_confluence"
        root_distance <- distance[candidates]
      } else if (length(candidates) > 1L) {
        audit$direction_status[i] <- "unresolved_multiple_receiving_stream_confluences"; next
      }
    }
    if (!length(roots) && !is.null(marine_geometry) && !is.na(receiving) &&
        grepl("(bay|sound|ocean|strait|sea)$", trimws(receiving), ignore.case = TRUE)) {
      distance <- as.numeric(sf::st_distance(terminal_geometry, sf::st_union(sf::st_geometry(marine_geometry))))
      candidates <- which(distance <= marine_tolerance)
      if (length(candidates) == 1L) {
        roots <- terminal[candidates]; root_method <- "unique_marine_contact_and_named_marine_receiver"
        root_distance <- distance[candidates]
      } else if (length(candidates) > 1L) {
        audit$direction_status[i] <- "unresolved_multiple_marine_contacts"; next
      }
    }
    if (!length(roots)) {
      mouth <- sf::st_transform(sf::st_sfc(sf::st_point(c(-as.numeric(substr(id, 1L, 7L)) / 10000,
        as.numeric(substr(id, 8L, 13L)) / 10000)), crs = 4326), crs)
      distance <- as.numeric(sf::st_distance(terminal_geometry, mouth))
      candidates <- which(distance <= mouth_tolerance)
      if (length(candidates) != 1L) {
        audit$direction_status[i] <- "unresolved_no_unique_validated_reference_mouth"
        audit$root_distance_m[i] <- min(distance); next
      }
      roots <- terminal[candidates]; root_method <- "unique_reference_terminal_corroborates_llid_mouth"
      root_distance <- distance[candidates]
    }
    audit$root_validation_method[i] <- root_method
    audit$root_distance_m[i] <- root_distance
    component <- igraph::components(graph)$membership
    root_component <- unname(component[as.character(roots)])
    own_edges <- which(unname(component[as.character(from)]) == root_component)
    own_nodes <- unique(c(from[own_edges], to[own_edges]))
    cycle_rank <- length(own_edges) - length(own_nodes) + 1L
    audit$root_cycle_rank[i] <- cycle_rank
    if (cycle_rank != 0L) { audit$direction_status[i] <- "unresolved_reference_has_alternative_paths"; next }
    distances <- drop(igraph::distances(graph, v = as.character(roots), weights = igraph::E(graph)$weight))
    pxy <- unclass(sf::st_geometry(stream_edges)[[edge]])[, 1:2, drop = FALSE]
    ptotal <- sum(sqrt(rowSums(diff(pxy)^2)))
    at <- network_project_point(pxy, sf::st_coordinates(barriers[i, ])[1L, 1:2])
    span <- min(local_anchor_distance, at$measure * .9, (ptotal - at$measure) * .9)
    if (span < 40) { audit$direction_status[i] <- "unresolved_selected_edge_too_short_for_local_direction"; next }
    measures <- seq(at$measure - span, at$measure + span, length.out = 7L)
    points <- lapply(measures, function(m) point_at(pxy, m))
    projected <- lapply(points, function(point) {
      alternatives <- lapply(xy, network_project_point, point = point)
      j <- which.min(vapply(alternatives, `[[`, numeric(1), "distance"))
      c(alternatives[[j]], list(edge = j))
    })
    local_distance <- vapply(projected, `[[`, numeric(1), "distance")
    reference_rows <- vapply(projected, `[[`, integer(1), "edge")
    audit$local_reference_max_distance_m[i] <- max(local_distance)
    if (any(local_distance > reference_tolerance) || any(!reference_rows %in% own_edges)) {
      audit$direction_status[i] <- "unresolved_local_reference_course_or_component"; next
    }
    heading <- vapply(seq_along(projected), function(k) {
      pvector <- point_at(pxy, min(ptotal, measures[k] + 5)) - point_at(pxy, max(0, measures[k] - 5))
      row <- reference_rows[k]; m <- projected[[k]]$measure
      rvector <- point_at(xy[[row]], min(lengths[row], m + 5)) - point_at(xy[[row]], max(0, m - 5))
      denom <- sqrt(sum(pvector^2) * sum(rvector^2))
      if (denom == 0) return(180)
      acos(min(1, abs(sum(pvector * rvector)) / denom)) * 180 / pi
    }, numeric(1))
    audit$local_reference_max_heading_degrees[i] <- max(heading)
    if (any(heading > 35)) { audit$direction_status[i] <- "unresolved_local_reference_heading"; next }
    root_distance_at <- vapply(seq_along(projected), function(k) {
      row <- reference_rows[k]; measure <- projected[[k]]$measure
      min(distances[as.character(from[row])] + measure,
        distances[as.character(to[row])] + lengths[row] - measure)
    }, numeric(1))
    delta <- diff(root_distance_at); step <- diff(measures)
    if (!all(is.finite(root_distance_at)) || !(all(delta > .5 * step) || all(delta < -.5 * step)) ||
        any(abs(delta) > 1.5 * step)) {
      audit$direction_status[i] <- "unresolved_nonmonotonic_reference_direction"; next
    }
    audit$minus_root_distance_m[i] <- root_distance_at[1L]
    audit$plus_root_distance_m[i] <- tail(root_distance_at, 1L)
    downstream <- if (root_distance_at[1L] < tail(root_distance_at, 1L)) 1L else length(points)
    anchor <- template
    anchor$anchor_id <- paste0("named_reference_downstream_", barriers$review_record_key[i])
    anchor$review_record_key <- as.character(barriers$review_record_key[i])
    anchor$graph_edge_id <- barriers$graph_edge_id[i]
    anchor$projection_rule <- "explicit_selected_physical_edge"
    anchor$max_snap_distance_m <- 1e-5
    anchor$direction_evidence_valid <- TRUE
    anchor$anchor_reviewed <- isTRUE(anchor_reviewed)
    anchor$expected_stream_name <- if ("matched_stream_name" %in% names(barriers)) as.character(barriers$matched_stream_name[i]) else NA_character_
    anchor$expected_llid <- id
    anchor$coordinate_accuracy_note <- "Exact point on the selected physical edge; direction validated against independent named-reference topology"
    anchor$direction_basis <- paste("Unique rooted named-reference tree;", root_method,
      "; seven local projections within25m and35degrees, with strictly monotonic reference distance to downstream root.")
    anchor$coordinate_source_url <- "https://geodataservices.wdfw.wa.gov/arcgis/rest/services/FP_Projects/NHDwithLLID/MapServer/4"
    anchor$direction_source_url <- paste(anchor$coordinate_source_url,
      "https://dnr.wa.gov/sites/default/files/2025-03/fp_form_fpars_wt_data_dic.pdf", sep = ";")
    if (root_method == "unique_marine_contact_and_named_marine_receiver") {
      marine_source <- attr(marine_geometry, "source_url", exact = TRUE)
      if (!is.null(marine_source)) anchor$direction_source_url <- paste(anchor$direction_source_url,
        as.character(marine_source), sep = ";")
    }
    anchor$root_validation_method <- root_method
    anchor$reference_root_distance_m <- root_distance
    anchor$reference_root_cycle_rank <- cycle_rank
    anchor$reference_minus_distance_m <- root_distance_at[1L]
    anchor$reference_plus_distance_m <- tail(root_distance_at, 1L)
    anchor$local_reference_max_distance_m <- max(local_distance)
    anchor$local_reference_max_heading_degrees <- max(heading)
    sf::st_geometry(anchor) <- sf::st_sfc(sf::st_point(points[[downstream]]), crs = crs)
    anchors[[length(anchors) + 1L]] <- anchor
    audit$direction_status[i] <- "validated_named_reference_downstream_anchor"
  }
  list(anchors = if (length(anchors)) do.call(rbind, anchors) else template[FALSE, ], audit = audit,
    settings = list(confluence_tolerance_m = confluence_tolerance, mouth_tolerance_m = mouth_tolerance,
      marine_tolerance_m = marine_tolerance, local_anchor_distance_m = local_anchor_distance,
      reference_tolerance_m = reference_tolerance, reference_max_heading_degrees = 35,
      reference_tree_required = TRUE, vertex_order_used_as_flow = FALSE))
}

cut_reviewed_barriers <- function(network, barriers, stream_edges,
    downstream_anchors = gorge_downstream_anchor(sf::st_crs(network)),
    node_tolerance = 0.25, support_tolerance = 1e-5,
    barrier_snap_distance = 50, junction_distance = 25) {
  for (package in c("sf", "igraph")) if (!requireNamespace(package, quietly = TRUE))
    stop("Install the ", package, " package")
  for (fun in c("network_project_point", "network_slice_line", "network_endpoint_groups",
      "classify_network_sources")) if (!exists(fun, mode = "function"))
    stop("Source R/network.R and R/source_classification.R first (missing ", fun, ")")
  if (!inherits(network, "sf") || !nrow(network) || is.na(sf::st_crs(network)) ||
      isTRUE(sf::st_is_longlat(network))) stop("network must be nonempty projected sf linework")
  crs <- sf::st_crs(network)
  if (!crs$units_gdal %in% c("metre", "meter", "m")) stop("Network CRS must use meters")
  if (any(!is.finite(c(node_tolerance, support_tolerance, barrier_snap_distance, junction_distance))) ||
      any(c(node_tolerance, support_tolerance, barrier_snap_distance, junction_distance) < 0))
    stop("Invalid distance settings")
  if (!inherits(barriers, "sf") || !inherits(stream_edges, "sf")) stop("Barriers and stream_edges must be sf")
  required <- c("graph_edge_id", "channel_chain_id", "from_degree", "to_degree")
  if (!all(required %in% names(stream_edges)) || anyDuplicated(stream_edges$graph_edge_id))
    stop("stream_edges must retain unique strict-review graph edge IDs and degree fields")
  if (!"review_record_key" %in% names(barriers) || anyDuplicated(barriers$review_record_key))
    stop("Barriers need unique review_record_key values")
  if (!all(c("source_class", "measure_habitat", "bridge_id") %in% names(network)))
    stop("Network needs prediction/SWIFD/alignment source attribution")
  barriers <- sf::st_transform(barriers, crs)
  stream_edges <- sf::st_transform(stream_edges, crs)
  downstream_anchors <- sf::st_transform(downstream_anchors, crs)
  if (nrow(downstream_anchors) && !all(c("anchor_id", "review_record_key", "direction_evidence_valid",
      "anchor_reviewed", "max_snap_distance_m", "graph_edge_id") %in% names(downstream_anchors)))
    stop("Downstream anchors need explicit review, target, tolerance and direction evidence")
  flag <- function(name) if (name %in% names(barriers)) barriers[[name]] %in% TRUE else rep(FALSE, nrow(barriers))
  # Eligibility is recomputed rather than trusting a display/candidate flag.
  requested <- flag("explicit_total_passage") | flag("override_future_cut")
  identity <- flag("snap_accepted") & flag("stream_identity_confirmed") & flag("identity_valid_for_cut")
  identity <- identity & !flag("junction_ambiguous")
  if ("matching_channel_count" %in% names(barriers)) identity <- identity &
    !is.na(barriers$matching_channel_count) & barriers$matching_channel_count == 1L
  eligible <- requested & identity
  if ("snap_distance_m" %in% names(barriers)) eligible <- eligible &
    is.finite(barriers$snap_distance_m) & barriers$snap_distance_m <= barrier_snap_distance
  else eligible[] <- FALSE

  # Node the complete augmented network, retaining source attribution numerically
  # before any cuts. Exact line overlays lose length on floating split segments.
  geometry <- suppressWarnings(sf::st_cast(sf::st_union(sf::st_geometry(network)), "LINESTRING"))
  geometry <- geometry[!sf::st_is_empty(geometry) & as.numeric(sf::st_length(geometry)) > 1e-8]
  source <- classify_network_sources(geometry, network, tolerance = support_tolerance)
  pieces <- sf::st_sf(source, geometry = geometry)
  xy <- lapply(geometry, function(x) unclass(x)[, 1:2, drop = FALSE])
  lengths <- as.numeric(sf::st_length(geometry))
  midpoint <- sf::st_sfc(lapply(seq_along(xy), function(i) sf::st_point(
    network_slice_line(xy[[i]], lengths[i] / 2, lengths[i])[1L, ])), crs = crs)
  physical <- which(pieces$measure_habitat)
  edge_row <- rep(NA_integer_, nrow(pieces))
  if (length(physical)) {
    edge_row[physical] <- sf::st_nearest_feature(midpoint[physical], stream_edges)
    distance <- as.numeric(sf::st_distance(midpoint[physical],
      sf::st_geometry(stream_edges)[edge_row[physical]], by_element = TRUE))
    if (any(distance > support_tolerance)) stop("Augmented physical network differs from strict-review edge support")
  }
  pieces$graph_edge_id <- stream_edges$graph_edge_id[edge_row]
  cuts <- lapply(xy, function(z) list(measure = c(0, sum(sqrt(rowSums(diff(z)^2))))))
  attach <- function(point, target_id) {
    rows <- which(pieces$measure_habitat & !is.na(pieces$graph_edge_id) & pieces$graph_edge_id == target_id)
    if (!length(rows)) return(NULL)
    projected <- lapply(rows, function(j) network_project_point(xy[[j]], point))
    distances <- vapply(projected, `[[`, numeric(1), "distance")
    j <- which.min(distances)
    if (distances[j] > support_tolerance) return(NULL)
    row <- rows[j]
    cuts[[row]]$measure <<- c(cuts[[row]]$measure, projected[[j]]$measure)
    list(row = row, measure = projected[[j]]$measure, xy = projected[[j]]$xy)
  }
  audit <- sf::st_drop_geometry(barriers)
  audit$cut_requested <- requested
  audit$cut_eligible <- eligible
  audit$removed_metrics_are_independent_barrier_effect <- rep(TRUE, nrow(audit))
  audit$applied <- audit$truncation_applied <- FALSE
  audit$cut_status <- ifelse(!requested, "not_requested_no_total_evidence", "rejected_stream_identity_or_snap")
  audit$downstream_anchor_id <- audit$downstream_direction_basis <- audit$downstream_direction_source_url <- NA_character_
  audit$anchor_snap_distance_m <- rep(NA_real_, nrow(audit))
  audit$barrier_graph_component <- rep(NA_integer_, nrow(audit))
  audit$affected_component_pieces <- audit$removed_pieces <- rep(0L, nrow(audit))
  for (field in c("removed_model_m", "removed_swifd_m", "removed_alignment_m", "retained_affected_m"))
    audit[[field]] <- rep(0, nrow(audit))
  barrier_attachment <- anchor_attachment <- vector("list", nrow(barriers))
  anchor_output <- downstream_anchors
  anchor_output$used <- rep(FALSE, nrow(anchor_output))
  anchor_output$snap_distance_m <- rep(NA_real_, nrow(anchor_output))
  anchor_output$target_graph_edge_id <- rep(NA_integer_, nrow(anchor_output))
  anchor_geometry <- sf::st_geometry(anchor_output)
  for (i in which(eligible)) {
    edge <- match(barriers$graph_edge_id[i], stream_edges$graph_edge_id)
    if (is.na(edge)) { audit$cut_status[i] <- "rejected_selected_edge_missing"; next }
    exy <- unclass(sf::st_geometry(stream_edges)[[edge]])[, 1:2, drop = FALSE]
    point <- sf::st_coordinates(barriers[i, ])[1L, 1:2]
    projection <- network_project_point(exy, point)
    if (projection$distance > support_tolerance) {
      audit$cut_status[i] <- "rejected_not_on_selected_edge"; next
    }
    edge_length <- sum(sqrt(rowSums(diff(exy)^2)))
    near_from <- projection$measure <= junction_distance && stream_edges$from_degree[edge] >= 3L
    near_to <- edge_length - projection$measure <= junction_distance && stream_edges$to_degree[edge] >= 3L
    if (near_from || near_to) { audit$cut_status[i] <- "rejected_junction_proximity"; next }
    barrier_attachment[[i]] <- attach(projection$xy, barriers$graph_edge_id[i])
    if (is.null(barrier_attachment[[i]])) { audit$cut_status[i] <- "rejected_selected_edge_attachment_missing"; next }
    audit$cut_status[i] <- "unresolved_downstream_anchor_missing"
    anchors <- which(downstream_anchors$review_record_key == barriers$review_record_key[i] &
      downstream_anchors$direction_evidence_valid %in% TRUE & downstream_anchors$anchor_reviewed %in% TRUE)
    if (length(anchors) != 1L) next
    a <- anchors[1L]
    target <- downstream_anchors$graph_edge_id[a]
    if (is.na(target) && "projection_rule" %in% names(downstream_anchors) &&
        identical(as.character(downstream_anchors$projection_rule[a]), "same_selected_barrier_graph_edge"))
      target <- barriers$graph_edge_id[i]
    target_row <- match(target, stream_edges$graph_edge_id)
    if (is.na(target_row)) { audit$cut_status[i] <- "unresolved_anchor_target_edge_missing"; next }
    if ("expected_llid" %in% names(downstream_anchors) && "matched_llid" %in% names(barriers) &&
        !is.na(downstream_anchors$expected_llid[a]) && !is.na(barriers$matched_llid[i]) &&
        downstream_anchors$expected_llid[a] != barriers$matched_llid[i]) {
      audit$cut_status[i] <- "unresolved_anchor_stream_reference_conflict"; next
    }
    axy <- unclass(sf::st_geometry(stream_edges)[[target_row]])[, 1:2, drop = FALSE]
    aproj <- network_project_point(axy, sf::st_coordinates(downstream_anchors[a, ])[1L, 1:2])
    audit$anchor_snap_distance_m[i] <- aproj$distance
    audit$downstream_anchor_id[i] <- as.character(downstream_anchors$anchor_id[a])
    for (pair in list(c("direction_basis", "downstream_direction_basis"),
        c("direction_source_url", "downstream_direction_source_url")))
      if (pair[1L] %in% names(downstream_anchors)) audit[[pair[2L]]][i] <- as.character(downstream_anchors[[pair[1L]]][a])
    if (!is.finite(downstream_anchors$max_snap_distance_m[a]) ||
        aproj$distance > downstream_anchors$max_snap_distance_m[a]) {
      audit$cut_status[i] <- "unresolved_anchor_beyond_reviewed_tolerance"; next
    }
    if (sqrt(sum((aproj$xy - projection$xy)^2)) <= junction_distance) {
      audit$cut_status[i] <- "unresolved_anchor_at_barrier"; next
    }
    anchor_attachment[[i]] <- attach(aproj$xy, target)
    if (is.null(anchor_attachment[[i]])) { audit$cut_status[i] <- "unresolved_anchor_attachment_missing"; next }
    anchor_output$used[a] <- TRUE
    anchor_output$snap_distance_m[a] <- aproj$distance
    anchor_output$target_graph_edge_id[a] <- target
    anchor_geometry[a] <- sf::st_sfc(sf::st_point(aproj$xy), crs = crs)
    audit$cut_status[i] <- "pending_component_test"
  }
  sf::st_geometry(anchor_output) <- anchor_geometry
  # Split on the original coordinates without re-unioning; injected target
  # vertices stay exact and source/native model lengths remain preserved.
  split_geometry <- list(); split_rows <- integer()
  for (j in seq_along(xy)) {
    at <- sort(unique(pmax(0, pmin(lengths[j], cuts[[j]]$measure))))
    for (k in seq_len(length(at) - 1L)) if (at[k + 1L] - at[k] > 1e-8) {
      split_geometry[[length(split_geometry) + 1L]] <- sf::st_linestring(network_slice_line(xy[[j]], at[k], at[k + 1L]))
      split_rows <- c(split_rows, j)
    }
  }
  lines <- sf::st_sf(sf::st_drop_geometry(pieces)[split_rows, , drop = FALSE],
    geometry = sf::st_sfc(split_geometry, crs = crs))
  lines$piece_id <- seq_len(nrow(lines))
  lines$length_m <- as.numeric(sf::st_length(lines))
  endpoints <- do.call(rbind, lapply(split_geometry, function(z) unclass(z)[c(1L, nrow(z)), 1:2, drop = FALSE]))
  groups <- network_endpoint_groups(endpoints, node_tolerance)
  from <- groups[seq.int(1L, length(groups), 2L)]
  to <- groups[seq.int(2L, length(groups), 2L)]
  node_xy <- endpoints[!duplicated(groups), , drop = FALSE]
  graph <- function(left, right) igraph::graph_from_edgelist(cbind(as.character(left), as.character(right)), directed = FALSE)
  component <- igraph::components(graph(from, to))$membership
  member <- function(m, nodes) unname(m[as.character(nodes)])
  baseline <- member(component, from)
  lines$original_component <- baseline
  removed_by <- unresolved_by <- downstream_by <- lapply(seq_len(nrow(lines)), function(i) character())
  point_node <- function(point) {
    d <- rowSums(sweep(node_xy, 2L, point)^2)
    if (min(d) > max(support_tolerance, node_tolerance)^2) return(NA_integer_)
    which.min(d)
  }
  append_key <- function(storage, rows, key) {
    for (j in rows) storage[[j]] <- unique(c(storage[[j]], key))
    storage
  }
  for (i in which(eligible)) {
    attachment <- barrier_attachment[[i]]
    if (is.null(attachment)) next
    node <- point_node(attachment$xy)
    if (is.na(node)) { audit$cut_status[i] <- "unresolved_barrier_graph_node_missing"; next }
    origin <- member(component, node)
    affected <- which(baseline == origin)
    audit$barrier_graph_component[i] <- origin
    audit$affected_component_pieces[i] <- length(affected)
    key <- as.character(barriers$review_record_key[i])
    if (is.null(anchor_attachment[[i]])) {
      unresolved_by <- append_key(unresolved_by, affected, key); next
    }
    anchor_node <- point_node(anchor_attachment[[i]]$xy)
    if (is.na(anchor_node) || member(component, anchor_node) != origin) {
      audit$cut_status[i] <- "unresolved_anchor_in_different_component"
      unresolved_by <- append_key(unresolved_by, affected, key); next
    }
    incidence <- c(which(from == node), which(to == node))
    if (length(incidence) != 2L || any(from[incidence] == to[incidence])) {
      audit$cut_status[i] <- "rejected_augmented_graph_junction_or_terminal"
      unresolved_by <- append_key(unresolved_by, affected, key); next
    }
    left <- from; right <- to
    # Duplicate the cut vertex per incident edge. Both line halves are retained
    # geometrically; no graph traversal can pass across the barrier.
    for (j in seq_along(incidence)) {
      row <- incidence[j]
      if (left[row] == node) left[row] <- nrow(node_xy) + j
      if (right[row] == node) right[row] <- nrow(node_xy) + j
    }
    after <- igraph::components(graph(left, right))$membership
    anchor_component <- member(after, anchor_node)
    sides <- member(after, c(nrow(node_xy) + 1L, nrow(node_xy) + 2L))
    if (length(unique(sides)) != 2L || !anchor_component %in% sides) {
      audit$cut_status[i] <- "unresolved_alternative_path_around_barrier"
      unresolved_by <- append_key(unresolved_by, affected, key); next
    }
    upstream <- affected[member(after, left[affected]) != anchor_component]
    downstream <- setdiff(affected, upstream)
    if (!length(upstream) || !any(lines$measure_habitat[upstream])) {
      audit$cut_status[i] <- "unresolved_no_proven_upstream_stream_pieces"
      unresolved_by <- append_key(unresolved_by, affected, key); next
    }
    removed_by <- append_key(removed_by, upstream, key)
    downstream_by <- append_key(downstream_by, downstream, key)
    audit$applied[i] <- audit$truncation_applied[i] <- TRUE
    audit$cut_status[i] <- "applied_proven_component_local_upstream"
    audit$removed_pieces[i] <- length(upstream)
    for (pair in list(c("prediction", "removed_model_m"), c("swifd_connector", "removed_swifd_m"),
        c("alignment_link", "removed_alignment_m"))) audit[[pair[2L]]][i] <-
          sum(lines$length_m[upstream][lines$source_class[upstream] == pair[1L]])
    audit$retained_affected_m[i] <- sum(lines$length_m[downstream])
  }
  lines$removed <- lengths(removed_by) > 0L
  lines$retained <- !lines$removed
  lines$unresolved_direction <- lengths(unresolved_by) > 0L & lines$retained
  lines$cut_status <- ifelse(lines$removed, "removed_upstream", ifelse(lines$unresolved_direction,
    "retained_unresolved", ifelse(lengths(downstream_by) > 0L, "retained_downstream", "retained_unaffected")))
  collapse <- function(x) vapply(x, paste, collapse = ";", character(1))
  lines$removed_by_barrier <- collapse(removed_by)
  lines$unresolved_by_barrier <- collapse(unresolved_by)
  lines$downstream_of_barrier <- collapse(downstream_by)
  original_length <- tapply(as.numeric(sf::st_length(network)), network$source_class, sum)
  split_length <- tapply(lines$length_m, lines$source_class, sum)
  error <- max(abs(split_length[names(original_length)] - original_length))
  if (!is.finite(error) || error > max(1e-5, sum(original_length) * 1e-10))
    stop("Source lengths changed before retained/removed partition: ", error, "m")
  list(network = lines, retained = lines[lines$retained, ], removed = lines[lines$removed, ],
    unresolved = lines[lines$unresolved_direction, ], barrier_audit = audit,
    downstream_anchors = anchor_output,
    summary = list(input_pieces = nrow(network), partition_pieces = nrow(lines),
      requested_barriers = sum(requested), eligible_barriers = sum(eligible), applied_barriers = sum(audit$applied),
      removed_physical_m = sum(lines$length_m[lines$removed & lines$measure_habitat]),
      retained_physical_m = sum(lines$length_m[lines$retained & lines$measure_habitat]),
      unresolved_retained_physical_m = sum(lines$length_m[lines$unresolved_direction & lines$measure_habitat]),
      unrelated_components_removed = 0L, source_partition_error_m = error),
    settings = list(node_tolerance_m = node_tolerance, support_tolerance_m = support_tolerance,
      barrier_snap_distance_m = barrier_snap_distance, junction_distance_m = junction_distance,
      direction_method = "explicit_reviewed_downstream_anchor_component_local_severance",
      maritime_connectivity_filter = FALSE,
      disconnected_unproven_geometry = "retained; no upstream direction inferred"))
}
