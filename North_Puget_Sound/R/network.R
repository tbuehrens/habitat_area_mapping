# Topological natural-barrier truncation for a modeled stream network.
#
# The prediction service has no flow-direction attributes. Downstream access is
# therefore defined by supplied, independently identified marine outlets. A line
# is retained only when connected to one of those outlets after barrier cuts.
# Components with no outlet are unresolved, rather than counted as accessible.

network_project_point <- function(xy, point) {
  delta <- xy[-1L, , drop = FALSE] - xy[-nrow(xy), , drop = FALSE]
  squared <- rowSums(delta^2)
  fraction <- pmax(0, pmin(1, rowSums(
    (matrix(point, nrow(delta), 2L, byrow = TRUE) -
       xy[-nrow(xy), , drop = FALSE]) * delta
  ) / ifelse(squared == 0, 1, squared)))
  projected <- xy[-nrow(xy), , drop = FALSE] + delta * fraction
  distance_squared <- rowSums(sweep(projected, 2L, point)^2)
  segment <- which.min(distance_squared)
  lengths <- sqrt(squared)
  list(
    distance = sqrt(distance_squared[segment]),
    measure = sum(lengths[seq_len(segment - 1L)]) +
      fraction[segment] * lengths[segment],
    xy = unname(projected[segment, ])
  )
}

network_slice_line <- function(xy, start, end) {
  cumulative <- c(0, cumsum(sqrt(rowSums(
    (xy[-1L, , drop = FALSE] - xy[-nrow(xy), , drop = FALSE])^2
  ))))
  at_distance <- function(distance) {
    if (distance <= 0) return(xy[1L, ])
    if (distance >= tail(cumulative, 1L)) return(xy[nrow(xy), ])
    segment <- max(which(cumulative < distance))
    fraction <- (distance - cumulative[segment]) /
      (cumulative[segment + 1L] - cumulative[segment])
    xy[segment, ] + fraction * (xy[segment + 1L, ] - xy[segment, ])
  }
  answer <- rbind(
    at_distance(start),
    xy[cumulative > start & cumulative < end, , drop = FALSE],
    at_distance(end)
  )
  answer[c(TRUE, rowSums(abs(answer[-1L, , drop = FALSE] -
    answer[-nrow(answer), , drop = FALSE])) > 0), , drop = FALSE]
}

# Merge endpoint locations by Euclidean distance, using a spatial hash rather
# than an all-pairs distance matrix. Exact intersections were noded by st_union.
network_endpoint_groups <- function(xy, tolerance) {
  exact_key <- paste(format(xy[, 1L], digits = 16L, trim = TRUE),
                     format(xy[, 2L], digits = 16L, trim = TRUE), sep = ":")
  first <- !duplicated(exact_key)
  unique_xy <- xy[first, , drop = FALSE]
  unique_key <- exact_key[first]
  exact_group <- match(exact_key, unique_key)
  if (tolerance == 0) return(exact_group)
  parent <- seq_len(nrow(unique_xy))
  find <- function(i) {
    while (parent[i] != i) i <- parent[i]
    i
  }
  cells <- new.env(hash = TRUE, parent = emptyenv())
  offsets <- as.matrix(expand.grid(-1:1, -1:1))
  for (i in seq_len(nrow(unique_xy))) {
    cell <- floor(unique_xy[i, ] / tolerance)
    for (j in seq_len(nrow(offsets))) {
      key <- paste(cell + offsets[j, ], collapse = ":")
      candidates <- cells[[key]]
      if (length(candidates)) {
        close <- candidates[rowSums(sweep(
          unique_xy[candidates, , drop = FALSE], 2L, unique_xy[i, ]
        )^2) <= tolerance^2]
        for (candidate in close) {
          left <- find(i)
          right <- find(candidate)
          if (left != right) parent[max(left, right)] <- min(left, right)
        }
      }
    }
    key <- paste(cell, collapse = ":")
    cells[[key]] <- c(cells[[key]], i)
  }
  representative <- vapply(seq_len(nrow(unique_xy)), find, integer(1L))
  match(representative[exact_group], unique(representative))
}

truncate_network_at_barriers <- function(
    lines, barriers, root_geometry,
    snap_distance = 50, endpoint_tolerance = 0.25,
    root_distance = 100, ambiguity_distance = 1,
    stream_reference = NULL, stream_reference_distance = 100) {
  for (package in c("sf", "igraph")) {
    if (!requireNamespace(package, quietly = TRUE)) {
      stop("Install the ", package, " package before tracing the network.")
    }
  }
  settings <- c(snap_distance, endpoint_tolerance, root_distance,
                ambiguity_distance)
  if (any(!is.finite(settings)) || any(settings < 0)) {
    stop("Network distances must be finite, nonnegative meters.")
  }
  if (!inherits(lines, c("sf", "sfc")) || length(sf::st_geometry(lines)) == 0) {
    stop("lines must contain stream geometries.")
  }
  crs <- sf::st_crs(lines)
  if (is.na(crs) || isTRUE(sf::st_is_longlat(lines)) ||
      !crs$units_gdal %in% c("metre", "meter", "m")) {
    stop("Use a projected CRS with meter units, such as EPSG:26910.")
  }
  if (!inherits(barriers, "sf") ||
      any(sf::st_geometry_type(barriers) != "POINT")) {
    stop("barriers must be an sf object containing POINT geometries.")
  }
  if (is.na(sf::st_crs(barriers))) stop("barriers must have a CRS.")
  barriers <- sf::st_transform(barriers, crs)
  if (any(sf::st_is_empty(barriers))) stop("Barrier geometry is empty.")
  if (missing(root_geometry) || is.null(root_geometry) ||
      length(sf::st_geometry(root_geometry)) == 0) {
    stop("Supply independently identified marine outlets or marine geometry.")
  }
  root_geometry <- sf::st_transform(sf::st_geometry(root_geometry), crs)
  if (any(sf::st_is_empty(root_geometry))) stop("Root geometry is empty.")
  if (any(!sf::st_geometry_type(lines) %in%
          c("LINESTRING", "MULTILINESTRING"))) {
    stop("lines must contain only LINESTRING or MULTILINESTRING geometries.")
  }
  geometry <- sf::st_zm(sf::st_geometry(lines), drop = TRUE, what = "ZM")
  geometry <- geometry[!sf::st_is_empty(geometry)]
  # Unary union both removes coincident prediction records and nodes crossings.
  noded <- suppressWarnings(sf::st_cast(sf::st_union(geometry), "LINESTRING"))
  lengths <- as.numeric(sf::st_length(noded))
  noded <- noded[lengths > 1e-8]
  if (!length(noded)) stop("No nonzero stream linework remains.")
  lengths <- as.numeric(sf::st_length(noded))
  # Assign provenance before barrier interpolation changes edge coordinates.
  # Exact line overlays after splitting can discard floating-point-collinear
  # geometry; a numerical-tolerance midpoint match preserves all source lengths.
  if (inherits(lines, "sf") && "source_class" %in% names(lines)) {
    source_info <- classify_network_sources(noded, lines)
  } else {
    source_info <- data.frame(source_class = rep("prediction", length(noded)),
      measure_habitat = rep(TRUE, length(noded)), bridge_id = rep(NA_integer_, length(noded)))
  }
  coordinates <- lapply(noded, function(x) unclass(x)[, 1:2, drop = FALSE])
  number_edges <- length(coordinates)
  endpoint_xy <- do.call(rbind, lapply(coordinates, function(x) {
    x[c(1L, nrow(x)), , drop = FALSE]
  }))
  group <- network_endpoint_groups(endpoint_xy, endpoint_tolerance)
  number_nodes <- max(group)
  node_xy <- cbind(
    as.numeric(rowsum(endpoint_xy[, 1L], group) / tabulate(group)),
    as.numeric(rowsum(endpoint_xy[, 2L], group) / tabulate(group))
  )
  from <- group[seq(1L, length(group), by = 2L)]
  to <- group[seq(2L, length(group), by = 2L)]
  vertices <- data.frame(name = as.character(seq_len(number_nodes)))
  graph <- igraph::graph_from_data_frame(
    data.frame(from = as.character(from), to = as.character(to)),
    directed = FALSE, vertices = vertices
  )
  nodes <- sf::st_as_sf(
    data.frame(node_id = seq_len(number_nodes), x = node_xy[, 1L],
               y = node_xy[, 2L]), coords = c("x", "y"), crs = crs
  )
  nodes$degree <- as.integer(igraph::degree(graph, loops = TRUE))
  nodes$component <- as.integer(igraph::components(graph)$membership)
  nearest_root <- sf::st_nearest_feature(nodes, root_geometry)
  nodes$root_distance_m <- as.numeric(sf::st_distance(
    nodes, root_geometry[nearest_root], by_element = TRUE
  ))
  root_types <- as.character(sf::st_geometry_type(root_geometry))
  if (all(root_types %in% c("POINT", "MULTIPOINT"))) {
    # Explicit reviewed mouth points identify their nearest graph node, rather
    # than every nearby node on both sides of a near-mouth barrier.
    mouth_points <- suppressWarnings(sf::st_cast(root_geometry, "POINT"))
    nearest_node <- sf::st_nearest_feature(mouth_points, nodes)
    mouth_distance <- as.numeric(sf::st_distance(
      mouth_points, nodes[nearest_node, ], by_element = TRUE
    ))
    nodes$is_root <- nodes$node_id %in%
      nearest_node[mouth_distance <= root_distance]
  } else {
    # Actual marine contacts can be internal vertices, while buffered contact
    # is permitted only for terminal endpoints. This prevents interior nodes
    # close to a mouth from becoming extra downstream access points.
    actual_contact <- lengths(sf::st_intersects(nodes, root_geometry)) > 0L
    nodes$is_root <- actual_contact |
      (nodes$degree == 1L & nodes$root_distance_m <= root_distance)
  }
  if (!any(nodes$is_root)) {
    stop("No network nodes match the supplied outlets within root_distance.")
  }
  rooted_components <- unique(nodes$component[nodes$is_root])
  nodes$baseline_connected <- nodes$component %in% rooted_components
  baseline <- nodes$baseline_connected[from]
  edge_components <- nodes$component[from]

  audit <- barriers
  audit$barrier_index <- seq_len(nrow(audit))
  audit$matched_edge <- rep(NA_integer_, nrow(audit))
  audit$snap_distance_m <- rep(NA_real_, nrow(audit))
  audit$snap_x <- rep(NA_real_, nrow(audit))
  audit$snap_y <- rep(NA_real_, nrow(audit))
  audit$measure_m <- rep(NA_real_, nrow(audit))
  audit$node_id <- rep(NA_integer_, nrow(audit))
  audit$applied <- rep(FALSE, nrow(audit))
  audit$match_status <- rep("unmatched", nrow(audit))
  audit$baseline_connected <- rep(FALSE, nrow(audit))
  edge_cuts <- vector("list", number_edges)
  node_cuts <- integer()
  if (nrow(barriers)) {
    edge_sf <- sf::st_sf(source_edge = seq_len(number_edges), geometry = noded)
    # Alignment links establish correspondence between two mapped supports;
    # they are not stream habitat and must not attract barrier snapping.
    physical_rows <- which(source_info$measure_habitat)
    if (!length(physical_rows)) stop("No physical habitat edges available for barrier matching")
    physical_edges <- edge_sf[physical_rows, ]
    nearest <- physical_rows[sf::st_nearest_feature(barriers, physical_edges)]
    # Build the spatial search once. Rebuilding it for every barrier is slow
    # for statewide synthetic stream geometry with many vertices.
    # An enclosing diamond supplies indexed candidates; exact projected
    # distances below determine eligibility, independent of buffer shape.
    search_regions <- sf::st_buffer(barriers,
      dist = (snap_distance + ambiguity_distance + 1e-8) * sqrt(2), nQuadSegs = 1)
    nearby_edges <- lapply(sf::st_intersects(search_regions, physical_edges),
      function(index) physical_rows[index])
    barrier_xy <- sf::st_coordinates(barriers)[, 1:2, drop = FALSE]
    projected_matches <- lapply(seq_len(nrow(barriers)), function(i) {
      network_project_point(coordinates[[nearest[i]]], barrier_xy[i, ])
    })
    if (!is.null(stream_reference)) {
      if (!exists("validate_barrier_stream_matches", mode = "function")) {
        stop("Source R/barrier_stream_identity.R before validating barrier stream assignments")
      }
      candidate_points <- sf::st_sfc(lapply(projected_matches, function(z) sf::st_point(z$xy)), crs = crs)
      identity <- validate_barrier_stream_matches(barriers, candidate_points, stream_reference,
        max_reference_distance = stream_reference_distance)
      for (field in names(identity)) audit[[field]] <- identity[[field]]
    } else {
      audit$stream_identity_status <- rep("not_checked", nrow(audit))
      audit$allow_cut <- rep(TRUE, nrow(audit))
    }
    for (i in seq_len(nrow(barriers))) {
      edge <- nearest[i]
      projected <- projected_matches[[i]]
      audit$matched_edge[i] <- edge
      audit$snap_distance_m[i] <- projected$distance
      audit$snap_x[i] <- projected$xy[1L]
      audit$snap_y[i] <- projected$xy[2L]
      audit$measure_m[i] <- projected$measure
      audit$baseline_connected[i] <- baseline[edge]
      if (projected$distance > snap_distance) next
      if (!audit$allow_cut[i]) {
        audit$match_status[i] <- "rejected_receiving_stream"
        next
      }
      # Identify competing matches close to the best projected distance.
      candidates <- nearby_edges[[i]]
      projections <- lapply(candidates, function(candidate) {
        network_project_point(coordinates[[candidate]], barrier_xy[i, ])
      })
      close <- vapply(projections, function(z) z$distance,
                      numeric(1)) <= projected$distance + ambiguity_distance + 1e-8
      candidates <- candidates[close]
      projections <- projections[close]
      endpoint_node <- function(candidate, position) {
        if (position <= endpoint_tolerance + 1e-8) return(from[candidate])
        if (lengths[candidate] - position <= endpoint_tolerance + 1e-8) {
          return(to[candidate])
        }
        NA_integer_
      }
      node <- endpoint_node(edge, projected$measure)
      if (!is.na(node)) audit$node_id[i] <- node
      candidate_nodes <- vapply(seq_along(candidates), function(j) {
        endpoint_node(candidates[j], projections[[j]]$measure)
      }, integer(1L))
      same_node <- length(candidates) > 0 && !is.na(node) &&
        all(!is.na(candidate_nodes) & candidate_nodes == node)
      if ((!is.na(node) && nodes$degree[node] > 2L) ||
          (length(candidates) > 1L && !same_node)) {
        audit$match_status[i] <- "ambiguous_requires_review"
        next
      }
      audit$applied[i] <- TRUE
      if (!is.na(node)) {
        node_cuts <- c(node_cuts, node)
        audit$match_status[i] <- "applied_at_node"
      } else {
        edge_cuts[[edge]] <- c(edge_cuts[[edge]], projected$measure)
        audit$match_status[i] <- "applied_on_reach"
      }
    }
  }
  node_cuts <- unique(node_cuts)
  piece_geometry <- list()
  piece_from <- piece_to <- character()
  piece_source <- integer()
  for (edge in seq_len(number_edges)) {
    cuts <- sort(unique(edge_cuts[[edge]]))
    limits <- c(0, cuts, lengths[edge])
    for (piece in seq_len(length(limits) - 1L)) {
      if (limits[piece + 1L] - limits[piece] < 1e-8) next
      index <- length(piece_geometry) + 1L
      piece_geometry[[index]] <- sf::st_linestring(network_slice_line(
        coordinates[[edge]], limits[piece], limits[piece + 1L]
      ))
      piece_source[index] <- edge
      piece_from[index] <- if (piece == 1L) {
        if (from[edge] %in% node_cuts) paste0("nodecut_", from[edge], "_", edge) else
          as.character(from[edge])
      } else paste0("cut_", edge, "_", piece - 1L, "_right")
      piece_to[index] <- if (piece == length(limits) - 1L) {
        if (to[edge] %in% node_cuts) paste0("nodecut_", to[edge], "_", edge) else
          as.character(to[edge])
      } else paste0("cut_", edge, "_", piece, "_left")
    }
  }
  after <- igraph::graph_from_data_frame(
    data.frame(from = piece_from, to = piece_to), directed = FALSE,
    vertices = data.frame(name = unique(c(vertices$name, piece_from, piece_to)))
  )
  after_component <- igraph::components(after)$membership
  root_names <- as.character(nodes$node_id[nodes$is_root])
  connected <- unique(after_component[root_names])
  retained <- unname(after_component[piece_from] %in% connected)
  baseline_piece <- baseline[piece_source]
  # An unassigned barrier is explicitly visible both in its audit and on the
  # affected component. It is never represented as a completed barrier cut.
  needs_review <- unique(edge_components[audit$matched_edge[
    audit$match_status == "ambiguous_requires_review"
  ]])
  output <- sf::st_sf(
    edge_piece = seq_along(piece_geometry), source_edge = piece_source,
    from_node = piece_from, to_node = piece_to,
    baseline_component = edge_components[piece_source],
    source_class = source_info$source_class[piece_source],
    measure_habitat = source_info$measure_habitat[piece_source],
    bridge_id = source_info$bridge_id[piece_source],
    baseline_connected = baseline_piece,
    retained = baseline_piece & retained,
    removed = baseline_piece & !retained,
    unresolved = !baseline_piece,
    review_required = edge_components[piece_source] %in% needs_review,
    geometry = sf::st_sfc(piece_geometry, crs = crs)
  )
  output$length_m <- as.numeric(sf::st_length(output))
  output$status <- ifelse(output$unresolved, "unresolved_connection",
    ifelse(output$retained, "retained", "removed_above_barrier"))
  summary <- data.frame(
    baseline_m = sum(output$length_m[output$baseline_connected]),
    retained_m = sum(output$length_m[output$retained]),
    removed_m = sum(output$length_m[output$removed]),
    unresolved_m = sum(output$length_m[output$unresolved]),
    barriers_total = nrow(audit), barriers_applied = sum(audit$applied),
    barriers_ambiguous = sum(audit$match_status == "ambiguous_requires_review"),
    barriers_unmatched = sum(audit$match_status == "unmatched"),
    barriers_rejected_receiving_stream = sum(audit$match_status == "rejected_receiving_stream")
  )
  list(lines = output, barrier_audit = audit,
       roots = nodes[nodes$is_root, ], nodes = nodes, summary = summary,
       settings = list(snap_distance_m = snap_distance,
                       endpoint_tolerance_m = endpoint_tolerance,
                       root_distance_m = root_distance,
                       ambiguity_distance_m = ambiguity_distance,
                       stream_identity_checked = !is.null(stream_reference),
                       stream_reference_distance_m = stream_reference_distance))
}
