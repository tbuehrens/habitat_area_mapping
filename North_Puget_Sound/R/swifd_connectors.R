# SWIFD supplies routes only for gaps between separate prediction components.
# The original prediction is retained; short alignment links are topology only.

bridge_graph <- function(geometry, tolerance = 0.25) {
  xy <- lapply(geometry, function(g) unclass(g)[, 1:2, drop = FALSE])
  ends <- do.call(rbind, lapply(xy, function(g) g[c(1L, nrow(g)), , drop = FALSE]))
  group <- network_endpoint_groups(ends, tolerance)
  first <- !duplicated(group)
  node_xy <- ends[first, , drop = FALSE]
  from <- group[seq(1L, length(group), by = 2L)]
  to <- group[seq(2L, length(group), by = 2L)]
  graph <- igraph::graph_from_edgelist(cbind(from, to), directed = FALSE)
  component <- as.integer(igraph::components(graph)$membership)
  list(geometry = geometry, xy = xy, node_xy = node_xy, from = from, to = to,
       graph = graph, component = component, degree = as.integer(igraph::degree(graph)),
       length = as.numeric(sf::st_length(geometry)))
}

bridge_node_lines <- function(x) {
  g <- suppressWarnings(sf::st_cast(sf::st_union(sf::st_geometry(x)), "LINESTRING"))
  g[as.numeric(sf::st_length(g)) > 1e-8]
}

bridge_projection <- function(xy, point) {
  z <- network_project_point(xy, point)
  total <- sum(sqrt(rowSums((xy[-1L, , drop = FALSE] - xy[-nrow(xy), , drop = FALSE])^2)))
  if (z$measure < 1e-7) { z$measure <- 0; z$xy <- xy[1L, ] }
  if (total - z$measure < 1e-7) { z$measure <- total; z$xy <- xy[nrow(xy), ] }
  z
}

# Insert attachment coordinates into their source lines explicitly. Using the
# same boundary coordinates for adjacent pieces prevents floating point gaps.
bridge_split_support <- function(geometry, cuts) {
  result <- vector("list", length(geometry) + nrow(cuts))
  result_count <- 0L
  totals <- as.numeric(sf::st_length(geometry))
  cut_rows <- split(seq_len(nrow(cuts)), factor(cuts$edge, levels = seq_along(geometry)))
  for (edge in seq_along(geometry)) {
    total <- totals[edge]
    rows <- cut_rows[[edge]]
    if (length(rows)) rows <- rows[cuts$measure[rows] > 1e-7 & cuts$measure[rows] < total - 1e-7]
    if (!length(rows)) {
      result_count <- result_count + 1L
      result[[result_count]] <- geometry[[edge]]
      next
    }
    xy <- unclass(geometry[[edge]])[, 1:2, drop = FALSE]
    cut <- cuts[rows, , drop = FALSE]
    cut <- cut[order(cut$measure), ]
    if (nrow(cut)) cut <- cut[c(TRUE, diff(cut$measure) > 1e-7), ]
    if (!nrow(cut)) {
      result_count <- result_count + 1L
      result[[result_count]] <- geometry[[edge]]
      next
    }
    limits <- c(0, cut$measure, total)
    boundary <- rbind(xy[1L, ], as.matrix(cut[, c("x", "y"), drop = FALSE]), xy[nrow(xy), ])
    for (piece in seq_len(length(limits) - 1L)) {
      if (limits[piece + 1L] - limits[piece] < 1e-8) next
      coordinates <- network_slice_line(xy, limits[piece], limits[piece + 1L])
      coordinates[1L, ] <- boundary[piece, ]
      coordinates[nrow(coordinates), ] <- boundary[piece + 1L, ]
      result_count <- result_count + 1L
      result[[result_count]] <- sf::st_linestring(coordinates)
    }
  }
  sf::st_sfc(result[seq_len(result_count)], crs = sf::st_crs(geometry))
}

bridge_union_find <- function(size) {
  parents <- seq_len(size)
  ranks <- integer(size)
  find <- function(i) {
    root <- i
    while (parents[root] != root) root <- parents[root]
    while (parents[i] != i) {
      next_node <- parents[i]; parents[i] <<- root; i <- next_node
    }
    root
  }
  join <- function(a, b) {
    a <- find(a); b <- find(b)
    if (a == b) return(FALSE)
    if (ranks[a] < ranks[b]) { temporary <- a; a <- b; b <- temporary }
    parents[b] <<- a
    if (ranks[a] == ranks[b]) ranks[a] <<- ranks[a] + 1L
    TRUE
  }
  list(find = find, join = join)
}

# Dijkstra from all eligible terminals in one current prediction component.
# Stop on the nearest attachment belonging to another current component.
bridge_nearest_route <- function(support, anchors, sources, target_at_node, membership) {
  number_nodes <- nrow(support$node_xy)
  distance <- rep(Inf, number_nodes)
  origin <- previous_node <- previous_edge <- rep(NA_integer_, number_nodes)
  heap_node <- integer(max(16L, length(support$from) * 3L + length(sources) + 1L))
  heap_distance <- numeric(length(heap_node))
  heap_size <- 0L
  push <- function(node, value) {
    heap_size <<- heap_size + 1L
    index <- heap_size
    while (index > 1L && heap_distance[index %/% 2L] > value) {
      heap_node[index] <<- heap_node[index %/% 2L]
      heap_distance[index] <<- heap_distance[index %/% 2L]
      index <- index %/% 2L
    }
    heap_node[index] <<- node; heap_distance[index] <<- value
  }
  pop <- function() {
    answer <- c(heap_node[1L], heap_distance[1L])
    node <- heap_node[heap_size]; value <- heap_distance[heap_size]
    heap_size <<- heap_size - 1L
    index <- 1L
    while (index * 2L <= heap_size) {
      child <- index * 2L
      if (child < heap_size && heap_distance[child + 1L] < heap_distance[child]) child <- child + 1L
      if (heap_distance[child] >= value) break
      heap_node[index] <<- heap_node[child]; heap_distance[index] <<- heap_distance[child]
      index <- child
    }
    if (heap_size) { heap_node[index] <<- node; heap_distance[index] <<- value }
    answer
  }
  for (source in sources) {
    node <- anchors$swifd_node[source]
    value <- anchors$snap_distance_m[source]
    if (value < distance[node]) {
      distance[node] <- value; origin[node] <- source
      push(node, value)
    }
  }
  cluster <- membership[anchors$model_component[sources[1L]]]
  best <- Inf; target <- end_node <- NA_integer_
  while (heap_size) {
    entry <- pop(); node <- as.integer(entry[1L]); value <- entry[2L]
    if (value > distance[node] + 1e-8) next
    if (value > best + 1e-8) break
    possible <- target_at_node[[node]]
    # A route consisting solely of two off-channel alignment links is not a
    # SWIFD stream path and could evade a barrier at their shared projection.
    if (length(possible) && !is.na(previous_edge[node])) {
      possible <- possible[membership[anchors$model_component[possible]] != cluster]
      if (length(possible)) {
        cost <- value + anchors$snap_distance_m[possible]
        nearest <- which.min(cost)
        if (cost[nearest] < best) {
          best <- cost[nearest]; target <- possible[nearest]; end_node <- node
        }
      }
    }
    for (edge in support$adjacency[[node]]) {
      other <- if (support$from[edge] == node) support$to[edge] else support$from[edge]
      next_value <- value + support$length[edge]
      if (next_value < distance[other] - 1e-8) {
        distance[other] <- next_value; origin[other] <- origin[node]
        previous_node[other] <- node; previous_edge[other] <- edge
        push(other, next_value)
      }
    }
  }
  if (is.na(target)) return(NULL)
  source <- origin[end_node]
  nodes <- end_node; edges <- integer()
  node <- end_node
  while (!is.na(previous_edge[node])) {
    edges <- c(previous_edge[node], edges)
    node <- previous_node[node]; nodes <- c(node, nodes)
  }
  list(source = source, target = target, nodes = nodes, edges = edges, cost = best)
}

bridge_route_geometry <- function(route, support) {
  if (!length(route$edges)) return(NULL)
  coordinates <- NULL
  for (i in seq_along(route$edges)) {
    edge <- route$edges[i]
    xy <- support$xy[[edge]]
    if (support$from[edge] != route$nodes[i]) xy <- xy[nrow(xy):1L, , drop = FALSE]
    if (!is.null(coordinates)) {
      # Endpoint grouping is a submeter topology tolerance. Explicitly connect
      # any remaining native SWIFD endpoint discrepancy within that tolerance.
      if (sum((tail(coordinates, 1L) - xy[1L, , drop = FALSE])^2) < 1e-16) xy <- xy[-1L, , drop = FALSE]
    }
    coordinates <- rbind(coordinates, xy)
  }
  if (nrow(coordinates) < 2L) NULL else sf::st_linestring(coordinates)
}

# Preserve all original prediction edges; only additions joining different
# physical components survive. Remove new dangling leaves after cycle pruning.
bridge_physical_forest <- function(sources, endpoint_tolerance, classification_tolerance) {
  noded <- bridge_node_lines(sources)
  message("Noded augmented network: ", length(noded), " pieces; classifying sources")
  classification <- classify_network_sources(noded, sources, tolerance = classification_tolerance)
  graph <- bridge_graph(noded, endpoint_tolerance)
  original <- classification$source_class == "prediction"
  forest <- bridge_union_find(nrow(graph$node_xy))
  keep <- original
  for (edge in which(original)) forest$join(graph$from[edge], graph$to[edge])
  additions <- which(!original)
  additions <- additions[order(graph$length[additions], classification$source_class[additions])]
  for (edge in additions) keep[edge] <- forest$join(graph$from[edge], graph$to[edge])
  prediction_nodes <- unique(c(graph$from[original], graph$to[original]))
  physical <- keep & classification$source_class != "alignment_link"
  physical_nodes <- unique(c(graph$from[physical], graph$to[physical]))
  degree <- tabulate(c(graph$from[keep], graph$to[keep]), nbins = nrow(graph$node_xy))
  unsupported <- which(degree > 1L & !seq_along(degree) %in% physical_nodes)
  keep[keep & classification$source_class == "alignment_link" &
    (graph$from %in% unsupported | graph$to %in% unsupported)] <- FALSE
  adjacency <- split(rep(seq_along(graph$from), 2L),
    factor(c(graph$from, graph$to), levels = seq_len(nrow(graph$node_xy))))
  degree <- tabulate(c(graph$from[keep], graph$to[keep]), nbins = nrow(graph$node_xy))
  queue <- which(degree == 1L & !seq_along(degree) %in% prediction_nodes)
  index <- 1L
  while (index <= length(queue)) {
    node <- queue[index]; index <- index + 1L
    if (degree[node] != 1L || node %in% prediction_nodes) next
    incident <- adjacency[[node]]
    incident <- incident[keep[incident]]
    if (!length(incident)) next
    edge <- incident[1L]
    if (original[edge]) next
    other <- if (graph$from[edge] == node) graph$to[edge] else graph$from[edge]
    keep[edge] <- FALSE
    degree[node] <- degree[node] - 1L; degree[other] <- degree[other] - 1L
    if (degree[other] == 1L && !other %in% prediction_nodes) queue <- c(queue, other)
  }
  output <- sf::st_sf(classification[keep, , drop = FALSE], geometry = noded[keep])
  output$length_m <- as.numeric(sf::st_length(output))
  output
}

bridge_prediction_with_swifd <- function(prediction, swifd, snap_distance = 100,
    endpoint_tolerance = 0.25, interior_anchors = TRUE, classification_tolerance = 1e-5) {
  for (package in c("sf", "igraph")) stopifnot(requireNamespace(package, quietly = TRUE))
  if (!exists("network_endpoint_groups", mode = "function") ||
      !exists("classify_network_sources", mode = "function")) {
    stop("Source R/network.R and R/source_classification.R before routing SWIFD connectors")
  }
  crs <- sf::st_crs(prediction)
  if (is.na(crs) || sf::st_is_longlat(prediction) || !crs$units_gdal %in% c("metre", "meter", "m")) {
    stop("Prediction must use a projected CRS with meter units")
  }
  if (!is.finite(snap_distance) || snap_distance < 0) stop("snap_distance must be nonnegative meters")
  model <- bridge_graph(bridge_node_lines(prediction), endpoint_tolerance)
  message("Prediction support: ", length(model$geometry), " noded pieces")
  guide <- bridge_graph(bridge_node_lines(sf::st_transform(swifd, crs)), endpoint_tolerance)
  message("SWIFD support: ", length(guide$geometry), " noded pieces")
  number_components <- max(model$component)
  terminal_nodes <- which(model$degree == 1L)
  terminal_xy <- model$node_xy[terminal_nodes, , drop = FALSE]
  terminal_sf <- sf::st_as_sf(data.frame(x = terminal_xy[,1L], y = terminal_xy[,2L]),
                             coords = c("x", "y"), crs = crs)
  nearest <- sf::st_nearest_feature(terminal_sf, guide$geometry)
  model_first_edge <- rep(seq_along(model$from), 2L)[match(seq_len(nrow(model$node_xy)), c(model$from, model$to))]
  anchors <- lapply(seq_along(terminal_nodes), function(i) {
    node <- terminal_nodes[i]
    model_edge <- model_first_edge[node]
    projection <- bridge_projection(guide$xy[[nearest[i]]], terminal_xy[i, ])
    data.frame(role = "terminal", is_terminal = TRUE, model_component = model$component[node],
      model_edge = model_edge, model_measure = if (model$from[model_edge] == node) 0 else model$length[model_edge],
      model_x = terminal_xy[i,1L], model_y = terminal_xy[i,2L], swifd_edge = nearest[i],
      swifd_measure = projection$measure, swifd_x = projection$xy[1L], swifd_y = projection$xy[2L],
      snap_distance_m = projection$distance)
  })
  anchors <- do.call(rbind, anchors)
  if (is.null(anchors)) anchors <- data.frame(role=character(),is_terminal=logical(),
    model_component=integer(),model_edge=integer(),model_measure=numeric(),
    model_x=numeric(),model_y=numeric(),swifd_edge=integer(),swifd_measure=numeric(),
    swifd_x=numeric(),swifd_y=numeric(),snap_distance_m=numeric())
  if (interior_anchors) {
    guide_nodes <- sf::st_as_sf(data.frame(x = guide$node_xy[,1L], y = guide$node_xy[,2L]),
                               coords = c("x", "y"), crs = crs)
    nearest_model <- sf::st_nearest_feature(guide_nodes, model$geometry)
    guide_first_edge <- rep(seq_along(guide$from), 2L)[match(seq_len(nrow(guide$node_xy)), c(guide$from, guide$to))]
    number_guide_nodes <- nrow(guide$node_xy)
    interior_measure <- interior_x <- interior_y <- interior_distance <- numeric(number_guide_nodes)
    for (node in seq_len(number_guide_nodes)) {
      edge <- nearest_model[node]
      projection <- bridge_projection(model$xy[[edge]], guide$node_xy[node, ])
      interior_distance[node] <- projection$distance
      interior_measure[node] <- projection$measure
      interior_x[node] <- projection$xy[1L]; interior_y[node] <- projection$xy[2L]
    }
    keep <- which(interior_distance <= snap_distance + 1e-8)
    guide_edge <- guide_first_edge[keep]
    interior <- data.frame(role = rep("interior_target", length(keep)),
      is_terminal = rep(FALSE, length(keep)),
      model_component = model$component[model$from[nearest_model[keep]]], model_edge = nearest_model[keep],
      model_measure = interior_measure[keep], model_x = interior_x[keep], model_y = interior_y[keep],
      swifd_edge = guide_edge, swifd_measure = ifelse(guide$from[guide_edge] == keep, 0, guide$length[guide_edge]),
      swifd_x = guide$node_xy[keep,1L], swifd_y = guide$node_xy[keep,2L],
      snap_distance_m = interior_distance[keep])
    anchors <- rbind(anchors, interior)
  }
  anchors$anchor_id <- seq_len(nrow(anchors))
  anchors$eligible <- anchors$snap_distance_m <= snap_distance + 1e-8
  eligible <- anchors[anchors$eligible, , drop = FALSE]
  guide_cuts <- data.frame(edge = eligible$swifd_edge, measure = eligible$swifd_measure,
                           x = eligible$swifd_x, y = eligible$swifd_y)
  routed <- bridge_graph(bridge_split_support(guide$geometry, guide_cuts), endpoint_tolerance)
  routed$adjacency <- split(rep(seq_along(routed$from), 2L),
    factor(c(routed$from, routed$to), levels = seq_len(nrow(routed$node_xy))))
  eligible_points <- sf::st_as_sf(data.frame(x = eligible$swifd_x, y = eligible$swifd_y),
                                  coords = c("x", "y"), crs = crs)
  routed_nodes <- sf::st_as_sf(data.frame(x = routed$node_xy[,1L], y = routed$node_xy[,2L]),
                               coords = c("x", "y"), crs = crs)
  eligible$swifd_node <- sf::st_nearest_feature(eligible_points, routed_nodes)
  target_at_node <- split(seq_len(nrow(eligible)),
    factor(eligible$swifd_node, levels = seq_len(nrow(routed$node_xy))))
  message("SWIFD routing: ", number_components, " prediction components; ",
    sum(eligible$is_terminal), " eligible terminals; ", nrow(routed$node_xy), " guide nodes")
  forest <- bridge_union_find(number_components)
  accepted <- list()
  routing_round <- 0L
  repeat {
    routing_round <- routing_round + 1L
    membership <- vapply(seq_len(number_components), forest$find, integer(1L))
    sources <- which(eligible$is_terminal)
    clusters <- split(sources, membership[eligible$model_component[sources]])
    candidates <- lapply(clusters, function(index) {
      bridge_nearest_route(routed, eligible, index, target_at_node, membership)
    })
    candidates <- Filter(Negate(is.null), candidates)
    if (!length(candidates)) break
    candidates <- candidates[order(vapply(candidates, function(z) z$cost, numeric(1)))]
    changes <- 0L
    for (route in candidates) {
      if (forest$join(eligible$model_component[route$source], eligible$model_component[route$target])) {
        accepted[[length(accepted) + 1L]] <- route
        changes <- changes + 1L
      }
    }
    if (!changes) break
    message("Routing round ", routing_round, ": ", changes, " component joins")
  }
  source_geometries <- list(); source_class <- source_id <- character()
  bridge_audit <- list()
  for (i in seq_along(accepted)) {
    route <- accepted[[i]]; id <- sprintf("SWIFD_%05d", i)
    a <- eligible[route$source, ]; b <- eligible[route$target, ]
    g <- bridge_route_geometry(route, routed)
    route_m <- 0
    if (!is.null(g)) {
      source_geometries[[length(source_geometries) + 1L]] <- g
      source_class <- c(source_class, "swifd_connector"); source_id <- c(source_id, id)
      route_m <- as.numeric(sf::st_length(sf::st_sfc(g, crs = crs)))
    }
    for (anchor in list(a, b)) {
      if (anchor$snap_distance_m <= 1e-8) next
      link <- sf::st_linestring(rbind(c(anchor$model_x, anchor$model_y),
                                      c(anchor$swifd_x, anchor$swifd_y)))
      source_geometries[[length(source_geometries) + 1L]] <- link
      source_class <- c(source_class, "alignment_link"); source_id <- c(source_id, id)
    }
    bridge_audit[[i]] <- data.frame(bridge_id = id, from_anchor = a$anchor_id, to_anchor = b$anchor_id,
      from_role = a$role, to_role = b$role, from_component = a$model_component,
      to_component = b$model_component, from_snap_m = a$snap_distance_m,
      to_snap_m = b$snap_distance_m, routed_swifd_m = route_m,
      alignment_m = a$snap_distance_m + b$snap_distance_m)
  }
  used_anchors <- unique(unlist(lapply(accepted, function(route) c(route$source, route$target))))
  selected <- eligible[used_anchors, , drop = FALSE]
  model_cuts <- data.frame(edge = selected$model_edge, measure = selected$model_measure,
                           x = selected$model_x, y = selected$model_y)
  model_support <- bridge_split_support(model$geometry, model_cuts)
  sources <- sf::st_sf(source_class = rep("prediction", length(model_support)),
    measure_habitat = TRUE, bridge_id = NA_character_, geometry = model_support)
  if (length(source_geometries)) {
    additions <- sf::st_sf(source_class = source_class,
      measure_habitat = source_class != "alignment_link", bridge_id = source_id,
      geometry = sf::st_sfc(source_geometries, crs = crs))
    sources <- rbind(sources, additions)
  }
  network <- bridge_physical_forest(sources, endpoint_tolerance, classification_tolerance)
  prediction_network <- network[network$source_class == "prediction", ]
  connectors <- network[network$source_class == "swifd_connector", ]
  alignment_links <- network[network$source_class == "alignment_link", ]
  bridge_audit <- if (length(bridge_audit)) do.call(rbind, bridge_audit) else data.frame(
    bridge_id = character(), from_anchor = integer(), to_anchor = integer(),
    from_role = character(), to_role = character(), from_component = integer(),
    to_component = integer(), from_snap_m = numeric(), to_snap_m = numeric(),
    routed_swifd_m = numeric(), alignment_m = numeric())
  bridge_audit$retained_swifd_m <- vapply(bridge_audit$bridge_id, function(id) {
    sum(connectors$length_m[!is.na(connectors$bridge_id) & connectors$bridge_id == id])
  }, numeric(1))
  bridge_audit$retained_alignment_m <- vapply(bridge_audit$bridge_id, function(id) {
    sum(alignment_links$length_m[!is.na(alignment_links$bridge_id) & alignment_links$bridge_id == id])
  }, numeric(1))
  bridge_audit$status <- ifelse(bridge_audit$retained_swifd_m + bridge_audit$retained_alignment_m > 1e-8,
                                 "retained_in_physical_forest", "redundant_route_pruned")
  used <- unique(c(bridge_audit$from_anchor[bridge_audit$status != "redundant_route_pruned"],
                   bridge_audit$to_anchor[bridge_audit$status != "redundant_route_pruned"]))
  anchors$match_status <- ifelse(!anchors$eligible, "outside_tolerance",
    ifelse(anchors$anchor_id %in% used, "used_by_connector", "eligible_no_retained_connector"))
  endpoint_audit <- sf::st_as_sf(anchors, coords = c("model_x", "model_y"), crs = crs, remove = FALSE)
  final_graph <- bridge_graph(sf::st_geometry(network), endpoint_tolerance)
  raw_original_rank <- length(model$from) - nrow(model$node_xy) + max(model$component)
  preserved_graph <- bridge_graph(sf::st_geometry(prediction_network), endpoint_tolerance)
  original_rank <- length(preserved_graph$from) - nrow(preserved_graph$node_xy) + max(preserved_graph$component)
  final_rank <- length(final_graph$from) - nrow(final_graph$node_xy) + max(final_graph$component)
  if (final_rank > original_rank) stop("SWIFD connectors introduced a new network cycle")
  if (abs(sum(prediction_network$length_m) - sum(model$length)) > 0.001) {
    stop("SWIFD routing failed to preserve original prediction length")
  }
  summary <- data.frame(original_components = number_components,
    augmented_components = max(final_graph$component),
    original_cycle_rank = original_rank, raw_original_cycle_rank = raw_original_rank,
    augmented_cycle_rank = final_rank,
    terminal_endpoints = sum(anchors$is_terminal),
    eligible_terminal_endpoints = sum(anchors$is_terminal & anchors$eligible),
    interior_anchors = sum(!anchors$is_terminal), proposed_bridges = nrow(bridge_audit),
    retained_bridges = sum(bridge_audit$status != "redundant_route_pruned"),
    original_prediction_m = sum(model$length), swifd_connector_m = sum(connectors$length_m),
    alignment_link_m = sum(alignment_links$length_m))
  list(network = network, prediction_network = prediction_network,
    connectors = connectors, alignment_links = alignment_links,
    endpoint_audit = endpoint_audit, bridge_audit = bridge_audit, summary = summary,
    settings = list(snap_distance_m = snap_distance, endpoint_tolerance_m = endpoint_tolerance,
                    interior_anchors = interior_anchors, classification_tolerance_m = classification_tolerance))
}
