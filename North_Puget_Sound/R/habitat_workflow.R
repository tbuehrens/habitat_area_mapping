STEELHEAD_SERVICE <- paste0(
  "https://services.arcgis.com/rcya3vExsaVBGUDp/arcgis/rest/services/",
  "Statewide_Dist_Models_1b66a3ddb471/FeatureServer/0")
BARRIER_SERVICE <- paste0(
  "https://geodataservices.wdfw.wa.gov/arcgis/rest/services/",
  "ApplicationServices/FP_Sites/MapServer/")
SHORELINE_SERVICE <- paste0(
  "https://geodataservices.wdfw.wa.gov/arcgis/rest/services/",
  "ApplicationServices/Marine_Areas/MapServer/1")

habitat_require_packages <- function() {
  packages <- c("sf", "dplyr", "ggplot2", "httr", "jsonlite", "igraph")
  missing <- packages[!vapply(packages, requireNamespace, logical(1), quietly = TRUE)]
  if (length(missing)) stop("Install required packages: ", paste(missing, collapse = ", "))
}

habitat_lines <- function(x) {
  if (!nrow(x)) return(x)
  x <- x[!sf::st_is_empty(x), ]
  if (any(sf::st_geometry_type(x) == "GEOMETRYCOLLECTION")) {
    x <- suppressWarnings(sf::st_collection_extract(x, "LINESTRING"))
  }
  x[sf::st_dimension(x) == 1, ]
}

habitat_clip <- function(lines, populations) {
  if (!nrow(lines)) return(sf::st_sf(
    pop_id = populations$pop_id[0], geometry = sf::st_sfc(crs = sf::st_crs(populations))))
  clipped <- habitat_lines(suppressWarnings(sf::st_intersection(lines, populations["pop_id"])))
  if ("length_m" %in% names(clipped)) clipped$length_m <- as.numeric(sf::st_length(clipped))
  clipped
}

habitat_length_by_pop <- function(lines, populations, field) {
  clipped <- if ("pop_id" %in% names(lines)) lines else habitat_clip(lines, populations)
  result <- data.frame(pop_id = populations$pop_id, value = 0)
  if (nrow(clipped)) {
    for (id in unique(clipped$pop_id)) {
      g <- sf::st_union(sf::st_geometry(clipped[clipped$pop_id == id, ]))
      result$value[result$pop_id == id] <- sum(as.numeric(sf::st_length(g))) / 1609.344
    }
  }
  names(result)[2] <- field
  result
}

habitat_map <- function(populations, habitat, barriers, title, subtitle, labels = TRUE,
                        review_only = FALSE) {
  colors <- c("Connected below barriers" = "#126a91", "Connectivity unresolved" = "#a6adb0")
  line_types <- c("Predictive model" = "solid", "SWIFD gap route" = "longdash",
                  "Endpoint alignment (not mileage)" = "dotted")
  if (review_only) {
    habitat$map_status <- ifelse(habitat$source_class == "swifd_connector", "SWIFD patch",
      ifelse(habitat$source_class == "alignment_link", "Endpoint alignment (not mileage)",
        ifelse(habitat$retained, "Occupancy model", "Disconnected model")))
    colors <- c("Occupancy model" = "#126a91", "SWIFD patch" = "#e66b18",
      "Disconnected model" = "#a6adb0", "Endpoint alignment (not mileage)" = "#776f68")
    line_types["SWIFD gap route"] <- "solid"
  }
  habitat$line_width <- if (review_only) {
    ifelse(habitat$source_class == "swifd_connector", 0.8,
      ifelse(habitat$source_class == "alignment_link", 0.3, 0.4))
  } else rep(0.4, nrow(habitat))
  p <- ggplot2::ggplot() +
    ggplot2::geom_sf(data = populations, fill = "#f3f2eb", color = "#414c4b", linewidth = 0.5) +
    ggplot2::geom_sf(data = habitat, ggplot2::aes(color = map_status, linetype = line_source,
                                                linewidth = line_width)) +
    ggplot2::scale_linewidth_identity() +
    ggplot2::scale_color_manual(name = if (review_only) "Geometry review" else "Predicted steelhead habitat",
      values = colors, limits = names(colors), drop = FALSE) +
    ggplot2::scale_linetype_manual(name = "Line source", values = line_types,
      limits = names(line_types), drop = FALSE) +
    ggplot2::geom_sf(data = barriers, ggplot2::aes(shape = verification),
                     color = "#b34c25", fill = "#f4a259", size = 1.9, stroke = 0.5) +
    ggplot2::scale_shape_manual(name = "Natural barriers", values = c(
      "Verified" = 24, "Not field verified" = 2),
      limits = c("Verified", "Not field verified"), drop = FALSE) +
    ggplot2::coord_sf(datum = sf::st_crs(4326), expand = TRUE) +
    ggplot2::labs(title = title, subtitle = subtitle, x = NULL, y = NULL,
      caption = paste("Sources: NOAA population boundaries; WDFW/TerrainWorks steelhead prediction;",
                      "SWIFD winter/summer steelhead gap routes; WDFW FPDSI natural barriers.",
                      "Gray reaches remain unresolved. Dotted endpoint links are excluded from mileage.",
                      "Barrier truncation assumes mapped natural barriers block upstream passage.", sep = "\n")) +
    ggplot2::theme_bw(base_size = 11) +
    ggplot2::theme(panel.grid.major = ggplot2::element_line(color = "#e1e7e8", linewidth = 0.15),
      plot.title = ggplot2::element_text(face = "bold", size = 17),
      plot.subtitle = ggplot2::element_text(size = 10),
      plot.caption = ggplot2::element_text(size = 8, hjust = 0),
      legend.position = "bottom", legend.box = "vertical")
  if (labels) {
    label_points <- suppressWarnings(sf::st_point_on_surface(populations))
    p <- p + ggplot2::geom_sf_label(data = label_points,
      ggplot2::aes(label = population), size = 2.5, alpha = 0.8,
      linewidth = 0.1, label.padding = grid::unit(0.12, "lines"))
  }
  if (review_only) {
    p <- p + ggplot2::guides(linetype = "none",
      color = ggplot2::guide_legend(nrow = 2, order = 1,
        override.aes = list(linetype = c("solid", "solid", "solid", "dotted"),
                            linewidth = c(0.4, 0.8, 0.4, 0.3))),
      shape = ggplot2::guide_legend(order = 2))
  }
  p
}

habitat_save_map <- function(plot, path, width = 11, height = 9) {
  ggplot2::ggsave(path, plot = plot, width = width, height = height,
                  units = "in", dpi = 250, bg = "white", limitsize = FALSE)
}

habitat_export <- function(populations, trace, verified_trace, steelhead, barriers,
                           output_dir, metadata, bridges = NULL, review_only = FALSE) {
  dir.create(file.path(output_dir, "maps"), recursive = TRUE, showWarnings = FALSE)
  # Source classes were assigned before barrier splitting. Do not re-intersect
  # split lines with source geometry: floating-point collinearity loses lengths.
  prediction_trace <- trace$lines
  prediction_verified <- verified_trace$lines
  prediction_trace <- habitat_clip(prediction_trace, populations)
  prediction_verified <- habitat_clip(prediction_verified, populations)
  for (name in c("prediction_trace", "prediction_verified")) {
    x <- get(name)
    if (!"source_class" %in% names(x)) x$source_class <- rep("prediction", nrow(x))
    if (!"measure_habitat" %in% names(x)) x$measure_habitat <- rep(TRUE, nrow(x))
    assign(name, x)
  }
  # An ambiguous barrier can invalidate passage claims for its whole component.
  for (name in c("prediction_trace", "prediction_verified")) {
    x <- get(name)
    uncertain <- x$retained & x$review_required
    x$retained[uncertain] <- FALSE
    x$unresolved[uncertain] <- TRUE
    x$status[uncertain] <- "unresolved_barrier_assignment"
    assign(name, x)
  }
  physical_trace <- prediction_trace[prediction_trace$measure_habitat, ]
  physical_verified <- prediction_verified[prediction_verified$measure_habitat, ]
  retained <- physical_trace[physical_trace$retained, ]
  unresolved <- physical_trace[physical_trace$unresolved, ]
  removed <- physical_trace[physical_trace$removed, ]
  # Tables carry unresolved miles separately; they are not certified accessible.
  table <- sf::st_drop_geometry(populations)
  table$area_sq_km <- as.numeric(sf::st_area(populations)) / 1e6
  measures <- list(
    habitat_length_by_pop(steelhead, populations, "predicted_untruncated_miles"),
    habitat_length_by_pop(physical_trace, populations, "augmented_untruncated_miles"),
    habitat_length_by_pop(physical_trace[physical_trace$source_class == "swifd_connector", ],
                          populations, "swifd_added_miles"),
    habitat_length_by_pop(prediction_trace[!prediction_trace$measure_habitat, ], populations,
                          "alignment_link_miles_not_counted"),
    habitat_length_by_pop(retained, populations, "barrier_truncated_miles"),
    habitat_length_by_pop(retained[retained$source_class == "prediction", ], populations,
                          "prediction_outlet_connected_miles"),
    habitat_length_by_pop(retained[retained$source_class == "swifd_connector", ], populations,
                          "swifd_outlet_connected_miles"),
    habitat_length_by_pop(removed, populations, "excluded_above_barriers_miles"),
    habitat_length_by_pop(unresolved, populations, "unresolved_connectivity_miles"),
    habitat_length_by_pop(physical_verified[physical_verified$retained, ], populations,
                          "verified_barriers_only_miles"),
    habitat_length_by_pop(physical_verified[physical_verified$unresolved, ], populations,
                          "verified_only_unresolved_miles"))
  for (measure in measures) table <- dplyr::left_join(table, measure, by = "pop_id")
  table$length_km <- table$barrier_truncated_miles * 1.609344
  table$retained_or_unresolved_miles <- table$barrier_truncated_miles + table$unresolved_connectivity_miles
  table$ambiguous_barriers <- vapply(sf::st_intersects(populations, trace$barrier_audit), function(index) {
    sum(trace$barrier_audit$match_status[index] == "ambiguous_requires_review")
  }, integer(1))
  table$qa_status <- ifelse(table$unresolved_connectivity_miles > 0.001,
    "PROVISIONAL: unresolved stream connections; see audit", "Traced to downstream outlets")
  table <- table[order(table$population), ]
  # Mass balance verifies clipping/dissolving did not omit or double-count lines.
  balance <- table$barrier_truncated_miles + table$excluded_above_barriers_miles +
    table$unresolved_connectivity_miles - table$augmented_untruncated_miles
  if (any(abs(balance) > 0.001)) stop("Habitat length partition failed: ", max(abs(balance)))
  source_balance <- table$augmented_untruncated_miles - table$predicted_untruncated_miles - table$swifd_added_miles
  if (any(abs(source_balance) > 0.001)) stop("Prediction plus SWIFD connector length partition failed")
  # A known portion is not a final whole-population estimate. Missing routing
  # information is NA in that estimate, never a misleading zero-mile result.
  table$outlet_connected_miles <- table$barrier_truncated_miles
  table$verified_only_outlet_connected_miles <- table$verified_barriers_only_miles
  table$barrier_truncated_miles[table$unresolved_connectivity_miles > 0.001] <- NA_real_
  table$verified_barriers_only_miles[table$verified_only_unresolved_miles > 0.001] <- NA_real_
  table$length_km <- table$barrier_truncated_miles * 1.609344
  wgs_geometry <- sf::st_geometry(sf::st_transform(populations, 4326))
  north_border <- vapply(wgs_geometry, function(g) sf::st_bbox(g)["ymax"] > 49.0015, logical(1))
  table$boundary_extends_north_of_49 <- north_border[match(table$pop_id, populations$pop_id)]
  outside_coverage <- table$boundary_extends_north_of_49
  table$barrier_truncated_miles[outside_coverage] <- NA_real_
  table$verified_barriers_only_miles[outside_coverage] <- NA_real_
  table$length_km <- table$barrier_truncated_miles * 1.609344
  table$qa_status[outside_coverage] <- paste0(table$qa_status[outside_coverage],
    "; PROVISIONAL: incomplete prediction coverage north of U.S. border")
  if (!review_only) utils::write.csv(table,
    file.path(output_dir, "North_Puget_Sound_Steelhead_hab_lengths.csv"), row.names = FALSE)
  audit <- trace$barrier_audit
  membership <- sf::st_intersects(audit, populations)
  audit$population_ids <- vapply(membership,
    function(index) paste(populations$pop_id[index], collapse = ";"), character(1))
  audit$population_names <- vapply(membership,
    function(index) paste(populations$population[index], collapse = ";"), character(1))
  audit$effect_status <- ifelse(audit$applied,
    ifelse(audit$baseline_connected, "cut_on_outlet_connected_component", "cut_on_unresolved_component"),
    audit$match_status)
  audit$longitude <- sf::st_coordinates(sf::st_transform(audit, 4326))[,1]
  audit$latitude <- sf::st_coordinates(sf::st_transform(audit, 4326))[,2]
  utils::write.csv(sf::st_drop_geometry(audit), file.path(output_dir, "natural_barrier_audit.csv"), row.names = FALSE)
  plot_lines <- rbind(
    dplyr::mutate(prediction_trace[prediction_trace$retained, ], map_status = "Connected below barriers"),
    dplyr::mutate(prediction_trace[prediction_trace$unresolved, ], map_status = "Connectivity unresolved"))
  source_labels <- c(prediction = "Predictive model", swifd_connector = "SWIFD gap route",
                     alignment_link = "Endpoint alignment (not mileage)")
  plot_lines$line_source <- unname(source_labels[plot_lines$source_class])
  inside <- lengths(sf::st_intersects(barriers, sf::st_union(populations))) > 0
  plot_barriers <- barriers[inside, ]
  bridge_tolerance <- if (!is.null(bridges)) bridges$settings$snap_distance_m else 100
  routing_subtitle <- sprintf("SWIFD-supported gap connections (%g m endpoint tolerance); natural barriers applied",
                               bridge_tolerance)
  make_map <- function(...) habitat_map(..., review_only = review_only)
  if (review_only) routing_subtitle <- sprintf(
    "REVIEW: full model patched along SWIFD within %g m, then natural barriers applied", bridge_tolerance)
  overview <- make_map(populations, plot_lines, plot_barriers,
    "North Puget Sound steelhead", routing_subtitle)
  habitat_save_map(overview, file.path(output_dir, "North_Puget_Sound_Steelhead_map.png"), 14, 14)
  pdf_file <- file.path(output_dir, "North_Puget_Sound_Steelhead_population_maps.pdf")
  grDevices::pdf(pdf_file, width = 10, height = 9, onefile = TRUE)
  for (i in seq_len(nrow(populations))) {
    pop <- populations[i, ]
    row <- table[table$pop_id == pop$pop_id, ]
    sub_lines <- plot_lines[plot_lines$pop_id == pop$pop_id, ]
    sub_barriers <- plot_barriers[lengths(sf::st_intersects(plot_barriers, pop)) > 0, ]
    sub <- sprintf("%.2f connected miles | %.2f unresolved miles | %.2f SWIFD route miles added",
                   row$outlet_connected_miles, row$unresolved_connectivity_miles, row$swifd_added_miles)
    if (review_only) sub <- sprintf("Geometry review | NOAA population %s | Orange = SWIFD patches", pop$pop_id)
    p <- make_map(pop, sub_lines, sub_barriers, pop$population, sub, labels = FALSE)
    slug <- gsub("[^A-Za-z0-9]+", "_", pop$population)
    habitat_save_map(p, file.path(output_dir, "maps", paste0(pop$pop_id, "_", slug, ".png")))
    print(p)
  }
  grDevices::dev.off()
  nooksack <- populations[grepl("Nooksack", populations$population, ignore.case = TRUE), ]
  if (nrow(nooksack)) {
    ids <- nooksack$pop_id
    b <- plot_barriers[lengths(sf::st_intersects(plot_barriers, sf::st_union(nooksack))) > 0, ]
    p <- make_map(nooksack, plot_lines[plot_lines$pop_id %in% ids, ], b,
      "Nooksack steelhead populations", routing_subtitle)
    habitat_save_map(p, file.path(output_dir, "Nooksack_Steelhead_map.png"), 12, 9)
    if (!review_only) utils::write.csv(table[table$pop_id %in% ids, ],
      file.path(output_dir, "Nooksack_Steelhead_hab_lengths.csv"), row.names = FALSE)
    winter <- nooksack[nooksack$run_timing == "wi", ]
    if (nrow(winter) && !review_only) {
      winter_ids <- winter$pop_id
      b <- plot_barriers[lengths(sf::st_intersects(plot_barriers, sf::st_union(winter))) > 0, ]
      p <- habitat_map(winter, plot_lines[plot_lines$pop_id %in% winter_ids, ], b,
        "Nooksack winter steelhead", "Prediction plus SWIFD gap routes; blue below barriers, gray unresolved")
      habitat_save_map(p, "Nooksack_Winter_Steelhead_map.png", 12, 9)
      utils::write.csv(table[table$pop_id %in% winter_ids, ],
        "Nooksack_Winter_Steelhead_hab_lengths.csv", row.names = FALSE)
    }
  }
  gpkg <- file.path(output_dir, "North_Puget_Sound_Steelhead.gpkg")
  sf::st_write(populations, gpkg, layer = "noaa_populations", delete_layer = TRUE, quiet = TRUE)
  sf::st_write(prediction_trace, gpkg, layer = "predicted_extent_status", delete_layer = TRUE, quiet = TRUE)
  sf::st_write(audit, gpkg, layer = "natural_barriers_audit", delete_layer = TRUE, quiet = TRUE)
  sf::st_write(trace$roots, gpkg, layer = "downstream_roots", delete_layer = TRUE, quiet = TRUE)
  if (!is.null(bridges)) {
    # These audit layers describe accepted routing before natural-barrier cuts.
    # The status layer above is the final cut network with source labels.
    study_routes <- habitat_clip(bridges$connectors, populations)
    study_links <- habitat_clip(bridges$alignment_links, populations)
    sf::st_write(study_routes, gpkg,
      layer = "swifd_gap_routes_before_barriers", delete_layer = TRUE, quiet = TRUE)
    sf::st_write(study_links, gpkg,
      layer = "endpoint_alignment_links", delete_layer = TRUE, quiet = TRUE)
    endpoints <- bridges$endpoint_audit
    inside_endpoints <- lengths(sf::st_intersects(endpoints, sf::st_union(populations))) > 0
    endpoints <- endpoints[inside_endpoints, ]
    sf::st_write(endpoints, gpkg, layer = "gap_endpoint_audit", delete_layer = TRUE, quiet = TRUE)
    utils::write.csv(sf::st_drop_geometry(endpoints), file.path(output_dir, "gap_endpoint_audit.csv"), row.names = FALSE)
    connector_audit <- bridges$bridge_audit
    ids_in_study <- function(id) {
      route_ids <- if (nrow(study_routes)) study_routes$pop_id[study_routes$bridge_id == id] else integer()
      link_ids <- if (nrow(study_links)) study_links$pop_id[study_links$bridge_id == id] else integer()
      paste(sort(unique(c(route_ids, link_ids))), collapse = ";")
    }
    connector_audit$population_ids <- vapply(connector_audit$bridge_id, ids_in_study, character(1))
    connector_audit$has_study_geometry <- nzchar(connector_audit$population_ids)
    utils::write.csv(connector_audit,
      file.path(output_dir, "swifd_connector_audit.csv"), row.names = FALSE)
    metadata$bridge_summary <- bridges$summary
    metadata$bridge_settings <- bridges$settings
    metadata$study_bridges_with_geometry <- sum(connector_audit$has_study_geometry)
    metadata$alignment_mileage_policy <- "Alignment links participate in topology but never contribute to habitat mileage or attract barrier snapping"
  }
  metadata$regional_raw_trace_summary <- trace$summary
  metadata$regional_raw_verified_only_summary <- verified_trace$summary
  metadata$review_checkpoint <- review_only
  metadata$mileage_export_deferred <- review_only
  if (review_only) metadata$review_colors <- c(occupancy_model = "blue",
    swifd_patch = "orange", disconnected_model = "gray", alignment_link = "dotted taupe")
  metadata$generated_utc <- format(Sys.time(), tz = "UTC", usetz = TRUE)
  jsonlite::write_json(metadata, file.path(output_dir, "run_metadata.json"), auto_unbox = TRUE, pretty = TRUE)
  capture.output(utils::sessionInfo(), file = file.path(output_dir, "sessionInfo.txt"))
  if (review_only) message("Geometry review maps exported; final mileage table export deferred") else
    print(table[, c("population", "outlet_connected_miles", "unresolved_connectivity_miles")], row.names = FALSE)
  invisible(list(lengths = table, populations = populations, trace = trace,
                  habitat = prediction_trace, map = overview, output_dir = output_dir))
}

run_habitat_analysis <- function(population_pattern = NULL,
    output_dir = "outputs/review_swifd_connections", cache_dir = "spatial_data",
    refresh = FALSE, snap_distance_m = 50, outlet_distance_m = 250,
    swifd_endpoint_distance_m = 100,
    review_only = TRUE,
    root_file = file.path(cache_dir, "marine_north.rds")) {
  habitat_require_packages()
  populations <- get_north_puget_populations(cache_dir = cache_dir, refresh = refresh)
  populations <- sf::st_transform(sf::st_make_valid(populations), 26910)
  if (!is.null(population_pattern)) {
    populations <- populations[grepl(population_pattern, populations$population, ignore.case = TRUE), ]
  }
  if (!nrow(populations)) stop("No NOAA populations match the requested selection")
  # Preserve downstream connections beyond population boundaries during tracing.
  region_bbox <- c(-123.2, 47.2, -120.5, 49.5)
  bb <- sf::st_bbox(sf::st_transform(populations, 4326))
  if (bb["xmin"] < region_bbox[1] || bb["ymin"] < region_bbox[2] ||
      bb["xmax"] > region_bbox[3] || bb["ymax"] > region_bbox[4]) {
    stop("Population boundaries extend beyond configured download bounds; expand region_bbox")
  }
  steelhead <- arcgis_download(STEELHEAD_SERVICE,
    file.path(cache_dir, "north_puget_steelhead.rds"), region_bbox, refresh = refresh)
  steelhead <- sf::st_transform(steelhead, 26910)
  swifd <- download_steelhead_swifd(cache_dir, region_bbox, refresh = refresh)
  bridge_input_files <- c(file.path(cache_dir, "north_puget_steelhead.rds"),
    file.path(cache_dir, "north_puget_swifd_steelhead.rds"),
    file.path("R", c("swifd_connectors.R", "source_classification.R", "network.R", "swifd_sources.R")))
  bridge_signature <- list(md5 = unname(tools::md5sum(bridge_input_files)),
    endpoint_distance_m = swifd_endpoint_distance_m)
  bridge_file <- file.path(cache_dir, "north_puget_swifd_bridges.rds")
  cached_bridges <- if (file.exists(bridge_file) && !refresh) readRDS(bridge_file) else NULL
  if (!is.null(cached_bridges) && identical(cached_bridges$signature, bridge_signature)) {
    bridges <- cached_bridges$bridges
    message("Using cached SWIFD-supported gap connections")
  } else {
    message("Connecting predictive model gaps along eligible SWIFD steelhead routes")
    bridges <- bridge_prediction_with_swifd(steelhead, swifd,
      snap_distance = swifd_endpoint_distance_m)
    saveRDS(list(signature = bridge_signature, bridges = bridges), bridge_file)
  }
  barrier_parts <- lapply(c(5, 6), function(id) {
    x <- arcgis_download(paste0(BARRIER_SERVICE, id),
      file.path(cache_dir, paste0("north_puget_barriers_", id, ".rds")), region_bbox, refresh = refresh)
    x$verification <- if (id == 5) "Verified" else "Not field verified"
    x$source_layer <- id
    sf::st_transform(x, 26910)
  })
  barriers <- dplyr::bind_rows(barrier_parts)
  barriers$barrier_key <- paste(barriers$source_layer, barriers$OBJECTID, sep = ":")
  if (review_only) {
    if (!exists("habitat_export_untruncated_review", mode = "function") ||
        !exists("gorge_dam_review_override", mode = "function")) {
      stop("Source R/barrier_review.R and R/untruncated_review.R for the current review workflow")
    }
    # The review intentionally returns before marine-outlet tracing or any
    # retained/removed filters. It always starts with the full bridge network.
    barriers$source_layer <- as.character(barriers$source_layer)
    barriers <- dplyr::bind_rows(barriers, gorge_dam_review_override(crs = 26910))
    definitions <- lapply(c(5, 6), function(id) {
      file <- file.path(cache_dir, paste0("barrier_layer_", id, ".json"))
      if (file.exists(file)) jsonlite::fromJSON(file, simplifyVector = FALSE) else NULL
    })
    identity_reference <- load_stream_identity_reference(cache_dir, region_bbox, refresh = refresh)
    metadata <- list(
      species = "Steelhead (all runs; prediction has no run-specific attribute)",
      population_source = attr(populations, "source_url"),
      prediction_source = STEELHEAD_SERVICE,
      prediction_downloaded_utc = attr(steelhead, "downloaded_utc"),
      swifd_connector_source = SWIFD_SERVICE,
      swifd_downloaded_utc = attr(swifd, "downloaded_utc"),
      swifd_selection_where = attr(swifd, "selection_where"),
      swifd_category_counts = dplyr::count(sf::st_drop_geometry(swifd),
        SPECIESRUN, DISTTYPE_DESC, USETYPE_DESC, name = "records"),
      barrier_sources = paste0(BARRIER_SERVICE, c(5, 6)),
      barrier_field_definitions = barrier_review_field_definitions(definitions),
      stream_identity_reference_source = attr(identity_reference, "identity_reference_service"),
      stream_identity_reference_scope = attr(identity_reference, "identity_reference_scope"),
      stream_identity_reference_downloaded_utc = attr(identity_reference, "downloaded_utc"),
      gorge_dam_source = "https://app.leg.wa.gov/wac/default.aspx?cite=173-201A-602",
      projection = "EPSG:26910 NAD83 / UTM zone 10N",
      meters_per_mile = 1609.344,
      assumptions = c(
        "Every raw model reach is preserved; no barrier cuts or outlet-connectivity filters are applied.",
        "Winter and summer SWIFD routes include artificial, transported, presumed, presence, spawning and rearing; gradient-accessible categories are excluded.",
        "Both gap attachments must be within 100 meters of eligible SWIFD; gaps without a qualifying continuous route remain unpatched.",
        "Alignment links are displayed dotted and excluded from stream mileage and barrier matching.",
        "All physical stream candidates within 50 meters are checked against complete named hydrography; only unique own-stream assignments clear of confluence ambiguity are eligible for later cuts.",
        "Numbered provisional locations are retained for manual review; a displayed snap does not approve a stream assignment.",
        "Unknown or blank passability is not evidence of a total barrier.",
        "Gorge Dam is a user-requested future-cut override at its published WAC location; it is not a fabricated WDFW natural-barrier record.",
        "Untruncated lengths are geometry review measurements, not accessible-habitat estimates.",
        "Overlapping NOAA populations can share linework and totals are not additive; Canadian prediction coverage is incomplete."))
    return(habitat_export_untruncated_review(populations, steelhead, swifd, bridges,
      barriers, output_dir, metadata, stream_reference = identity_reference))
  }
  stop("Barrier truncation is deferred. Run the untruncated review with review_only = TRUE.")
}
