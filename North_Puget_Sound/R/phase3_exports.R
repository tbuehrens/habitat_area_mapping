# Phase 3 reuses the complete original model/SWIFD source. Prior scenarios are
# read only. Additional upstream barrier cuts precede mapped-current tidal cuts.
phase3_gpkg_fields <- function(x) {
  fields <- names(x)
  used <- character()
  for (i in seq_along(fields)) {
    value <- if (tolower(fields[i]) == "fid") paste0("source_", fields[i]) else fields[i]
    while (tolower(value) %in% used) value <- paste0(value, "_source")
    fields[i] <- value; used <- c(used, tolower(value))
  }
  names(x) <- fields
  x
}

phase3_check_previous_snapshot <- function(cache_dir = "spatial_data",
    review = "stillaguamish_review", snapshot = "previous_phase3") {
  manifest_file <- file.path(cache_dir, review, snapshot, "snapshot_manifest.csv")
  if (!file.exists(manifest_file)) return(NULL)
  manifest <- nps_read_cache_csv(manifest_file, stringsAsFactors = FALSE)
  paths <- nps_resolve_cache_path(manifest$snapshot_path)
  if (!all(file.exists(paths)) || !identical(unname(tools::md5sum(paths)), manifest$md5))
    stop("The frozen review snapshot changed: ", manifest_file)
  manifest$snapshot_path <- paths
  manifest
}

phase3_apply_tidal_mask <- function(network, mask) {
  stopifnot(inherits(network, "sf"), inherits(mask, "sf"), nrow(mask) > 0L)
  mask <- sf::st_union(sf::st_geometry(sf::st_transform(mask, sf::st_crs(network))))
  initial <- tapply(as.numeric(sf::st_length(network)), network$source_class, sum)
  nearby <- lengths(sf::st_intersects(network, mask)) > 0L
  network$inside_current_tidal <- FALSE
  inside <- upstream_line_parts(suppressWarnings(sf::st_intersection(network[nearby, ], mask)))
  outside <- upstream_line_parts(suppressWarnings(sf::st_difference(network[nearby, ], mask)))
  inside$inside_current_tidal <- TRUE
  lines <- rbind(network[!nearby, ], outside, inside)
  lines$barrier_removed <- lines$removed
  lines$tidal_removed <- lines$inside_current_tidal & !lines$barrier_removed
  lines$removed <- lines$barrier_removed | lines$tidal_removed
  lines$retained <- !lines$removed
  lines$unresolved_direction <- lines$unresolved_direction & lines$retained
  lines$phase3_status <- ifelse(lines$barrier_removed, "Removed upstream of approved barrier",
    ifelse(lines$tidal_removed, "Removed mapped current tidal influence", "Remaining non-tidal distribution"))
  lines$length_m <- as.numeric(sf::st_length(lines))
  lines$phase3_piece_id <- seq_len(nrow(lines))
  final <- tapply(lines$length_m, lines$source_class, sum)
  error <- max(abs(initial - final[names(initial)]))
  if (!is.finite(error) || error > max(.0001, sum(initial) * 1e-10)) stop("Tidal split failed source conservation")
  remaining <- lines[lines$retained & lines$measure_habitat, ]
  residual <- upstream_line_parts(suppressWarnings(sf::st_intersection(remaining, mask)))
  residual_m <- sum(as.numeric(sf::st_length(residual)))
  if (residual_m > .001) stop("Remaining source geometry intersects the approved tidal mask")
  list(network = lines, summary = list(source_partition_error_m = error,
    newly_removed_tidal_physical_m = sum(lines$length_m[lines$tidal_removed & lines$measure_habitat]),
    remaining_inside_tidal_m = residual_m))
}

phase3_tidal_cutpoints <- function(before_tidal, mask, populations) {
  physical <- before_tidal[!before_tidal$removed & before_tidal$measure_habitat, ]
  boundary <- sf::st_boundary(sf::st_union(sf::st_geometry(mask)))
  geometry <- suppressWarnings(sf::st_intersection(sf::st_union(sf::st_geometry(physical)), boundary))
  geometry <- geometry[!sf::st_is_empty(geometry)]
  if (length(geometry)) {
    geometry <- suppressWarnings(sf::st_collection_extract(geometry, "POINT"))
    geometry <- suppressWarnings(sf::st_cast(geometry, "POINT"))
    geometry <- geometry[!duplicated(sf::st_as_binary(geometry))]
  }
  points <- sf::st_sf(tidal_id = sprintf("T%03d", seq_along(geometry)), geometry = geometry)
  if (nrow(points)) {
    # A cutoff must adjoin positive-length source on BOTH sides. A river
    # endpoint merely touching the seaward polygon boundary is not head of tide.
    inside <- upstream_line_parts(suppressWarnings(sf::st_intersection(physical, sf::st_union(sf::st_geometry(mask)))))
    outside <- upstream_line_parts(suppressWarnings(sf::st_difference(physical, sf::st_union(sf::st_geometry(mask)))))
    keep <- lengths(sf::st_is_within_distance(points, inside, dist = .0001)) > 0L &
      lengths(sf::st_is_within_distance(points, outside, dist = .0001)) > 0L
    points <- points[keep, ]
  }
  if (nrow(points)) {
    at <- sf::st_coordinates(points)
    points <- points[order(-at[, 2L], at[, 1L]), ]
    points$tidal_id <- sprintf("T%03d", seq_len(nrow(points)))
    hit <- sf::st_intersects(points, populations)
    keep <- lengths(hit) > 0L
    points <- points[keep, ]; hit <- hit[keep]
    points$tidal_id <- sprintf("T%03d", seq_len(nrow(points)))
    points$population_ids <- vapply(hit, function(x) paste(populations$pop_id[x], collapse = ";"), character(1))
    points$population_names <- vapply(hit, function(x) paste(populations$population[x], collapse = ";"), character(1))
    at <- sf::st_coordinates(sf::st_transform(points, 4326))
    points$longitude <- at[, 1L]; points$latitude <- at[, 2L]
  } else {
    points$population_ids <- points$population_names <- character()
    points$longitude <- points$latitude <- numeric()
  }
  points$interpretation <- rep("Intersection with mapped current tidal extent; not a surveyed head-of-tide", nrow(points))
  points
}

phase3_population_table <- function(populations, before, after_barriers, final, phase2_table, cfs_mask = NULL) {
  table <- sf::st_drop_geometry(populations)
  physical <- final[final$measure_habitat, ]
  measures <- list(
    habitat_length_by_pop(before[before$measure_habitat, ], populations, "baseline_untruncated_miles"),
    habitat_length_by_pop(after_barriers[after_barriers$measure_habitat & !after_barriers$removed, ], populations, "remaining_after_barriers_miles"),
    habitat_length_by_pop(physical[physical$barrier_removed, ], populations, "removed_upstream_barriers_miles"),
    habitat_length_by_pop(physical[physical$tidal_removed, ], populations, "removed_current_tidal_miles"),
    habitat_length_by_pop(physical[physical$retained, ], populations, "phase3_remaining_miles"),
    habitat_length_by_pop(physical[physical$retained & physical$source_class == "prediction", ], populations, "phase3_remaining_model_miles"),
    habitat_length_by_pop(physical[physical$retained & physical$source_class == "swifd_connector", ], populations, "phase3_remaining_swifd_miles"))
  for (measure in measures) table <- dplyr::left_join(table, measure, by = "pop_id")
  table$phase2_remaining_miles <- phase2_table$phase2_remaining_visible_miles[match(table$pop_id, phase2_table$pop_id)]
  table$new_barrier_removed_miles <- table$phase2_remaining_miles - table$remaining_after_barriers_miles
  table$change_from_phase2_miles <- table$phase3_remaining_miles - table$phase2_remaining_miles
  for (field in c("new_barrier_removed_miles", "change_from_phase2_miles"))
    table[[field]][abs(table[[field]]) < 1e-10] <- 0
  if (!is.null(cfs_mask)) {
    cfs_removed <- upstream_line_parts(suppressWarnings(sf::st_intersection(
      after_barriers[after_barriers$measure_habitat & !after_barriers$removed, ], sf::st_union(sf::st_geometry(cfs_mask)))))
    measure <- habitat_length_by_pop(cfs_removed, populations, "removed_CFS_tidal_miles")
    table <- dplyr::left_join(table, measure, by = "pop_id")
    table$removed_NWI_additional_tidal_miles <- pmax(0, table$removed_current_tidal_miles - table$removed_CFS_tidal_miles)
  }
  table$estimate_scope <- "Existing distribution geometry outside approved upstream barrier basins and mapped current tidal influence"
  balance <- table$baseline_untruncated_miles - table$removed_upstream_barriers_miles -
    table$removed_current_tidal_miles - table$phase3_remaining_miles
  if (any(abs(balance) > .00001) || any(table$new_barrier_removed_miles < -.00001))
    stop("Phase 3 mileage partition or no-restoration check failed")
  front <- c("pop_id", "population", "is_custom", "phase2_remaining_miles", "new_barrier_removed_miles",
    "removed_current_tidal_miles", "removed_CFS_tidal_miles", "removed_NWI_additional_tidal_miles",
    "phase3_remaining_miles", "phase3_remaining_model_miles", "phase3_remaining_swifd_miles",
    "change_from_phase2_miles", "baseline_untruncated_miles", "removed_upstream_barriers_miles")
  front <- intersect(front, names(table))
  table <- table[, c(front, setdiff(names(table), front)), drop = FALSE]
  table
}

phase3_map <- function(populations, remaining, applied, pending, tidal_polygons,
    tidal_points, title, length_summary = NULL, labels = FALSE) {
  if ("phase3_display_effective_cut" %in% names(applied)) applied <- applied[applied$phase3_display_effective_cut %in% TRUE, ]
  # Tidal polygons are drawn beneath the remaining linework; each map's data is
  # clipped to its own analysis boundary so coastal data cannot enlarge inland maps.
  tidal <- suppressWarnings(sf::st_intersection(tidal_polygons, sf::st_union(sf::st_geometry(populations))))
  tidal <- tidal[!sf::st_is_empty(tidal), ]
  if (!"tidal_source_family" %in% names(tidal)) tidal$tidal_source_family <- rep("WDFW/CFS current tidal extent", nrow(tidal))
  remaining$line_source <- ifelse(remaining$source_class == "prediction", "Remaining model",
    ifelse(remaining$source_class == "swifd_connector", "Remaining SWIFD patch", "Alignment (not mileage)"))
  remaining$width <- ifelse(remaining$source_class == "swifd_connector", .8,
    ifelse(remaining$source_class == "alignment_link", .25, .45))
  p <- ggplot2::ggplot() +
    ggplot2::geom_sf(data = populations, fill = "#f3f2eb", color = "#414c4b", linewidth = .5) +
    ggplot2::geom_sf(data = tidal, ggplot2::aes(fill = tidal_source_family), color = NA, alpha = .4) +
    ggplot2::scale_fill_manual(name = NULL, values = c("WDFW/CFS current tidal extent" = "#49b7b0", "NWI freshwater-tidal addition" = "#a5a0d8"),
      limits = c("WDFW/CFS current tidal extent", "NWI freshwater-tidal addition"), breaks = unique(tidal$tidal_source_family)) +
    ggplot2::geom_sf(data = remaining, ggplot2::aes(color = line_source, linetype = line_source, linewidth = width), show.legend = "line") +
    ggplot2::scale_linewidth_identity() +
    ggplot2::scale_color_manual(name = NULL, values = c("Remaining model" = "#126a91", "Remaining SWIFD patch" = "#e66b18", "Alignment (not mileage)" = "#776f68")) +
    ggplot2::scale_linetype_manual(name = NULL, values = c("Remaining model" = "solid", "Remaining SWIFD patch" = "solid", "Alignment (not mileage)" = "dotted")) +
    ggplot2::geom_sf(data = applied, shape = 21, fill = "#d72e32", color = "#333333", size = 3) +
    ggplot2::geom_sf(data = pending, shape = 24, fill = "#ffc24b", color = "#5b4c25", size = 3) +
    ggplot2::geom_sf(data = tidal_points, shape = 22, fill = "#087f8c", color = "white", size = 2.2, stroke = .3) +
    ggplot2::coord_sf(datum = sf::st_crs(4326)) + ggplot2::theme_bw(base_size = 11) +
    ggplot2::theme(plot.title = ggplot2::element_text(face = "bold", size = 15),
      plot.subtitle = ggplot2::element_text(size = 10), plot.caption = ggplot2::element_text(size = 8, hjust = 0),
      legend.position = "bottom", legend.text = ggplot2::element_text(size = 9),
      panel.grid.major = ggplot2::element_line(color = "#e1e7e8", linewidth = .15))
  add_labels <- function(plot, data, field, color = "#222222", size = 2.5) {
    if (!nrow(data)) return(plot)
    data <- sf::st_sf(label = as.character(data[[field]]), geometry = sf::st_geometry(data))
    plot + ggrepel::geom_text_repel(data = data, ggplot2::aes(label = label, geometry = geometry), stat = "sf_coordinates",
      color = color, size = size, fontface = "bold", seed = 914, box.padding = .25, point.padding = .15,
      min.segment.length = 0, segment.color = color, segment.size = .2, max.overlaps = Inf, max.time = 1, max.iter = 10000)
  }
  p <- add_labels(p, applied, "review_number")
  p <- add_labels(p, pending, "review_number", "#875200")
  if (!labels) p <- add_labels(p, tidal_points, "tidal_id", "#086f79", 2.1)
  if (labels) p <- p + ggplot2::geom_sf_label(data = suppressWarnings(sf::st_point_on_surface(populations)), ggplot2::aes(label = population), size = 2.3, alpha = .8, linewidth = .1)
  subtitle <- "PHASE 3: reviewed Total barriers + mapped current tidal influence"
  display_miles <- function(x) ifelse(abs(x) < .005, 0, x)
  if (!is.null(length_summary)) subtitle <- paste(subtitle, sprintf(
    "Phase 2 %.2f mi | Additional barriers %.2f mi | Tidal exclusion %.2f mi | Remaining %.2f mi",
    display_miles(length_summary$phase2_remaining_miles), display_miles(length_summary$new_barrier_removed_miles),
    display_miles(length_summary$removed_current_tidal_miles), display_miles(length_summary$phase3_remaining_miles)), sep = "\n")
  notes <- c("Red numbers: applied barriers. Amber triangle: reviewed Total held off the modeled channel.",
    "Teal: WDFW/CFS tidal extent. Violet: additional NWI freshwater-tidal channel. Teal T# squares: mapped cutoff intersections.",
    "CFS/WDFW 2025 service: maximum tidal-influence proxy with current connectivity; underlying mapping vintages vary.",
    "NWI imagery dates vary; additions are screened against current CFS restrictions and unresolved source areas.",
    "Disconnected, developed and unknown CFS areas are not removed.",
    "Mapped tidal boundaries are a review proxy for head of tide; they are not surveyed hydrodynamic limits.",
    "Blue: original model; orange: qualifying SWIFD patches. Dotted alignment links excluded from mileage.",
    "Sources: NOAA; WDFW/TerrainWorks; SWIFD; WDFW FPDSI; USGS; Cramer Fish Sciences; USFWS National Wetlands Inventory.")
  if (any(populations$pop_id == 290)) notes <- c("New checks: Woods Creek #10 and lower Olney Creek #331; West Fork Woods #914 is held if no channel lies within 50m.", notes)
  if (any(populations$pop_id == 283)) notes <- c("Additional upstream exclusion uses Woods Creek #10, downstream outside this NOAA population polygon.",
    "The source linework lies inside the approved Woods drainage mask; population outlines and drainage boundaries differ.", notes)
  if (!nrow(tidal)) notes <- c("No mapped current tidal polygon overlaps this analysis boundary.", notes)
  if (any(populations$pop_id == 1001)) notes <- c("Custom North Fork Stillaguamish watershed above Deer Creek; separate overlapping review area, not an official NOAA population.", notes)
  if (any(populations$pop_id %in% c(296, 1001))) notes <- c(
    "French Creek: lower Total gradient barrier #581 replaces #563 as the effective cut; farther-downstream #915 is Partial and held.",
    if (any(populations$pop_id == 296)) "Pilchuck Creek: Total waterfall #634 below Lake Cavanaugh; adjacent Partial waterfall #633 is excluded.",
    if (any(populations$pop_id == 296)) "The complete Lake Cavanaugh contributing drainage is included above #634; conflicting routes inside the lake are corrected.", notes)
  p + ggplot2::labs(title = title, subtitle = subtitle, x = NULL, y = NULL,
    caption = paste(vapply(notes, function(x) paste(strwrap(x, 130), collapse = "\n"), character(1)), collapse = "\n"))
}

phase3_render_maps <- function(populations, custom, remaining, applied, pending, tidal, points, table, output_dir) {
  all <- phase2_bind_rows(populations, custom)
  for_map <- function(pop, title, labels = FALSE) {
    ids <- pop$pop_id
    hit <- function(data) data[lengths(sf::st_intersects(data, sf::st_union(sf::st_geometry(pop)))) > 0L, ]
    phase3_map(pop, remaining[remaining$pop_id %in% ids, ], hit(applied), hit(pending), tidal,
      hit(points), title, if (length(ids) == 1L) table[table$pop_id == ids, ] else NULL, labels = labels)
  }
  habitat_save_map(for_map(populations, "North Puget Sound steelhead: Phase 3", TRUE), file.path(output_dir, "North_Puget_Sound_Steelhead_map.png"), 14, 14)
  grDevices::pdf(file.path(output_dir, "North_Puget_Sound_Steelhead_population_maps.pdf"), width = 12, height = 10)
  tryCatch({ for (i in seq_len(nrow(all))) {
    pop <- all[i, ]; p <- for_map(pop, pop$population)
    slug <- gsub("[^A-Za-z0-9]+", "_", pop$population)
    habitat_save_map(p, file.path(output_dir, "maps", paste0(pop$pop_id, "_", slug, ".png")), 12, 10)
    print(p)
  } }, finally = grDevices::dev.off())
  nooksack <- populations[grepl("Nooksack", populations$population), ]
  habitat_save_map(for_map(nooksack, "Nooksack steelhead: Phase 3"), file.path(output_dir, "Nooksack_Steelhead_map.png"), 12, 10)
  invisible(TRUE)
}

refresh_phase3_maps <- function(output_dir = "outputs/phase3_tidal_barriers") {
  file <- file.path(output_dir, "North_Puget_Sound_Steelhead_phase3.gpkg")
  read <- function(layer) phase2_geometry(sf::st_read(file, layer = layer, quiet = TRUE))
  points <- read("tidal_cutpoints")
  points <- points[!is.na(points$population_ids) & nzchar(points$population_ids), ]
  points$tidal_id <- sprintf("T%03d", seq_len(nrow(points)))
  sf::st_write(points, file, layer = "tidal_cutpoints", delete_layer = TRUE, quiet = TRUE)
  utils::write.csv(sf::st_drop_geometry(points), file.path(output_dir, "tidal_cutpoint_review.csv"), row.names = FALSE, na = "")
  phase3_render_maps(read("noaa_populations"), read("custom_review_watershed"), read("phase3_remaining"),
    read("applied_barriers"), read("held_target_barriers"), read("tidal_current_polygons"), points,
    utils::read.csv(file.path(output_dir, "population_phase3_miles.csv"), stringsAsFactors = FALSE), output_dir)
}

run_phase3_tidal_barriers <- function(phase2_dir = "outputs/phase2_reviewed_barriers",
    baseline_dir = "outputs/review_swifd_connections", output_dir = "outputs/phase3_tidal_barriers", cache_dir = "spatial_data") {
  habitat_require_packages()
  previous_snapshot <- phase3_check_previous_snapshot(cache_dir)
  lake_snapshot <- phase3_check_previous_snapshot(cache_dir, "lake_cavanaugh_review", "before_fix")
  preserved_dirs <- setdiff(list.dirs("outputs", recursive = FALSE, full.names = TRUE), output_dir)
  preserved <- phase2_source_manifest(preserved_dirs)
  read2 <- function(layer) phase2_geometry(sf::st_read(file.path(phase2_dir, "North_Puget_Sound_Steelhead_phase2.gpkg"), layer = layer, quiet = TRUE))
  populations <- read2("noaa_populations"); custom <- read2("custom_review_watershed")
  all_populations <- phase2_bind_rows(populations, custom)
  barriers <- read2("barrier_characteristics")
  old_masks <- read2("upstream_basin_masks"); old_anchors <- read2("downstream_anchors")
  edges <- phase2_geometry(sf::st_read(file.path(baseline_dir, "North_Puget_Sound_Steelhead.gpkg"), layer = "stream_matching_edges", quiet = TRUE))
  raw <- readRDS(file.path(cache_dir, "north_puget_swifd_bridges.rds"))$bridges$network
  woods <- load_phase3_woods_barriers()
  olney <- load_phase3_olney_barriers()
  french <- load_french_creek_review()
  pilchuck <- load_pilchuck_creek_review()
  tidal <- load_phase3_tidal_sources()
  freshwater_tidal <- load_phase3_freshwater_tidal()
  tidal$tidal_polygons$tidal_source_family <- "WDFW/CFS current tidal extent"
  freshwater_tidal$polygons$tidal_source_family <- "NWI freshwater-tidal addition"
  combined_tidal_polygons <- phase2_bind_rows(phase2_geometry(tidal$tidal_polygons), phase2_geometry(freshwater_tidal$polygons))
  combined_tidal_mask <- sf::st_sf(mask_id = "CFS_current_plus_screened_NWI_freshwater_tidal",
    geometry = sf::st_union(c(sf::st_geometry(tidal$mask), sf::st_geometry(freshwater_tidal$mask))))
  legacy_additions <- phase2_bind_rows(phase2_geometry(woods$barriers), phase2_geometry(olney$barriers))
  additions <- phase2_bind_rows(legacy_additions, phase2_geometry(french$barriers), phase2_geometry(pilchuck$barriers))
  barriers <- phase2_bind_rows(barriers[!barriers$review_record_key %in% additions$review_record_key, ], additions)
  barriers <- barriers[order(barriers$review_number), ]
  if (anyDuplicated(barriers$review_number) || anyDuplicated(barriers$review_record_key)) stop("Phase 3 barrier numbers must be unique")
  barriers$phase3_selected <- barriers$phase2_selected %in% TRUE
  new_applied_keys <- c(woods$masks$review_record_key, olney$masks$review_record_key,
    french$masks$review_record_key, pilchuck$masks$review_record_key)
  barriers$phase3_selected[barriers$review_record_key %in% new_applied_keys] <- TRUE
  selected <- barriers[barriers$phase3_selected, ]
  selected$explicit_total_passage <- selected$confirmed_total_for_scenario %in% TRUE
  selected$phase1_selected <- TRUE # local shared-validator current-scenario gate
  replace_anchor_keys <- c(legacy_additions$review_record_key,
    french$anchors$review_record_key, pilchuck$anchors$review_record_key)
  anchors <- phase2_bind_rows(old_anchors[!old_anchors$review_record_key %in% replace_anchor_keys, ],
    phase2_geometry(woods$anchors), phase2_geometry(olney$anchors),
    phase2_geometry(french$anchors), phase2_geometry(pilchuck$anchors))
  anchors <- anchors[!duplicated(anchors$anchor_id), ]
  message("Phase 3: rechecking reviewed barriers on the complete raw model + SWIFD source")
  graph <- cut_reviewed_barriers(raw, selected, edges, anchors)
  woods_graph <- prepare_phase3_woods_graph(graph$network, selected, edges)
  graph$network <- woods_graph$network
  pilchuck_graph <- prepare_pilchuck_creek_graph(graph$network, selected, edges)
  graph$network <- pilchuck_graph$network
  woods_masks <- phase2_geometry(woods$masks)
  woods_checked <- validate_nldi_basin_masks(woods_masks, graph$network, selected)
  if (nrow(woods_checked$masks) != nrow(woods_masks)) stop("A requested Woods mask failed complete upstream validation")
  reviewed_masks <- phase2_bind_rows(phase2_geometry(french$masks), phase2_geometry(pilchuck$masks))
  reviewed_checked <- validate_nldi_basin_masks(reviewed_masks, graph$network, selected)
  if (nrow(reviewed_checked$masks) != nrow(reviewed_masks)) {
    print(reviewed_checked$audit)
    stop("A requested Stillaguamish mask failed complete upstream validation")
  }
  ordinary_masks <- phase2_bind_rows(old_masks, phase2_geometry(woods_checked$masks), phase2_geometry(reviewed_checked$masks))
  ordinary_masks <- ordinary_masks[!duplicated(ordinary_masks$review_record_key), ]
  # Preserve the independently proved downstream portion at the terminal dam.
  terminal <- validate_phase2_terminal_dam(ordinary_masks[ordinary_masks$review_record_key == "user_override:Culmback_Dam", ], graph$network, selected, edges)
  if (nrow(terminal$masks) != 1L) stop("Culmback terminal proof failed")
  network <- terminal$network
  approved_keys <- ordinary_masks$review_record_key
  causes <- strsplit(ifelse(is.na(network$removed_by_barrier), "", network$removed_by_barrier), ";", fixed = TRUE)
  network$removed_by_barrier <- vapply(lapply(causes, intersect, y = approved_keys), paste, collapse = ";", character(1))
  network$removed <- nzchar(network$removed_by_barrier); network$retained <- !network$removed
  unresolved <- strsplit(ifelse(is.na(network$unresolved_by_barrier), "", network$unresolved_by_barrier), ";", fixed = TRUE)
  network$unresolved_by_barrier <- vapply(lapply(unresolved, setdiff, y = c("", approved_keys, olney$masks$review_record_key)), paste, collapse = ";", character(1))
  network$unresolved_direction <- nzchar(network$unresolved_by_barrier) & network$retained
  ordinary <- apply_upstream_basin_masks(network, ordinary_masks, barrier_points = selected)
  olney_result <- apply_phase3_olney_mask(ordinary$network, olney$masks, selected, edges)
  after_barriers <- olney_result$network
  masks <- phase2_bind_rows(ordinary$masks, phase2_geometry(olney_result$mask))
  saveRDS(list(network = after_barriers, masks = masks, barriers = barriers,
    ordinary_audit = ordinary$barrier_audit, olney_audit = olney_result$barrier_audit,
    olney_proof = olney_result$proof_audit, woods_graph_audit = woods_graph$audit,
    pilchuck_graph_audit = pilchuck_graph$audit, stillaguamish_validation = reviewed_checked$audit),
    file.path(cache_dir, "phase3", "barrier_stage.rds"))
  message("Phase 3: full upstream cuts checked; excluding mapped current tidal influence")
  # Trim only far-away source geometry. The 1km margin around the ENTIRE raw
  # network keeps the trim edge away from every source line and cutoff point.
  source_bounds <- sf::st_buffer(sf::st_as_sfc(sf::st_bbox(raw)), 1000)
  tidal_mask <- phase2_geometry(suppressWarnings(sf::st_intersection(combined_tidal_mask, source_bounds)))
  region_union <- sf::st_union(sf::st_geometry(all_populations))
  region_tidal <- phase2_geometry(suppressWarnings(sf::st_intersection(combined_tidal_polygons, region_union)))
  region_tidal <- region_tidal[!sf::st_is_empty(region_tidal), ]
  tidal_result <- phase3_apply_tidal_mask(after_barriers, tidal_mask)
  final_global <- tidal_result$network
  before <- habitat_clip(raw, all_populations)
  before_tide <- habitat_clip(after_barriers, all_populations)
  final <- habitat_clip(final_global, all_populations)
  old_table <- utils::read.csv(file.path(phase2_dir, "population_phase2_miles.csv"), stringsAsFactors = FALSE)
  table <- phase3_population_table(all_populations, before, before_tide, final, old_table, cfs_mask = tidal$mask)
  cutpoints <- phase3_tidal_cutpoints(after_barriers, tidal_mask, all_populations)
  applied_keys <- masks$review_record_key
  barriers$phase3_applied <- barriers$review_record_key %in% applied_keys
  barriers$phase3_cut_status <- ifelse(barriers$phase3_applied, "applied_complete_upstream_basin",
    ifelse(barriers$phase3_selected, "pending_complete_upstream_basin", "not_selected"))
  barriers$phase3_cut_status[barriers$review_number == 914L & !barriers$phase3_applied] <- "reported_Total_held_no_model_channel_within_50m"
  barriers$phase3_display_effective_cut <- barriers$phase3_applied
  if (any(barriers$review_number == 331L & barriers$phase3_applied)) {
    barriers$phase3_display_effective_cut[barriers$review_number == 379L] <- FALSE
    barriers$phase3_cut_status[barriers$review_number == 379L] <- "approved_mask_retained_as_evidence_upstream_of_effective_Olney331_cut"
  }
  if (any(barriers$review_number == 581L & barriers$phase3_applied)) {
    barriers$phase3_display_effective_cut[barriers$review_number == 563L] <- FALSE
    barriers$phase3_cut_status[barriers$review_number == 563L] <- "approved_mask_retained_as_evidence_upstream_of_effective_French581_cut"
  }
  barriers$phase3_cut_status[barriers$review_number == 915L] <- "reported_Partial_held_original_point_outside_50m_no_cut"
  applied <- barriers[barriers$phase3_applied, ]
  pending <- barriers[barriers$review_number == 914L & !barriers$phase3_applied, ]
  remaining <- final[final$retained, ]; removed <- final[final$removed, ]
  dir.create(file.path(output_dir, "maps"), recursive = TRUE, showWarnings = FALSE)
  write <- function(x, name) utils::write.csv(if (inherits(x, "sf")) sf::st_drop_geometry(x) else x,
    file.path(output_dir, name), row.names = FALSE, na = "")
  write(table, "population_phase3_miles.csv")
  if (!is.null(previous_snapshot)) {
    previous_file <- previous_snapshot$snapshot_path[basename(previous_snapshot$original_path) == "population_phase3_miles.csv"]
    if (length(previous_file) != 1L) stop("Frozen previous Phase3 mileage table is not unique")
    previous <- utils::read.csv(previous_file, stringsAsFactors = FALSE)
    changes <- table[, c("pop_id", "population", "phase3_remaining_miles")]
    changes$previous_phase3_miles <- previous$phase3_remaining_miles[match(changes$pop_id, previous$pop_id)]
    changes$additional_removed_miles <- changes$previous_phase3_miles - changes$phase3_remaining_miles
    changes <- changes[abs(changes$additional_removed_miles) > 1e-8, ]
    write(changes, "stillaguamish_mileage_changes.csv")
  }
  if (!is.null(lake_snapshot)) {
    previous_file <- lake_snapshot$snapshot_path[basename(lake_snapshot$original_path) == "population_phase3_miles.csv"]
    if (length(previous_file) != 1L) stop("Frozen Lake Cavanaugh comparison table is not unique")
    previous <- utils::read.csv(previous_file, stringsAsFactors = FALSE)
    changes <- table[, c("pop_id", "population", "phase3_remaining_miles")]
    changes$before_lake_correction_miles <- previous$phase3_remaining_miles[match(changes$pop_id, previous$pop_id)]
    changes$additional_removed_miles <- changes$before_lake_correction_miles - changes$phase3_remaining_miles
    write(changes[abs(changes$additional_removed_miles) > 1e-8, ], "lake_cavanaugh_mileage_change.csv")
  }
  decision <- sf::st_drop_geometry(barriers)
  front <- intersect(c("review_number", "SiteId", "StreamName", "verification", "report_blockage", "reported_feature_type", "reported_height_m",
    "phase3_selected", "phase3_applied", "phase3_cut_status", "phase2_applied", "report_url", "review_record_key"), names(decision))
  decision <- decision[, c(front, setdiff(names(decision), front)), drop = FALSE]
  write(decision, "phase3_barrier_decisions.csv"); write(decision[decision$phase3_applied, ], "phase3_cut_barriers.csv")
  write(cutpoints, "tidal_cutpoint_review.csv")
  write(phase2_bind_rows(woods$audit, olney$audit, french$audit, pilchuck$audit), "targeted_barrier_review.csv")
  write(phase2_bind_rows(french$audit, pilchuck$audit), "stillaguamish_barrier_review.csv")
  write(pilchuck_graph$audit, "pilchuck_graph_direction_review.csv")
  if (!is.null(pilchuck$lake_cavanaugh_audit)) write(pilchuck$lake_cavanaugh_audit, "lake_cavanaugh_correction_review.csv")
  write(reviewed_checked$audit, "stillaguamish_basin_validation.csv")
  write(woods_graph$audit, "woods_graph_direction_review.csv")
  write(phase2_bind_rows(ordinary$barrier_audit, olney_result$barrier_audit), "upstream_basin_audit.csv")
  write(olney_result$proof_audit, "olney_downstream_proof.csv")
  write(tidal$audit, "tidal_source_audit.csv")
  if (!is.null(tidal$classification_audit)) write(tidal$classification_audit, "tidal_classification_audit.csv")
  if (!is.null(tidal$geometry_audit)) write(tidal$geometry_audit, "tidal_geometry_audit.csv")
  write(freshwater_tidal$audit, "freshwater_tidal_source_audit.csv")
  if (!is.null(freshwater_tidal$source_vintage_join)) write(freshwater_tidal$source_vintage_join, "freshwater_tidal_source_vintage.csv")
  write(preserved, "preserved_source_manifest.csv")
  file <- file.path(output_dir, "North_Puget_Sound_Steelhead_phase3.gpkg")
  sf::st_write(populations, file, layer = "noaa_populations", delete_dsn = TRUE, quiet = TRUE)
  gis <- list(custom_review_watershed = custom, phase3_extent_status = final, phase3_remaining = remaining,
    phase3_removed_upstream = final[final$barrier_removed, ], phase3_removed_tidal = final[final$tidal_removed, ],
    before_tidal_extent_status = before_tide, barrier_characteristics = barriers, applied_barriers = applied,
    held_target_barriers = pending, upstream_basin_masks = masks, downstream_anchors = graph$downstream_anchors,
    tidal_current_polygons = region_tidal, tidal_clip_mask = tidal_mask, tidal_cutpoints = cutpoints)
  for (name in names(gis)) sf::st_write(phase3_gpkg_fields(gis[[name]]), file, layer = name, quiet = TRUE)
  phase3_render_maps(populations, custom, remaining, applied, pending, region_tidal, cutpoints, table, output_dir)
  jsonlite::write_json(list(phase = "Phase 3: reviewed Total barriers, French/Pilchuck Creek corrections and mapped current tidal influence",
    original_population_count = 16L, custom_review_watershed_count = 1L, applied_barriers = nrow(applied),
    new_applied_review_numbers = barriers$review_number[barriers$review_record_key %in% new_applied_keys],
    held_review_numbers = pending$review_number, tidal_summary = tidal_result$summary,
    tidal_source_notes = c(tidal$notes, freshwater_tidal$notes), generated_utc = format(Sys.time(), tz = "UTC", usetz = TRUE)),
    file.path(output_dir, "run_metadata.json"), auto_unbox = TRUE, pretty = TRUE)
  writeLines(c("# Phase 3: reviewed barriers and mapped current tidal influence", "",
    "This is the latest North Puget Sound review scenario. Earlier outputs remain byte-for-byte unchanged.",
    "Start with maps/ or North_Puget_Sound_Steelhead_population_maps.pdf (16 NOAA populations plus the custom Stillaguamish area).", "",
    "## Barrier review", "",
    "- Woods Creek #10: field-verified Total waterfall (reported height 18.3 m). Independent same-stream routing resolves the old channel-match hold.",
    "- Olney Creek #331: lowest confirmed Total, below #379. The complete upstream basin removes disconnected headwaters too.",
    "- West Fork Woods #914: source-reported Total but no modeled/SWIFD channel within 50 m; retained at its original location as an amber review point, without a forced snap or cut.",
    "- French Creek #581: unverified source-reported Total gradient barrier, about 2.025 km downstream of the previous #563 cut. Its 10.59 m own-channel snap and complete upstream basin pass the unchanged safeguards. Applies to the official Stillaguamish and custom North Fork watershed.",
    "- French Creek #915 (Site 890155): farther downstream but reported Partial and 85.13 m from the physical modeled channel; original point retained in the review table without a snap or cut.",
    "- Pilchuck Creek #634 (Site 890682): source-reported Total waterfall below Lake Cavanaugh; the adjacent #633 waterfall is Partial and is excluded. This is Pilchuck Creek in the Stillaguamish basin, separate from the NOAA Pilchuck River population.",
    "- Lake Cavanaugh correction: the original coarse routing split two artificial paths inside the same named lake and wrongly exempted part of Lake Creek. The #634 basin now includes the verified contributing lake catchment. Fine WDFW channel geometry and Ecology's documented lake outlet to Lake Creek/Pilchuck corroborate the correction; genuine neighboring Deer drainage remains protected.",
    "- The lower Pilchuck Creek gradient Total #628 remains pending and unchanged; this review specifically applies the requested waterfall #634. Source metadata and the independent downstream ordering are retained in stillaguamish_barrier_review.csv.",
    "- Previous Phase 2 passage corrections and approved cuts remain in effect.", "",
    "The Woods drainage mask also removes 3.10 miles inside NOAA's Pilchuck polygon while barrier #10 lies outside that outline. The Pilchuck map labels this watershed/population-boundary difference.", "",
    "phase3_barrier_decisions.csv contains all numbered source records. targeted_barrier_review.csv, woods_graph_direction_review.csv, olney_downstream_proof.csv and upstream_basin_audit.csv record the checks.", "",
    "stillaguamish_barrier_review.csv, stillaguamish_basin_validation.csv and pilchuck_graph_direction_review.csv record the French/Pilchuck checks. The previous Phase 3 version is frozen under spatial_data/stillaguamish_review/previous_phase3/ with a file-hash manifest; the current results stay in this one output folder.", "",
    "French_Creek_barrier_detail.png and Pilchuck_Creek_barrier_detail.png show the full upstream basins, removed source reaches, effective cuts and nearby excluded records. stillaguamish_mileage_changes.csv compares this revision with the frozen previous Phase 3.", "",
    "lake_cavanaugh_mileage_change.csv isolates the latest missed-creek correction against the saved before-fix revision under spatial_data/lake_cavanaugh_review/before_fix/. Source geometry remains unchanged; only upstream exclusion at #634 is extended.", "",
    "## Tidal exclusion", "",
    "Teal polygons show approved WDFW/CFS current tidal influence. Violet polygons add independently mapped NWI freshwater-tidal channel. Teal T# squares match tidal_cutpoint_review.csv.",
    "For crowded markers, use tidal_cutoff_details.pdf or tidal_details/; tidal_detail_index.csv links close-up pages to the T# identifiers.",
    "The source is WDFW/Cramer Fish Sciences CFS_Tidal_Restrictions_2025, polygon layers 2 and 3:",
    "https://geodataservices.wdfw.wa.gov/arcgis/rest/services/HP_Projects/CFS_Tidal_Restrictions_2025/MapServer", "",
    "Selection uses landscape connectivity: unrestricted, partially restricted and significantly restricted tidal areas. Disconnected, developed, unknown, conflicting or untrusted-geometry areas are deferred.",
    "USFWS National Wetlands Inventory R1 (Riverine Tidal) supplements freshwater tidal channels beyond the CFS footprint. Older NWI source imagery does not override known current CFS restrictions or unresolved source areas; overlaps are deferred. Original mapping years are retained in freshwater_tidal_source_vintage.csv.",
    "The source represents maximum mapped tidal influence with current connectivity; underlying mapping dates vary. It includes freshwater tidal areas and is a regional review proxy, not a surveyed daily-tide or discharge-independent hydraulic head-of-tide boundary.",
    "Only positively mapped tidal areas are excluded. Connection to tidal water alone never deletes an entire upstream stream component.",
    "tidal_source_audit.csv records source checks. Original attributes, dates, raw source batches and geometry-quality holds are preserved under spatial_data/phase3/tidal_sources/.", "",
    "## Mileage and GIS", "",
    "population_phase3_miles.csv separates Phase 2 miles, additional barrier exclusions, tidal exclusions (CFS and additional NWI), and final miles by source. Alignment links do not count as habitat.",
    "The mileage balance is baseline = upstream barrier exclusions + additional tidal exclusions + remaining distribution. Barrier exclusions take precedence where masks overlap.",
    "The custom Stillaguamish area overlaps official populations; do not add its mileage to their totals.",
    "North_Puget_Sound_Steelhead_phase3.gpkg contains the geometry used for maps, tidal mask/current polygons/cutpoints, applied and held barriers, upstream basins, and source-class linework.",
    "preserved_source_manifest.csv verifies that earlier scenarios are unchanged.", "",
    "Reproduce from the North_Puget_Sound folder with Rscript Phase3_Tidal_Barriers.R after preparing the explicit source bundles."),
    file.path(output_dir, "README.md"))
  capture.output(utils::sessionInfo(), file = file.path(output_dir, "sessionInfo.txt"))
  phase2_check_preserved(preserved)
  phase3_check_previous_snapshot(cache_dir)
  phase3_check_previous_snapshot(cache_dir, "lake_cavanaugh_review", "before_fix")
  message("Phase 3 exported; all previous scenarios unchanged")
  invisible(list(table = table, barriers = barriers, output_dir = output_dir))
}
