# Review full source geometry and SWIFD patches without tracing or cutting it.
# Barrier matching is independent: proposed cuts are only attributes at this stage.

habitat_review_map <- function(populations, lines, barriers, title, labels = FALSE,
                               overview = FALSE) {
  source_labels <- c(prediction = "Raw occupancy model", swifd_connector = "SWIFD gap patch",
                     alignment_link = "Endpoint alignment (not mileage)")
  lines$map_source <- unname(source_labels[lines$source_class])
  lines$map_width <- ifelse(lines$source_class == "swifd_connector", .8,
                            ifelse(lines$source_class == "alignment_link", .25, .45))
  barriers$barrier_review_status <- ifelse(barriers$eligible_for_future_cut,
    "Cut eligible (not applied)", ifelse(barriers$future_cut_requested,
      "Cut requested; match pending", ifelse(barriers$snap_accepted,
        "Stream match confirmed", "Stream match needs review")))
  p <- ggplot2::ggplot() +
    ggplot2::geom_sf(data = populations, fill = "#f3f2eb", color = "#414c4b", linewidth = .5) +
    ggplot2::geom_sf(data = lines,
      ggplot2::aes(color = map_source, linetype = map_source, linewidth = map_width)) +
    ggplot2::scale_linewidth_identity() +
    ggplot2::scale_color_manual(name = "Untruncated distribution", values = c(
      "Raw occupancy model" = "#126a91", "SWIFD gap patch" = "#e66b18",
      "Endpoint alignment (not mileage)" = "#776f68"), drop = FALSE) +
    ggplot2::scale_linetype_manual(name = "Untruncated distribution", values = c(
      "Raw occupancy model" = "solid", "SWIFD gap patch" = "solid",
      "Endpoint alignment (not mileage)" = "dotted"), drop = FALSE) +
    ggplot2::geom_sf(data = barriers, ggplot2::aes(fill = barrier_review_status),
      shape = 21, size = if (overview) 2.6 else 3.2, color = "#333333", stroke = .35) +
    ggplot2::scale_fill_manual(name = "Numbered barriers (no cuts applied)", values = c(
      "Cut eligible (not applied)" = "#d72e32", "Cut requested; match pending" = "#efbd4d",
      "Stream match confirmed" = "#5c9e7d", "Stream match needs review" = "#dddddd"),
      limits = c("Cut eligible (not applied)", "Cut requested; match pending",
        "Stream match confirmed", "Stream match needs review"), drop = FALSE) +
    ggplot2::coord_sf(datum = sf::st_crs(4326), expand = TRUE) +
    ggplot2::labs(title = title,
      subtitle = "FULL raw model + SWIFD gap patches | NO barrier truncation applied",
      x = NULL, y = NULL,
      caption = paste("Blue: all raw model reaches. Orange: SWIFD patches; both attachments within 100 m.",
        "Barrier numbers match barrier_review.csv; gray/amber locations require stream-assignment review.",
        "Stream matches use local channel geometry and named hydrography. Unknown passability never establishes total blockage.",
        "Sources: NOAA; WDFW/TerrainWorks; SWIFD; FPDSI; WDFW NHD-with-LLID; Gorge Dam: WAC 173-201A-602.", sep = "\n")) +
    ggplot2::theme_bw(base_size = 11) +
    ggplot2::theme(panel.grid.major = ggplot2::element_line(color = "#e1e7e8", linewidth = .15),
      plot.title = ggplot2::element_text(face = "bold", size = 16),
      plot.subtitle = ggplot2::element_text(size = 10),
      plot.caption = ggplot2::element_text(size = 8, hjust = 0),
      legend.position = "bottom", legend.box = "vertical") +
    ggplot2::guides(color = ggplot2::guide_legend(order = 1),
      linetype = ggplot2::guide_legend(order = 1), fill = ggplot2::guide_legend(order = 2, nrow = 2))
  if (nrow(barriers)) {
    # Labels use the same regional integer on every overlapping population map.
    # Place labels just above the snapped symbol so multi-digit IDs remain readable.
    p <- p + ggplot2::geom_sf_text(data = barriers,
      ggplot2::aes(label = review_number), color = "#111111",
      size = if (overview) 1.7 else 2.2, nudge_y = if (overview) 400 else 100,
      fontface = "bold", check_overlap = FALSE)
  }
  if (labels) {
    centers <- suppressWarnings(sf::st_point_on_surface(populations))
    p <- p + ggplot2::geom_sf_label(data = centers, ggplot2::aes(label = population),
      size = 2.3, alpha = .8, linewidth = .1, label.padding = grid::unit(.12, "lines"))
  }
  p
}

habitat_barrier_membership <- function(points, populations) {
  membership <- sf::st_intersects(points, populations)
  points$population_ids <- vapply(membership,
    function(index) paste(populations$pop_id[index], collapse = ";"), character(1))
  points$population_names <- vapply(membership,
    function(index) paste(populations$population[index], collapse = ";"), character(1))
  points
}

habitat_export_untruncated_review <- function(populations, steelhead, swifd, bridges,
                                             barriers, output_dir, metadata,
                                             stream_reference = swifd) {
  dir.create(file.path(output_dir, "maps"), recursive = TRUE, showWarnings = FALSE)
  if (!exists("prepare_barrier_review", mode = "function")) {
    stop("Source R/barrier_review.R before exporting the untruncated review")
  }
  # Use the pre-barrier network directly. Never read retained/removed trace caches.
  complete_network <- bridges$network
  if (!all(c("source_class", "measure_habitat") %in% names(complete_network))) {
    stop("The SWIFD network is missing source provenance")
  }
  if (any(c("retained", "removed", "unresolved") %in% names(complete_network))) {
    stop("Expected the full pre-barrier SWIFD network, not a barrier trace")
  }
  raw <- habitat_clip(steelhead, populations)
  patched <- habitat_clip(complete_network, populations)
  routes <- patched[patched$source_class == "swifd_connector", ]
  links <- patched[patched$source_class == "alignment_link", ]
  model <- patched[patched$source_class == "prediction", ]
  table <- sf::st_drop_geometry(populations)
  measures <- list(habitat_length_by_pop(raw, populations, "raw_model_miles"),
    habitat_length_by_pop(model, populations, "preserved_model_miles"),
    habitat_length_by_pop(routes, populations, "swifd_added_miles"),
    habitat_length_by_pop(patched[patched$measure_habitat, ], populations, "patched_untruncated_miles"),
    habitat_length_by_pop(links, populations, "alignment_link_miles_not_counted"))
  for (measure in measures) table <- dplyr::left_join(table, measure, by = "pop_id")
  if (any(abs(table$raw_model_miles - table$preserved_model_miles) > .001)) {
    stop("The review must preserve every original model mile")
  }
  if (any(abs(table$patched_untruncated_miles - table$raw_model_miles - table$swifd_added_miles) > .001)) {
    stop("Untruncated model plus SWIFD length partition failed")
  }
  table$barrier_truncation_applied <- FALSE
  table$estimate_scope <- "Untruncated geometry only; not barrier-accessible habitat"
  table <- table[order(table$population), ]
  utils::write.csv(table, file.path(output_dir, "population_distribution_miles.csv"), row.names = FALSE)

  previous_table <- file.path(output_dir, "barrier_review.csv")
  previous_numbers <- if (file.exists(previous_table))
    utils::read.csv(previous_table, stringsAsFactors = FALSE) else NULL
  barrier_review <- prepare_barrier_review(barriers, complete_network,
    swifd_reference = stream_reference, snap_distance = 50,
    previous_review_numbers = previous_numbers)
  snapped <- habitat_barrier_membership(barrier_review$points, populations)
  originals <- habitat_barrier_membership(barrier_review$audit, populations)
  snapped$truncation_applied <- FALSE
  originals$truncation_applied <- FALSE
  utils::write.csv(sf::st_drop_geometry(snapped),
    file.path(output_dir, "barrier_review.csv"), row.names = FALSE, na = "")
  rejected <- originals[is.na(originals$review_number), ]
  utils::write.csv(sf::st_drop_geometry(rejected),
    file.path(output_dir, "barrier_snap_rejections.csv"), row.names = FALSE, na = "")
  utils::write.csv(sf::st_drop_geometry(barrier_review$candidate_matches),
    file.path(output_dir, "barrier_match_candidates.csv"), row.names = FALSE, na = "")
  jsonlite::write_json(metadata$barrier_field_definitions,
    file.path(output_dir, "barrier_field_definitions.json"), auto_unbox = TRUE, pretty = TRUE, null = "null")

  in_pop <- lengths(sf::st_intersects(snapped, sf::st_union(populations))) > 0
  map_barriers <- snapped[in_pop, ]
  overview <- habitat_review_map(populations, patched, map_barriers,
    "North Puget Sound steelhead: untruncated review", labels = TRUE, overview = TRUE)
  habitat_save_map(overview, file.path(output_dir, "North_Puget_Sound_Steelhead_map.png"), 14, 14)
  pdf_path <- file.path(output_dir, "North_Puget_Sound_Steelhead_population_maps.pdf")
  grDevices::pdf(pdf_path, width = 11, height = 10, onefile = TRUE)
  tryCatch({
    for (i in seq_len(nrow(populations))) {
      pop <- populations[i, ]
      pop_lines <- patched[patched$pop_id == pop$pop_id, ]
      pop_barriers <- snapped[lengths(sf::st_intersects(snapped, pop)) > 0, ]
      p <- habitat_review_map(pop, pop_lines, pop_barriers, pop$population)
      slug <- gsub("[^A-Za-z0-9]+", "_", pop$population)
      habitat_save_map(p, file.path(output_dir, "maps", paste0(pop$pop_id, "_", slug, ".png")), 11, 10)
      print(p)
    }
  }, finally = grDevices::dev.off())
  nooksack <- populations[grepl("Nooksack", populations$population, ignore.case = TRUE), ]
  if (nrow(nooksack)) {
    b <- snapped[lengths(sf::st_intersects(snapped, sf::st_union(nooksack))) > 0, ]
    p <- habitat_review_map(nooksack, patched[patched$pop_id %in% nooksack$pop_id, ], b,
      "Nooksack steelhead: untruncated review", labels = TRUE)
    habitat_save_map(p, file.path(output_dir, "Nooksack_Steelhead_map.png"), 12, 10)
  }
  # Replace the old cut-status GeoPackage, including its obsolete layers.
  gpkg <- file.path(output_dir, "North_Puget_Sound_Steelhead.gpkg")
  sf::st_write(populations, gpkg, layer = "noaa_populations", delete_dsn = TRUE, quiet = TRUE)
  layers <- list(raw_occupancy_model = raw, steelhead_patched_untruncated = patched,
    swifd_gap_routes = routes, endpoint_alignment_links = links,
    barriers_snapped = snapped, barriers_original = originals,
    barrier_match_candidates = barrier_review$candidate_matches,
    stream_matching_edges = barrier_review$stream_edges,
    stream_identity_samples = barrier_review$identity_samples)
  for (name in names(layers)) {
    sf::st_write(layers[[name]], gpkg, layer = name, quiet = TRUE)
  }
  endpoints <- bridges$endpoint_audit
  endpoints <- endpoints[lengths(sf::st_intersects(endpoints, sf::st_union(populations))) > 0, ]
  sf::st_write(endpoints, gpkg, layer = "gap_endpoint_audit", quiet = TRUE)
  utils::write.csv(sf::st_drop_geometry(endpoints),
    file.path(output_dir, "gap_endpoint_audit.csv"), row.names = FALSE)
  connector_audit <- bridges$bridge_audit
  connector_audit$population_ids <- vapply(connector_audit$bridge_id, function(id) {
    paste(sort(unique(c(routes$pop_id[!is.na(routes$bridge_id) & routes$bridge_id == id],
      links$pop_id[!is.na(links$bridge_id) & links$bridge_id == id]))), collapse = ";")
  }, character(1))
  utils::write.csv(connector_audit, file.path(output_dir, "swifd_connector_audit.csv"), row.names = FALSE)
  metadata$review_checkpoint <- TRUE
  metadata$barrier_truncation_applied <- FALSE
  metadata$barrier_policy <- paste("Review only: explicit total-passability evidence or user-requested Gorge Dam,",
    "plus unique positively confirmed own-stream identity and no junction ambiguity; NO cuts applied")
  metadata$unknown_passability_policy <- "Unknown or blank passability is never classified as total"
  metadata$parameters <- list(swifd_endpoint_distance_m = 100, barrier_snap_distance_m = 50)
  metadata$stream_identity_controls <- barrier_review$settings$identity
  metadata$bridge_summary <- bridges$summary
  metadata$bridge_settings <- bridges$settings
  metadata$barrier_summary <- list(source_records = nrow(originals), snapped_numbered_records = nrow(snapped),
    rejected_records = nrow(rejected), proposed_later_cuts = sum(snapped$future_cut_candidate), actual_cuts = 0)
  metadata$barrier_summary$confirmed_stream_assignments <- sum(snapped$snap_accepted)
  metadata$barrier_summary$assignments_requiring_review <- sum(!snapped$snap_accepted)
  metadata$barrier_summary$requested_later_cuts <- sum(snapped$future_cut_requested)
  metadata$barrier_summary$candidate_pairs_reviewed <- nrow(barrier_review$candidate_matches)
  metadata$review_colors <- list(prediction = "blue", swifd_connector = "orange", alignment_link = "dotted taupe")
  metadata$generated_utc <- format(Sys.time(), tz = "UTC", usetz = TRUE)
  jsonlite::write_json(metadata, file.path(output_dir, "run_metadata.json"), auto_unbox = TRUE, pretty = TRUE, null = "null")
  capture.output(utils::sessionInfo(), file = file.path(output_dir, "sessionInfo.txt"))
  # This obsolete audit describes applied cuts and must not survive this export.
  old_cut_audit <- file.path(output_dir, "natural_barrier_audit.csv")
  if (file.exists(old_cut_audit)) unlink(old_cut_audit)
  message("Untruncated review exported: all original model reaches + SWIFD patches; ZERO barrier cuts")
  invisible(list(lengths = table, populations = populations, habitat = patched,
    barriers = snapped, barrier_review = barrier_review, map = overview, output_dir = output_dir))
}
