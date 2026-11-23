# Read-only detail figures from the saved final barrier partition. These plots
# never rerun snapping, direction classification, watershed cuts or mileage.
render_stillaguamish_details <- function(output_dir = "outputs/phase3_tidal_barriers") {
  packages <- c("sf", "ggplot2", "ggrepel", "gridExtra")
  missing <- packages[!vapply(packages, requireNamespace, logical(1), quietly = TRUE)]
  if (length(missing)) stop("Missing detail-map packages: ", paste(missing, collapse = ", "))
  gpkg <- file.path(output_dir, "North_Puget_Sound_Steelhead_phase3.gpkg")
  stage_file <- "spatial_data/phase3/barrier_stage.rds"
  protected <- c(gpkg, stage_file, file.path(output_dir, c("population_phase3_miles.csv",
    "phase3_barrier_decisions.csv", "upstream_basin_audit.csv", "stillaguamish_barrier_review.csv")))
  if (!all(file.exists(protected))) stop("Finish the revised final Phase3 export before rendering details")
  hashes <- unname(tools::md5sum(protected))
  on.exit({
    if (!identical(unname(tools::md5sum(protected)), hashes))
      stop("A scientific source changed during detail rendering; rerender only after export is frozen")
  }, add = TRUE)
  stage <- readRDS(stage_file)
  network <- stage$network
  masks <- sf::st_read(gpkg, layer = "upstream_basin_masks", quiet = TRUE)
  points <- sf::st_read(gpkg, layer = "barrier_characteristics", quiet = TRUE)
  points <- sf::st_sf(sf::st_drop_geometry(points), geometry = sf::st_geometry(points))
  crs <- sf::st_crs(network)
  masks <- sf::st_transform(masks, crs)
  points <- sf::st_transform(points, crs)
  if (sf::st_is_longlat(network) || !all(c("removed", "measure_habitat", "source_class") %in% names(network)))
    stop("Use the final projected source-tagged barrier partition")
  specifications <- list(
    list(stream = "French Creek", target = 581L, context = c(581L, 563L, 915L),
      file = "French_Creek_barrier_detail.png", watershed = "North Fork Stillaguamish River",
      note = "#581 is about 2.025 km downstream of the former #563 cut. #915 is farther downstream but Partial and 85.13 m from the physical model channel; it is shown at its original source location."),
    list(stream = "Pilchuck Creek", target = 634L, context = c(634L, 633L),
      file = "Pilchuck_Creek_barrier_detail.png", watershed = "Stillaguamish basin; separate from Pilchuck River",
      note = "#634 is the source-reported Total waterfall below Lake Cavanaugh. Its complete contributing lake drainage is included after correcting conflicting routes inside the lake. The adjacent #633 waterfall is Partial and does not authorize a cut. This figure shows Pilchuck CREEK, not the NOAA Pilchuck RIVER population."))
  crop <- function(data, box) {
    near <- lengths(sf::st_intersects(data, box)) > 0L
    data <- data[near, , drop = FALSE]
    if (!nrow(data)) return(data)
    result <- suppressWarnings(sf::st_intersection(data, box))
    result[!sf::st_is_empty(result), , drop = FALSE]
  }
  rendered <- character()
  for (spec in specifications) {
    context <- points[match(spec$context, points$review_number), ]
    target <- context[context$review_number == spec$target, ]
    if (anyNA(context$review_number) || nrow(target) != 1L || target$report_blockage != "Total" ||
        !isTRUE(target$phase3_applied) || !isTRUE(target$phase3_display_effective_cut))
      stop("The final GIS does not contain the requested effective Total cut: ", spec$target)
    mask <- masks[masks$review_record_key == target$review_record_key, ]
    if (nrow(mask) != 1L || !isTRUE(mask$coverage_complete) || !isTRUE(mask$local_outlet_precision_validated))
      stop("The detail requires its approved complete upstream mask")
    # Include the entire watershed, not only graph-connected linework. Nearby
    # downstream context records remain visible even when outside the basin.
    bounds <- sf::st_bbox(c(sf::st_geometry(mask), sf::st_geometry(context)))
    bounds[c("xmin", "ymin")] <- bounds[c("xmin", "ymin")] - 450
    bounds[c("xmax", "ymax")] <- bounds[c("xmax", "ymax")] + 450
    box <- sf::st_as_sfc(bounds)
    lines <- crop(network[network$measure_habitat, ], box)
    lines$display_class <- ifelse(lines$removed, "Removed upstream habitat",
      ifelse(lines$source_class == "prediction", "Remaining occupancy model", "Remaining SWIFD patch"))
    lines$display_class <- factor(lines$display_class,
      levels = c("Remaining occupancy model", "Remaining SWIFD patch", "Removed upstream habitat"))
    context$point_role <- ifelse(context$review_number == spec$target, "Effective Total cut",
      ifelse(context$report_blockage == "Partial", "Partial / excluded", "Upstream Total evidence"))
    context$point_role <- factor(context$point_role,
      levels = c("Effective Total cut", "Upstream Total evidence", "Partial / excluded"))
    visible_classes <- levels(lines$display_class)[levels(lines$display_class) %in% as.character(lines$display_class)]
    lake_layer <- lake_label_layer <- NULL
    if (spec$target == 634L) {
      if (!"source_lake_waterbody_cache_file" %in% names(mask) ||
          is.na(mask$source_lake_waterbody_cache_file) || !nzchar(mask$source_lake_waterbody_cache_file))
        stop("The corrected Pilchuck detail requires its exact Lake Cavanaugh source polygon")
      lake_file <- mask$source_lake_waterbody_cache_file
      if (exists("nps_resolve_cache_path", mode = "function")) lake_file <- nps_resolve_cache_path(lake_file)
      if (!file.exists(lake_file) || !identical(unname(tools::md5sum(lake_file)), as.character(mask$source_lake_waterbody_md5)))
        stop("The named Lake Cavanaugh source snapshot changed")
      protected <- c(protected, lake_file)
      hashes <- c(hashes, unname(tools::md5sum(lake_file)))
      lake <- sf::st_transform(sf::st_read(lake_file, quiet = TRUE), crs)
      if (nrow(lake) != 1L || as.character(lake$comid) != "24277115" || lake$gnis_name != "Lake Cavanaugh")
        stop("Draw only the independently identified Lake Cavanaugh waterbody24277115")
      lake_label <- sf::st_sf(label = "Lake Cavanaugh",
        geometry = suppressWarnings(sf::st_point_on_surface(sf::st_geometry(lake))))
      lake_layer <- ggplot2::geom_sf(data = lake, fill = "#dbeaf3", color = "#7297ab", linewidth = .45)
      lake_label_layer <- ggplot2::geom_sf_text(data = lake_label, ggplot2::aes(label = label),
        color = "#47697e", size = 3.5, fontface = "italic")
    }
    plot <- ggplot2::ggplot() +
      ggplot2::geom_sf(data = mask, fill = "#f0f4ec", color = "#4a6b46", linewidth = .65, linetype = "longdash") +
      lake_layer +
      ggplot2::geom_sf(data = lines, ggplot2::aes(color = display_class, linetype = display_class), linewidth = .55) +
      lake_label_layer +
      ggplot2::scale_color_manual(name = NULL, breaks = visible_classes, values = c(
        "Remaining occupancy model" = "#126a91", "Remaining SWIFD patch" = "#e66b18", "Removed upstream habitat" = "#a8a8a8")) +
      ggplot2::scale_linetype_manual(name = NULL, breaks = visible_classes, values = c(
        "Remaining occupancy model" = "solid", "Remaining SWIFD patch" = "solid", "Removed upstream habitat" = "dashed")) +
      ggplot2::geom_sf(data = context, ggplot2::aes(fill = point_role, shape = point_role),
        color = "#333333", size = 3.8, stroke = .5) +
      ggplot2::scale_fill_manual(name = NULL, breaks = unique(as.character(context$point_role)),
        values = c("Effective Total cut" = "#d72e32", "Upstream Total evidence" = "white", "Partial / excluded" = "#ffc24b")) +
      ggplot2::scale_shape_manual(name = NULL, breaks = unique(as.character(context$point_role)),
        values = c("Effective Total cut" = 21, "Upstream Total evidence" = 21, "Partial / excluded" = 24)) +
      ggrepel::geom_text_repel(data = context, ggplot2::aes(label = review_number, geometry = geometry),
        stat = "sf_coordinates", color = ifelse(context$review_number == spec$target, "#a0171b", "#574829"),
        size = 3.5, fontface = "bold", seed = 634, force = 2,
        box.padding = .5, point.padding = .3, min.segment.length = 0, max.overlaps = Inf) +
      ggplot2::coord_sf(datum = sf::st_crs(4326), expand = FALSE,
        xlim = as.numeric(bounds[c("xmin", "xmax")]), ylim = as.numeric(bounds[c("ymin", "ymax")])) +
      ggplot2::labs(title = paste0(spec$stream, ": effective Total barrier #", spec$target),
        subtitle = paste0(spec$watershed, " | Entire approved upstream watershed"), x = NULL, y = NULL,
        caption = paste(vapply(c(spec$note,
          "Green dashed outline: approved complete upstream basin. Gray dashed lines: saved original model/SWIFD habitat excluded upstream of approved barriers; blue/orange lines remain. Disconnected upstream pieces are included.",
          "All illustrated natural-barrier records are unverified; missing height values remain unknown. The detail is a view of saved geometry, with no added habitat or new clipping. Refer to population_phase3_miles.csv for official/custom mileage."),
          function(x) paste(strwrap(x, 112), collapse = "\n"), character(1)), collapse = "\n")) +
      ggplot2::theme_bw(base_size = 10) +
      ggplot2::theme(plot.title = ggplot2::element_text(face = "bold", size = 16),
        plot.subtitle = ggplot2::element_text(size = 10), plot.caption = ggplot2::element_text(hjust = 0, size = 8),
        panel.grid.major = ggplot2::element_line(color = "#e4e8e7", linewidth = .2),
        legend.position = "bottom", legend.box = "vertical", legend.text = ggplot2::element_text(size = 9),
        axis.text = ggplot2::element_text(size = 8))
    location <- ifelse(context$review_number == 915L, "85.13 m; original point held",
      sprintf("%.2f m; own-channel match", context$snap_distance_m))
    role <- ifelse(context$review_number == spec$target, "Applied lower Total cut",
      ifelse(context$report_blockage == "Partial", "Excluded: Partial", "Upstream evidence; superseded by lower cut"))
    table <- data.frame(`Map #` = context$review_number,
      `WDFW site / source report` = paste(context$SiteId, paste(context$report_blockage, context$reported_feature_type), sep = " / "),
      `Source-to-model distance` = location, `Role in this review` = role, check.names = FALSE)
    table <- gridExtra::tableGrob(table, rows = NULL, theme = gridExtra::ttheme_minimal(base_size = 9,
      core = list(fg_params = list(hjust = 0, x = .02)),
      colhead = list(fg_params = list(fontface = "bold", hjust = 0, x = .02))))
    figure <- gridExtra::arrangeGrob(plot, table, ncol = 1, heights = c(9.5, 1.15))
    file <- file.path(output_dir, spec$file)
    ggplot2::ggsave(file, figure, width = 12, height = 11, dpi = 250, bg = "white", limitsize = FALSE)
    rendered <- c(rendered, file)
  }
  if (!identical(unname(tools::md5sum(protected)), hashes)) stop("Scientific files changed while rendering detail maps")
  message("French/Pilchuck Creek detail maps rendered; scientific files unchanged")
  invisible(rendered)
}
