# A display-only atlas built from the final saved Phase3 geometry and IDs.
# No cuts, tidal classifications, measurements or saved source layers change.
phase3_tidal_detail_clusters <- function(points, adjacency_m = 1200) {
  if (!inherits(points, "sf") || sf::st_is_longlat(points) ||
      length(adjacency_m) != 1L || !is.finite(adjacency_m) || adjacency_m <= 0)
    stop("Supply projected tidal points and a positive adjacency distance")
  if (!nrow(points)) return(integer())
  neighbors <- sf::st_is_within_distance(points, points, dist = adjacency_m)
  pairs <- lapply(seq_along(neighbors), function(i) {
    other <- neighbors[[i]][neighbors[[i]] > i]
    if (!length(other)) return(NULL)
    cbind(rep(i, length(other)), other)
  })
  pairs <- do.call(rbind, pairs)
  graph <- igraph::make_empty_graph(n = nrow(points), directed = FALSE)
  if (length(pairs)) graph <- igraph::add_edges(graph, as.vector(t(pairs)))
  membership <- igraph::components(graph)$membership
  xy <- sf::st_coordinates(points)
  components <- split(seq_len(nrow(points)), membership)
  north <- vapply(components, function(i) max(xy[i, 2L]), numeric(1))
  west <- vapply(components, function(i) min(xy[i, 1L]), numeric(1))
  ordered <- as.integer(names(components)[order(-north, west)])
  match(membership, ordered)
}

render_phase3_tidal_details <- function(output_dir = "outputs/phase3_tidal_barriers",
    adjacency_m = 1200, margin_m = 600, width = 12, height = 10, dpi = 250) {
  if (!exists("phase3_map", mode = "function")) stop("Source R/phase3_exports.R before rendering tidal details")
  needed <- c("sf", "igraph", "ggplot2", "ggrepel")
  missing <- needed[!vapply(needed, requireNamespace, logical(1), quietly = TRUE)]
  if (length(missing)) stop("Missing atlas packages: ", paste(missing, collapse = ", "))
  if (length(margin_m) != 1L || !is.finite(margin_m) || margin_m <= 0)
    stop("Supply a positive detail-view margin")
  gpkg <- file.path(output_dir, "North_Puget_Sound_Steelhead_phase3.gpkg")
  protected <- file.path(output_dir, c("North_Puget_Sound_Steelhead_phase3.gpkg",
    "population_phase3_miles.csv", "phase3_barrier_decisions.csv", "tidal_cutpoint_review.csv"))
  if (!all(file.exists(protected))) stop("Finish the final Phase3 export before rendering the atlas")
  hashes <- unname(tools::md5sum(protected))
  read <- function(layer) sf::st_read(gpkg, layer = layer, quiet = TRUE)
  points <- read("tidal_cutpoints")
  if (!nrow(points) || !"tidal_id" %in% names(points) || anyNA(points$tidal_id) ||
      anyDuplicated(points$tidal_id)) stop("Saved tidal cutpoints require unique final IDs")
  if (sf::st_is_longlat(points)) stop("The detail atlas requires the final projected analysis CRS")
  populations <- read("noaa_populations")
  custom <- read("custom_review_watershed")
  outline <- function(x) sf::st_sf(pop_id = x$pop_id, population = x$population,
    geometry = sf::st_geometry(x))
  populations <- rbind(outline(populations), outline(custom))
  populations <- sf::st_transform(populations, sf::st_crs(points))
  remaining <- read("phase3_remaining")
  tidal <- read("tidal_current_polygons")
  applied <- read("applied_barriers")
  held <- read("held_target_barriers")
  # Reading the saved mileage table establishes the same17 review units; local
  # detail boxes have no mileage estimate of their own.
  mileage <- utils::read.csv(file.path(output_dir, "population_phase3_miles.csv"), stringsAsFactors = FALSE)
  if (!setequal(populations$pop_id, mileage$pop_id)) stop("Saved analysis boundaries and mileage table disagree")
  membership <- phase3_tidal_detail_clusters(points, adjacency_m)
  groups <- split(seq_len(nrow(points)), membership)
  detail_dir <- file.path(output_dir, "tidal_details")
  dir.create(detail_dir, recursive = TRUE, showWarnings = FALSE)
  pdf <- file.path(output_dir, "tidal_cutoff_details.pdf")
  index <- vector("list", length(groups))
  drawn <- character()
  crop <- function(data, box) {
    if (!nrow(data)) return(data)
    nearby <- lengths(sf::st_intersects(data, box)) > 0L
    data <- data[nearby, , drop = FALSE]
    if (!nrow(data)) return(data)
    result <- suppressWarnings(sf::st_intersection(data, box))
    result[!sf::st_is_empty(result), , drop = FALSE]
  }
  grDevices::pdf(pdf, width = width, height = height, onefile = TRUE)
  tryCatch({
    for (page in seq_along(groups)) {
      members <- groups[[page]]
      bounds <- sf::st_bbox(points[members, ])
      bounds[c("xmin", "ymin")] <- bounds[c("xmin", "ymin")] - margin_m
      bounds[c("xmax", "ymax")] <- bounds[c("xmax", "ymax")] + margin_m
      box <- sf::st_as_sfc(bounds)
      view_populations <- crop(populations, box)
      view_remaining <- crop(remaining, box)
      view_tidal <- crop(tidal, box)
      visible <- points[lengths(sf::st_intersects(points, box)) > 0L, ]
      view_applied <- applied[lengths(sf::st_intersects(applied, box)) > 0L, ]
      view_held <- held[lengths(sf::st_intersects(held, box)) > 0L, ]
      if (!nrow(view_populations)) stop("A tidal-detail cluster has no saved NOAA/custom analysis boundary")
      population_names <- sort(unique(as.character(view_populations$population)))
      detail_id <- sprintf("D%03d", page)
      title <- paste(strwrap(paste0(detail_id, ": ", paste(population_names, collapse = "; ")), 78), collapse = "\n")
      plot <- phase3_map(view_populations, view_remaining, view_applied, view_held,
        view_tidal, visible, title, length_summary = NULL, labels = FALSE)
      plot <- suppressMessages(plot + ggplot2::coord_sf(datum = sf::st_crs(4326),
        xlim = as.numeric(bounds[c("xmin", "xmax")]), ylim = as.numeric(bounds[c("ymin", "ymax")]),
        expand = FALSE))
      note <- sprintf("Detail %s | %d mapped cutoff %s | %dm adjacency clusters with %dm view margin | Original T# IDs match tidal_cutpoint_review.csv.",
        detail_id, nrow(visible), if (nrow(visible) == 1L) "ID" else "IDs", adjacency_m, margin_m)
      # Keep the shared map's symbology while making this caption specific to
      # the local tidal view, rather than repeating whole-population cut notes.
      notes <- c(note,
        "Teal: WDFW/CFS current tidal extent. Violet: screened NWI freshwater-tidal additions. Teal T# squares: mapped boundary intersections.",
        "Blue: remaining occupancy model; orange: remaining qualifying SWIFD patches. Dotted alignment links do not count as habitat.",
        "Red numbers: applied barriers in this view. Amber triangles: held barriers. Saved NOAA/custom population outlines are cropped to the view.",
        "CFS areas with current connectivity are excluded; disconnected, developed and unknown classes are held. NWI additions are screened against CFS restrictions and unresolved areas.",
        "Mapped tidal boundaries are review proxies, not surveyed hydrodynamic limits; source imagery and mapping vintages vary.",
        "Sources: NOAA; WDFW/TerrainWorks; SWIFD; WDFW FPDSI; USGS; Cramer Fish Sciences; USFWS National Wetlands Inventory.")
      plot <- plot + ggplot2::labs(caption = paste(vapply(notes, function(text)
        paste(strwrap(text, 130), collapse = "\n"), character(1)), collapse = "\n")) +
        ggplot2::theme(axis.text = ggplot2::element_text(size = 8))
      file <- sprintf("detail_%03d.png", page)
      ggplot2::ggsave(file.path(detail_dir, file), plot = plot, width = width, height = height,
        units = "in", dpi = dpi, bg = "white", limitsize = FALSE)
      print(plot)
      drawn <- c(drawn, as.character(visible$tidal_id))
      index[[page]] <- data.frame(detail_id = detail_id, page = page,
        png = paste0("tidal_details/", file),
        tidal_ids = paste(sort(as.character(visible$tidal_id)), collapse = ";"),
        cluster_member_tidal_ids = paste(sort(as.character(points$tidal_id[members])), collapse = ";"),
        visible_cutpoint_count = nrow(visible), cluster_member_count = length(members),
        population_ids = paste(sort(unique(view_populations$pop_id)), collapse = ";"),
        population_names = paste(population_names, collapse = "; "),
        adjacency_m = adjacency_m, view_margin_m = margin_m,
        xmin = unname(bounds["xmin"]), ymin = unname(bounds["ymin"]),
        xmax = unname(bounds["xmax"]), ymax = unname(bounds["ymax"]),
        projected_crs = sf::st_crs(points)$input, stringsAsFactors = FALSE)
    }
  }, finally = grDevices::dev.off())
  if (!setequal(drawn, as.character(points$tidal_id))) stop("The detail atlas omitted a saved tidal cutpoint")
  index <- do.call(rbind, index)
  utils::write.csv(index, file.path(output_dir, "tidal_detail_index.csv"), row.names = FALSE, na = "")
  # Remove only obsolete, previously generated atlas PNGs on a rerender; no
  # scientific files, main figures or unrelated detail files are touched.
  old_pngs <- list.files(detail_dir, pattern = "^detail_[0-9]{3}\\.png$", full.names = TRUE)
  keep <- file.path(detail_dir, basename(index$png))
  stale <- setdiff(old_pngs, keep)
  if (length(stale)) {
    actual_parent <- normalizePath(dirname(stale), winslash = "/", mustWork = TRUE)
    expected_parent <- normalizePath(detail_dir, winslash = "/", mustWork = TRUE)
    if (any(actual_parent != expected_parent)) stop("Atlas cleanup escaped the generated detail directory")
    unlink(stale)
  }
  if (!identical(unname(tools::md5sum(protected)), hashes))
    stop("The final Phase3 science files changed while the display-only atlas was rendering")
  message("Tidal detail atlas: ", nrow(index), " pages, all ", nrow(points), " final mapped cutoff IDs covered; science files unchanged")
  invisible(index)
}
