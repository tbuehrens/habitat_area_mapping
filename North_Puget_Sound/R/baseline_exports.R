# A fixed, raw-model reference. This exporter never downloads data and never
# invokes connectivity repair, SWIFD, or natural-barrier filtering.

baseline_population_snapshot <- function(cache_dir) {
  environment <- new.env(parent = globalenv())
  environment$arcgis_download <- function(layer, cache, ...) {
    if (!file.exists(cache)) stop("Baseline requires the existing cached NOAA snapshot: ", cache)
    readRDS(cache)
  }
  sys.source(file.path("R", "noaa_populations.R"), envir = environment)
  environment$get_north_puget_populations(cache_dir = cache_dir, refresh = FALSE)
}

baseline_map <- function(populations, model, subtitle, labels = FALSE) {
  plot <- ggplot2::ggplot() +
    ggplot2::geom_sf(data = populations, fill = "#f3f2eb", color = "#414c4b", linewidth = 0.5) +
    ggplot2::geom_sf(data = model, color = "#126a91", linewidth = 0.4) +
    ggplot2::coord_sf(datum = sf::st_crs(4326), expand = TRUE) +
    ggplot2::labs(title = "Raw occupancy model \u2014 before workflow filters",
      subtitle = subtitle, x = NULL, y = NULL,
      caption = paste(
        "Reference only: raw WDFW/TerrainWorks occupancy model within NOAA population polygons.",
        "No SWIFD, connectivity, or natural-barrier filtering. Not an accessible-habitat estimate.",
        sep = "\n")) +
    ggplot2::theme_bw(base_size = 11) +
    ggplot2::theme(panel.grid.major = ggplot2::element_line(color = "#e1e7e8", linewidth = 0.15),
      plot.title = ggplot2::element_text(face = "bold", size = 16),
      plot.subtitle = ggplot2::element_text(size = 10),
      plot.caption = ggplot2::element_text(size = 8, hjust = 0))
  if (labels) {
    points <- suppressWarnings(sf::st_point_on_surface(populations))
    plot <- plot + ggplot2::geom_sf_label(data = points,
      ggplot2::aes(label = population), size = 2.5, alpha = 0.8,
      linewidth = 0.1, label.padding = grid::unit(0.12, "lines"))
  }
  plot
}

export_raw_model_baseline <- function(cache_dir = "spatial_data",
    output_dir = "outputs/baseline_model_only") {
  packages <- c("sf", "dplyr", "ggplot2", "jsonlite")
  missing <- packages[!vapply(packages, requireNamespace, logical(1), quietly = TRUE)]
  if (length(missing)) stop("Install required packages: ", paste(missing, collapse = ", "))
  inputs <- c(prediction = file.path(cache_dir, "north_puget_steelhead.rds"),
              populations = file.path(cache_dir, "noaa_ps_live.rds"))
  if (!all(file.exists(inputs))) stop("Baseline requires the existing prediction and NOAA cache snapshots")
  signature <- as.list(tools::md5sum(inputs))
  names(signature) <- names(inputs)
  metadata_path <- file.path(output_dir, "baseline_metadata.json")
  if (file.exists(metadata_path)) {
    previous <- jsonlite::fromJSON(metadata_path, simplifyVector = FALSE)
    if (!identical(previous$input_md5, signature)) {
      stop("This baseline is fixed to another input snapshot. Choose a new output_dir; existing baseline was not changed.")
    }
    message("Existing raw-model baseline is unchanged; its input snapshots match.")
    return(invisible(list(lengths = utils::read.csv(file.path(output_dir, "baseline_raw_lengths.csv")),
                          output_dir = output_dir)))
  }
  if (dir.exists(output_dir) && length(list.files(output_dir, all.files = TRUE, no.. = TRUE))) {
    stop("Baseline output directory is not empty. Choose a new output_dir rather than overwriting the reference.")
  }
  raw <- readRDS(inputs[["prediction"]])
  populations <- baseline_population_snapshot(cache_dir)
  populations <- sf::st_transform(sf::st_make_valid(populations), 26910)
  raw <- sf::st_transform(raw, 26910)
  # Unary union removes coincident records without selecting distribution,
  # connectivity, access, barrier, run, or source categories.
  source_union <- sf::st_union(sf::st_geometry(raw))
  geometries <- lapply(seq_len(nrow(populations)), function(i) {
    clipped <- suppressWarnings(sf::st_intersection(source_union, sf::st_geometry(populations[i, ])))
    clipped <- clipped[!sf::st_is_empty(clipped)]
    if (any(sf::st_geometry_type(clipped) == "GEOMETRYCOLLECTION")) {
      clipped <- suppressWarnings(sf::st_collection_extract(clipped, "LINESTRING"))
    }
    clipped <- clipped[sf::st_dimension(clipped) == 1]
    if (!length(clipped)) return(sf::st_multilinestring())
    sf::st_cast(sf::st_union(clipped), "MULTILINESTRING")[[1L]]
  })
  model <- sf::st_sf(sf::st_drop_geometry(populations),
    geometry = sf::st_sfc(geometries, crs = 26910))
  model$raw_model_miles <- as.numeric(sf::st_length(model)) / 1609.344
  model$raw_model_km <- model$raw_model_miles * 1.609344
  table <- sf::st_drop_geometry(model)
  table$area_sq_km <- as.numeric(sf::st_area(populations)) / 1e6
  table$estimate_scope <- "Raw occupancy-model linework; no accessibility filtering"
  if (any(!is.finite(table$raw_model_miles)) || any(table$raw_model_miles < 0)) {
    stop("Invalid raw-model baseline lengths")
  }
  dir.create(file.path(output_dir, "maps"), recursive = TRUE, showWarnings = FALSE)
  utils::write.csv(table, file.path(output_dir, "baseline_raw_lengths.csv"), row.names = FALSE)
  save_plot <- function(plot, name, width = 11, height = 9) {
    ggplot2::ggsave(file.path(output_dir, name), plot = plot,
      width = width, height = height, units = "in", dpi = 250, bg = "white")
  }
  regional <- baseline_map(populations, model, "North Cascades MPG steelhead populations | Raw model reference", labels = TRUE)
  save_plot(regional, "baseline_raw_regional_map.png", 14, 14)
  grDevices::pdf(file.path(output_dir, "baseline_raw_population_maps.pdf"),
                 width = 10, height = 9, onefile = TRUE)
  for (i in seq_len(nrow(populations))) {
    pop <- populations[i, ]
    plot <- baseline_map(pop, model[i, ], sprintf("%s | %.2f raw model miles | NOAA population %s",
      pop$population, table$raw_model_miles[i], pop$pop_id))
    slug <- gsub("[^A-Za-z0-9]+", "_", pop$population)
    save_plot(plot, file.path("maps", paste0(pop$pop_id, "_", slug, ".png")))
    print(plot)
  }
  grDevices::dev.off()
  nooksack <- grepl("Nooksack", populations$population, ignore.case = TRUE)
  if (any(nooksack)) save_plot(baseline_map(populations[nooksack, ], model[nooksack, ],
    "Nooksack winter and South Fork Nooksack summer populations | Raw model reference", labels = TRUE),
    "baseline_raw_nooksack_map.png", 12, 9)
  gpkg <- file.path(output_dir, "baseline_model_only.gpkg")
  sf::st_write(model, gpkg, layer = "raw_model", quiet = TRUE)
  sf::st_write(populations, gpkg, layer = "noaa_pops", quiet = TRUE)
  metadata <- list(
    baseline_type = "RAW occupancy model before workflow filters",
    immutable_reference = TRUE,
    interpretation = "Raw linework reference only; not an estimate of accessible habitat",
    filtering = "NOAA MPG population selection and polygon clipping only; no SWIFD, graph/connectivity, or natural-barrier filtering",
    geometry_processing = "Dissolve coincident raw model geometry and clip to each NOAA population; population overlaps are not additive",
    input_files = as.list(inputs), input_md5 = signature,
    model_source = attr(raw, "source_url"),
    model_downloaded_utc = attr(raw, "downloaded_utc"),
    noaa_source = attr(populations, "source_url"),
    projection = "EPSG:26910 NAD83 / UTM zone 10N",
    meters_per_mile = 1609.344, population_count = nrow(populations),
    input_prediction_records = nrow(raw),
    generated_utc = format(Sys.time(), tz = "UTC", usetz = TRUE))
  # Written last: a complete reference is never overwritten on subsequent runs.
  jsonlite::write_json(metadata, metadata_path, auto_unbox = TRUE, pretty = TRUE, na = "null")
  capture.output(utils::sessionInfo(), file = file.path(output_dir, "sessionInfo.txt"))
  print(table[, c("pop_id", "population", "raw_model_miles")], row.names = FALSE)
  invisible(list(lengths = table, populations = populations, raw_model = model,
                  map = regional, output_dir = output_dir))
}
