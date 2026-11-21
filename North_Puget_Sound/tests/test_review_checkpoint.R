# A geometry-review export must not publish mileage tables or replace legacy
# final products. Everything runs in a temporary working directory with guards.
source(file.path("R", "source_classification.R"))
source(file.path("R", "network.R"))
source(file.path("R", "habitat_workflow.R"))
library(sf)

run_review_checkpoint_check <- function() {
  original_directory <- getwd()
  on.exit(setwd(original_directory), add = TRUE)
  directory <- tempfile("review_checkpoint_")
  dir.create(directory, recursive = TRUE)
  setwd(directory)
  writeBin(charToRaw("original legacy map sentinel"), "Nooksack_Winter_Steelhead_map.png")
  writeLines("original legacy mileage sentinel", "Nooksack_Winter_Steelhead_hab_lengths.csv")
  guards <- c("Nooksack_Winter_Steelhead_map.png", "Nooksack_Winter_Steelhead_hab_lengths.csv")
  original_hashes <- tools::md5sum(guards)

  offset <- c(500000, 5400000)
  xy <- function(points) sweep(points, 2, offset, "+")
  line <- function(points) st_linestring(xy(points))
  point <- function(location) st_point(location + offset)
  rectangle <- function(xmin, ymin, xmax, ymax) st_polygon(list(xy(rbind(
    c(xmin, ymin), c(xmax, ymin), c(xmax, ymax), c(xmin, ymax), c(xmin, ymin)))))
  network <- st_sf(
    source_class = c("prediction", "prediction", "swifd_connector",
      "alignment_link", "alignment_link", "prediction"),
    bridge_id = c(NA, NA, "manual_1", "manual_1", "manual_1", NA),
    geometry = st_sfc(
      line(rbind(c(0, 0), c(100, 0))),
      line(rbind(c(300, 0), c(400, 0))),
      line(rbind(c(100, 10), c(150, 60), c(250, 60), c(300, 10))),
      line(rbind(c(100, 0), c(100, 10))),
      line(rbind(c(300, 10), c(300, 0))),
      line(rbind(c(300, 90), c(400, 90))), crs = 26910))
  network$length_m <- as.numeric(st_length(network))
  steelhead <- network[network$source_class == "prediction", ]
  barrier <- st_sf(id = 1L, verification = "Not field verified",
    geometry = st_sfc(point(c(200, 60)), crs = 26910))
  mouth <- st_sfc(point(c(0, 0)), crs = 26910)
  trace <- truncate_network_at_barriers(network, barrier, mouth,
    root_distance = .01, snap_distance = 1)
  verified <- truncate_network_at_barriers(network, barrier[0, ], mouth,
    root_distance = .01, snap_distance = 1)
  populations <- st_sf(pop_id = c(1L, 2L),
    population = c("Nooksack synthetic winter", "Nooksack synthetic summer"),
    run_timing = c("wi", "su"),
    geometry = st_sfc(rectangle(-10, -10, 180, 110),
                      rectangle(180, -10, 410, 110), crs = 26910))
  endpoints <- st_sf(anchor_id = 1:2, snap_distance_m = c(10, 10),
    eligible = c(TRUE, TRUE), match_status = c("used_by_connector", "used_by_connector"),
    geometry = st_sfc(point(c(100, 0)), point(c(300, 0)), crs = 26910))
  bridges <- list(connectors = network[network$source_class == "swifd_connector", ],
    alignment_links = network[network$source_class == "alignment_link", ],
    endpoint_audit = endpoints,
    bridge_audit = data.frame(bridge_id = "manual_1", from_anchor = 1L, to_anchor = 2L,
      from_snap_m = 10, to_snap_m = 10, routed_swifd_m = 2 * sqrt(50^2 + 50^2) + 100,
      alignment_m = 20, status = "retained_in_physical_forest"),
    summary = data.frame(retained_bridges = 1L), settings = list(snap_distance_m = 100))

  output <- file.path(directory, "outputs", "review_swifd_connections")
  result <- habitat_export(populations, trace, verified, steelhead, barrier,
    output, metadata = list(synthetic_check = TRUE), bridges = bridges, review_only = TRUE)
  stopifnot(identical(unname(tools::md5sum(guards)), unname(original_hashes)),
    length(list.files(output, pattern = "hab_lengths\\.csv$", recursive = TRUE)) == 0L,
    length(list.files(output, pattern = "\\.png$", recursive = TRUE)) >= 3L,
    length(list.files(output, pattern = "\\.pdf$", recursive = TRUE)) >= 1L,
    file.exists(file.path(output, "natural_barrier_audit.csv")),
    file.exists(file.path(output, "gap_endpoint_audit.csv")),
    file.exists(file.path(output, "swifd_connector_audit.csv")))
  gpkg <- list.files(output, pattern = "\\.gpkg$", full.names = TRUE)
  stopifnot(length(gpkg) == 1L)
  layers <- st_layers(gpkg)$name
  stopifnot(all(c("predicted_extent_status", "swifd_gap_routes_before_barriers",
                 "endpoint_alignment_links", "gap_endpoint_audit") %in% layers))
  metadata <- jsonlite::fromJSON(file.path(output, "run_metadata.json"))
  stopifnot(isTRUE(metadata$review_checkpoint), isTRUE(metadata$mileage_export_deferred))

  # Inspect the rendered scale, rather than relying on legend-label wording.
  # Original connected prediction is blue; patches orange; unconnected model
  # remains gray; geometric alignment is visibly dotted.
  line_layers <- which(vapply(result$map$layers, function(layer) {
    inherits(layer$data, "sf") && "source_class" %in% names(layer$data)
  }, logical(1)))
  stopifnot(length(line_layers) == 1L)
  index <- line_layers[1L]
  data <- result$map$layers[[index]]$data
  rendered <- ggplot2::ggplot_build(result$map)$data[[index]]
  colors_for <- function(selection) unique(rendered$colour[selection])
  prediction_color <- colors_for(data$source_class == "prediction" & data$retained)
  patch_color <- colors_for(data$source_class == "swifd_connector")
  disconnected_color <- colors_for(data$source_class == "prediction" & data$unresolved)
  stopifnot(length(prediction_color) == 1L, length(patch_color) == 1L,
    length(disconnected_color) == 1L,
    length(unique(c(prediction_color, patch_color, disconnected_color))) == 3L)
  blue <- grDevices::col2rgb(prediction_color)[, 1L]
  orange <- grDevices::col2rgb(patch_color)[, 1L]
  gray <- grDevices::col2rgb(disconnected_color)[, 1L]
  stopifnot(blue[3L] > blue[1L], orange[1L] > orange[2L], orange[2L] > orange[3L],
    max(gray) - min(gray) < 30,
    all(rendered$linetype[data$source_class == "alignment_link"] == "dotted"))
  cat("Review checkpoint maps/audits/GIS pass; blue/orange/gray sources and dotted links are distinct; final tables and legacy files untouched.\n")
}

run_review_checkpoint_check()
