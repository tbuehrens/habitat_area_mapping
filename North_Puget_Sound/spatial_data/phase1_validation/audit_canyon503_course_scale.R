library(sf)
source("R/barrier_stream_identity.R")
source("R/stream_edge_identity.R")
records <- jsonlite::fromJSON("spatial_data/upstream_basins/batch/manifest.json", simplifyVector = FALSE)
record <- records[[which(vapply(records, function(x) x$review_record_key == "6:49841", logical(1)))]]
selected <- readRDS("spatial_data/phase1_validation/phase1_selected_totals.rds")
point <- selected[selected$review_record_key == "6:49841", ]
edges <- st_read("outputs/review_swifd_connections/North_Puget_Sound_Steelhead.gpkg", layer = "stream_matching_edges", quiet = TRUE)
edge <- edges[edges$graph_edge_id == point$graph_edge_id, ]
routes <- st_transform(st_read(file.path(record$cache_folder, "named_flowline.json"), quiet = TRUE), st_crs(edge))
route <- routes[as.character(routes$comid) == as.character(record$comid), ]
route <- suppressWarnings(st_cast(route, "LINESTRING"))
route <- route[st_nearest_feature(point, route), ]
p <- unclass(st_geometry(edge)[[1]])[, 1:2, drop = FALSE]
r <- unclass(st_geometry(route)[[1]])[, 1:2, drop = FALSE]
length_of <- function(x) sum(sqrt(rowSums(diff(x)^2)))
at <- stream_identity_project(p, st_coordinates(point)[1, 1:2])$measure
positions <- seq(at - 50, at + 50, length.out = 7)
sample <- t(vapply(positions, function(m) stream_identity_at(p, m), numeric(2)))
projection <- lapply(seq_len(7), function(i) stream_identity_project(r, sample[i, ]))
distance <- vapply(projection, `[[`, numeric(1), "distance")
for (span in c(5, 15, 25, 40)) {
  angle <- vapply(seq_len(7), function(i) {
    pv <- stream_identity_at(p, min(length_of(p), positions[i] + span)) - stream_identity_at(p, max(0, positions[i] - span))
    m <- projection[[i]]$measure
    rv <- stream_identity_at(r, min(length_of(r), m + span)) - stream_identity_at(r, max(0, m - span))
    acos(min(1, abs(sum(pv * rv)) / sqrt(sum(pv^2) * sum(rv^2)))) * 180 / pi
  }, numeric(1))
  cat("Tangent half-span", span, "m; coverage", mean(distance <= 25 & angle <= 35),
    "maxdistance", max(distance), "maxangle", max(angle), "angles", paste(round(angle, 1), collapse = ","), "\n")
}
library(ggplot2)
box <- st_bbox(st_buffer(point, 180))
plot <- ggplot() + geom_sf(data = edge, color = "#126a91", linewidth = 1) +
  geom_sf(data = route, color = "#e66b18", linewidth = .7) + geom_sf(data = point, color = "red", size = 3) +
  coord_sf(xlim = box[c("xmin", "xmax")], ylim = box[c("ymin", "ymax")], datum = st_crs(4326)) +
  labs(title = "Canyon Creek Total #503: model and NHDPlusV2 course", subtitle = "Both names Canyon Creek; all7 sample distances <=19.35m", x = NULL, y = NULL) + theme_bw()
ggsave("spatial_data/phase1_validation/canyon503_course_scale.png", plot, width = 8, height = 7, dpi = 160, bg = "white")
