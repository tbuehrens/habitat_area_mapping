# Separate post-correction diagnostic. Never overwrite the earlier audit products.
# Run only after the owner confirms that the corrected SWIFD trace cache is ready.
library(sf)
library(ggplot2)
source("R/arcgis.R")
source("R/noaa_populations.R")
source("R/habitat_workflow.R")

args <- commandArgs(trailingOnly = TRUE)
if (!"--corrected-cache-ready" %in% args) {
  stop("Wait for the corrected cache, then run with --corrected-cache-ready")
}
out <- "outputs/canyon_creek_audit"
dir.create(out, recursive = TRUE, showWarnings = FALSE)
prefix <- file.path(out, "canyon_after_correction")
cache_path <- "spatial_data/north_puget_traces_swifd.rds"
cache <- readRDS(cache_path)
if (!"stream_identity_status" %in% names(cache$trace$barrier_audit)) {
  stop("Trace cache does not yet contain the receiving-stream assignment correction")
}
bad_assignments <- cache$trace$barrier_audit[
  as.character(cache$trace$barrier_audit$SiteId) %in% c("600836", "600837"), ]
if (nrow(bad_assignments) != 2 || any(bad_assignments$applied)) {
  stop("The two documented receiving-mainstem assignments are not both rejected")
}
pops <- get_north_puget_populations()
canyon <- pops[pops$pop_id == 269, ]
model <- st_transform(readRDS("spatial_data/north_puget_steelhead.rds"), st_crs(canyon))
raw <- habitat_clip(model, canyon)
swifd <- st_transform(readRDS("spatial_data/north_puget_swifd_steelhead.rds"), st_crs(canyon))

clip_trace <- function(x) {
  z <- habitat_clip(x$lines, canyon)
  if (!"source_class" %in% names(z)) z$source_class <- "prediction"
  if (!"measure_habitat" %in% names(z)) z$measure_habitat <- TRUE
  z
}
verified <- clip_trace(cache$verified)
all_barriers <- clip_trace(cache$trace)
length_mi <- function(z) {
  if (!nrow(z)) return(0)
  sum(as.numeric(st_length(st_union(st_geometry(z))))) / 1609.344
}
stage_row <- function(z, scenario) {
  z <- z[z$source_class == "prediction" & z$measure_habitat, ]
  data.frame(scenario = scenario, original_prediction_miles = length_mi(z),
    retained_miles = length_mi(z[z$retained, ]),
    unresolved_miles = length_mi(z[z$unresolved, ]),
    removed_above_barrier_miles = length_mi(z[z$removed, ]),
    drawn_on_production_map_miles = length_mi(z[z$retained | z$unresolved, ]))
}
lengths_table <- rbind(
  data.frame(scenario = "Raw occupancy model", original_prediction_miles = length_mi(raw),
    retained_miles = NA_real_, unresolved_miles = NA_real_,
    removed_above_barrier_miles = NA_real_, drawn_on_production_map_miles = length_mi(raw)),
  stage_row(verified, "Corrected verified natural barriers"),
  stage_row(all_barriers, "Corrected all natural barriers"))
lengths_table$corrected_cache_mtime <- format(file.info(cache_path)$mtime, tz = "UTC", usetz = TRUE)
write.csv(lengths_table, paste0(prefix, "_lengths.csv"), row.names = FALSE)

# Preserve the exact barrier path documented before correction, then join its
# records to the corrected audit by SiteId. This avoids redefining the evidence.
old_path <- readRDS("spatial_data/canyon_audit/path.rds")$barriers
route_ids <- as.character(old_path$SiteId)
aud <- cache$trace$barrier_audit
route <- aud[match(route_ids, as.character(aud$SiteId)), ]
route <- route[!is.na(route$SiteId), ]
cols <- intersect(c("SiteId", "OBJECTID", "StreamName", "TributaryToName",
  "verification", "SiteLatitude", "SiteLongitude", "source_layer", "DataSource",
  "SurveyDate", "PercentFishPassableCode", "Comments", "Comment", "CommentsText",
  "BarrierComments", "FormLinkURL", "match_status", "applied", "snap_distance_m",
  "matched_edge", "measure_m", "node_id", "matched_stream_name", "stream_name_status",
  "stream_name_match", "matched_swifd_stream", "stream_validation_status",
  "stream_identity_status", "matched_llid", "reference_distance_m", "allow_cut"), names(route))
barrier_table <- st_drop_geometry(route)[, cols, drop = FALSE]
nearest <- st_nearest_feature(route, swifd)
barrier_table$nearest_SWIFD_stream <- swifd$LLID_STRM_NAME[nearest]
barrier_table$nearest_SWIFD_LLID <- swifd$LLID[nearest]
barrier_table$nearest_SWIFD_distance_m <- as.numeric(st_distance(route, swifd[nearest, ], by_element = TRUE))
barrier_table$fish_passability_missing <- if ("PercentFishPassableCode" %in% names(route))
  is.na(route$PercentFishPassableCode) else TRUE
barrier_table$interpretation <- ifelse(route$source_layer == 6,
  "Named Canyon/SF Canyon record; missing passability does not establish a complete barrier to summer steelhead.",
  "Record names an unnamed tributary of SF Stillaguamish; previous receiving-mainstem assignment was incorrect.")
write.csv(barrier_table, paste0(prefix, "_route_barriers.csv"), row.names = FALSE)
unverified <- barrier_table[barrier_table$source_layer == 6, ]
write.csv(unverified, paste0(prefix, "_nine_unverified_barriers.csv"), row.names = FALSE)

# Candidate barrier points in/near the Canyon population are shown on the
# corrected panels. The two receiving-river records are far downstream and
# are documented in the route CSV and the preserved pre-correction figure.
local <- route[route$source_layer == 6 |
  lengths(st_intersects(route, st_buffer(canyon, 1500))) > 0, ]
local$verification_label <- ifelse(tolower(local$verification) == "verified", "Verified", "Unverified")
local_xy <- st_coordinates(local)
labels <- data.frame(x = local_xy[, 1], y = local_xy[, 2], id = as.character(local$SiteId))
if (nrow(labels)) {
  labels <- labels[order(labels$y, labels$x), ]
  labels$label_x <- labels$x + rep(c(-1800, 1800), length.out = nrow(labels))
  labels$label_y <- labels$y + rep(c(-200, 200), length.out = nrow(labels))
  offsets <- data.frame(id = c("890606", "890168", "890988", "890602", "890603",
      "890598", "890600", "890176", "890599"),
    dx = c(1400, 900, -1000, -1400, -1400, 200, -300, 100, -1200),
    dy = c(500, -650, 800, 500, -700, -1200, 1100, -1000, 400))
  ix <- match(labels$id, offsets$id)
  matched <- !is.na(ix)
  labels$label_x[matched] <- labels$x[matched] + offsets$dx[ix[matched]]
  labels$label_y[matched] <- labels$y[matched] + offsets$dy[ix[matched]]
}
focus <- st_union(c(st_geometry(canyon), st_geometry(local)))
window <- st_bbox(st_buffer(focus, 2200))
background <- suppressWarnings(st_crop(model, window))
base_map <- function(title, subtitle) {
  ggplot() + geom_sf(data = canyon, fill = "grey98", color = "grey35", linewidth = .5) +
    geom_sf(data = background, color = "grey85", linewidth = .25) +
    coord_sf(xlim = window[c("xmin", "xmax")], ylim = window[c("ymin", "ymax")], datum = st_crs(4326)) +
    labs(title = title, subtitle = subtitle, x = NULL, y = NULL) +
    theme_bw(base_size = 10) + theme(plot.title = element_text(face = "bold"),
      legend.position = "bottom", plot.caption = element_text(hjust = 0, size = 8))
}
status_string <- function(row) sprintf("%.2f mi retained | %.2f mi unresolved | %.2f mi excluded",
  row$retained_miles, row$unresolved_miles, row$removed_above_barrier_miles)
raw_view <- base_map("Raw steelhead occupancy model", sprintf("%.2f miles inside NOAA Canyon Creek population", length_mi(raw))) +
  geom_sf(data = raw, color = "#146b96", linewidth = .8) +
  labs(caption = "Blue: complete occupancy-model linework before natural-barrier cuts.")
make_corrected <- function(z, row, title) {
  pred <- z[z$source_class == "prediction", ]
  bridge <- z[z$source_class != "prediction", ]
  pred$display_status <- ifelse(pred$removed, "Excluded above barrier",
    ifelse(pred$retained, "Retained", "Unresolved connection"))
  view <- base_map(title, status_string(row)) +
    geom_sf(data = pred, aes(color = display_status, linetype = display_status), linewidth = .65) +
    geom_sf(data = bridge[!bridge$removed, ], color = "#e69b28", linewidth = .7) +
    geom_sf(data = local, aes(shape = verification_label), color = "#6a3d9a", fill = "white", size = 2.2) +
    scale_color_manual(name = NULL, values = c("Retained" = "#146b96", "Unresolved connection" = "#7d7d7d", "Excluded above barrier" = "#c52d2d"), drop = FALSE) +
    scale_linetype_manual(name = NULL, values = c("Retained" = "solid", "Unresolved connection" = "solid", "Excluded above barrier" = "dashed"), drop = FALSE) +
    scale_shape_manual(name = "Barrier record", values = c("Verified" = 24, "Unverified" = 21), drop = FALSE)
  if (nrow(labels)) view <- view +
    geom_segment(data = labels, aes(x = x, y = y, xend = label_x, yend = label_y), color = "#6a3d9a", linewidth = .25) +
    geom_label(data = labels, aes(x = label_x, y = label_y, label = id), color = "#6a3d9a", size = 2.1, label.padding = grid::unit(.1, "lines"))
  view
}
verified_view <- make_corrected(verified, lengths_table[2, ], "Corrected: verified barriers only") +
  labs(caption = "Incorrect downstream receiving-river assignments are rejected.\nUnverified points are shown for context and do not cut this scenario.")
all_view <- make_corrected(all_barriers, lengths_table[3, ], "Corrected: both natural-barrier layers") +
  labs(caption = "Red dashed reaches remain visible here for review. Zero retained mileage\ndoes not prove absence: this scenario treats natural records as barriers to all runs.")
views <- lapply(list(raw_view, verified_view, all_view), ggplotGrob)
equal_heights <- do.call(grid::unit.pmax, lapply(views, function(g) g$heights))
views <- lapply(views, function(g) {g$heights <- equal_heights; g})
combined <- gridExtra::arrangeGrob(grobs = views, ncol = 3,
  top = grid::textGrob("Canyon Creek summer steelhead: occupancy model and natural-barrier scenarios",
    gp = grid::gpar(fontsize = 15, fontface = "bold")),
  bottom = grid::textGrob("Canyon Creek summer steelhead | NOAA polygon 269 | Original pre-correction audit products preserved separately",
    gp = grid::gpar(fontsize = 10)))
ggsave(paste0(prefix, ".png"), combined, width = 22, height = 6.2, dpi = 180, bg = "white")
ggsave(paste0(prefix, ".pdf"), combined, width = 22, height = 6.2, bg = "white")
writeLines(c("Canyon Creek post-correction diagnostic",
  paste("Corrected trace cache:", normalizePath(cache_path)),
  paste("Cache modification time:", lengths_table$corrected_cache_mtime[1]),
  "This diagnostic preserves all raw prediction geometry and draws excluded reaches as red dashed lines.",
  "The all-barriers scenario does not establish species/run-specific biological absence.",
  "Nine unverified records lie along the documented Canyon/SF Canyon downstream route; their fish-passability fields are missing.",
  "Two verified unnamed-tributary records were previously placed on the receiving SF Stillaguamish mainstem; corrected status is reported in the route CSV.",
  "Pre-correction audit figures and tables were not overwritten."), paste0(prefix, "_notes.txt"))
print(lengths_table)
print(barrier_table)
