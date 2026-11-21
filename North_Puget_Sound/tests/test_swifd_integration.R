# Integration checks for SWIFD routing provenance, barrier cuts, and exports.
# No network requests are made; core gap-route selection has separate tests.
source(file.path("R", "source_classification.R"))
source(file.path("R", "network.R"))
source(file.path("R", "swifd_sources.R"))
source(file.path("R", "habitat_workflow.R"))
library(sf)

near <- function(actual, expected) {
  stopifnot(isTRUE(all.equal(actual, expected, tolerance = 1e-8)))
}
offset <- c(500000, 5400000)
xy <- function(points) sweep(points, 2, offset, "+")
line <- function(points) st_linestring(xy(points))
point <- function(location) st_point(location + offset)
rectangle <- function(xmin, ymin, xmax, ymax) {
  st_polygon(list(xy(rbind(c(xmin, ymin), c(xmax, ymin), c(xmax, ymax),
                           c(xmin, ymax), c(xmin, ymin)))))
}

# The physical SWIFD route bends away from a direct endpoint-to-endpoint chord.
# Two10m alignment links join it to the unchanged prediction; these links are
# topology only and must not contribute to habitat mileage.
network <- st_sf(
  source_class = c("prediction", "prediction", "swifd_connector",
                   "alignment_link", "alignment_link"),
  bridge_id = c(NA, NA, "manual_1", "manual_1", "manual_1"),
  geometry = st_sfc(
    line(rbind(c(0, 0), c(100, 0))),
    line(rbind(c(300, 0), c(400, 0))),
    line(rbind(c(100, 10), c(150, 60), c(250, 60), c(300, 10))),
    line(rbind(c(100, 0), c(100, 10))),
    line(rbind(c(300, 10), c(300, 0))), crs = 26910))
steelhead <- network[network$source_class == "prediction", ]
barrier <- st_sf(id = 1L, verification = "Not field verified",
  geometry = st_sfc(point(c(200, 60)), crs = 26910))
mouth <- st_sfc(point(c(0, 0)), crs = 26910)
trace <- truncate_network_at_barriers(network, barrier, mouth,
  root_distance = .01, snap_distance = 1)
verified <- truncate_network_at_barriers(network, barrier[0, ], mouth,
  root_distance = .01, snap_distance = 1)

diagonal <- sqrt(50^2 + 50^2)
route_length <- 2 * diagonal + 100
physical_length <- 200 + route_length
stopifnot(trace$summary$barriers_applied == 1L,
  all(trace$lines$baseline_connected), !any(trace$lines$unresolved),
  setequal(trace$lines$source_class,
           c("prediction", "swifd_connector", "alignment_link")),
  all(trace$lines$measure_habitat == (trace$lines$source_class != "alignment_link")))
near(sum(trace$lines$length_m[trace$lines$measure_habitat]), physical_length)
near(sum(trace$lines$length_m[!trace$lines$measure_habitat]), 20)
near(sum(trace$lines$length_m[trace$lines$retained & trace$lines$measure_habitat]),
     100 + diagonal + 50)

# The model above the new SWIFD-route barrier must be disconnected; there is no
# parallel routing path around the natural barrier.
predicted_parts <- trace$lines[trace$lines$source_class == "prediction", ]
centers <- st_coordinates(st_centroid(predicted_parts))[, 1] - offset[1]
stopifnot(all(!predicted_parts$retained[centers > 200]),
          all(predicted_parts$removed[centers > 200]),
          all(predicted_parts$retained[centers < 200]))

# An alignment link must not attract barrier snapping, even at zero distance.
on_alignment <- barrier
st_geometry(on_alignment) <- st_sfc(point(c(100, 5)), crs = 26910)
alignment_trace <- truncate_network_at_barriers(network, on_alignment, mouth,
  root_distance = .01, snap_distance = 1)
stopifnot(!alignment_trace$barrier_audit$applied,
          alignment_trace$barrier_audit$match_status == "unmatched")
near(alignment_trace$barrier_audit$snap_distance_m, 5)

# The population edge cuts a retained SWIFD piece before the barrier, ensuring
# exported length_m measures each clipped piece rather than its regional parent.
populations <- st_sf(pop_id = c(1L, 2L),
  population = c("Synthetic west", "Synthetic east"),
  geometry = st_sfc(rectangle(-10, -10, 180, 80),
                    rectangle(180, -10, 410, 80), crs = 26910))
directory <- file.path(tempdir(), "swifd_integration")
result <- habitat_export(populations, trace, verified, steelhead, barrier,
  directory, metadata = list(synthetic_check = TRUE))
tab <- result$lengths[match(c(1L, 2L), result$lengths$pop_id), ]
west_added <- diagonal + 30
east_added <- diagonal + 70
near(tab$predicted_untruncated_miles, c(100, 100) / 1609.344)
near(tab$swifd_added_miles, c(west_added, east_added) / 1609.344)
near(tab$augmented_untruncated_miles,
     tab$predicted_untruncated_miles + tab$swifd_added_miles)
near(tab$alignment_link_miles_not_counted, c(10, 10) / 1609.344)
near(tab$outlet_connected_miles, c(100 + west_added, 20) / 1609.344)
near(tab$barrier_truncated_miles, tab$outlet_connected_miles)
near(tab$prediction_outlet_connected_miles, c(100, 0) / 1609.344)
near(tab$swifd_outlet_connected_miles, c(west_added, 20) / 1609.344)
near(tab$outlet_connected_miles,
     tab$prediction_outlet_connected_miles + tab$swifd_outlet_connected_miles)
near(tab$excluded_above_barriers_miles, c(0, 100 + diagonal + 50) / 1609.344)
near(tab$unresolved_connectivity_miles, c(0, 0))
near(tab$augmented_untruncated_miles,
     tab$outlet_connected_miles + tab$excluded_above_barriers_miles +
       tab$unresolved_connectivity_miles)
near(sum(tab$verified_barriers_only_miles), physical_length / 1609.344)

written <- st_read(file.path(directory, "North_Puget_Sound_Steelhead.gpkg"),
  layer = "predicted_extent_status", quiet = TRUE)
near(written$length_m, as.numeric(st_length(written)))
near(sum(written$length_m[written$measure_habitat]), physical_length)
near(sum(written$length_m[!written$measure_habitat]), 20)
stopifnot(setequal(written$source_class, network$source_class),
  all(is.na(written$bridge_id[written$source_class == "prediction"])),
  all(written$bridge_id[written$source_class != "prediction"] == "manual_1"))
plot_lines <- result$map$layers[[2L]]$data
stopifnot(setequal(plot_lines$line_source, c("Predictive model", "SWIFD gap route",
  "Endpoint alignment (not mileage)")),
  file.exists(file.path(directory, "North_Puget_Sound_Steelhead_population_maps.pdf")),
  length(list.files(file.path(directory, "maps"), pattern = "\\.png$")) == 2L)

# User exclusions apply to artificial, transported, and gradient categories;
# ordinary presumed distribution is deliberately retained for either run.
categories <- data.frame(
  id = 1:10,
  SPECIESRUN = c("Winter Steelhead", "Summer Steelhead", "Winter Steelhead",
    "Winter Steelhead", "Summer Steelhead", "Winter Steelhead",
    "Winter Steelhead", "Summer Steelhead", "Coho Salmon", "Summer Steelhead"),
  DISTTYPE_DESC = c("Documented", "Presumed", "Historic - Documented",
    "Artificial - Documented", "Transported - Potential", "Gradient Accessible",
    "Presumed", "Documented", "Documented", "Gradient Presumed"),
  USETYPE_DESC = c("Presence", "Rearing", "Spawning", "Presence", "Presence",
    "Presence", "Gradient Accessible Presence", "Artificial - Documented",
    "Presence", "Presence"))
stopifnot(identical(eligible_steelhead_swifd(categories)$id, c(1:5, 8L)))
cat("SWIFD source provenance, physical-only mileage, barrier cuts, clipped GIS, maps, and category filters passed.\n")
