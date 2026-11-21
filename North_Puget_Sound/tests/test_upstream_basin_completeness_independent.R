# A connected-component test cannot establish complete upstream exclusion.
# This independently specified drainage polygon contains isolated headwaters
# west of the barrier while excluding an adjacent parallel drainage to the east.
source(file.path("R", "upstream_basins.R"))
suppressPackageStartupMessages(library(sf))
check <- function(x, message) if (!isTRUE(x)) stop(message, call. = FALSE)
offset <- c(500000, 5400000)
xy <- function(x) sweep(x, 2, offset, "+")
line <- function(x) st_linestring(xy(x))
network <- st_sf(source_class = c(rep("prediction", 4), "swifd_connector", "swifd_connector", "alignment_link"),
  measure_habitat = c(rep(TRUE, 6), FALSE), bridge_id = c(rep(NA_character_, 4), "patch1", "patch2", "patch1"),
  geometry = st_sfc(
    line(rbind(c(0, 0), c(400, 0))),
    line(rbind(c(20, 300), c(80, 300))),
    line(rbind(c(250, 300), c(350, 300))),
    line(rbind(c(20, 100), c(80, 100))),
    line(rbind(c(250, 100), c(350, 100))),
    line(rbind(c(150, 25), c(250, 25))),
    line(rbind(c(250, 100), c(250, 110))), crs = 26910))
near_headwater <- network[2, ]
st_geometry(near_headwater) <- st_sfc(line(rbind(c(210, 10), c(230, 10))), crs = 26910)
network <- rbind(network, near_headwater)
mask <- st_sf(review_record_key = "test:upstream", watershed_validated = TRUE,
  stream_identity_validated = TRUE, total_barrier_approved = TRUE,
  coverage_complete = TRUE, local_outlet_precision_validated = TRUE,
  geometry = st_sfc(st_polygon(list(xy(rbind(c(200, -50), c(450, -50),
    c(450, 200), c(100, 200), c(100, 400), c(0, 400), c(0, 150),
    c(200, 150), c(200, -50))))), crs = 26910))
physical_length <- function(x) sum(as.numeric(st_length(x[x$measure_habitat, ])))
close <- function(a, b) abs(a - b) < 1e-7
result <- apply_upstream_basin_masks(network, mask)
check(close(physical_length(result$removed), 430) && close(physical_length(result$retained), 410),
  "Upstream basin clipping did not split the exact 430/410m physical partition")
left_inside <- suppressWarnings(st_intersection(st_union(st_geometry(result$retained)), st_union(st_geometry(mask))))
check(close(sum(as.numeric(st_length(left_inside))), 0),
  "Remaining linework survives inside an approved upstream drainage mask")
retained_geometry <- st_union(st_geometry(result$retained))
removed_geometry <- st_union(st_geometry(result$removed))
covered_length <- function(reference, geometry) {
  sum(as.numeric(st_length(suppressWarnings(st_intersection(st_geometry(reference), geometry)))))
}
check(close(covered_length(network[2, ], removed_geometry), 60),
  "An isolated upstream headwater west of the dam survived a graph/longitude filter")
check(close(covered_length(network[5, ], removed_geometry), 100),
  "A disconnected upstream SWIFD patch survived basin clipping")
check(close(covered_length(network[3, ], retained_geometry), 100) &&
  close(covered_length(network[4, ], retained_geometry), 60),
  "An adjacent parallel drainage or downstream disconnected reach was removed")
check(close(covered_length(network[6, ], retained_geometry), 50) &&
  close(covered_length(network[6, ], removed_geometry), 50),
  "A boundary-crossing polyline was dropped whole instead of split")
check(close(covered_length(network[7, ], removed_geometry), 10),
  "An orphan alignment link remained visible upstream after its physical route was removed")
check(close(covered_length(network[8, ], removed_geometry), 20),
  "A blanket local precision buffer protected an unproven upstream headwater")
for (source in unique(network$source_class)) {
  before <- sum(as.numeric(st_length(network[network$source_class == source, ])))
  after <- sum(as.numeric(st_length(result$network[result$network$source_class == source, ])))
  check(close(before, after), paste("Basin clipping changed source geometry length:", source))
}

# Missing hydrologic, identity, approval, coverage, or outlet proof cannot be
# silently promoted to a complete watershed cut. Refusal or an uncut result is safe.
for (field in c("watershed_validated", "stream_identity_validated", "total_barrier_approved",
    "coverage_complete", "local_outlet_precision_validated")) {
  invalid <- mask
  invalid[[field]] <- FALSE
  attempt <- tryCatch(apply_upstream_basin_masks(network, invalid), error = function(e) e)
  check(inherits(attempt, "error") || close(physical_length(attempt$removed), 0),
    paste("An upstream mask was applied without required proof:", field))
}
cat("PASS: isolated upstream model/SWIFD/headwater geometry removed, adjacent/downstream drainage retained, crossing lines split, orphan links removed, complete-mask residual zero, validation gates and source partition preserved.\n")
