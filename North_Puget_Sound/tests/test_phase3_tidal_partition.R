source("R/project_paths.R")
source("R/upstream_basins.R")
source("R/phase3_exports.R")
suppressPackageStartupMessages(library(sf))
offset <- c(500000, 5400000)
line <- function(a, b) st_linestring(rbind(a + offset, b + offset))
network <- st_sf(source_class = c("prediction", "swifd_connector", "alignment_link"),
  measure_habitat = c(TRUE, TRUE, FALSE), removed = c(FALSE, TRUE, FALSE),
  unresolved_direction = FALSE, removed_by_barrier = c("", "barrier_fixture", ""),
  geometry = st_sfc(line(c(0, 0), c(300, 0)), line(c(300, 0), c(400, 0)),
    line(c(100, 5), c(130, 5)), crs = 26910))
ring <- rbind(c(0, -10), c(200, -10), c(200, 10), c(0, 10), c(0, -10))
mask <- st_sf(geometry = st_sfc(st_polygon(list(sweep(ring, 2, offset, "+"))), crs = 26910))
result <- phase3_apply_tidal_mask(network, mask)
x <- result$network
sum_m <- function(rows) sum(as.numeric(st_length(x[rows, ])))
stopifnot(abs(sum_m(x$measure_habitat & x$retained) - 100) < 1e-8,
  abs(sum_m(x$measure_habitat & x$tidal_removed) - 200) < 1e-8,
  abs(sum_m(x$measure_habitat & x$barrier_removed) - 100) < 1e-8,
  abs(sum_m(!x$measure_habitat) - 30) < 1e-8,
  result$summary$source_partition_error_m < 1e-8,
  result$summary$remaining_inside_tidal_m < 1e-8)
pop <- st_sf(pop_id = 999L, population = "Fixture", geometry = st_buffer(st_union(st_geometry(network)), 20))
points <- phase3_tidal_cutpoints(network, mask, pop)
stopifnot(nrow(points) == 1L, abs(st_coordinates(points)[1L, 1L] - offset[1L] - 200) < 1e-8)
# Reversing line order cannot change a spatial tidal exclusion or its mileage.
reversed <- network
st_geometry(reversed) <- st_sfc(lapply(st_geometry(network), function(x) st_linestring(unclass(x)[nrow(x):1L, ])), crs = 26910)
again <- phase3_apply_tidal_mask(reversed, mask)
stopifnot(abs(again$summary$newly_removed_tidal_physical_m - 200) < 1e-8,
  length(st_equals(st_union(st_geometry(result$network[result$network$retained, ])),
    st_union(st_geometry(again$network[again$network$retained, ])))[[1L]]) == 1L)
cat("PASS: tidal/barrier/source partition, alignment exclusion, true crossing points, direction independence.\n")
