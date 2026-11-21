# Synthetic end-to-end export check; no API requests or external data needed.
# Run from the repository root: Rscript tests/test_workflow.R
source(file.path("R", "network.R"))
source(file.path("R", "habitat_workflow.R"))
library(sf)

rectangle <- function(xmin, ymin, xmax, ymax) {
  st_polygon(list(rbind(c(xmin, ymin), c(xmax, ymin), c(xmax, ymax),
                       c(xmin, ymax), c(xmin, ymin))))
}
populations <- st_sf(
  pop_id = c(1L, 2L, 3L),
  population = c("Nooksack synthetic west", "Synthetic east", "Synthetic empty"),
  geometry = st_sfc(
    rectangle(499900, 5399900, 501000, 5401100),
    rectangle(501000, 5399900, 502100, 5401100),
    rectangle(502100, 5399900, 503100, 5401100), crs = 26910
  )
)
steelhead <- st_sf(geometry = st_sfc(
  st_linestring(rbind(c(500000, 5400000), c(501000, 5401000),
                       c(502000, 5401000))),
  st_linestring(rbind(c(501200, 5401050), c(501800, 5401050))), crs = 26910
))
barriers <- st_sf(
  id = 1L, verification = "Verified",
  geometry = st_sfc(st_point(c(500500, 5400500)), crs = 26910)
)
mouth <- st_sfc(st_point(c(500000, 5400000)), crs = 26910)
trace <- truncate_network_at_barriers(
  steelhead, barriers, mouth, root_distance = 0.01, snap_distance = 1
)
output_dir <- file.path(tempdir(), "habitat_workflow_synthetic")
result <- habitat_export(populations, trace, trace, steelhead, barriers,
                         output_dir, metadata = list(synthetic_check = TRUE))

table <- result$lengths
west <- table[table$pop_id == 1L, ]
east <- table[table$pop_id == 2L, ]
empty <- table[table$pop_id == 3L, ]
near <- function(actual, expected) {
  stopifnot(isTRUE(all.equal(actual, expected, tolerance = 1e-8)))
}
near(west$barrier_truncated_miles, sqrt(2) * 500 / 1609.344)
near(west$excluded_above_barriers_miles, sqrt(2) * 500 / 1609.344)
stopifnot(is.na(east$barrier_truncated_miles))
near(east$outlet_connected_miles, 0)
near(east$excluded_above_barriers_miles, 1000 / 1609.344)
near(east$unresolved_connectivity_miles, 600 / 1609.344)
near(empty$predicted_untruncated_miles, 0)
stopifnot(grepl("PROVISIONAL", east$qa_status))
expected_files <- c(
  "North_Puget_Sound_Steelhead_hab_lengths.csv", "natural_barrier_audit.csv",
  "North_Puget_Sound_Steelhead_map.png",
  "North_Puget_Sound_Steelhead_population_maps.pdf",
  "Nooksack_Steelhead_map.png", "Nooksack_Steelhead_hab_lengths.csv",
  "North_Puget_Sound_Steelhead.gpkg", "run_metadata.json", "sessionInfo.txt"
)
stopifnot(all(file.exists(file.path(output_dir, expected_files))),
          length(list.files(file.path(output_dir, "maps"), pattern = "\\.png$")) == 3L)
layers <- st_layers(file.path(output_dir, "North_Puget_Sound_Steelhead.gpkg"))$name
stopifnot(all(c("noaa_populations", "predicted_extent_status",
               "natural_barriers_audit", "downstream_roots") %in% layers))
written <- st_read(file.path(output_dir, "North_Puget_Sound_Steelhead.gpkg"),
                   layer = "predicted_extent_status", quiet = TRUE)
near(sum(as.numeric(st_length(written))), sqrt(2) * 1000 + 1600)
cat("Synthetic workflow clips, mass balance, maps, CSV, PDF, and GeoPackage passed.\n")
