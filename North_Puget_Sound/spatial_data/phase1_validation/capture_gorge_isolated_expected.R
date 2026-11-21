# Independent expected upstream geometry from the frozen untruncated export.
# Save once so a later Phase1 rerender cannot redefine the reference.
library(sf)
destination <- "spatial_data/phase1_validation/gorge_isolated_upstream_expected.rds"
if (file.exists(destination)) stop("Expected geometry already captured; refusing to overwrite")
before <- st_read("outputs/review_swifd_connections/North_Puget_Sound_Steelhead.gpkg",
  layer = "steelhead_patched_untruncated", quiet = TRUE)
before <- before[before$measure_habitat, ]
reference <- readRDS("spatial_data/phase1_validation/gorge_graph_reference_before_basin.rds")
mask <- st_read("spatial_data/upstream_basins/gorge_split_basin.json", quiet = TRUE)
mask <- st_transform(mask[mask$id == "drainageBasin", ], st_crs(before))
expected <- suppressWarnings(st_intersection(before, st_union(st_geometry(mask))))
expected <- expected[!st_is_empty(expected) & st_dimension(expected) == 1, ]
already <- st_union(c(st_geometry(reference$upstream), st_geometry(reference$downstream)))
new <- suppressWarnings(st_difference(expected, st_buffer(already, 1e-5)))
new <- new[!st_is_empty(new) & st_dimension(new) == 1, ]
unique_length <- sum(as.numeric(st_length(st_union(st_geometry(new)))))
saveRDS(list(geometry = new, upstream_mask = mask, expected_unique_m = unique_length,
  baseline_gpkg_md5 = unname(tools::md5sum("outputs/review_swifd_connections/North_Puget_Sound_Steelhead.gpkg")),
  note = "Complete upstream physical source geometry outside both old graph-proven upstream and downstream sets; must not survive the basin-complete scenario"), destination)
cat("Additional isolated/disconnected Gorge upstream source geometry expected removed:", unique_length, "m\n")
