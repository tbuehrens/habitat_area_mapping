library(sf)
mask <- st_transform(st_read("spatial_data/upstream_basins/gorge_basin_whole.json", quiet = TRUE), 26910)
dam <- st_transform(st_sf(geometry = st_sfc(st_point(c(-121.2081565270582, 48.69786237252027)), crs = 4326)), 26910)
gage <- st_transform(st_sf(geometry = st_sfc(st_point(c(-121.246234788845, 48.6717922878038)), crs = 4326)), 26910)
cat("MASK AREA MI2", as.numeric(st_area(st_union(mask))) / 2589988.110336, "\n")
cat("DAM TO BOUNDARY M", as.numeric(st_distance(dam, st_boundary(st_union(mask)))), "\n")
cat("DAM INSIDE", lengths(st_intersects(dam, mask)) > 0, "DOWNSTREAM NEWHALEM INSIDE", lengths(st_intersects(gage, mask)) > 0, "\n")
old <- st_read("outputs/phase1_total_barriers/North_Puget_Sound_Steelhead_phase1.gpkg", layer = "phase1_extent_status", quiet = TRUE)
down <- old[old$measure_habitat & grepl("user_override:Gorge_Dam", old$downstream_of_barrier, fixed = TRUE), ]
overlap <- suppressWarnings(st_intersection(st_union(st_geometry(down)), st_union(st_geometry(mask))))
cat("GRAPH-PROVEN DOWNSTREAM MODEL/PATCH INSIDE WHOLE MASK M", sum(as.numeric(st_length(overlap))), "\n")
up <- old[old$measure_habitat & grepl("user_override:Gorge_Dam", old$removed_by_barrier, fixed = TRUE), ]
outside <- suppressWarnings(st_difference(st_union(st_geometry(up)), st_union(st_geometry(mask))))
cat("GRAPH-PROVEN UPSTREAM OUTSIDE WHOLE MASK M", sum(as.numeric(st_length(outside))), "\n")
reference_path <- "spatial_data/phase1_validation/gorge_graph_reference_before_basin.rds"
if (!file.exists(reference_path)) saveRDS(list(downstream = st_sf(geometry = st_union(st_geometry(down))),
  upstream = st_sf(geometry = st_union(st_geometry(up))), dam = dam, downstream_gage = gage,
  whole_mask = mask), reference_path)
