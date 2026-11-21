library(sf)
# Frozen pre-correction route audit: do not read or modify a regenerating cache.
route <- readRDS("spatial_data/canyon_audit/path.rds")$barriers
route <- route[route$source_layer == 6, ]
swifd <- st_transform(readRDS("spatial_data/north_puget_swifd_steelhead.rds"), st_crs(route))
nearest <- st_nearest_feature(route, swifd)
table <- st_drop_geometry(route)
table$nearest_SWIFD_stream_at_barrier <- swifd$LLID_STRM_NAME[nearest]
table$nearest_SWIFD_LLID_at_barrier <- swifd$LLID[nearest]
table$nearest_SWIFD_distance_m_at_barrier <- as.numeric(st_distance(route, swifd[nearest, ], by_element = TRUE))
table$passability_missing <- is.na(route$PercentFishPassableCode)
table$interpretation <- paste("Named Canyon/SF Canyon record; a missing passability value does not establish",
  "a complete barrier for summer steelhead. These are distinct from the two receiving-mainstem misassignments.")
write.csv(table, "outputs/canyon_creek_audit/canyon_nine_unverified_barriers_original_metadata.csv", row.names = FALSE)
print(table[, intersect(c("SiteId", "StreamName", "TributaryToName", "PercentFishPassableCode",
  "nearest_SWIFD_stream_at_barrier", "nearest_SWIFD_distance_m_at_barrier", "DataSource"), names(table))])
