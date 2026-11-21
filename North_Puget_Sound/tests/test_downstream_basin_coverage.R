source("R/upstream_basins.R")
source("R/barrier_stream_identity.R")
source("R/stream_edge_identity.R")
source("R/nldi_delineation.R")
library(sf)
offset <- c(500000, 5400000)
xy <- function(x) sweep(x, 2L, offset, "+")
mask <- st_sf(review_record_key = "lower:total", outlet_comid = "100",
  watershed_validated = TRUE, stream_identity_validated = TRUE, total_barrier_approved = TRUE,
  coverage_complete = TRUE, local_outlet_precision_validated = TRUE,
  geometry = st_sfc(st_polygon(list(xy(rbind(c(0, 0), c(2000, 0), c(2000, 2000), c(0, 2000), c(0, 0))))), crs = 26910))
keys <- c("upper:covered", "parallel:unproved", "same:upstream", "edge:near", "missing:channel",
  "same:downstream", "same:unknown", "same:reversed", "lower:total")
locations <- rbind(c(1000,1000), c(1000,1200), c(1000,1300), c(50,1000), c(1000,1500),
  c(1000,1800), c(1000,900), c(1000,800), c(1000,1700))
barriers <- st_sf(review_record_key = keys, StreamName = "Test Creek", graph_edge_id = c(1,1,1,2,1,1,1,1,1),
  snap_accepted = TRUE, stream_identity_confirmed = TRUE, identity_valid_for_cut = TRUE,
  matching_channel_count = 1L, junction_ambiguous = FALSE,
  geometry = st_sfc(lapply(seq_len(nrow(locations)), function(i) st_point(offset + locations[i, ])), crs = 26910))
edges <- st_sf(graph_edge_id = 1:2, from_node = c(1L,3L), to_node = c(2L,4L),
  geometry = st_sfc(st_linestring(xy(rbind(c(1000,0),c(1000,2000)))),
    st_linestring(xy(rbind(c(50,0),c(50,2000)))), crs = 26910))
coordinates <- st_coordinates(st_transform(barriers, 4326))
cache <- tempfile("coverage_cache_"); dir.create(cache)
rows <- setdiff(seq_along(keys), 5L)
ids <- c("200", "300", "100", "400", NA, "100", "100", "100", "100")
records <- lapply(rows, function(i) {
  folder <- file.path(cache, as.character(i)); dir.create(folder)
  reference <- edges[if (i == 4L) 2L else 1L, ]
  if (i == 8L) st_geometry(reference) <- st_sfc(st_linestring(xy(rbind(c(1000,2000),c(1000,0)))),crs=26910)
  reference$comid <- ids[i]; reference$gnis_name <- "Test Creek"
  reference$flowdir <- if (i == 7L) "Unknown" else "With Digitized"
  st_write(st_transform(reference,4326), file.path(folder,"named_flowline.json"), driver="GeoJSON",quiet=TRUE)
  list(review_record_key=keys[i], graph_edge_id=barriers$graph_edge_id[i], comid=ids[i],
    indexed_longitude=coordinates[i,1L], indexed_latitude=coordinates[i,2L], cache_folder=folder,
    # Deliberately bogus manifest approval metrics cannot replace geometry.
    approved=TRUE, route_support_coverage=0, reference_stream_name="Wrong cached label")
})
coverage <- assess_downstream_basin_coverage(barriers,mask,records,
  list("lower:total"=c("100","200","400")),stream_edges=edges)$audit
stopifnot(identical(coverage$covered_by_downstream_basin,c(TRUE,FALSE,TRUE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE)),
  coverage$own_cut_applied[9L], !any(coverage$own_cut_applied[-9L]),
  coverage$same_comid_upstream_gap_m[3L] > 100, all(coverage$cached_course_revalidated[rows[-length(rows)]]),
  coverage$model_boundary_distance_m[1L] > 100, coverage$indexed_boundary_distance_m[1L] > 100)
# A forged manifest approval cannot rescue a changed public reference name.
bad_record <- records[[1L]]
bad_route <- st_read(file.path(bad_record$cache_folder,"named_flowline.json"),quiet=TRUE)
bad_route$gnis_name <- "Different Creek"
st_write(bad_route,file.path(bad_record$cache_folder,"named_flowline.json"),driver="GeoJSON",delete_dsn=TRUE,quiet=TRUE)
recheck <- assess_downstream_basin_coverage(barriers,mask,records,
  list("lower:total"=c("100","200","400")),stream_edges=edges)$audit
stopifnot(!recheck$covered_by_downstream_basin[1L], !recheck$cached_course_revalidated[1L])
cat("Coverage independently checks current cached course; same-COMID coverage requires consistent published direction and >100m upstream order, while downstream/reversed/unknown cases stay pending.\n")
