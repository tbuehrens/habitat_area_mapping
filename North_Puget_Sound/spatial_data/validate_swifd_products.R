library(sf)
cache <- readRDS("spatial_data/north_puget_swifd_bridges.rds")$bridges
stopifnot(cache$summary$augmented_cycle_rank <= cache$summary$original_cycle_rank,
  all(cache$bridge_audit$from_snap_m <= 100 + 1e-8),
  all(cache$bridge_audit$to_snap_m <= 100 + 1e-8))
for (directory in c("outputs/north_puget_sound", "outputs/nooksack")) {
  tab <- read.csv(file.path(directory,"North_Puget_Sound_Steelhead_hab_lengths.csv"))
  file <- file.path(directory,"North_Puget_Sound_Steelhead.gpkg")
  g <- st_read(file,layer="predicted_extent_status",quiet=TRUE)
  stopifnot(all(g$measure_habitat == (g$source_class != "alignment_link")),
    all(abs(g$length_m-as.numeric(st_length(g))) < 1e-6),
    all(abs(tab$augmented_untruncated_miles-tab$predicted_untruncated_miles-tab$swifd_added_miles)<.001),
    all(abs(tab$augmented_untruncated_miles-tab$outlet_connected_miles-
      tab$excluded_above_barriers_miles-tab$unresolved_connectivity_miles)<.001),
    all(abs(tab$outlet_connected_miles-tab$prediction_outlet_connected_miles-
      tab$swifd_outlet_connected_miles)<.001),
    length(list.files(file.path(directory,"maps"),pattern="\\.png$")) == nrow(tab))
  for (i in seq_len(nrow(tab))) {
    one <- g[g$pop_id==tab$pop_id[i] & g$measure_habitat & g$retained,]
    miles <- if (nrow(one)) sum(as.numeric(st_length(st_union(st_geometry(one)))))/1609.344 else 0
    stopifnot(abs(miles-tab$outlet_connected_miles[i])<.001)
  }
  layers <- st_layers(file)$name
  stopifnot(all(c("swifd_gap_routes_before_barriers","endpoint_alignment_links","gap_endpoint_audit") %in% layers))
  audit <- read.csv(file.path(directory,"swifd_connector_audit.csv"))
  stopifnot(all(audit$from_snap_m<=100+1e-8),all(audit$to_snap_m<=100+1e-8))
  cat(directory,":",nrow(tab),"populations; sources, endpoint tolerance, GIS lengths, and habitat partitions verified\n")
}
a <- read.csv("outputs/north_puget_sound/Nooksack_Steelhead_hab_lengths.csv")
b <- read.csv("outputs/nooksack/Nooksack_Steelhead_hab_lengths.csv")
w <- read.csv("Nooksack_Winter_Steelhead_hab_lengths.csv")
stopifnot(isTRUE(all.equal(a,b)),
  abs(w$outlet_connected_miles-a$outlet_connected_miles[a$run_timing=="wi"])<1e-6)
cat("Nooksack aliases and root-level winter product agree with regional analysis\n")
