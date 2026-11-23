library(sf)
source("R/phase3_tidal_sources.R")
x<-load_phase3_tidal_sources()
held<-x$all_polygons[x$all_polygons$source_geometry_repair_held,]
pops<-st_read("outputs/phase2_reviewed_barriers/North_Puget_Sound_Steelhead_phase2.gpkg",layer="noaa_populations",quiet=TRUE)
pop_geom<-st_union(st_geometry(pops))
net<-readRDS("spatial_data/north_puget_swifd_bridges.rds")$bridges$network
net<-net[net$measure_habitat,]
scope<-suppressWarnings(st_intersection(net,pop_geom))
length_m<-function(g)if(!length(g)||all(st_is_empty(g)))0 else sum(as.numeric(st_length(st_union(g))))
audit<-do.call(rbind,lapply(seq_len(nrow(held)),function(i) {
 raw<-suppressWarnings(st_intersection(st_geometry(net),st_geometry(held)[i]))
 nps<-suppressWarnings(st_intersection(st_geometry(scope),st_geometry(held)[i]))
 data.frame(tidal_source_key=held$tidal_source_key[i],tidal_class=held$tidal_class[i],
  repaired_area_change_m2=held$source_geometry_area_change_m2[i],full_download_line_m=length_m(raw),
  official_NPS_line_m=length_m(nps))
}))
print(audit,digits=12,row.names=FALSE)
write.csv(audit,"spatial_data/phase3/tidal_sources/held_geometry_scope_audit.csv",row.names=FALSE)
empty<-x$all_polygons[!x$all_polygons$source_geometry_present,]
print(st_drop_geometry(empty)[,intersect(c("tidal_source_key","Latitude_mid","Longitude_mid","Shape_Area","Feature_Area","tidal_class"),names(empty)),drop=FALSE])
stopifnot(all(audit$official_NPS_line_m<.001))
