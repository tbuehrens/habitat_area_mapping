library(sf)
source("R/phase3_tidal_sources.R")
source("R/phase3_nwi_tidal.R")
cfs<-load_phase3_tidal_sources()
nwi<-load_phase3_nwi_tidal()
r<-st_transform(readRDS("spatial_data/north_puget_named_stream_reference.rds"),26910)
names<-c("Snohomish River","Snoqualmie River","Skykomish River","Stillaguamish River","Nooksack River","Samish River","Skagit River","Skagit River, North Fork","Skagit River, South Fork")
measure<-function(g)if(!length(g)||all(st_is_empty(g)))0 else sum(as.numeric(st_length(st_union(g))))
rows<-list()
for(label in names) {
 own<-r[r$LLID_STREAMNAME==label & !is.na(r$LLID_STREAMNAME),]
 line<-st_union(st_geometry(own))
 a<-suppressWarnings(st_intersection(line,st_geometry(cfs$mask)))
 b<-suppressWarnings(st_intersection(line,st_geometry(nwi$mask)))
 extra<-suppressWarnings(st_intersection(line,st_geometry(nwi$added_mask)))
 touch<-nwi$all_polygons[lengths(st_intersects(nwi$all_polygons,line))>0,]
 years<-nwi$source_vintage_join[nwi$source_vintage_join$nwi_source_key %in% touch$nwi_source_key,]
 rows[[length(rows)+1]]<-data.frame(stream=label,CFS_tidal_reference_m=measure(a),
  NWI_R1_reference_m=measure(b),NWI_added_beyond_CFS_reference_m=measure(extra),
  NWI_codes=paste(unique(touch$ATTRIBUTE),collapse=";"),
  source_image_years=paste(sort(unique(na.omit(years$IMAGE_YR))),collapse=";"))
 if(label %in% c("Snohomish River","Snoqualmie River","Skykomish River")) {
  cat(label,"NWI_added_bbox_WGS84\n");print(st_bbox(st_transform(extra,4326)))
 }
}
audit<-do.call(rbind,rows)
print(audit,digits=12,row.names=FALSE)
write.csv(audit,"spatial_data/phase3/nwi_tidal/river_reference_comparison.csv",row.names=FALSE)
pops<-st_read("outputs/phase2_reviewed_barriers/North_Puget_Sound_Steelhead_phase2.gpkg",layer="noaa_populations",quiet=TRUE)
before<-st_read("outputs/phase2_reviewed_barriers/North_Puget_Sound_Steelhead_phase2.gpkg",layer="phase2_remaining",quiet=TRUE)
before<-before[before$measure_habitat,]
physical<-do.call(rbind,lapply(pops$pop_id,function(id) {
 z<-before[before$pop_id==id,]
 a<-suppressWarnings(st_intersection(st_geometry(z),st_geometry(cfs$mask)))
 b<-suppressWarnings(st_intersection(st_geometry(z),st_geometry(nwi$added_mask)))
 data.frame(pop_id=id,population=pops$population[pops$pop_id==id],
  CFS_removed_miles=measure(a)/1609.344,NWI_additional_removed_miles=measure(b)/1609.344)
}))
print(physical,digits=12,row.names=FALSE)
write.csv(physical,"spatial_data/phase3/nwi_tidal/population_remaining_habitat_comparison.csv",row.names=FALSE)
saveRDS(list(reference_comparison=audit,population_comparison=physical),"spatial_data/phase3/nwi_tidal/comparison.rds")
