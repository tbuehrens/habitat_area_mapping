library(sf)
f<-'outputs/phase3_tidal_barriers/North_Puget_Sound_Steelhead_phase3.gpkg'
pop<-st_read(f,layer='noaa_populations',quiet=TRUE);pop<-pop[pop$pop_id==286,]
raw<-readRDS('spatial_data/north_puget_swifd_bridges.rds')$bridges$network
raw<-raw[raw$source_class=='swifd_connector',]
original<-suppressWarnings(st_intersection(raw,pop['pop_id']))
before<-st_read(f,layer='before_tidal_extent_status',quiet=TRUE)
before<-before[before$pop_id==286 & before$source_class=='swifd_connector',]
old<-st_read('outputs/phase2_reviewed_barriers/North_Puget_Sound_Steelhead_phase2.gpkg',layer='phase2_extent_status',quiet=TRUE)
old<-old[old$pop_id==286 & old$source_class=='swifd_connector',]
for(n in c('original','before','old')) {
 x<-get(n)
 cat(n,'rows',nrow(x),'sum_m',sum(as.numeric(st_length(x))),'union_m',sum(as.numeric(st_length(st_union(st_geometry(x))))),'\n')
}
extra<-st_difference(st_union(st_geometry(before)),st_buffer(st_union(st_geometry(original)),1e-5))
missing<-st_difference(st_union(st_geometry(original)),st_buffer(st_union(st_geometry(before)),1e-5))
cat('EXTRA',sum(as.numeric(st_length(extra))),'MISSING',sum(as.numeric(st_length(missing))),'\n')
print(st_bbox(extra))
cat('originaltype\n');print(table(st_geometry_type(original)))
saveRDS(list(original=original,before=before,old=old,extra=extra,missing=missing),
 'spatial_data/phase3/sauk_source_conservation.rds')
allpop<-st_read(f,layer='noaa_populations',quiet=TRUE)
custom<-st_read(f,layer='custom_review_watershed',quiet=TRUE)
boundaries<-rbind(st_sf(pop_id=allpop$pop_id,geometry=st_geometry(allpop)),st_sf(pop_id=custom$pop_id,geometry=st_geometry(custom)))
x<-suppressWarnings(st_intersection(readRDS('spatial_data/north_puget_swifd_bridges.rds')$bridges$network,boundaries))
status<-function(label) {
 z<-x[x$pop_id==286 & x$source_class=='swifd_connector',]
 cat(label,'rows',nrow(z),'length',sum(as.numeric(st_length(z))),'\n')
 print(table(st_geometry_type(z)))
}
status('allraw')
if(any(st_geometry_type(x)=='GEOMETRYCOLLECTION'))x<-suppressWarnings(st_collection_extract(x,'LINESTRING'))
status('extracted')
x<-x[st_geometry_type(x) %in% c('LINESTRING','MULTILINESTRING'),]
status('selected')
x<-suppressWarnings(st_cast(x,'LINESTRING'))
status('cast')
