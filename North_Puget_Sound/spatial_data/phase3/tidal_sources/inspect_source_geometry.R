library(sf)
x<-readRDS("spatial_data/phase3/tidal_sources/tidal_sources.rds")
print(vapply(x,object.size,numeric(1))/1024^2)
m<-jsonlite::fromJSON("spatial_data/phase3/tidal_sources/source_manifest.json",simplifyVector=FALSE)
r<-m$layers[[2]]
raw<-dplyr::bind_rows(lapply(r$batch_files,function(f) {
 q<-st_read(file.path("spatial_data/phase3/tidal_sources",f),quiet=TRUE)
 v<-st_drop_geometry(q);v[]<-lapply(v,as.character)
 st_sf(v,geometry=st_geometry(q))
}))
raw<-st_transform(raw,26910)
a<-as.numeric(st_area(raw))
j<-match(as.character(raw$OBJECTID),x$all_polygons$OBJECTID[x$all_polygons$tidal_source_layer==3])
z<-x$all_polygons[x$all_polygons$tidal_source_layer==3,][j,]
out<-data.frame(OBJECTID=raw$OBJECTID,tidal_class=raw$LS_TidalConnectivity,
 before_m2=a,after_m2=as.numeric(st_area(z)),published_m2=as.numeric(raw$Shape_Area)*.3048006096012192^2,
 valid_before=st_is_valid(raw),reason=st_is_valid(raw,reason=TRUE))
out$delta<-out$after_m2-out$before_m2
out$after_to_published<-out$after_m2/out$published_m2
out$before_to_published<-out$before_m2/out$published_m2
print(out[order(-abs(out$delta)),][1:20,],digits=12,row.names=FALSE)
write.csv(out,"spatial_data/phase3/tidal_sources/geometry_repair_diagnostic.csv",row.names=FALSE)
bad<-raw[abs(out$delta)>1,]
net<-readRDS("spatial_data/north_puget_swifd_bridges.rds")$bridges$network
net<-net[net$measure_habitat,]
cat("bad feature count",nrow(bad),"\n")
for(i in seq_len(nrow(bad))) {
 zz<-z[z$OBJECTID==as.character(bad$OBJECTID[i]),]
 overlap<-suppressWarnings(st_intersection(net,st_geometry(zz)))
 cat("ID",bad$OBJECTID[i],"bounds",as.numeric(st_bbox(st_transform(zz,4326))),
   "habitatm",sum(as.numeric(st_length(st_union(st_geometry(overlap))))),"\n")
}
