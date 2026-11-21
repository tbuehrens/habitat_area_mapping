library(sf)
ref <- st_transform(readRDS("spatial_data/north_puget_named_stream_reference.rds"),26910)
deer <- ref[ref$LLID=="1219314482681",]
main <- ref[ref$LLID=="1221262482038",]
mouth <- st_transform(st_sfc(st_point(c(-121.9314,48.2681)),crs=4326),26910)
junction <- suppressWarnings(st_intersection(st_union(st_geometry(deer)),st_union(st_geometry(main))))
print(junction)
print(st_coordinates(st_transform(junction,4326)))
print(st_distance(junction,mouth))
f <- st_read("spatial_data/phase2/deer_confluence_fabric.json",quiet=TRUE)
print(st_drop_geometry(f)[,c("comid","gnis_name","fromnode","tonode","flowdir","areasqkm","totdasqkm")])
for(i in seq_len(nrow(f))) {
  g <- suppressWarnings(st_cast(st_geometry(f)[i],"LINESTRING"))
  for(j in seq_along(g)) {
    xy <- unclass(g[[j]])
    cat("COMID",f$comid[i],f$gnis_name[i],"part",j,"first",xy[1,],"last",xy[nrow(xy),],"\n")
  }
}
site <- st_read("spatial_data/phase2/nf_stillaguamish_oso_site.json",quiet=TRUE)
print(site)
