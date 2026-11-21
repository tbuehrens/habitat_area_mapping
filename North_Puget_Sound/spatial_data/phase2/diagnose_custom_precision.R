library(sf)
x<-readRDS("spatial_data/phase2/custom_partition_diagnostic.rds")
for(n in c("source","extent")) {
 other<-if(n=="source") "extent" else "source"
 coords<-st_coordinates(x[[n]])[,1:2,drop=FALSE]
 points<-st_as_sf(data.frame(x=coords[,1],y=coords[,2]),coords=c("x","y"),crs=st_crs(x[[n]]))
 distances<-as.numeric(st_distance(points,st_union(st_geometry(x[[other]]))))
 cat(n,"maxvertexdistance_m",max(distances),"median",median(distances),"\n")
}
cat("Hausdorff_m",as.numeric(st_distance(st_union(st_geometry(x$source)),
 st_union(st_geometry(x$extent)),which="Hausdorff")),"\n")
cat("Lengthdelta_m",sum(as.numeric(st_length(st_union(st_geometry(x$extent)))))-
 sum(as.numeric(st_length(st_union(st_geometry(x$source))))),"\n")
try(source("tests/validate_phase2_custom_stillaguamish.R"))
old<-st_read("outputs/phase1_total_barriers/North_Puget_Sound_Steelhead_phase1.gpkg",layer="phase1_removed_upstream",quiet=TRUE)
old<-old[old$pop_id==292 & old$measure_habitat,]
keep<-vapply(strsplit(old$removed_by_barrier,";",fixed=TRUE),function(z) any(z %in% c("6:59136","6:59712")),logical(1))
newexpected<-suppressWarnings(st_difference(expected,st_union(st_geometry(old[keep,]))))
cat("Expected excluding retained Phase1 graph810/833",length_m(newexpected)/1609.344,"missing",missing_m(newexpected,sf_remaining),"\n")
