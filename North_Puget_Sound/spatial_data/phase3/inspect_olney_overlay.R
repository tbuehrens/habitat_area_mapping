library(sf)
source('R/upstream_basins.R')
d<-readRDS('spatial_data/phase3/olney_apply_debug.rds')
b<-readRDS('spatial_data/phase3/olney_barriers.rds')
down<-d$fresh_network[upstream_has_key(d$fresh_network$downstream_of_barrier,'6:43814'),]
for(label in c('debug_source','debug_mask','bundle_mask')) {
  geometry<-switch(label,debug_source=d$source_geometry,debug_mask=st_geometry(d$mask),bundle_mask=st_geometry(b$masks))
  for(group in c('all','down')) {
    lines<-if(group=='all')d$selected else down
    x<-upstream_line_parts(suppressWarnings(st_intersection(lines,geometry)))
    x<-x[x$graph_edge_id %in% 6133L,]
    p<-b$barriers
    dist<-as.numeric(st_distance(st_centroid(st_geometry(x)),p))
    cat(label,group,'total=',sum(as.numeric(st_length(x))),'remote=',sum(as.numeric(st_length(x))[dist>100]),'\n')
  }
}
cat('inside saved remote lengths\n')
x<-d$inside[d$inside$graph_edge_id %in% 6133L,]
print(data.frame(length=as.numeric(st_length(x)),distance=as.numeric(st_distance(st_centroid(st_geometry(x)),b$barriers))))
cat('Raw WKB equality: ',identical(st_as_binary(d$source_geometry),st_as_binary(st_geometry(d$mask))),
 identical(st_as_binary(st_geometry(d$mask)),st_as_binary(st_geometry(b$masks))),'\n')
