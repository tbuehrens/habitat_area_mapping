library(sf)
p <- 'outputs/phase1_total_barriers/North_Puget_Sound_Steelhead_phase1.gpkg'
print(st_layers(p))
for(n in c('spatial_data/north_puget_steelhead.rds','spatial_data/noded_steelhead.rds')) {
  x <- readRDS(n);cat(n,'\n');print(class(x));print(names(x))
}
review <- st_read('outputs/review_swifd_connections/North_Puget_Sound_Steelhead.gpkg',layer='barriers_snapped',quiet=TRUE)
edges <- st_read('outputs/review_swifd_connections/North_Puget_Sound_Steelhead.gpkg',layer='stream_matching_edges',quiet=TRUE)
print(names(edges))
point <- st_transform(st_sfc(st_point(c(-121.686111111111,47.9744444444444)),crs=4326),26910)
probes<-data.frame(label=c('USGS_historical_gauge','USGS_current_NWIS','Ecology_dam','NID_dam','USGS_GNIS_dam'),longitude=c(-121.686111111111,-121.6873414,-121.687897,-121.686,-121.6865081),latitude=c(47.9744444444444,47.97427,47.974825,47.97492,47.9753812))
probes_sf<-st_transform(st_as_sf(probes,coords=c('longitude','latitude'),crs=4326),26910)
probes$nearest_edge<-edges$graph_edge_id[st_nearest_feature(probes_sf,edges)]
probes$nearest_distance_m<-as.numeric(st_distance(probes_sf,edges[st_nearest_feature(probes_sf,edges),],by_element=TRUE))
print(probes)
near <- order(drop(st_distance(edges,point)))[1:6]
print(cbind(st_drop_geometry(edges[near,]),distance_m=drop(st_distance(edges[near,],point))))
saveRDS(list(review=review,edges=edges),'spatial_data/phase2/dam_tolt_inspect.rds')
named<-readRDS('spatial_data/north_puget_named_stream_reference.rds')
print(class(named));print(names(named))
if(is.list(named)&&!inherits(named,'sf')) named<-named$data
sf_route<-named[as.character(named$LLID)=='1218201476970',]
print(st_bbox(sf_route));print(names(sf_route))
st_write(sf_route,'spatial_data/phase2/sf_tolt_reference.geojson',delete_dsn=TRUE,quiet=TRUE)
for(i in c(170,222)) {
  z<-read.csv('outputs/phase1_total_barriers/phase1_barrier_decisions.csv')
  print(z[z$review_number==i,c('review_number','StreamName','graph_edge_id','matched_llid','matched_route_key','snapped_x','snapped_y')])
}
