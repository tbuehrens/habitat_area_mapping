library(sf)
source("R/phase3_tidal_sources.R")
x<-load_phase3_tidal_sources()
quick<-readRDS("spatial_data/phase3/tidal_sources/major_channel_quick_audit.rds")
net<-readRDS("spatial_data/north_puget_swifd_bridges.rds")$bridges$network
net<-net[net$measure_habitat,]
rows<-list()
for(record in quick) {
 gap<-suppressWarnings(st_cast(record$gaps,"LINESTRING"))
 gap<-gap[!st_is_empty(gap) & as.numeric(st_length(gap))>.001]
 for(i in seq_along(gap)) {
  candidates<-x$all_polygons[lengths(st_intersects(x$all_polygons,gap[i]))>0,]
  overlap<-suppressWarnings(st_intersection(candidates,gap[i]))
  overlap<-overlap[!st_is_empty(overlap) & st_dimension(overlap)==1,]
  labels<-if(nrow(overlap))paste(unique(paste0(overlap$tidal_source_layer,":",overlap$tidal_class)),collapse=";")else"outside_all_mapped_source_polygons"
  # Proximity is a diagnostic, never independent cut authorization.
  near<-net[lengths(st_intersects(net,st_buffer(gap[i],25)))>0,]
  outside<-suppressWarnings(st_difference(near,st_geometry(x$mask)))
  inside25<-suppressWarnings(st_intersection(outside,st_buffer(gap[i],25)))
  source_m<-if(nrow(inside25))sum(as.numeric(st_length(st_union(st_geometry(inside25))))) else 0
  rows[[length(rows)+1]]<-data.frame(stream=record$stream,LLID=record$LLID,
   gap_piece=i,reference_gap_m=as.numeric(st_length(gap[i])),classifications=labels,
   physical_source_outside_mask_within25m_of_gap_m=source_m,
   root_method=record$root_method)
 }
}
out<-do.call(rbind,rows)
print(out,digits=12,row.names=FALSE)
write.csv(out,"spatial_data/phase3/tidal_sources/major_channel_gap_classification.csv",row.names=FALSE)
