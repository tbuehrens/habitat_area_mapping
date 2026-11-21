source("R/barrier_stream_identity.R")
source("R/stream_edge_identity.R")
source("R/nldi_delineation.R")
suppressPackageStartupMessages(library(sf))
suppressPackageStartupMessages(library(ggplot2))
audit<-read.csv("spatial_data/phase1_validation/course_scale_audit.csv")
recover<-audit[audit$name_matches & audit$point_route_distance_m<=50 & audit$coverage_5<.8 & audit$coverage_25>=.8,]
selected<-readRDS("spatial_data/phase1_validation/phase1_selected_totals.rds")
edges<-st_read("outputs/review_swifd_connections/North_Puget_Sound_Steelhead.gpkg",layer="stream_matching_edges",quiet=TRUE)
index<-nldi_support_index(edges)
records<-jsonlite::fromJSON("spatial_data/upstream_basins/batch/manifest.json",simplifyVector=FALSE)
figures<-lapply(seq_len(nrow(recover)),function(i) {
  row<-recover[i,]
  selected_row<-selected[selected$review_record_key==row$review_record_key,]
  record<-records[[which(vapply(records,function(x)identical(x$review_record_key,row$review_record_key),logical(1)))]]
  raw<-jsonlite::fromJSON(file.path(record$cache_folder,"named_flowline.json"),simplifyVector=FALSE)
  feature<-Filter(function(x)as.character(x$properties$comid)==as.character(record$comid),raw$features)
  ref<-st_read(jsonlite::toJSON(list(type="FeatureCollection",features=feature),auto_unbox=TRUE,digits=NA),quiet=TRUE)
  ref<-st_transform(ref,st_crs(edges))
  at<-st_coordinates(selected_row)[1,1:2]
  edge<-match(selected_row$graph_edge_id,edges$graph_edge_id)
  pos<-stream_identity_project(index$coordinates[[edge]],at)$measure
  support<-stream_identity_local_support(index,edge,pos,50)
  own<-st_sf(geometry=st_sfc(st_linestring(support$xy),crs=st_crs(edges)))
  samples<-st_sfc(lapply(seq(0,support$length,length.out=7),function(m)st_point(stream_identity_at(support$xy,m))),crs=st_crs(edges))
  ggplot()+geom_sf(data=ref,color="#e67c20",linewidth=1.0)+geom_sf(data=own,color="#126a91",linewidth=.8)+
    geom_sf(data=st_sf(geometry=samples),color="#126a91",size=1.0)+geom_sf(data=selected_row,shape=21,fill="white",size=2)+
    coord_sf(xlim=at[1]+c(-80,80),ylim=at[2]+c(-80,80),expand=FALSE,datum=NA)+
    labs(title=paste0("#",row$review_number," ",row$barrier_stream),
      subtitle=sprintf("Same GNIS name | coverage 5m %.0f%% -> 25m %.0f%%\nmax point distance %.1fm",100*row$coverage_5,100*row$coverage_25,row$max_sample_distance_m),x=NULL,y=NULL)+
    theme_bw(base_size=9)+theme(plot.title=element_text(face="bold",size=10),plot.subtitle=element_text(size=8),axis.text=element_blank(),axis.ticks=element_blank())
})
combined<-gridExtra::arrangeGrob(grobs=figures,ncol=5,
  top="Course-scale audit: blue = 100m physical-model support; orange = NHDPlusV2 route; dots = 7 samples",
  bottom="Read-only diagnostic. Both windows use identical <=25m distance, <=35 degree heading and >=80% coverage gates; all names match.")
ggsave("spatial_data/phase1_validation/course_scale_recoveries.png",combined,width=17,height=8,dpi=180,bg="white")
ggsave("spatial_data/phase1_validation/course_scale_recoveries.pdf",combined,width=17,height=8,bg="white")

# Counterexamples retain independent distance/name gates under either window.
course_coverage<-function(own,reference,width) {
  length_of<-function(xy)sum(sqrt(rowSums(diff(xy)^2)))
  own_len<-length_of(own);ref_len<-length_of(reference)
  positions<-seq(0,own_len,length.out=7)
  mean(vapply(positions,function(m) {
    point<-stream_identity_at(own,m);hit<-stream_identity_project(reference,point)
    a<-stream_identity_at(own,min(own_len,m+width))-stream_identity_at(own,max(0,m-width))
    b<-stream_identity_at(reference,min(ref_len,hit$measure+width))-stream_identity_at(reference,max(0,hit$measure-width))
    norm<-sqrt(sum(a^2)*sum(b^2));angle<-if(norm)acos(min(1,max(0,abs(sum(a*b))/norm)))*180/pi else 90
    hit$distance<=25 && angle<=35
  },logical(1)))
}
own<-rbind(c(-50,0),c(50,0))
crossing<-rbind(c(0,-100),c(0,100))
parallel_far<-rbind(c(-100,30),c(100,30))
parallel_near<-rbind(c(-100,20),c(100,20))
for(width in c(5,25,40)) {
  stopifnot(course_coverage(own,crossing,width)<.8,
    course_coverage(own,parallel_far,width)<.8,
    !(stream_identity_names_match("Own Creek","Other Creek") && course_coverage(own,parallel_near,width)>=.8))
}
cat("Crossing, >25m parallel, and wrong-named nearby parallel counterexamples stay blocked at 5m/25m/40m windows.\n")
