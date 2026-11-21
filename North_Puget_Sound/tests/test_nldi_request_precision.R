source(file.path("R","barrier_stream_identity.R"))
source(file.path("R","stream_edge_identity.R"))
source(file.path("R","nldi_delineation.R"))
suppressPackageStartupMessages(library(sf))
edge<-st_sf(graph_edge_id=1L,from_node=1L,to_node=2L,length_m=200,
  geometry=st_sfc(st_linestring(rbind(c(500000,5400000),c(500200,5400000))),crs=26910))
barrier<-st_sf(review_record_key="5:test",review_number=123L,SiteId="test",StreamName="Own Cr",
  graph_edge_id=1L,override_future_cut=FALSE,
  geometry=st_sfc(st_point(c(500100,5400000)),crs=26910))
folder<-tempfile("nldi_precision_")
prepared<-prepare_phase1_basin_requests(barrier,edge,folder)
cached<-jsonlite::fromJSON(prepared$path,simplifyVector=FALSE)[[1L]]
stopifnot(cached$preparation_status=="ready",length(cached$support_headings)==7L,
  identical(cached$review_record_key,"5:test"),cached$review_number==123L,
  cached$course_tangent_halfspan_m==25,nldi_course_controls()$heading_halfspan_m==25)
expected_spans<-c(25,125/3,50,50,50,125/3,25)
for(i in seq_along(cached$support_headings)) {
  heading<-cached$support_headings[[i]]
  first<-unlist(heading[[1L]]);last<-unlist(heading[[2L]])
  stopifnot(sqrt(sum((last-first)^2))>1e-6)
  metric<-st_coordinates(st_transform(st_sfc(st_point(first),st_point(last),crs=4326),26910))
  stopifnot(abs(sqrt(sum((metric[2,]-metric[1,])^2))-expected_spans[i])<1e-5)
}
point<-st_coordinates(st_transform(barrier,4326))[1L,1:2]
stopifnot(abs(cached$longitude-point[1L])<1e-12,abs(cached$latitude-point[2L])<1e-12)
cat("NLDI request source coordinates and symmetric25m heading chords retain full precision\n")
