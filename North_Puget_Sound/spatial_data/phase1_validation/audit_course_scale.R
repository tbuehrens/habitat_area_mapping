# Read-only analysis: compare local tangent windows on cached source geometry.
source("R/barrier_stream_identity.R")
source("R/stream_edge_identity.R")
source("R/nldi_delineation.R")
suppressPackageStartupMessages(library(sf))
selected <- readRDS("spatial_data/phase1_validation/phase1_selected_totals.rds")
edges <- st_read("outputs/review_swifd_connections/North_Puget_Sound_Steelhead.gpkg",layer="stream_matching_edges",quiet=TRUE)
index <- nldi_support_index(edges)
records <- jsonlite::fromJSON("spatial_data/upstream_basins/batch/manifest.json",simplifyVector=FALSE)
answer <- list()
for(record in records) {
  latest <- file.path(record$cache_folder,"manifest.json")
  if(file.exists(latest)) {
    candidate <- jsonlite::fromJSON(latest,simplifyVector=FALSE)
    if(identical(candidate$request_sha256,record$request_sha256)) record <- candidate
  }
  file <- file.path(record$cache_folder,"named_flowline.json")
  if(!file.exists(file) || is.null(record$comid)) next
  row <- match(record$review_record_key,selected$review_record_key)
  if(is.na(row)) next
  raw <- jsonlite::fromJSON(file,simplifyVector=FALSE)
  features <- Filter(function(x) identical(as.character(x$properties$comid),as.character(record$comid)),raw$features)
  if(length(features)!=1) next
  feature <- features[[1]]
  route <- st_read(jsonlite::toJSON(list(type="FeatureCollection",features=features),auto_unbox=TRUE,digits=NA),quiet=TRUE)
  route <- suppressWarnings(st_cast(st_transform(route,st_crs(edges)),"LINESTRING"))
  edge <- match(selected$graph_edge_id[row],edges$graph_edge_id)
  point <- st_coordinates(selected[row,])[1L,1:2]
  projection <- stream_identity_project(index$coordinates[[edge]],point)
  support <- stream_identity_local_support(index,edge,projection$measure,50)
  if(support$length<40 || support$topology_ambiguous || support$junction_distance<=25) next
  m <- seq(0,support$length,length.out=7)
  xy <- t(vapply(m,function(value)stream_identity_at(support$xy,value),numeric(2)))
  sample_points <- st_sfc(lapply(seq_len(7),function(j)st_point(xy[j,])),crs=st_crs(edges))
  near <- st_nearest_feature(sample_points,route)
  result <- data.frame(review_number=selected$review_number[row],review_record_key=record$review_record_key,
    barrier_stream=selected$StreamName[row],reference_stream=feature$properties$gnis_name,
    name_matches=stream_identity_names_match(selected$StreamName[row],feature$properties$gnis_name),
    cached_status=record$status,stringsAsFactors=FALSE)
  ref_coords <- lapply(near,function(j)unclass(st_geometry(route)[[j]])[,1:2,drop=FALSE])
  refs <- lapply(seq_len(7),function(j)stream_identity_project(ref_coords[[j]],xy[j,]))
  d <- vapply(refs,`[[`,numeric(1),"distance")
  result$max_sample_distance_m <- max(d)
  result$median_sample_distance_m <- median(d)
  result$point_route_distance_m <- min(as.numeric(st_distance(selected[row,],route)))
  for(width in c(5,25,40)) {
    angles <- vapply(seq_len(7),function(j) {
      own <- stream_identity_at(support$xy,min(support$length,m[j]+width))-
        stream_identity_at(support$xy,max(0,m[j]-width))
      coords <- ref_coords[[j]]
      len <- sum(sqrt(rowSums(diff(coords)^2)))
      ref <- stream_identity_at(coords,min(len,refs[[j]]$measure+width))-
        stream_identity_at(coords,max(0,refs[[j]]$measure-width))
      norm <- sqrt(sum(own^2)*sum(ref^2))
      if(norm>1e-12) acos(min(1,max(0,abs(sum(own*ref))/norm)))*180/pi else 90
    },numeric(1))
    result[[paste0("coverage_",width)]] <- mean(d<=25+1e-8 & angles<=35+1e-8)
    result[[paste0("angles_",width)]] <- paste(round(angles,2),collapse=";")
  }
  answer[[length(answer)+1L]] <- result
}
out <- do.call(rbind,answer)
write.csv(out,"spatial_data/phase1_validation/course_scale_audit.csv",row.names=FALSE)
eligible <- out$name_matches & out$point_route_distance_m<=50+1e-8
cat("Cached rows audited:",nrow(out),"same-name<=50m:",sum(eligible),"\n")
for(width in c(5,25,40))cat("Pass at",width,"m tangent halfspan:",sum(eligible & out[[paste0('coverage_',width)]]>=.8),"\n")
recovered <- eligible & out$coverage_5<.8 & out$coverage_25>=.8
lost <- eligible & out$coverage_5>=.8 & out$coverage_25<.8
cat("Recovered5->25:",sum(recovered),"lost:",sum(lost),"\n")
print(out[recovered,c('review_number','barrier_stream','reference_stream','max_sample_distance_m','coverage_5','coverage_25','coverage_40')])
cat("Lost rows:\n")
print(out[lost,c('review_number','barrier_stream','reference_stream','max_sample_distance_m','coverage_5','coverage_25','coverage_40')])
