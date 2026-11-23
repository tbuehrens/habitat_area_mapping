# Read-only channel coverage assessment; writes diagnostics only to phase3 cache.
# It does not modify the tidal mask, habitat, maps, or any earlier scenario.
suppressPackageStartupMessages(library(sf))
source("R/phase3_tidal_sources.R")
source("R/network.R")
source("R/barrier_stream_identity.R")
source("R/stream_edge_identity.R")
bundle <- load_phase3_tidal_sources()
reference <- st_transform(readRDS("spatial_data/north_puget_named_stream_reference.rds"),26910)
name_field <- intersect(c("reference_stream_name","LLID_STRM_NAME","LLID_STREAMNAME"),names(reference))[1L]
if(is.na(name_field)) stop("Named reference stream field is missing")
tidal <- st_union(st_geometry(bundle$mask))
footprint <- st_union(st_geometry(bundle$all_polygons))
possible <- lengths(st_intersects(reference,tidal))>0L
ids <- unique(as.character(reference$LLID[possible & !is.na(reference$LLID)]))
measure <- function(g) if(!length(g) || all(st_is_empty(g))) 0 else sum(as.numeric(st_length(st_union(g))))
linear <- function(g) {
  g <- g[!st_is_empty(g)]
  if(!length(g)) return(st_sfc(crs=26910))
  g <- suppressWarnings(st_collection_extract(g,"LINESTRING"))
  g <- suppressWarnings(st_cast(g,"LINESTRING"))
  g[as.numeric(st_length(g))>1e-7]
}
rows <- list(); routes <- list(); gaps <- list(); boundaries <- list()
for(id in ids) {
  own <- reference[!is.na(reference$LLID) & as.character(reference$LLID)==id,]
  label <- paste(unique(na.omit(own[[name_field]])),collapse=";")
  row <- data.frame(LLID=id,stream_name=label,root_status="not_evaluated",root_distance_m=NA_real_,
    reference_cycle_rank=NA_integer_,positive_tidal_channel_m=0,head_proxy_distance_from_mouth_m=NA_real_,
    downstream_channel_m=NA_real_,uncovered_downstream_channel_m=NA_real_,
    unmapped_downstream_channel_m=NA_real_,classified_nonselected_downstream_channel_m=NA_real_,
    longest_uncovered_piece_m=NA_real_,stringsAsFactors=FALSE)
  result <- tryCatch({
    geometry <- linear(st_geometry(own))
    geometry <- linear(st_union(geometry))
    xy <- lapply(geometry,function(x) unclass(x)[,1:2,drop=FALSE])
    ep <- do.call(rbind,lapply(xy,function(x) x[c(1L,nrow(x)),,drop=FALSE]))
    groups <- network_endpoint_groups(ep,.25)
    from <- groups[seq.int(1L,length(groups),2L)]
    to <- groups[seq.int(2L,length(groups),2L)]
    node_xy <- ep[!duplicated(groups),,drop=FALSE]
    graph <- igraph::graph_from_edgelist(cbind(as.character(from),as.character(to)),directed=FALSE)
    edge_length <- as.numeric(st_length(geometry))
    igraph::E(graph)$weight <- edge_length
    terminals <- as.integer(names(igraph::degree(graph))[igraph::degree(graph)==1L])
    if(!grepl("^[0-9]{13}$",id) || !length(terminals)) stop("no_unique_corroborated_reference_mouth")
    mouth <- st_transform(st_sfc(st_point(c(-as.numeric(substr(id,1,7))/10000,
      as.numeric(substr(id,8,13))/10000)),crs=4326),26910)
    terminal_points <- st_sfc(lapply(terminals,function(j) st_point(node_xy[j,])),crs=26910)
    root_distance <- as.numeric(st_distance(terminal_points,mouth))
    root <- which(root_distance<=100)
    row$root_distance_m <- min(root_distance)
    if(length(root)!=1L) stop("no_unique_corroborated_reference_mouth")
    root <- terminals[root]
    component <- igraph::components(graph)$membership
    root_component <- component[as.character(root)]
    edge_rows <- which(component[as.character(from)]==root_component)
    row$reference_cycle_rank <- length(edge_rows)-length(unique(c(from[edge_rows],to[edge_rows])))+1L
    if(row$reference_cycle_rank!=0L) stop("reference_has_alternative_paths_diagnostic_only")
    distance <- drop(igraph::distances(graph,v=as.character(root),weights=igraph::E(graph)$weight))
    high <- -Inf; head_point <- NULL
    for(j in edge_rows) {
      present <- linear(suppressWarnings(st_intersection(geometry[j],tidal)))
      if(!length(present)) next
      row$positive_tidal_channel_m <- row$positive_tidal_channel_m+measure(present)
      endpoints <- do.call(rbind,lapply(present,function(z) unclass(z)[c(1L,nrow(z)),1:2,drop=FALSE]))
      for(k in seq_len(nrow(endpoints))) {
        at <- network_project_point(xy[[j]],endpoints[k,])
        value <- min(distance[as.character(from[j])]+at$measure,
          distance[as.character(to[j])]+edge_length[j]-at$measure)
        if(value>high) { high<-value;head_point<-endpoints[k,] }
      }
    }
    if(!is.finite(high)) stop("no_positive_tidal_mapping_on_rooted_channel")
    parts <- list()
    for(j in edge_rows) {
      a <- distance[as.character(from[j])]; b <- distance[as.character(to[j])]
      if(min(a,b)>=high) next
      if(max(a,b)<=high) part <- xy[[j]] else if(a<b)
        part <- network_slice_line(xy[[j]],0,min(edge_length[j],high-a)) else
        part <- network_slice_line(xy[[j]],max(0,edge_length[j]-(high-b)),edge_length[j])
      if(nrow(part)>=2L && sum(sqrt(rowSums(diff(part)^2)))>1e-7)
        parts[[length(parts)+1L]] <- st_linestring(part)
    }
    route <- st_sfc(parts,crs=26910)
    gap <- linear(suppressWarnings(st_difference(st_union(route),tidal)))
    unmapped <- linear(suppressWarnings(st_difference(gap,footprint)))
    row$root_status <- "unique_reference_terminal_corroborates_LLID_mouth_tree"
    row$head_proxy_distance_from_mouth_m <- high
    row$downstream_channel_m <- measure(route)
    row$uncovered_downstream_channel_m <- measure(gap)
    row$unmapped_downstream_channel_m <- measure(unmapped)
    row$classified_nonselected_downstream_channel_m <- max(0,measure(gap)-measure(unmapped))
    row$longest_uncovered_piece_m <- if(length(gap)) max(as.numeric(st_length(gap))) else 0
    routes[[length(routes)+1L]] <- st_sf(LLID=id,stream_name=label,geometry=route)
    if(length(gap)) gaps[[length(gaps)+1L]] <- st_sf(LLID=id,stream_name=label,geometry=gap)
    boundaries[[length(boundaries)+1L]] <- st_sf(LLID=id,stream_name=label,
      method="furthest upstream positive mapped tidal intersection on independently corroborated same-stream reference tree",
      geometry=st_sfc(st_point(unname(head_point)),crs=26910))
    TRUE
  },error=function(e) {row$root_status<<-conditionMessage(e);FALSE})
  rows[[length(rows)+1L]] <- row
}
audit <- do.call(rbind,rows)
bind <- function(x) if(length(x)) do.call(rbind,x) else st_sf(geometry=st_sfc(crs=26910))
result <- list(audit=audit,downstream_reference_routes=bind(routes),
  uncovered_reference_channels=bind(gaps),mapped_head_of_tide_proxies=bind(boundaries),
  notes=c("Diagnostic only: no habitat or tidal mask is altered.",
    "A corroborated reference mouth and acyclic same-stream route are required for a downstream coverage assessment.",
    "Unmapped channel pieces are distinguished from explicitly nonselected tidal classifications; neither is automatically removed.",
    "Freshwater tributaries above their own mapped tidal limit remain independent; do not remove entire connected components."))
saveRDS(result,"spatial_data/phase3/tidal_sources/channel_coverage.rds")
write.csv(audit,"spatial_data/phase3/tidal_sources/channel_coverage.csv",row.names=FALSE,na="")
print(audit[order(-audit$uncovered_downstream_channel_m),],row.names=FALSE)
