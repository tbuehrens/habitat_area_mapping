library(sf)
source("R/arcgis.R")
source("R/noaa_populations.R")
source("R/habitat_workflow.R")
pops <- get_north_puget_populations()
target <- pops[pops$pop_id==269,]
raw <- st_transform(readRDS("spatial_data/north_puget_steelhead.rds"),26910)
cat("RAW CANYON MILES",habitat_length_by_pop(raw,target,"miles")$miles,"\n")
for (file in c("spatial_data/north_puget_traces.rds","spatial_data/north_puget_traces_swifd.rds")) {
  cached <- readRDS(file)
  cat("FILE",file,"\n");print(names(cached))
  for (scenario in c("trace","verified")) {
    trace <- cached[[scenario]]
    z <- habitat_clip(trace$lines,target)
    if (!"source_class" %in% names(z)) z$source_class <- "prediction"
    cat(scenario,"pieces",nrow(z),"\n")
    print(aggregate(as.numeric(st_length(z))/1609.344,
      st_drop_geometry(z)[,c("status","source_class")],sum))
  }
}
trace <- readRDS("spatial_data/north_puget_traces_swifd.rds")$trace
near <- trace$barrier_audit[lengths(st_intersects(trace$barrier_audit,st_buffer(target,1000)))>0,]
print(st_drop_geometry(near)[,intersect(c("OBJECTID","SiteId","StreamName","TributaryToName","verification","matched_edge","snap_distance_m","node_id","measure_m","match_status","applied","baseline_connected"),names(near))])
saveRDS(list(population=target,trace=trace),"spatial_data/canyon_audit/working.rds")

# Rejoin barrier-split graph vertices to reconstruct the pre-barrier graph.
normalize <- function(x) {
  x <- sub("^nodecut_([0-9]+)_[0-9]+$","\\1",x)
  sub("_(left|right)$","",x)
}
g <- trace$lines
g$before_from <- normalize(g$from_node); g$before_to <- normalize(g$to_node)
edges <- data.frame(from=g$before_from,to=g$before_to,
  edge_piece=g$edge_piece,weight=as.numeric(st_length(g)))
graph <- igraph::graph_from_data_frame(edges,directed=FALSE)
inside <- which(lengths(st_intersects(g,target))>0)
start <- g$before_from[inside[which.max(as.numeric(st_length(g[inside,])))]]
roots <- intersect(as.character(trace$roots$node_id),igraph::V(graph)$name)
distance <- igraph::distances(graph,v=start,to=roots,weights=igraph::E(graph)$weight)
root <- roots[which.min(distance)]
cat("START",start,"ROOT",root,"distance",min(distance)/1609.344,"miles\n")
route <- igraph::shortest_paths(graph,from=start,to=root,weights=igraph::E(graph)$weight,output="both")
route_nodes <- igraph::as_ids(route$vpath[[1]])
print(route_nodes[grepl("cut_",route_nodes)])
cut_anchors <- character()
barrier_indices <- integer()
for (node in route_nodes) {
  if (grepl("^cut_",node)) {
    parts <- strsplit(node,"_")[[1]]
    edge <- as.integer(parts[2]); number <- as.integer(parts[3])
    audit <- trace$barrier_audit
    on_edge <- which(audit$applied & audit$matched_edge==edge & is.na(audit$node_id))
    measures <- sort(unique(audit$measure_m[on_edge]))
    ix <- on_edge[abs(audit$measure_m[on_edge]-measures[number])<1e-5]
    barrier_indices <- c(barrier_indices,ix)
  } else if (grepl("^[0-9]+$",node)) {
    barrier_indices <- c(barrier_indices,which(trace$barrier_audit$applied &
      !is.na(trace$barrier_audit$node_id) & trace$barrier_audit$node_id==as.integer(node)))
  }
}
barriers <- trace$barrier_audit[unique(barrier_indices),]
cat("BARRIERS ON PATH TO ROOT\n")
print(st_drop_geometry(barriers)[,intersect(c("OBJECTID","SiteId","StreamName","TributaryToName","verification","matched_edge","snap_distance_m","node_id","measure_m","match_status","applied","baseline_connected","DataSource","FormLinkURL","PercentFishPassableCode"),names(barriers))])
saveRDS(list(population=target,path=g[igraph::as_ids(route$epath[[1]]) |> as.integer(),],
  barriers=barriers,nodes=route_nodes),"spatial_data/canyon_audit/path.rds")
