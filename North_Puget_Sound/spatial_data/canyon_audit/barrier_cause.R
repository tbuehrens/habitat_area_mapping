library(sf)
source("R/arcgis.R");source("R/noaa_populations.R");source("R/habitat_workflow.R")
pop <- get_north_puget_populations();pop <- pop[pop$pop_id==269,]
trace <- readRDS("spatial_data/north_puget_traces_swifd.rds")$trace
g <- trace$lines
clip <- suppressWarnings(st_intersection(g,pop["pop_id"]))
normalize <- function(x) sub("_(left|right)$","",sub("^nodecut_([0-9]+)_[0-9]+$","\\1",x))
root_names <- as.character(trace$roots$node_id)
for (site in c("600836","600837")) {
  audit <- trace$barrier_audit
  b <- audit[audit$SiteId==site,]
  edge <- b$matched_edge
  measures <- sort(unique(audit$measure_m[audit$applied & audit$matched_edge==edge & is.na(audit$node_id)]))
  cut <- paste0("cut_",edge,"_",which.min(abs(measures-b$measure_m)))
  patch_names <- function(x) {
    y <- normalize(x)
    keep <- y==cut
    y[keep] <- x[keep]
    y
  }
  from <- patch_names(g$from_node);to <- patch_names(g$to_node)
  graph <- igraph::graph_from_data_frame(data.frame(from=from,to=to),directed=FALSE)
  memberships <- igraph::components(graph)$membership
  root_components <- unique(memberships[intersect(root_names,names(memberships))])
  retained <- memberships[from] %in% root_components
  clip$retained_single <- retained[match(clip$edge_piece,g$edge_piece)]
  cat("SINGLE SITE",site,"Canyon retainedmi",sum(as.numeric(st_length(clip[clip$retained_single,])))/1609.344,"\n")
  atedge <- g[g$source_edge==edge,]
  print(st_drop_geometry(b)[,c("SiteId","StreamName","TributaryToName","SiteLatitude","SiteLongitude","SurveyDate","source_layer","matched_edge","snap_distance_m","measure_m")])
  print(st_drop_geometry(atedge)[,intersect(c("source_class","bridge_id","length_m","status","baseline_component"),names(atedge))])
}
swifd <- st_transform(readRDS("spatial_data/north_puget_swifd_steelhead.rds"),26910)
model <- st_transform(readRDS("spatial_data/north_puget_steelhead.rds"),26910)
b <- trace$barrier_audit[trace$barrier_audit$SiteId %in% c("600836","600837"),]
ix <- st_nearest_feature(b,swifd)
print(st_drop_geometry(swifd[ix,])[,c("LLID_STRM_NAME","LLID","SPECIESRUN","DISTTYPE_DESC","USETYPE_DESC")])
print(as.numeric(st_distance(b,swifd[ix,],by_element=TRUE)))
saveRDS(list(barriers=b,edge=g[g$source_edge %in% b$matched_edge,],model=model,
  swifd_nearest=swifd[ix,],swifd=swifd,population=pop),"spatial_data/canyon_audit/barrier_cause.rds")
