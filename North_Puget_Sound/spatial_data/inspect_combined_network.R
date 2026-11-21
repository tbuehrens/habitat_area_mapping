library(sf)
source("R/network.R")
steelhead <- st_transform(readRDS("spatial_data/model_north_0.rds"), 26910)
coho <- st_transform(readRDS("spatial_data/model_north_1.rds"), 26910)
combined <- c(st_geometry(steelhead), st_geometry(coho))
timing <- system.time(noded <- st_cast(st_union(combined), "LINESTRING"))
cat("Combined noding seconds", timing["elapsed"], "noded lines", length(noded), "\n")
p <- do.call(rbind, lapply(noded, function(x) x[c(1, nrow(x)), 1:2]))
for (tolerance in c(0.25, 1)) {
  timing <- system.time(groups <- network_endpoint_groups(p, tolerance))
  edges <- matrix(groups, ncol = 2, byrow = TRUE)
  graph <- igraph::graph_from_edgelist(edges, directed = FALSE)
  components <- igraph::components(graph)
  cat("Tolerance", tolerance, "merge seconds", timing["elapsed"],
      "components", components$no, "largest nodes", head(sort(components$csize, decreasing = TRUE), 20), "\n")
}
saveRDS(st_sf(geometry = noded), "spatial_data/noded_combined.rds")
