library(sf)
x <- readRDS("spatial_data/north_puget_traces.rds")$trace
nodes <- x$nodes[x$nodes$degree==1,]
lines <- x$lines
near <- st_intersects(st_buffer(nodes,dist=20*sqrt(2),nQuadSegs=1),lines)
gaps <- list()
for (i in which(lengths(near)>1)) {
  rows <- near[[i]]
  own <- which(lines$from_node==as.character(nodes$node_id[i]) | lines$to_node==as.character(nodes$node_id[i]))
  rows <- rows[!lines$source_edge[rows] %in% lines$source_edge[own]]
  if (length(rows)) {
    d <- as.numeric(st_distance(nodes[i,],lines[rows,]))
    row <- rows[which.min(d)]
    gaps[[length(gaps)+1]] <- data.frame(node_id=nodes$node_id[i],component=nodes$component[i],
      edge=lines$source_edge[row],other_component=lines$baseline_component[row],gap_m=min(d))
  }
}
gaps <- do.call(rbind,gaps)
print(nrow(gaps)); print(summary(gaps$gap_m)); print(head(gaps[order(gaps$gap_m),],20))
saveRDS(gaps,"spatial_data/gaps_under_20m.rds")
