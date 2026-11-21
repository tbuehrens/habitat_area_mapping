library(sf)
source("R/arcgis.R"); source("R/noaa_populations.R")
z <- readRDS("spatial_data/north_puget_traces.rds")$trace
pop <- get_north_puget_populations()
nook <- pop[pop$pop_id==280,]
ids <- unique(z$lines$baseline_component[lengths(st_intersects(z$lines,nook))>0])
nodes <- z$nodes[z$nodes$component %in% ids & z$nodes$degree==1,]
shore <- st_transform(readRDS("spatial_data/marine_north.rds"),26910)
nodes$shore_distance <- as.numeric(st_distance(nodes,shore[st_nearest_feature(nodes,shore),],by_element=TRUE))
print(head(st_drop_geometry(nodes[order(nodes$shore_distance),]),20))
for (id in head(unique(nodes$component[order(nodes$shore_distance)]),15)) {
  cat("component",id,"miles",sum(z$lines$length_m[z$lines$baseline_component==id])/1609.344,"\n")
}
png("spatial_data/shore_debug.png",width=1200,height=1000)
plot(st_geometry(nook),col="grey95",xlim=c(520000,555000),ylim=c(5385000,5420000))
plot(st_geometry(shore),col="red",add=TRUE)
plot(st_geometry(z$lines),col="blue",add=TRUE)
plot(st_geometry(nodes),col="black",pch=19,add=TRUE)
dev.off()
