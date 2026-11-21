library(sf)
path <- "spatial_data/upstream_basins/gorge_split_basin.json"
z <- st_read(path, quiet = TRUE)
z <- z[z$id == "drainageBasin", ]
geo <- st_transform(z, 4326)
b <- st_bbox(geo)
b["ymin"] <- 49
canada <- suppressWarnings(st_crop(geo, b))
cat("Total sqmi", sum(as.numeric(st_area(st_transform(z, 26910)))) / 2589988.110336, "\n")
cat("North of49 sqmi", sum(as.numeric(st_area(st_transform(canada, 26910)))) / 2589988.110336, "\n")
print(st_bbox(geo))
