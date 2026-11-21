source("R/arcgis.R")
source("R/habitat_workflow.R")
bbox <- c(-123.2,47.2,-120.5,49.2)
arcgis_download(STEELHEAD_SERVICE, "spatial_data/north_puget_steelhead.rds", bbox)
for (id in c(5,6)) arcgis_download(paste0(BARRIER_SERVICE,id),
  paste0("spatial_data/north_puget_barriers_",id,".rds"), bbox)
