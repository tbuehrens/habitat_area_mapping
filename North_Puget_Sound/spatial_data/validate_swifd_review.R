library(sf)
source("R/habitat_workflow.R")
directory <- "outputs/review_swifd_connections"
file <- file.path(directory,"North_Puget_Sound_Steelhead.gpkg")
pops <- st_read(file,layer="noaa_populations",quiet=TRUE)
extent <- st_read(file,layer="predicted_extent_status",quiet=TRUE)
model <- st_transform(readRDS("spatial_data/north_puget_steelhead.rds"),26910)
expected <- habitat_length_by_pop(model,pops,"expected")
actual <- habitat_length_by_pop(extent[extent$source_class=="prediction",],pops,"actual")
meta <- jsonlite::fromJSON(file.path(directory,"run_metadata.json"))
audit <- read.csv(file.path(directory,"swifd_connector_audit.csv"))
categories <- meta$swifd_category_counts
stopifnot(nrow(pops)==16,
  all(abs(expected$expected-actual$actual)<.001),
  all(abs(extent$length_m-as.numeric(st_length(extent)))<1e-6),
  all(extent$measure_habitat == (extent$source_class!="alignment_link")),
  all(audit$from_snap_m<=100+1e-8),all(audit$to_snap_m<=100+1e-8),
  !any(grepl("Gradient",categories$DISTTYPE_DESC,ignore.case=TRUE)),
  any(grepl("Artificial",categories$DISTTYPE_DESC)),
  any(grepl("Transported",categories$DISTTYPE_DESC)),
  any(categories$DISTTYPE_DESC=="Presumed"),
  isTRUE(meta$review_checkpoint),isTRUE(meta$mileage_export_deferred),
  length(list.files(directory,pattern="hab_lengths.*\\.csv$",recursive=TRUE))==0,
  length(list.files(file.path(directory,"maps"),pattern="\\.png$"))==16,
  file.exists(file.path(directory,"Nooksack_Steelhead_map.png")),
  file.exists(file.path(directory,"North_Puget_Sound_Steelhead_population_maps.pdf")))
cat("Review checkpoint validated: full original model preserved; artificial/transported/presumed included; gradient excluded; both ends <=100m; 16 maps and no final mileage exports.\n")
