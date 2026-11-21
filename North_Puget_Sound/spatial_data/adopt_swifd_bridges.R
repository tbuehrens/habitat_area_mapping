source("R/arcgis.R")
source("R/swifd_sources.R")
source("R/source_classification.R")
source("R/network.R")
source("R/swifd_connectors.R")
result_file <- "spatial_data/swifd_bridge_actual_test.rds"
bridges <- readRDS(result_file)
stopifnot(bridges$settings$snap_distance_m == 100,
  bridges$summary$augmented_cycle_rank <= bridges$summary$original_cycle_rank,
  bridges$summary$augmented_components <= bridges$summary$original_components,
  all(bridges$bridge_audit$from_snap_m <= 100 + 1e-8),
  all(bridges$bridge_audit$to_snap_m <= 100 + 1e-8),
  all(!bridges$alignment_links$measure_habitat),
  all(bridges$network$source_class %in% c("prediction","swifd_connector","alignment_link")))
inputs <- c("spatial_data/north_puget_steelhead.rds", "spatial_data/north_puget_swifd_steelhead.rds",
  file.path("R",c("swifd_connectors.R","source_classification.R","network.R")))
signature <- list(md5=unname(tools::md5sum(inputs)),endpoint_distance_m=100)
saveRDS(list(signature=signature,bridges=bridges),"spatial_data/north_puget_swifd_bridges.rds")
print(bridges$summary)
cat("Validated final SWIFD routing result adopted into analysis cache\n")
