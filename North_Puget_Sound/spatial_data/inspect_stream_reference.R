source("R/arcgis.R")
base <- "https://geodataservices.wdfw.wa.gov/arcgis/rest/services/FP_Projects/NHDwithLLID/MapServer"
for (id in c(4, 7)) {
  metadata <- arcgis_json(paste0(base, "/", id), list(f = "json"))$data
  jsonlite::write_json(metadata, paste0("spatial_data/stream_reference_layer_", id, ".json"),
                        auto_unbox = TRUE, pretty = TRUE)
  cat("LAYER", id, metadata$name, metadata$description, "\n")
  print(vapply(metadata$fields, function(z) paste(z$name, z$type, sep = ": "), character(1)))
  result <- arcgis_json(paste0(base, "/", id, "/query"), list(
    where = "1=1", geometry = "-123.2,47.2,-120.5,49.5", geometryType = "esriGeometryEnvelope",
    inSR = 4326, spatialRel = "esriSpatialRelIntersects", returnCountOnly = "true", f = "json"))$data
  cat("REGION COUNT", result$count, "\n")
}
for (name in c("north_puget_steelhead", "north_puget_barriers_5", "north_puget_swifd_steelhead")) {
  x <- readRDS(paste0("spatial_data/", name, ".rds"))
  cat(name, paste(names(x), collapse = ", "), "\n")
}
