options(timeout = 600)
library(httr)
library(jsonlite)
base <- "https://services.arcgis.com/rcya3vExsaVBGUDp/arcgis/rest/services/Statewide_Dist_Models_1b66a3ddb471/FeatureServer"
dir.create("spatial_data", showWarnings = FALSE)
fetch <- function(url, name) {
  r <- RETRY("GET", url, query = list(f = "pjson"), timeout(180), quiet = TRUE)
  stop_for_status(r)
  txt <- content(r, as = "text", encoding = "UTF-8")
  writeLines(txt, file.path("spatial_data", paste0(name, ".json")))
  fromJSON(txt)
}
x <- fetch(base, "model_service")
print(x$layers)
for (i in x$layers$id) {
  z <- fetch(paste0(base, "/", i), paste0("model_layer_", i))
  cat("\nLAYER", i, z$name, "\n", z$description, "\n")
  print(z$fields[, c("name", "alias", "type")])
}
for (i in c(5, 6)) {
  z <- fetch(paste0("https://geodataservices.wdfw.wa.gov/arcgis/rest/services/ApplicationServices/FP_Sites/MapServer/", i), paste0("barrier_layer_", i))
  cat("\nBARRIER", i, z$name, "\n", z$definitionExpression, "\n")
}
for (i in 0:1) {
  r <- GET(paste0(base, "/", i, "/query"), query = list(
    where = "1=1", returnCountOnly = "true", f = "json"), timeout(120))
  cat("COUNT", i, content(r, as = "text"), "\n")
  r <- GET(paste0(base, "/", i, "/query"), query = list(
    where = "1=1", outFields = "*", resultRecordCount = 2,
    returnGeometry = "true", outSR = 26910, f = "geojson"), timeout(120))
  writeLines(content(r, as = "text"), paste0("spatial_data/model_sample_", i, ".geojson"))
  z <- sf::st_read(paste0("spatial_data/model_sample_", i, ".geojson"), quiet = TRUE)
  print(z)
}
