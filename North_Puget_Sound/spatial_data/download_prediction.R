library(sf)
source("R/arcgis.R")
base <- "https://services.arcgis.com/rcya3vExsaVBGUDp/arcgis/rest/services/Statewide_Dist_Models_1b66a3ddb471/FeatureServer"
bbox <- c(-123.2, 47.6, -120.5, 49.1)
for (i in 0:1) {
  z <- arcgis_download(paste0(base, "/", i), paste0("spatial_data/model_north_", i, ".rds"), bbox)
  print(z[seq_len(min(3, nrow(z))), ])
  cat("PARTS", table(st_geometry_type(z)), "\n")
  cat("LENGTH KM", sum(as.numeric(st_length(st_transform(z, 26910)))) / 1000, "\n")
}
for (i in c(5,6)) {
  z <- arcgis_download(paste0("https://geodataservices.wdfw.wa.gov/arcgis/rest/services/ApplicationServices/FP_Sites/MapServer/", i), paste0("spatial_data/barriers_north_", i, ".rds"), bbox)
  print(table(z$PercentFishPassableCode, useNA = "always"))
}
marine <- arcgis_download(
  "https://hydro.nationalmap.gov/arcgis/rest/services/NHDPlus_HR/MapServer/8",
  "spatial_data/marine_north.rds", bbox, where = "FTYPE = 445")
print(sf::st_drop_geometry(marine))
