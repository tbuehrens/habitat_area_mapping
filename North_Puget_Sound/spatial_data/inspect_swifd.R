source("R/arcgis.R")
url <- "https://geodataservices.wdfw.wa.gov/arcgis/rest/services/MapServices/SWIFD/MapServer/0"
meta <- arcgis_json(url, list(f="json"))
writeLines(meta$text,"spatial_data/swifd_metadata.json")
for (field in meta$data$fields) if (field$name %in% c("SPECIESRUN","DISTTYPE_DESC","USETYPE_DESC","RUNTIME_DESC")) {
  cat(field$name,"\n");print(field$domain)
}
q <- list(where="SPECIESRUN IN ('Winter Steelhead','Summer Steelhead')",
  outFields="SPECIESRUN,DISTTYPE_DESC,USETYPE_DESC", returnGeometry="false",
  returnDistinctValues="true", f="json")
x <- arcgis_json(paste0(url,"/query"),q)
writeLines(x$text,"spatial_data/swifd_categories.json")
print(do.call(rbind,lapply(x$data$features,function(z)as.data.frame(z$attributes))))
