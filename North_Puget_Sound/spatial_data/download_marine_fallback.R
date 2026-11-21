source("R/arcgis.R")
base <- "https://geodataservices.wdfw.wa.gov/arcgis/rest/services/ApplicationServices/Marine_Areas/MapServer"
url <- paste0(base,"/1")
meta <- arcgis_json(url,list(f="json"))$data
cat(meta$description,"\n")
print(vapply(meta$fields,function(z)z$name,character(1)))
x <- arcgis_download(url,"spatial_data/marine_north.rds",c(-123.2,47.2,-120.5,49.5))
print(names(x))
print(utils::head(sf::st_drop_geometry(x)))
