source("R/arcgis.R")
url <- "https://maps.fisheries.noaa.gov/server/rest/services/WCR/ESA_Species_Ranges/MapServer/57"
z <- arcgis_download(url,"spatial_data/noaa_ps_live.rds")
print(names(z))
cols <- intersect(c("NWFSC_POP_ID","POPULATION","RUN_TIMING","MPG","MPG_NAME","WCR_POP_ID","TRT_POP_ID"),names(z))
print(unique(sf::st_drop_geometry(z)[,cols]))
