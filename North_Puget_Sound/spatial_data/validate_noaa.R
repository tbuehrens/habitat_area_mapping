x <- readRDS("spatial_data/noaa_ps_live.rds.incomplete.rds")
z <- x$returned
stopifnot(setequal(z$OBJECTID_12, x$ids), !anyDuplicated(z$OBJECTID_12))
attr(z, "source_url") <- "https://maps.fisheries.noaa.gov/server/rest/services/WCR/ESA_Species_Ranges/MapServer/57"
attr(z, "downloaded_utc") <- format(Sys.time(), tz = "UTC", usetz = TRUE)
saveRDS(z, "spatial_data/noaa_ps_live.rds")
jsonlite::write_json(x$metadata, "spatial_data/noaa_ps_live.rds.metadata.json", auto_unbox=TRUE,pretty=TRUE)
print(unique(sf::st_drop_geometry(z)[,c("NWFSC_POP_ID","POPULATION","RUN_TIMING","MIDLEVEL_GROUP","WCR_POP_ID")]))
