# NOAA's live population layer is the published fish feature class used by the
# WCR geodatabase. MIDLEVEL_GROUP 713 is the North Cascades steelhead MPG.
get_north_puget_populations <- function(cache_dir = "spatial_data", refresh = FALSE) {
  stopifnot(requireNamespace("sf", quietly = TRUE), requireNamespace("dplyr", quietly = TRUE))
  url <- "https://maps.fisheries.noaa.gov/server/rest/services/WCR/ESA_Species_Ranges/MapServer/57"
  raw <- arcgis_download(url, file.path(cache_dir, "noaa_ps_live.rds"), refresh = refresh)
  raw <- raw[raw$MIDLEVEL_GROUP == 713 & !is.na(raw$NWFSC_POP_ID), ]
  if (!nrow(raw)) stop("No North Cascades steelhead populations in NOAA layer")
  expected_ids <- c(268,269,271,272,279,280,281,283,285,286,288,290,291,292,296,297)
  if (!setequal(unique(raw$NWFSC_POP_ID), expected_ids)) {
    stop("NOAA MPG population membership changed; inspect before rerunning")
  }
  raw <- sf::st_transform(sf::st_make_valid(raw), 26910)
  populations <- dplyr::summarise(dplyr::group_by(raw, NWFSC_POP_ID),
    population = dplyr::first(POPULATION),
    run_timing = dplyr::first(RUN_TIMING),
    wcr_pop_id = dplyr::first(WCR_POP_ID), .groups = "drop")
  names(populations)[names(populations) == "NWFSC_POP_ID"] <- "pop_id"
  populations$population <- paste0(populations$population, " (",
    c(wi = "winter", su = "summer", sw = "summer/winter")[populations$run_timing], ")")
  populations$mpg <- "North Cascades"
  populations$species <- "Steelhead"
  attr(populations, "source_url") <- url
  attr(populations, "downloaded_utc") <- attr(raw, "downloaded_utc")
  populations[order(populations$population), ]
}
