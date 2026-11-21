SWIFD_SERVICE <- paste0(
  "https://geodataservices.wdfw.wa.gov/arcgis/rest/services/",
  "MapServices/SWIFD/MapServer/0")

eligible_steelhead_swifd <- function(x) {
  required <- c("SPECIESRUN", "DISTTYPE_DESC", "USETYPE_DESC")
  if (!all(required %in% names(x))) stop("SWIFD distribution fields are missing")
  runs <- x$SPECIESRUN %in% c("Winter Steelhead", "Summer Steelhead")
  positive_use <- x$USETYPE_DESC %in% c("Presence", "Spawning", "Rearing") |
    (!is.na(x$USETYPE_DESC) & grepl("^(Artificial|Transported|Presumed)",
      trimws(x$USETYPE_DESC), ignore.case = TRUE))
  excluded <- function(z) {
    !is.na(z) & grepl("^Gradient", trimws(z), ignore.case = TRUE)
  }
  x[runs & positive_use & !excluded(x$DISTTYPE_DESC) & !excluded(x$USETYPE_DESC), ]
}

download_steelhead_swifd <- function(cache_dir, bbox, refresh = FALSE) {
  where <- paste(
    "SPECIESRUN IN ('Winter Steelhead','Summer Steelhead')",
    "AND (USETYPE_DESC IN ('Presence','Spawning','Rearing')",
    "OR USETYPE_DESC LIKE 'Artificial%' OR USETYPE_DESC LIKE 'Transported%' OR USETYPE_DESC LIKE 'Presumed%')",
    "AND DISTTYPE_DESC NOT LIKE 'Gradient%'"
  )
  x <- arcgis_download(SWIFD_SERVICE, file.path(cache_dir, "north_puget_swifd_steelhead.rds"),
                        bbox = bbox, where = where, refresh = refresh)
  eligible <- eligible_steelhead_swifd(x)
  if (!nrow(eligible)) stop("No eligible winter/summer SWIFD distribution")
  eligible <- sf::st_transform(eligible, 26910)
  categories <- dplyr::count(sf::st_drop_geometry(eligible),
    SPECIESRUN, DISTTYPE_DESC, USETYPE_DESC, name = "records")
  utils::write.csv(categories, file.path(cache_dir, "swifd_eligible_categories.csv"), row.names = FALSE)
  attr(eligible, "selection_where") <- where
  eligible
}
