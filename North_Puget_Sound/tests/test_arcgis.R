# Download contract checks using an in-memory ArcGIS response, without HTTP.
source(file.path("R", "arcgis.R"))
calls <- 0L
incomplete <- FALSE
arcgis_json <- function(url, params = list()) {
  calls <<- calls + 1L
  if (!grepl("/query$", url)) return(list(data = list(
    name = "Synthetic joined layer", maxRecordCount = 2,
    fields = list(list(name = "OBJECTID_12", type = "esriFieldTypeOID"),
                  list(name = "value", type = "esriFieldTypeDouble")))))
  if (identical(params$returnIdsOnly, "true")) {
    ids <- if (identical(params$where, "subset")) 1:2 else 1:5
    return(list(data = list(objectIds = as.list(ids))))
  }
  ids <- as.integer(strsplit(params$objectIds, ",", fixed = TRUE)[[1]])
  if (incomplete) ids <- head(ids, 1)
  features <- lapply(ids, function(id) list(type = "Feature",
    properties = list(OBJECTID_12 = id, value = if (id == 1) NULL else id),
    geometry = list(type = "Point", coordinates = c(-122 + id / 1000, 48))))
  txt <- jsonlite::toJSON(list(type = "FeatureCollection", features = features),
                          auto_unbox = TRUE, null = "null")
  list(text = txt, data = list(features = features))
}
cache <- tempfile(fileext = ".rds")
url <- "https://example.invalid/FeatureServer/0"
bb <- c(-123, 47, -121, 49)
all <- arcgis_download(url, cache, bb)
stopifnot(nrow(all) == 5L, setequal(all$OBJECTID_12, 1:5),
          is.numeric(all$value), is.na(all$value[1]), calls == 5L)
before <- calls
same <- arcgis_download(url, cache, bb)
stopifnot(calls == before, nrow(same) == 5L)
subset <- arcgis_download(url, cache, bb, where = "subset")
stopifnot(nrow(subset) == 2L, calls > before)
before <- calls
changed_bbox <- arcgis_download(url, cache, bb + c(0, 0, 0, 0.5), where = "subset")
stopifnot(calls > before, identical(attr(changed_bbox, "query_where"), "subset"))
incomplete <- TRUE
failed <- tryCatch({arcgis_download(url, tempfile(fileext = ".rds")); FALSE},
                   error = function(e) grepl("Incomplete ArcGIS batch", conditionMessage(e)))
stopifnot(failed)
cat("ArcGIS batch completeness, joined IDs, field types, and cache selection checks passed.\n")
