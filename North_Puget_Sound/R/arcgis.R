# ArcGIS downloads use an ID inventory, then bounded batches. No OBJECTID ceiling.
arcgis_field_types <- function(x, metadata) {
  for (field in metadata$fields) {
    if (!field$name %in% names(x)) next
    if (field$type %in% c("esriFieldTypeOID", "esriFieldTypeInteger", "esriFieldTypeSmallInteger",
                         "esriFieldTypeDouble", "esriFieldTypeSingle")) {
      x[[field$name]] <- as.numeric(x[[field$name]])
    } else if (field$type %in% c("esriFieldTypeString", "esriFieldTypeGUID", "esriFieldTypeGlobalID")) {
      x[[field$name]] <- as.character(x[[field$name]])
    }
  }
  x
}

arcgis_json <- function(url, params = list()) {
  response <- httr::RETRY("GET", url, query = params, httr::timeout(300),
                          times = 4, quiet = TRUE)
  httr::stop_for_status(response)
  txt <- httr::content(response, as = "text", encoding = "UTF-8")
  parsed <- jsonlite::fromJSON(txt, simplifyVector = FALSE)
  if (!is.null(parsed$error)) stop(url, ": ", paste(unlist(parsed$error), collapse = "; "))
  list(text = txt, data = parsed)
}

arcgis_download <- function(layer, cache, bbox = NULL, where = "1=1", refresh = FALSE) {
  stopifnot(requireNamespace("sf", quietly = TRUE))
  if (file.exists(cache) && !refresh) {
    cached <- readRDS(cache)
    if (identical(attr(cached, "source_url"), layer)) {
      same_bbox <- isTRUE(all.equal(unname(attr(cached, "query_bbox")), unname(bbox)))
      same_where <- identical(attr(cached, "query_where"), where)
      if (same_bbox && same_where) {
        metadata_file <- paste0(cache, ".metadata.json")
        if (file.exists(metadata_file)) cached <- arcgis_field_types(cached,
          jsonlite::fromJSON(metadata_file, simplifyVector = FALSE))
        return(cached)
      }
      message("Download selection changed; refreshing ", basename(cache))
    } else {
      stop("Cache source differs from requested layer; use a separate cache file: ", cache)
    }
  }
  metadata <- arcgis_json(layer, list(f = "json"))$data
  params <- list(where = where, returnIdsOnly = "true", f = "json")
  if (!is.null(bbox)) {
    params$geometry <- paste(bbox, collapse = ",")
    params$geometryType <- "esriGeometryEnvelope"
    params$inSR <- 4326
    params$spatialRel <- "esriSpatialRelIntersects"
  }
  ids <- sort(unique(unlist(arcgis_json(paste0(layer, "/query"), params)$data$objectIds)))
  if (!length(ids)) stop("No records: ", layer)
  batch_size <- min(200L, as.integer(metadata$maxRecordCount))
  batches <- split(ids, ceiling(seq_along(ids) / batch_size))
  message("Downloading ", length(ids), " features from ", metadata$name)
  parts <- lapply(seq_along(batches), function(i) {
    batch <- batches[[i]]
    result <- arcgis_json(paste0(layer, "/query"), list(
      objectIds = paste(batch, collapse = ","), outFields = "*",
      returnGeometry = "true", returnZ = "false", returnM = "false",
      outSR = 4326, f = "geojson"))
    if (isTRUE(result$data$exceededTransferLimit)) stop("ArcGIS transfer limit exceeded")
    z <- sf::st_read(result$text, quiet = TRUE)
    if (nrow(z) != length(batch)) stop("Incomplete ArcGIS batch ", i)
    z
  })
  out <- do.call(rbind, parts)
  out <- arcgis_field_types(out, metadata)
  oid <- metadata$objectIdField
  if (is.null(oid)) oid <- metadata$objectIdFieldName
  if (is.null(oid)) {
    oid_fields <- Filter(function(field) identical(field$type, "esriFieldTypeOID"), metadata$fields)
    if (length(oid_fields) != 1) stop("Cannot identify the ArcGIS object ID field")
    oid <- oid_fields[[1]]$name
  }
  if (!setequal(out[[oid]], ids) || anyDuplicated(out[[oid]])) {
    saveRDS(list(metadata = metadata, ids = ids, returned = out), paste0(cache, ".incomplete.rds"))
    stop("ArcGIS ID mismatch (diagnostic saved at ", cache, ".incomplete.rds)")
  }
  attr(out, "source_url") <- layer
  attr(out, "downloaded_utc") <- format(Sys.time(), tz = "UTC", usetz = TRUE)
  attr(out, "query_bbox") <- bbox
  attr(out, "query_where") <- where
  dir.create(dirname(cache), recursive = TRUE, showWarnings = FALSE)
  saveRDS(out, cache)
  jsonlite::write_json(metadata, paste0(cache, ".metadata.json"), auto_unbox = TRUE, pretty = TRUE)
  out
}
