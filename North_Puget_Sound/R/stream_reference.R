# Named hydrography for stream identity checks, independent of fish occupancy.
# This file does not modify or supplement the SWIFD gap-routing network.
STREAM_IDENTITY_REFERENCE_SERVICE <- paste0(
  "https://geodataservices.wdfw.wa.gov/arcgis/rest/services/",
  "FP_Projects/NHDwithLLID/MapServer/4")

load_stream_identity_reference <- function(cache_dir = "spatial_data",
    bbox = c(-123.2, 47.2, -120.5, 49.5), refresh = FALSE) {
  if (!requireNamespace("sf", quietly = TRUE)) stop("Install the sf package")
  if (!exists("arcgis_download", mode = "function")) stop("Source R/arcgis.R first")
  reference <- arcgis_download(STREAM_IDENTITY_REFERENCE_SERVICE,
    file.path(cache_dir, "north_puget_named_stream_reference.rds"),
    bbox = bbox, where = "1=1", refresh = refresh)
  required <- c("LLID", "LLID_STREAMNAME", "GNIS_STREAMNAME", "GNIS_Name")
  if (!all(required %in% names(reference))) stop("Named-stream reference schema changed; inspect before matching")
  reference <- sf::st_transform(reference, 26910)
  name <- as.character(reference$LLID_STREAMNAME)
  for (field in c("GNIS_STREAMNAME", "GNIS_Name")) {
    missing <- is.na(name) | !nzchar(trimws(name))
    name[missing] <- as.character(reference[[field]][missing])
  }
  name[!is.na(name) & !nzchar(trimws(name))] <- NA_character_
  reference$LLID <- as.character(reference$LLID)
  # The alias allows the existing SWIFD-oriented identity helper to consume the
  # complete named network while retaining all original name fields for review.
  reference$LLID_STRM_NAME <- name
  reference$reference_llid <- reference$LLID
  reference$reference_stream_name <- name
  reference$reference_source <- "WDFW NHD-with-LLID named hydrography (not fish-distribution filtered)"
  attr(reference, "identity_reference_scope") <- "All named-stream features intersecting bbox, without species or distribution filters"
  attr(reference, "identity_reference_service") <- STREAM_IDENTITY_REFERENCE_SERVICE
  reference
}
