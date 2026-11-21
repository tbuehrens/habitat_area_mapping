# A moved clone must use its own identical source snapshots, even when an old
# clone still exists. Recorded manifests/geometry stay byte-for-byte unchanged.
source("R/project_paths.R")
suppressPackageStartupMessages(library(sf))
fixture <- tempfile("nps_relocation_")
region <- file.path(fixture, "North_Puget_Sound")
old <- file.path(fixture, "old_clone")
dir.create(file.path(region, "spatial_data", "basins"), recursive = TRUE)
dir.create(file.path(region, "outputs", "review"), recursive = TRUE)
dir.create(file.path(old, "spatial_data", "basins"), recursive = TRUE)
target <- file.path(region, "spatial_data", "basins", "source.json")
old_target <- file.path(old, "spatial_data", "basins", "source.json")
writeLines('{"value":17}', target)
writeLines('{"value":99}', old_target)
windows_path <- "C:\\former\\workspace\\spatial_data\\basins\\source.json"
resolved <- normalizePath(target, winslash = "/")
stopifnot(identical(nps_resolve_cache_path(windows_path, root = region), resolved),
  identical(nps_resolve_cache_path(old_target, root = region), resolved),
  identical(nps_resolve_cache_path("spatial_data/basins/source.json", root = region), resolved),
  identical(nps_read_cache_json(windows_path, root = region)$value, 17L))
url <- "https://example.invalid/spatial_data/source.json"
stopifnot(identical(nps_resolve_cache_path(url, root = region), url),
  identical(nps_resolve_cache_path(NA_character_, root = region), NA_character_))
external <- tempfile("unrelated_")
writeLines("outside named cache roots", external)
stopifnot(identical(nps_resolve_cache_path(external, root = region), external))

record <- list(cache_folder = dirname(old_target), basin_path = windows_path,
  omitted = NULL, scientific_value = 5.123456789,
  note = "C:/former/workspace/spatial_data/not_a_resource", nested = list(
    source_split_basin_cache_file = windows_path, source_url = url))
manifest <- file.path(region, "spatial_data", "manifest.json")
jsonlite::write_json(record, manifest, auto_unbox = TRUE, null = "null", digits = NA)
before <- unname(tools::md5sum(manifest))
read <- nps_read_cache_json(manifest, root = region, simplifyVector = FALSE)
stopifnot(identical(read$basin_path, resolved),
  identical(read$nested$source_split_basin_cache_file, resolved),
  "omitted" %in% names(read), is.null(read$omitted),
  identical(read$scientific_value, record$scientific_value), identical(read$note, record$note),
  identical(unname(tools::md5sum(manifest)), before))

point <- st_sf(id = 663L, source_split_basin_cache_file = windows_path,
  geometry = st_sfc(st_point(c(588808.8, 5357359)), crs = 26910))
bundle <- list(barriers = point, metadata = record)
rds <- file.path(region, "spatial_data", "bundle.rds")
saveRDS(bundle, rds)
rds_before <- unname(tools::md5sum(rds))
copy <- nps_read_cache_rds(rds, root = region)
stopifnot(inherits(copy$barriers, "sf"), identical(copy$barriers$id, point$id),
  identical(st_as_binary(st_geometry(copy$barriers)), st_as_binary(st_geometry(point))),
  identical(copy$barriers$source_split_basin_cache_file, resolved),
  identical(unname(tools::md5sum(rds)), rds_before))

# A missing moved resource must fail, even if a stale original still exists.
only_old <- file.path(old, "spatial_data", "basins", "stale_only.json")
writeLines('{"value":99}', only_old)
failed <- tryCatch({nps_read_cache_json(only_old, root = region); FALSE}, error = function(e)
  grepl("Recorded resource is missing", conditionMessage(e), fixed = TRUE))
stopifnot(failed)
cat("Relocated cache paths, immutable source bytes, NULL fields, geometry and stale-clone rejection: PASS\n")
