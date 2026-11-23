# CFS/WDFW present mapped tidal connectivity. These polygons are an explicit
# mapped head-of-tide proxy, not a surveyed, discharge-dependent hydraulic limit.
# Full raw snapshots include potential/disconnected areas; only positively
# classified CURRENT landscape tidal influence is eligible for the cut mask.

phase3_tidal_classes <- function(layer) {
  if (as.integer(layer) == 2L) return(c("Unrestricted", "Partial", "Significant"))
  if (as.integer(layer) == 3L) return(c("Unrestricted Tidal", "Partially Restricted Tidal",
    "Significantly Restricted Tidal", "Unrestricted - nearshore deep",
    "Unrestricted - nearshore shallow", "Partially Restricted - nearshore deep",
    "Partially Restricted - nearshore shallow"))
  stop("Only the published CFS2025 polygon layers 2 and 3 are supported")
}

build_phase3_tidal_sources <- function(cache_dir="spatial_data/phase3/tidal_sources") {
  if (!requireNamespace("sf",quietly=TRUE) || !requireNamespace("jsonlite",quietly=TRUE) ||
      !requireNamespace("dplyr",quietly=TRUE)) stop("Install sf, jsonlite and dplyr")
  manifest_file <- file.path(cache_dir,"source_manifest.json")
  if (!file.exists(manifest_file)) stop("Fetch explicit source snapshots with scripts/fetch_phase3_tidal_sources.py first")
  manifest <- jsonlite::fromJSON(manifest_file,simplifyVector=FALSE)
  files <- do.call(rbind,lapply(manifest$files,function(x) as.data.frame(x,stringsAsFactors=FALSE)))
  paths <- file.path(cache_dir,files$file)
  if (!all(file.exists(paths)) || !identical(tolower(unname(tools::md5sum(paths))),tolower(files$md5)))
    stop("Tidal source snapshot is missing or differs from its manifest")
  rows <- list(); audit_rows <- list()
  for (record in manifest$layers) {
    layer <- as.integer(record$layer)
    chunks <- lapply(record$batch_files,function(file) sf::st_read(file.path(cache_dir,file),quiet=TRUE))
    harmonize <- function(x) {
      values <- sf::st_drop_geometry(x)
      values[] <- lapply(values,as.character)
      sf::st_sf(values,geometry=sf::st_geometry(x))
    }
    polygons <- dplyr::bind_rows(lapply(chunks,harmonize))
    ids <- unlist(jsonlite::fromJSON(file.path(cache_dir,record$ids_file))$objectIds)
    found <- suppressWarnings(as.numeric(polygons[[record$objectid_field]]))
    if (nrow(polygons)!=as.integer(record$count) || anyNA(found) || anyDuplicated(found) ||
        !setequal(ids,found)) stop("Cached polygon geometry does not exactly match the complete source ID inventory")
    polygons$source_geometry_present <- !sf::st_is_empty(polygons)
    polygons <- sf::st_transform(polygons,26910)
    polygons$source_geometry_valid_before <- sf::st_is_valid(polygons)
    polygons$source_geometry_valid_reason <- sf::st_is_valid(polygons,reason=TRUE)
    polygons$source_area_before_m2 <- as.numeric(sf::st_area(polygons))
    original_area <- sum(as.numeric(sf::st_area(polygons)))
    empty <- polygons[!polygons$source_geometry_present,]
    polygons <- sf::st_make_valid(polygons[polygons$source_geometry_present,])
    polygons <- suppressWarnings(sf::st_collection_extract(polygons,"POLYGON"))
    polygons <- polygons[!sf::st_is_empty(polygons) & as.numeric(sf::st_area(polygons))>0,]
    if(nrow(empty)) sf::st_geometry(empty) <- sf::st_sfc(rep(list(sf::st_polygon()),nrow(empty)),crs=26910)
    polygons <- dplyr::bind_rows(polygons,empty)
    if (nrow(polygons)!=length(ids) || any(!sf::st_is_valid(polygons)))
      stop("Polygon normalization removed a source feature or left invalid geometry")
    area_difference <- sum(as.numeric(sf::st_area(polygons)))-original_area
    polygons$source_area_after_m2 <- as.numeric(sf::st_area(polygons))
    polygons$source_geometry_area_change_m2 <- polygons$source_area_after_m2-polygons$source_area_before_m2
    # A structural repair of a malformed source can turn an out-of-shell hole
    # into a new island. That is not positive evidence of tidal inundation.
    polygons$source_geometry_repair_held <- abs(polygons$source_geometry_area_change_m2)>1
    polygons$tidal_source_layer <- layer
    polygons$tidal_source_key <- paste0("CFS2025:",layer,":",polygons[[record$objectid_field]])
    polygons$tidal_source_url <- paste0(manifest$service_url,"/",layer)
    polygons$tidal_class <- polygons$LS_TidalConnectivity
    polygons$tidal_class_qualifies <- polygons$tidal_class %in% phase3_tidal_classes(layer)
    polygons$tidal_present <- polygons$tidal_class_qualifies & polygons$source_geometry_present &
      !polygons$source_geometry_repair_held
    polygons$tidal_selection_basis <- "published landscape connectivity; currently tidal including muted/partial tidal influence"
    polygons$tidal_mapping_scope <- if(layer==2L) "large_river_deltas" else "remaining_Puget_Sound_nearshore"
    rows[[length(rows)+1L]] <- polygons
    audit_rows[[length(audit_rows)+1L]] <- data.frame(check="complete_source_inventory",
      source_layer=layer,observed=nrow(polygons),units="features",passed=nrow(polygons)==length(ids))
    audit_rows[[length(audit_rows)+1L]] <- data.frame(check="polygon_normalization_area_change",
      source_layer=layer,observed=area_difference,units="m2",passed=abs(area_difference)<1)
    audit_rows[[length(audit_rows)+1L]] <- data.frame(check="published_empty_geometry_held",
      source_layer=layer,observed=sum(!polygons$source_geometry_present),units="features",
      passed=!any(!polygons$source_geometry_present))
    audit_rows[[length(audit_rows)+1L]] <- data.frame(check="substantive_geometry_repairs_held_from_cut_mask",
      source_layer=layer,observed=sum(polygons$source_geometry_repair_held),units="features",
      passed=!any(polygons$tidal_present & polygons$source_geometry_repair_held))
  }
  all_polygons <- dplyr::bind_rows(rows)
  tidal_polygons <- all_polygons[all_polygons$tidal_present,]
  if(!nrow(tidal_polygons)) stop("No positively classified current tidal polygons")
  # Quantify overlap before union. The sources cover large deltas and the rest
  # of the nearshore respectively; a disagreement must be reviewed explicitly.
  layer1 <- sf::st_union(sf::st_geometry(all_polygons[all_polygons$tidal_source_layer==2L,]))
  layer2 <- sf::st_union(sf::st_geometry(all_polygons[all_polygons$tidal_source_layer==3L,]))
  overlap <- suppressWarnings(sf::st_intersection(layer1,layer2))
  overlap_m2 <- sum(as.numeric(sf::st_area(overlap)))
  selected1 <- sf::st_union(sf::st_geometry(tidal_polygons[tidal_polygons$tidal_source_layer==2L,]))
  selected2 <- sf::st_union(sf::st_geometry(tidal_polygons[tidal_polygons$tidal_source_layer==3L,]))
  rejected1 <- sf::st_union(sf::st_geometry(all_polygons[all_polygons$tidal_source_layer==2L & !all_polygons$tidal_present,]))
  rejected2 <- sf::st_union(sf::st_geometry(all_polygons[all_polygons$tidal_source_layer==3L & !all_polygons$tidal_present,]))
  conflicts <- sf::st_union(c(suppressWarnings(sf::st_intersection(selected1,rejected2)),
    suppressWarnings(sf::st_intersection(selected2,rejected1))))
  conflict_area <- sum(as.numeric(sf::st_area(conflicts)))
  # Positive classification is required in every overlapping source. Do not
  # allow the union to overwrite an explicit non-tidal or unknown designation.
  geometry <- sf::st_union(sf::st_geometry(tidal_polygons))
  if(conflict_area>0) geometry <- suppressWarnings(sf::st_difference(geometry,conflicts))
  geometry <- sf::st_make_valid(geometry)
  geometry <- suppressWarnings(sf::st_collection_extract(geometry,"POLYGON"))
  mask <- sf::st_sf(mask_id="CFS2025_current_landscape_tidal_influence",
    source_layers="2;3",method="positive current landscape tidal classifications; conflicting source overlap deferred",
    head_of_tide_status="mapped regional proxy; hydraulic head of tide varies with tide and river discharge",
    geometry=sf::st_union(geometry))
  classification_audit <- sf::st_drop_geometry(all_polygons)
  classification_audit$area_km2 <- as.numeric(sf::st_area(all_polygons))/1e6
  classification_audit <- dplyr::summarise(dplyr::group_by(classification_audit,
    tidal_source_layer,tidal_class,tidal_present,Data_Year),features=dplyr::n(),
    area_km2=sum(area_km2),.groups="drop")
  audit <- dplyr::bind_rows(audit_rows,data.frame(
    check=c("source_overlap_before_union","contradictory_selection_overlap_deferred","current_tidal_union_area"),
    source_layer=NA_integer_,observed=c(overlap_m2,conflict_area,as.numeric(sf::st_area(mask))),
    units="m2",passed=TRUE))
  notes <- c(
    "Landscape connectivity, not feature connectivity, controls selection; disconnected/developed/unknown classes are excluded.",
    "Partially and significantly restricted tidal polygons remain tidally influenced and are included.",
    "Layer 2 maps large river deltas (primarily circa 2020); layer 3 extends mapping along remaining Puget Sound nearshore (review completed 2024; report 2025). Original per-feature dates and certainty fields are retained.",
    "PMEP base mapping explicitly includes the freshwater tidal zone but also historical disconnected areas; the CFS current connectivity classifications distinguish those areas.",
    "The draft 2022 PMEP basis is modeled maximum tidal flooding under current topography and extreme tidal water levels; it is not a universal daily-high-tide contour.",
    "Published geometry is NAD83(HARN) Washington South State Plane US survey feet (EPSG:2927); cached analysis geometry is EPSG:26910. The polygons publish no vertical coordinate/datum cutoff, so no NAVD88 or MHHW elevation is inferred.",
    "This is a mapped current tidal-influence proxy; it is not an exact surveyed, discharge-independent top-of-tide line, floodplain boundary, future sea-level-rise scenario, or salinity boundary.",
    "Source polygons can contain channel gaps or classification seams; direct spatial clipping requires an independent channel coverage check. No whole stream component may be deleted solely because it connects to tidal water.",
    "Source overlap is measured before union; areas where one source is selected and the other unselected are deferred from the cut mask rather than overriding uncertain/non-tidal data.")
  if(any(!all_polygons$source_geometry_present)) notes <- c(notes,paste(
    "Published source records with empty native and GeoJSON geometry are retained as explicit holds, never fabricated:",
    paste(all_polygons$tidal_source_key[!all_polygons$source_geometry_present],collapse=", ")))
  if(any(all_polygons$source_geometry_repair_held)) notes <- c(notes,paste(
    "Substantive invalid-source repairs (area change over 1 m2) are retained for review but held from the cut mask:",
    paste(all_polygons$tidal_source_key[all_polygons$source_geometry_repair_held],collapse=", ")))
  bundle <- list(all_polygons=all_polygons,tidal_polygons=tidal_polygons,mask=mask,
    audit=audit,classification_audit=classification_audit,conflicts=sf::st_sf(geometry=conflicts),
    notes=notes,sources=files,manifest=manifest,
    generated_utc=format(Sys.time(),tz="UTC",usetz=TRUE))
  # Keep every individual derived cache under the ordinary Git file limit.
  # Recreate the selected subset in memory, rather than storing geometry twice.
  geometry_cache <- list(all_polygons_layer_2=all_polygons[all_polygons$tidal_source_layer==2L,],
    all_polygons_layer_3=all_polygons[all_polygons$tidal_source_layer==3L,],mask=mask)
  geometry_files <- data.frame(role=names(geometry_cache),file=paste0(names(geometry_cache),".rds"),
    stringsAsFactors=FALSE)
  for(i in seq_len(nrow(geometry_files))) saveRDS(geometry_cache[[i]],file.path(cache_dir,geometry_files$file[i]))
  geometry_files$md5 <- unname(tools::md5sum(file.path(cache_dir,geometry_files$file)))
  geometry_files$bytes <- file.info(file.path(cache_dir,geometry_files$file))$size
  saved_bundle <- bundle[setdiff(names(bundle),c("all_polygons","tidal_polygons","mask"))]
  saved_bundle$geometry_cache_files <- geometry_files
  saveRDS(saved_bundle,file.path(cache_dir,"tidal_sources.rds"))
  utils::write.csv(audit,file.path(cache_dir,"source_audit.csv"),row.names=FALSE)
  utils::write.csv(classification_audit,file.path(cache_dir,"classification_audit.csv"),row.names=FALSE,na="")
  bundle
}

load_phase3_tidal_sources <- function(cache_dir="spatial_data/phase3/tidal_sources",rebuild=FALSE) {
  path <- file.path(cache_dir,"tidal_sources.rds")
  if(rebuild || !file.exists(path)) return(build_phase3_tidal_sources(cache_dir))
  bundle <- readRDS(path)
  paths <- file.path(cache_dir,bundle$sources$file)
  if(!all(file.exists(paths)) || !identical(tolower(unname(tools::md5sum(paths))),tolower(bundle$sources$md5)))
    stop("Cached tidal source snapshots changed; rebuild and independently revalidate them")
  if(!is.null(bundle$geometry_cache_files)) {
    parts <- bundle$geometry_cache_files
    paths <- file.path(cache_dir,parts$file)
    if(!all(file.exists(paths)) || !identical(unname(tools::md5sum(paths)),parts$md5))
      stop("Derived tidal geometry caches changed; rebuild from the raw source snapshots")
    geometry <- lapply(paths,readRDS);names(geometry)<-parts$role
    bundle$all_polygons <- dplyr::bind_rows(geometry[grepl("^all_polygons_layer_",names(geometry))])
    bundle$tidal_polygons <- bundle$all_polygons[bundle$all_polygons$tidal_present,]
    bundle$mask <- geometry$mask
  }
  if(!inherits(bundle$mask,"sf") || !nrow(bundle$mask) || any(sf::st_is_empty(bundle$mask)) ||
      any(!sf::st_is_valid(bundle$mask))) stop("Invalid current tidal mask")
  bundle
}
