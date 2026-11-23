# Approved NWI freshwater-tidal additions beyond the frozen CFS footprint.
# Required sources: R/phase3_tidal_sources.R and R/phase3_nwi_tidal.R.
# Reproducible public download: scripts/fetch_phase3_nwi_tidal.py.

build_phase3_freshwater_tidal <- function(cache_dir="spatial_data/phase3/nwi_tidal",
    cfs_cache_dir="spatial_data/phase3/tidal_sources",rebuild_nwi=FALSE) {
  nwi <- load_phase3_nwi_tidal(cache_dir,rebuild=rebuild_nwi,cfs_cache_dir=cfs_cache_dir)
  cfs <- load_phase3_tidal_sources(cfs_cache_dir)
  if(!"cfs_exclusion_basis" %in% names(nwi$conflicts))
    stop("Rebuild the NWI comparison to include explicit current-source geometry holds")
  polygons <- suppressWarnings(sf::st_difference(nwi$eligible_polygons,
    sf::st_union(sf::st_geometry(cfs$mask))))
  polygons <- suppressWarnings(sf::st_collection_extract(polygons,"POLYGON"))
  polygons <- polygons[!sf::st_is_empty(polygons) & as.numeric(sf::st_area(polygons))>0,]
  polygons$tidal_source <- "FWS NWI Riverine Tidal (R1)"
  polygons$tidal_status <- "additional mapped freshwater tidal extent; current CFS restrictions/unknown/geometry holds excluded"
  mask <- nwi$added_mask
  mask$tidal_source <- "FWS NWI freshwater tidal R1 additions beyond CFS"
  mask$restriction_screened <- TRUE
  mask$head_of_tide_status <- "mapped tidal classification proxy; no invented extension to the physical backwater limit"
  conflicts <- nwi$conflicts
  conflict_rows <- list()
  if(nrow(conflicts)) for(basis in unique(conflicts$cfs_exclusion_basis)) {
    selected <- conflicts[conflicts$cfs_exclusion_basis==basis,]
    area <- sum(as.numeric(sf::st_area(sf::st_union(sf::st_geometry(selected)))))
    conflict_rows[[length(conflict_rows)+1L]] <- data.frame(cfs_exclusion_basis=basis,
      source_polygons=length(unique(selected$nwi_source_key)),conflict_area_m2=area,
      NWI_additions_authorized=FALSE)
  }
  conflict_audit <- dplyr::bind_rows(conflict_rows)
  gap <- suppressWarnings(sf::st_difference(sf::st_union(sf::st_geometry(
    nwi$all_polygons[!nwi$all_polygons$source_geometry_repair_held,])),
    sf::st_union(c(sf::st_geometry(cfs$mask),sf::st_geometry(mask)))))
  gap <- suppressWarnings(sf::st_collection_extract(sf::st_make_valid(gap),"POLYGON"))
  gap_geometry <- sf::st_sf(gap_type="published NWI R1 area deferred by current CFS restrictions/unknown/geometry holds",
    geometry=gap)
  audit <- dplyr::bind_rows(nwi$audit,data.frame(
    check=c("approved_additional_NWI_polygons","approved_additional_NWI_area","deferred_NWI_coverage_gap_area"),
    observed=c(nrow(polygons),as.numeric(sf::st_area(mask)),sum(as.numeric(sf::st_area(gap_geometry)))),
    units=c("features","m2","m2"),passed=TRUE))
  source_cache_files <- data.frame(file=c("nwi_tidal.rds","source_manifest.json"),
    md5=unname(tools::md5sum(file.path(cache_dir,c("nwi_tidal.rds","source_manifest.json")))),stringsAsFactors=FALSE)
  cfs_cache_files <- cfs$geometry_cache_files
  notes <- c(nwi$notes,
    "The returned polygons and mask contain only approved NWI additions outside the frozen CFS current-tidal mask; union with CFS once, rather than adding overlapping miles.",
    "NWI additions are screened against current CFS complete non-tidal, developed, unknown, and held malformed-source footprints; conflict geometry and class-resolved audits are explicit.",
    "Neither mapped dataset reaches every published weak, discharge-dependent tidal backwater limit. No extra polygon or cutoff is invented across this remaining evidence gap.",
    "The gap geometry records deferred mapped NWI conflicts only; it does not purport to bound all unmapped physical tidal influence.")
  result <- list(polygons=polygons,mask=mask,audit=audit,conflict_audit=conflict_audit,
    notes=notes,sources=nwi$sources,gap_geometry=gap_geometry,conflict_geometry=conflicts,
    source_vintage_join=nwi$source_vintage_join,all_polygons=nwi$all_polygons,
    source_cache_files=source_cache_files,cfs_cache_files=cfs_cache_files,
    generated_utc=format(Sys.time(),tz="UTC",usetz=TRUE))
  saveRDS(result,file.path(cache_dir,"freshwater_tidal.rds"))
  utils::write.csv(audit,file.path(cache_dir,"freshwater_tidal_audit.csv"),row.names=FALSE)
  utils::write.csv(conflict_audit,file.path(cache_dir,"freshwater_tidal_conflicts.csv"),row.names=FALSE)
  result
}

load_phase3_freshwater_tidal <- function(cache_dir="spatial_data/phase3/nwi_tidal",
    cfs_cache_dir="spatial_data/phase3/tidal_sources",rebuild=FALSE) {
  path<-file.path(cache_dir,"freshwater_tidal.rds")
  if(rebuild || !file.exists(path)) return(build_phase3_freshwater_tidal(cache_dir,cfs_cache_dir))
  x<-readRDS(path)
  paths<-file.path(cache_dir,x$sources$file)
  if(!all(file.exists(paths)) || !identical(unname(tools::md5sum(paths)),x$sources$md5))
    stop("NWI raw source snapshots changed")
  for(record in list(list(directory=cache_dir,files=x$source_cache_files),
      list(directory=cfs_cache_dir,files=x$cfs_cache_files))) {
    paths<-file.path(record$directory,record$files$file)
    if(!all(file.exists(paths)) || !identical(unname(tools::md5sum(paths)),record$files$md5))
      stop("A screened source cache changed; rebuild the freshwater tidal supplement")
  }
  if(!inherits(x$mask,"sf") || nrow(x$mask)!=1L || any(!sf::st_is_valid(x$mask)) ||
      any(sf::st_is_empty(x$mask))) stop("Invalid approved freshwater tidal additions")
  x
}
