# Independent freshwater-tidal channel supplement. NWI R1 expressly maps the
# Riverine Tidal subsystem, including freshwater tidal fluctuations upstream of
# the Estuarine salt boundary. Publication refresh and source imagery age differ.

build_phase3_nwi_tidal <- function(cache_dir="spatial_data/phase3/nwi_tidal",
    cfs_cache_dir="spatial_data/phase3/tidal_sources") {
  manifest <- jsonlite::fromJSON(file.path(cache_dir,"source_manifest.json"),simplifyVector=FALSE)
  sources <- do.call(rbind,lapply(manifest$sources,function(x) as.data.frame(x,stringsAsFactors=FALSE)))
  paths <- file.path(cache_dir,sources$file)
  if(!all(file.exists(paths)) || !identical(unname(tools::md5sum(paths)),sources$md5))
    stop("NWI source snapshots changed or are incomplete")
  read_layer <- function(label) {
    record <- Filter(function(x)identical(x$label,label),manifest$layers)[[1L]]
    chunks <- lapply(record$batch_files,function(f) {
      x<-sf::st_read(file.path(cache_dir,f),quiet=TRUE)
      values<-sf::st_drop_geometry(x);values[]<-lapply(values,as.character)
      sf::st_sf(values,geometry=sf::st_geometry(x))
    })
    x <- sf::st_transform(dplyr::bind_rows(chunks),26910)
    if(nrow(x)!=record$count || anyDuplicated(x[[record$oid_field]])) stop("NWI source inventory failed")
    x
  }
  raw <- read_layer("r1")
  if(!all(grepl("^R1",raw$ATTRIBUTE))) stop("Non-tidal or estuarine-only wetland entered R1 input")
  raw$source_geometry_valid_before <- sf::st_is_valid(raw)
  raw$source_geometry_valid_reason <- sf::st_is_valid(raw,reason=TRUE)
  raw$source_area_before_m2 <- as.numeric(sf::st_area(raw))
  polygons <- sf::st_make_valid(raw)
  polygons <- suppressWarnings(sf::st_collection_extract(polygons,"POLYGON"))
  if(nrow(polygons)!=nrow(raw) || any(sf::st_is_empty(polygons))) stop("NWI polygon normalization lost source records")
  polygons$source_area_after_m2 <- as.numeric(sf::st_area(polygons))
  polygons$source_geometry_area_change_m2 <- polygons$source_area_after_m2-polygons$source_area_before_m2
  polygons$source_geometry_repair_held <- abs(polygons$source_geometry_area_change_m2)>1
  polygons$nwi_source_key <- paste0("FWS_NWI_R1:",polygons$OBJECTID)
  polygons$nwi_selection_basis <- "published current NWI Riverine Tidal (R1) classification; not salinity/floodplain or future sea level"
  polygons$nwi_service_url <- manifest$layers[[1L]]$endpoint
  vintage <- sf::st_make_valid(read_layer("source_vintage"))
  vintage <- suppressWarnings(sf::st_collection_extract(vintage,"POLYGON"))
  matches <- sf::st_intersects(polygons,vintage)
  vintage_rows <- lapply(seq_len(nrow(polygons)),function(i) {
    j <- matches[[i]]
    if(!length(j)) return(data.frame(nwi_source_key=polygons$nwi_source_key[i],
      source_vintage_key=NA_character_,PROJECT_NAME=NA_character_,STATUS=NA_character_,
      IMAGE_YR=NA_character_,IMAGE_DATE=NA_character_,SUPPMAPINFO=NA_character_,FGDC_METADATA=NA_character_))
    values<-sf::st_drop_geometry(vintage[j,intersect(c("PROJECT_NAME","STATUS","IMAGE_YR","IMAGE_DATE",
      "SUPPMAPINFO","FGDC_METADATA"),names(vintage)),drop=FALSE])
    cbind(nwi_source_key=polygons$nwi_source_key[i],source_vintage_key=vintage$OBJECTID[j],values)
  })
  vintage_join <- dplyr::bind_rows(vintage_rows)
  cfs <- load_phase3_tidal_sources(cfs_cache_dir)
  good <- polygons[!polygons$source_geometry_repair_held,]
  candidate_union <- sf::st_union(sf::st_geometry(good))
  # An old/current classification conflict is not authority to override a
  # documented present restriction, developed area, or unresolved CFS area.
  blocked <- cfs$all_polygons[(!cfs$all_polygons$tidal_class_qualifies |
    cfs$all_polygons$source_geometry_repair_held) & cfs$all_polygons$source_geometry_present,]
  blocked$cfs_exclusion_basis <- ifelse(blocked$source_geometry_repair_held,"held_source_geometry",
    ifelse(grepl("Complete",blocked$tidal_class),"current_complete_non_tidal",
      ifelse(blocked$tidal_class=="Developed","current_developed","current_unknown")))
  blocked <- blocked[lengths(sf::st_intersects(blocked,candidate_union))>0L,]
  conflicts <- if(nrow(blocked)) suppressWarnings(sf::st_intersection(good,
    blocked[,c("tidal_source_key","tidal_class","cfs_exclusion_basis")])) else good[0,]
  if(nrow(conflicts)) {
    conflicts <- suppressWarnings(sf::st_collection_extract(conflicts,"POLYGON"))
    conflicts <- conflicts[!sf::st_is_empty(conflicts) & as.numeric(sf::st_area(conflicts))>0,]
  }
  blocked_union <- if(nrow(blocked)) sf::st_union(sf::st_geometry(blocked)) else sf::st_sfc(crs=26910)
  eligible <- if(nrow(blocked)) suppressWarnings(sf::st_difference(good,blocked_union)) else good
  eligible <- suppressWarnings(sf::st_collection_extract(eligible,"POLYGON"))
  eligible <- eligible[!sf::st_is_empty(eligible) & as.numeric(sf::st_area(eligible))>0,]
  nwi_union <- sf::st_union(sf::st_geometry(eligible))
  added <- suppressWarnings(sf::st_difference(nwi_union,sf::st_union(sf::st_geometry(cfs$mask))))
  added <- suppressWarnings(sf::st_collection_extract(sf::st_make_valid(added),"POLYGON"))
  combined <- sf::st_union(c(nwi_union,sf::st_geometry(cfs$mask)))
  mask <- sf::st_sf(mask_id="FWS_NWI_R1_current_restrictions_screened",geometry=nwi_union)
  added_mask <- sf::st_sf(mask_id="FWS_NWI_R1_added_beyond_CFS",geometry=sf::st_union(added))
  combined_mask <- sf::st_sf(mask_id="CFS2025_plus_FWS_NWI_R1_current_restrictions_screened",geometry=combined)
  audit <- data.frame(check=c("complete_R1_source_inventory","substantive_geometry_repairs_held",
      "current_CFS_conflict_area","NWI_added_area_beyond_CFS"),
    observed=c(nrow(polygons),sum(polygons$source_geometry_repair_held),
      if(nrow(conflicts))sum(as.numeric(sf::st_area(sf::st_union(sf::st_geometry(conflicts)))))else 0,
      as.numeric(sf::st_area(added_mask))),units=c("features","features","m2","m2"),
    passed=c(TRUE,!any(eligible$source_geometry_repair_held),TRUE,TRUE))
  notes <- c("NWI R1 is specifically freshwater riverine tidal habitat; it extends to the upstream limit of tidal fluctuations in the FWS classification.",
    "The public NWI service is refreshed biannually, but source imagery/project dates vary and are retained in source_vintage_join. Service retrieval date is not the imagery date.",
    "Any overlap with CFS current Completely Restricted, Developed, or Unknown classes is held rather than allowing older NWI mapping to override current evidence.",
    "NWI is biological wetland/deepwater mapping with scale and interpretation limitations, not a flow-dependent surveyed head-of-tide boundary; narrower channels and classification seams can be omitted.",
    "CFS base cache and all existing scenario products are unchanged by this comparison/supplement preparation.")
  bundle <- list(all_polygons=polygons,eligible_polygons=eligible,mask=mask,added_mask=added_mask,
    combined_mask=combined_mask,conflicts=conflicts,audit=audit,source_vintage=vintage,
    source_vintage_join=vintage_join,sources=sources,notes=notes,manifest=manifest)
  saveRDS(bundle,file.path(cache_dir,"nwi_tidal.rds"))
  utils::write.csv(audit,file.path(cache_dir,"source_audit.csv"),row.names=FALSE)
  utils::write.csv(vintage_join,file.path(cache_dir,"source_vintage_join.csv"),row.names=FALSE,na="")
  bundle
}

load_phase3_nwi_tidal <- function(cache_dir="spatial_data/phase3/nwi_tidal",rebuild=FALSE,
    cfs_cache_dir="spatial_data/phase3/tidal_sources") {
  path<-file.path(cache_dir,"nwi_tidal.rds")
  if(rebuild || !file.exists(path)) return(build_phase3_nwi_tidal(cache_dir,cfs_cache_dir))
  x<-readRDS(path)
  paths<-file.path(cache_dir,x$sources$file)
  if(!all(file.exists(paths)) || !identical(unname(tools::md5sum(paths)),x$sources$md5))
    stop("NWI source snapshots changed; rebuild and validate")
  x
}
