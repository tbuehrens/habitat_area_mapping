# Independent offline check: old NWI mapping cannot override current CFS holds.
suppressPackageStartupMessages(library(sf))
source("R/phase3_tidal_sources.R")
source("R/phase3_nwi_tidal.R")
source("R/phase3_freshwater_tidal.R")
x<-load_phase3_freshwater_tidal()
cfs<-load_phase3_tidal_sources()
assert<-function(ok,text)if(!isTRUE(ok))stop(text,call.=FALSE)
assert(nrow(x$all_polygons)==129L && all(grepl("^R1",x$all_polygons$ATTRIBUTE)),
  "Freshwater tidal source lost its complete R1 classification inventory")
assert(nrow(x$polygons)>0 && all(grepl("^R1",x$polygons$ATTRIBUTE)) &&
  all(st_is_valid(x$polygons)) && !any(st_is_empty(x$polygons)),"Invalid or non-tidal approved supplement")
assert(nrow(x$mask)==1L && st_crs(x$mask)$epsg==26910L && st_is_valid(x$mask) &&
  !st_is_empty(x$mask),"Approved freshwater mask invalid")
excluded<-cfs$all_polygons[(!cfs$all_polygons$tidal_class_qualifies |
  cfs$all_polygons$source_geometry_repair_held) & cfs$all_polygons$source_geometry_present,]
excluded<-excluded[lengths(st_intersects(excluded,x$mask))>0,]
overlap<-suppressWarnings(st_intersection(st_geometry(x$mask),st_union(st_geometry(excluded))))
assert(sum(as.numeric(st_area(overlap)))<.01,
  "NWI overrides a current disconnected/developed/unknown/held CFS polygon")
duplicate<-suppressWarnings(st_intersection(st_geometry(x$mask),st_geometry(cfs$mask)))
assert(sum(as.numeric(st_area(duplicate)))<.01,"NWI addition duplicates existing CFS tidal area")
assert(all(x$conflict_audit$NWI_additions_authorized %in% FALSE) &&
  all(c("current_complete_non_tidal","current_unknown","current_developed","held_source_geometry") %in%
    x$conflict_audit$cfs_exclusion_basis),"Current restriction conflict holds were lost")
assert(all(c("PROJECT_NAME","STATUS","IMAGE_YR","IMAGE_DATE","SUPPMAPINFO","FGDC_METADATA") %in%
  names(x$source_vintage_join)),"Actual source vintage/provenance is missing")
assert(any(x$source_vintage_join$IMAGE_YR=="1980") &&
  any(x$source_vintage_join$IMAGE_YR=="2015"),"Old imagery uncertainty was hidden")
assert(any(grepl("No extra polygon|No.*invented|no invented",x$notes)),
  "Remaining physical head-of-tide evidence gap is not disclosed")
cat(sprintf("PASS: 129 NWI R1 source polygons; %.6f km2 additions; %.6f km2 deferred current conflicts; no CFS override or duplicated area; source vintages and physical-limit uncertainty retained.\n",
  as.numeric(st_area(x$mask))/1e6,sum(as.numeric(st_area(x$gap_geometry)))/1e6))
