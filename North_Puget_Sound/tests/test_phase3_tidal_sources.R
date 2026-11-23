# Offline verification of the exact source snapshots and conservative tidal
# selection. This test does not alter saved geometry or earlier products.
suppressPackageStartupMessages(library(sf))
source("R/phase3_tidal_sources.R")
x <- load_phase3_tidal_sources()
assert <- function(ok,text) if(!isTRUE(ok)) stop(text,call.=FALSE)
assert(nrow(x$all_polygons)==10004L && !anyDuplicated(x$all_polygons$tidal_source_key),
  "Complete published source inventory was lost or duplicated")
assert(sum(x$all_polygons$tidal_source_layer==2L)==4132L &&
  sum(x$all_polygons$tidal_source_layer==3L)==5872L,"Source layer inventory changed")
assert(nrow(x$mask)==1L && st_crs(x$mask)$epsg==26910L && st_is_valid(x$mask) &&
  !st_is_empty(x$mask),"Tidal mask is empty, invalid, or in the wrong CRS")
for(layer in c(2L,3L)) {
  rows <- x$all_polygons[x$all_polygons$tidal_source_layer==layer,]
  wanted <- rows$tidal_class %in% phase3_tidal_classes(layer) &
    rows$source_geometry_present & !rows$source_geometry_repair_held
  assert(identical(rows$tidal_present,wanted),"Current landscape connectivity selection changed")
}
assert(!any(grepl("Complete|Developed|Unknown|Non-tidal",x$tidal_polygons$tidal_class)),
  "Disconnected, developed, or unknown source classes were allowed to authorize a cut")
assert(!any(x$tidal_polygons$source_geometry_repair_held) &&
  all(x$tidal_polygons$source_geometry_present),"An unproven source geometry entered the cut mask")
assert(sum(x$all_polygons$source_geometry_repair_held)==12L,
  "Substantive source geometry repair holds are missing")
empty <- x$all_polygons[!x$all_polygons$source_geometry_present,]
assert(nrow(empty)==1L && empty$tidal_source_key=="CFS2025:3:3818" &&
  st_is_empty(empty) && !empty$tidal_present,"Published empty geometry was dropped or fabricated")
assert(all(c("LS_TidalConnectivity","LS_TidalConnectivity_Certainty","Data_Year",
  "Modification_Date","Verification_Method","ReviewerDate") %in% names(x$all_polygons)),
  "Original source uncertainty/date/verification fields were lost")
assert(all(x$geometry_cache_files$bytes<100*1024^2),"A derived cache exceeds the regular Git file limit")
audit <- read.csv("spatial_data/phase3/tidal_sources/held_geometry_scope_audit.csv",stringsAsFactors=FALSE)
assert(nrow(audit)==12L && all(audit$official_NPS_line_m<.001),
  "A deferred source repair overlaps mapped NPS distribution; explicit review is required")
cat("PASS: all 10004 source records and hashes; only positive current landscape tidal classes; 12 substantive repairs and one native empty geometry explicitly held; valid projected mask; source fields preserved; no held repair overlaps NPS habitat.\n")
