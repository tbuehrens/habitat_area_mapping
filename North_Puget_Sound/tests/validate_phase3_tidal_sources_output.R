# Independent read-only source-to-export checks. No production tidal split,
# population aggregation, graph-cut, or map-rendering function is invoked.
suppressPackageStartupMessages(library(sf))
source("R/phase3_tidal_sources.R")
source("R/phase3_freshwater_tidal.R")
assert <- function(ok,message) if(!isTRUE(ok)) stop(message,call.=FALSE)
out <- "outputs/phase3_tidal_barriers"
gpkg <- file.path(out,"North_Puget_Sound_Steelhead_phase3.gpkg")
read_layer <- function(layer) st_read(gpkg,layer=layer,quiet=TRUE)
area_m2 <- function(g) if(!length(g) || all(st_is_empty(g))) 0 else sum(as.numeric(st_area(g)))
length_m <- function(g) {
  if(inherits(g,"sf")) g <- st_geometry(g)
  if(!length(g) || all(st_is_empty(g))) return(0)
  sum(as.numeric(st_length(st_union(g))))
}
same_area <- function(a,b,tolerance=.01) {
  difference <- suppressWarnings(st_sym_difference(st_union(st_geometry(a)),st_union(st_geometry(b))))
  area_m2(difference)<tolerance
}
same_lines <- function(a,b) {
  am <- length_m(a);bm <- length_m(b)
  if(abs(am-bm)>.01) return(FALSE)
  if(am<.001 && bm<.001) return(TRUE)
  distance <- as.numeric(st_distance(st_union(st_geometry(a)),st_union(st_geometry(b)),which="Hausdorff"))
  is.finite(distance) && distance<1e-7
}
cfs <- load_phase3_tidal_sources()
fresh <- load_phase3_freshwater_tidal()
raw <- readRDS("spatial_data/north_puget_swifd_bridges.rds")$bridges$network
bounds <- st_buffer(st_as_sfc(st_bbox(raw)),1000)
expected <- st_union(c(st_geometry(cfs$mask),st_geometry(fresh$mask)))
expected <- suppressWarnings(st_intersection(expected,bounds))
saved_mask <- read_layer("tidal_clip_mask")
assert(st_crs(saved_mask)$epsg==26910L && all(st_is_valid(saved_mask)) &&
  !any(st_is_empty(saved_mask)),"Invalid exported tidal mask")
assert(same_area(expected,saved_mask),"Exported tidal mask differs from approved CFS + screened NWI sources and 1 km source-bound crop")
assert(all(lengths(st_covered_by(st_geometry(raw),bounds))>0L),"Crop bounds do not cover every original source reach")

# Only positive current classifications and approved R1 additions may appear
# in the exported overlays. Original attributes/date uncertainty are preserved.
display <- read_layer("tidal_current_polygons")
cfs_display <- display[display$tidal_source_family=="WDFW/CFS current tidal extent",]
nwi_display <- display[display$tidal_source_family=="NWI freshwater-tidal addition",]
assert(nrow(cfs_display)>0L && nrow(nwi_display)>0L,"One tidal source family is absent from output GIS")
assert(all(cfs_display$tidal_source_key %in% cfs$tidal_polygons$tidal_source_key) &&
  all(cfs_display$tidal_present %in% TRUE) && !any(cfs_display$source_geometry_repair_held %in% TRUE) &&
  !any(grepl("Complete|Developed|Unknown|Non-tidal",cfs_display$tidal_class)),
  "Exported CFS overlay includes an unapproved class or geometry hold")
j <- match(cfs_display$tidal_source_key,cfs$all_polygons$tidal_source_key)
for(field in c("tidal_class","Data_Year","LS_TidalConnectivity_Certainty"))
  assert(identical(as.character(cfs_display[[field]]),as.character(cfs$all_polygons[[field]][j])),
    paste("Exported CFS source attribute changed:",field))
assert(all(nwi_display$nwi_source_key %in% fresh$polygons$nwi_source_key) &&
  all(grepl("^R1",nwi_display$ATTRIBUTE)),"Exported NWI overlay is not an approved freshwater-tidal addition")
pops <- read_layer("noaa_populations")
custom <- read_layer("custom_review_watershed")
region <- st_union(c(st_geometry(pops),st_geometry(custom)))
want_cfs_display <- suppressWarnings(st_intersection(st_geometry(cfs$tidal_polygons),region))
want_nwi_display <- suppressWarnings(st_intersection(st_geometry(fresh$polygons),region))
assert(same_area(cfs_display,want_cfs_display) && same_area(nwi_display,want_nwi_display),
  "GIS source overlays differ from the original approved polygons clipped to analysis areas")

# The export must retain its source inventory/quality holds and actual NWI
# imagery years, rather than replacing them with the service retrieval date.
source_audit <- read.csv(file.path(out,"tidal_source_audit.csv"),stringsAsFactors=FALSE)
for(i in seq_len(nrow(cfs$audit))) {
  j <- which(source_audit$check==cfs$audit$check[i] &
    (source_audit$source_layer==cfs$audit$source_layer[i] |
      is.na(source_audit$source_layer) & is.na(cfs$audit$source_layer[i])))
  j <- j[!is.na(j)]
  # write.csv preserves 15 significant digits; area totals can exceed 1e9 m2.
  csv_tolerance <- max(1e-8,abs(cfs$audit$observed[i])*1e-14)
  assert(length(j)==1L && abs(source_audit$observed[j]-cfs$audit$observed[i])<csv_tolerance &&
    identical(source_audit$passed[j],cfs$audit$passed[i]),"Exported source audit hides or changes a source-quality hold")
}
vintage <- read.csv(file.path(out,"freshwater_tidal_source_vintage.csv"),stringsAsFactors=FALSE,na.strings="")
assert(nrow(vintage)==nrow(fresh$source_vintage_join),"NWI source-vintage inventory changed")
for(field in names(fresh$source_vintage_join))
  assert(identical(as.character(vintage[[field]]),as.character(fresh$source_vintage_join[[field]])),
    paste("NWI source-vintage field changed:",field))
metadata <- jsonlite::fromJSON(file.path(out,"run_metadata.json"))
assert(any(grepl("Neither mapped dataset reaches",metadata$tidal_source_notes)),
  "Remaining physical head-of-tide uncertainty is not disclosed")

before <- read_layer("before_tidal_extent_status")
before <- before[before$measure_habitat & !before$removed,]
remaining <- read_layer("phase3_remaining")
remaining <- remaining[remaining$measure_habitat,]
removed <- read_layer("phase3_removed_tidal")
removed <- removed[removed$measure_habitat,]
residual <- suppressWarnings(st_intersection(st_union(st_geometry(remaining)),st_geometry(saved_mask)))
residual_m <- length_m(residual)
assert(residual_m<.001,"Retained habitat remains inside the approved mapped-tidal mask")
assert(!any(removed$barrier_removed %in% TRUE),"Barrier and tidal exclusions double-count the same source piece")
table <- read.csv(file.path(out,"population_phase3_miles.csv"),stringsAsFactors=FALSE)
assert(nrow(table)==17L && all(abs(table$removed_CFS_tidal_miles+
  table$removed_NWI_additional_tidal_miles-table$removed_current_tidal_miles)<1e-8),
  "Tidal source attribution does not partition the population exclusions")
rows <- list()
for(id in table$pop_id) {
  a <- before[before$pop_id==id,]
  b <- removed[removed$pop_id==id,]
  cfs_part <- suppressWarnings(st_intersection(a,st_geometry(cfs$mask)))
  nwi_part <- suppressWarnings(st_intersection(a,st_geometry(fresh$mask)))
  measured_cfs <- length_m(cfs_part)/1609.344
  measured_nwi <- length_m(nwi_part)/1609.344
  j <- match(id,table$pop_id)
  assert(abs(measured_cfs-table$removed_CFS_tidal_miles[j])<1e-8 &&
    abs(measured_nwi-table$removed_NWI_additional_tidal_miles[j])<1e-8,
    paste("Exported tidal source mileage disagrees with independent geometry in population",id))
  for(kind in unique(a$source_class)) {
    wanted <- suppressWarnings(st_intersection(a[a$source_class==kind,],st_geometry(saved_mask)))
    got <- b[b$source_class==kind,]
    assert(same_lines(wanted,got),paste("Tidal exclusion changed a source class or lost geometry:",id,kind))
  }
  rows[[length(rows)+1L]] <- data.frame(pop_id=id,CFS_miles=measured_cfs,NWI_additional_miles=measured_nwi)
}
measured <- do.call(rbind,rows)
expected_coastal <- data.frame(pop_id=c(272L,280L,285L,288L,290L,296L),
  CFS_miles=c(1.105994588267,5.898124149508,.117666563523,10.332408966549,15.050349241194,2.556642528271),
  NWI_additional_miles=c(0,.0052683674126,.089340649851,.0113273044669,1.3017436000385,5.765708078307))
j <- match(expected_coastal$pop_id,measured$pop_id)
assert(all(abs(measured$CFS_miles[j]-expected_coastal$CFS_miles)<1e-8) &&
  all(abs(measured$NWI_additional_miles[j]-expected_coastal$NWI_additional_miles)<1e-8),
  "The six independently assessed coastal population exclusions changed")

points <- read_layer("tidal_cutpoints")
point_table <- read.csv(file.path(out,"tidal_cutpoint_review.csv"),stringsAsFactors=FALSE)
assert(!anyDuplicated(points$tidal_id) && identical(points$tidal_id,point_table$tidal_id),
  "Mapped cutoff symbols and their review table disagree")
point_distance <- as.numeric(st_distance(points,st_boundary(st_union(st_geometry(saved_mask)))))
assert(all(point_distance<1e-5),"A mapped cutoff symbol is off the applied tidal boundary")
assert(all(grepl("not a surveyed",points$interpretation)),"Cutoff symbols imply a surveyed hydraulic limit")
cat(sprintf("PASS: exported tidal mask matches frozen CFS + screened NWI and source-bound crop; residual %.12g m; all 17 populations/source classes and six coastal mileage expectations match; dates, holds, overlays and %d cutoff IDs preserved.\n",
  residual_m,nrow(points)))
print(measured[match(expected_coastal$pop_id,measured$pop_id),],digits=12,row.names=FALSE)
