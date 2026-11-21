library(sf)
gpkg <- "outputs/phase1_total_barriers/North_Puget_Sound_Steelhead_phase1.gpkg"
masks <- st_read(gpkg,layer="upstream_basin_masks",quiet=TRUE)
baseline <- st_read("outputs/review_swifd_connections/North_Puget_Sound_Steelhead.gpkg",layer="steelhead_patched_untruncated",quiet=TRUE)
pops <- st_read(gpkg,layer="noaa_populations",quiet=TRUE)
barriers <- read.csv("outputs/phase1_total_barriers/phase1_barrier_decisions.csv",stringsAsFactors=FALSE)
sf_pop <- pops[pops$pop_id==292,]
before <- baseline[baseline$pop_id==292 & baseline$measure_habitat,]
remove <- masks[masks$review_number %in% c(797,798),]
keep <- masks[masks$review_number %in% c(810,833),]
cat("Requested masks\n");print(st_drop_geometry(masks[masks$review_number %in% c(797,798,810,833),c("review_number","review_record_key")]))
area_overlap <- function(a,b) sum(as.numeric(st_area(suppressWarnings(st_intersection(st_geometry(a),st_geometry(b))))))/1e6
cat("Area797",sum(as.numeric(st_area(remove[remove$review_number==797,])))/1e6,"Area798",sum(as.numeric(st_area(remove[remove$review_number==798,])))/1e6,"\n")
cat("Overlap797798",area_overlap(remove[remove$review_number==797,],remove[remove$review_number==798,]),"km2\n")
target <- st_union(st_geometry(remove))
if(nrow(keep)) target <- st_difference(target,st_union(st_geometry(keep)))
restore <- suppressWarnings(st_intersection(before,target))
restore <- restore[!st_is_empty(restore) & st_dimension(restore)==1,]
measure <- function(x) if(!nrow(x)) 0 else sum(as.numeric(st_length(st_union(st_geometry(x)))))/1609.344
rows <- lapply(seq_len(nrow(masks)),function(i) {
  z <- suppressWarnings(st_intersection(restore,st_geometry(masks)[i]))
  z <- z[!st_is_empty(z) & st_dimension(z)==1,]
  j <- match(masks$review_record_key[i],barriers$review_record_key)
  data.frame(review_number=masks$review_number[i],review_record_key=masks$review_record_key[i],
    StreamName=barriers$StreamName[j],report_blockage=barriers$report_blockage[j],
    target_restore_overlap_miles=measure(z),excluded=masks$review_number[i] %in% c(797,798),
    explicit_keep=masks$review_number[i] %in% c(810,833))
})
audit <- do.call(rbind,rows)
audit <- audit[audit$target_restore_overlap_miles>1e-7 | audit$review_number %in% c(797,798,810,833),]
audit <- audit[order(-audit$target_restore_overlap_miles),]
print(audit)
other <- masks[!masks$review_number %in% c(797,798,810,833),]
blocked <- suppressWarnings(st_intersection(restore,st_union(st_geometry(other))))
blocked <- blocked[!st_is_empty(blocked) & st_dimension(blocked)==1,]
cat("before physical miles",measure(before),"target after810/833 excludes",measure(restore),"still blocked byother masks",measure(blocked),"\n")
utils::write.csv(audit,"spatial_data/phase2/sf_nooksack_restoration_nesting.csv",row.names=FALSE)
saveRDS(list(audit=audit,target_geometry=target,target_lines=restore,
  physical_before_miles=measure(before),target_restoration_miles=measure(restore),
  still_blocked_by_other_masks_miles=measure(blocked)),"spatial_data/phase2/sf_nooksack_restoration_nesting.rds")
