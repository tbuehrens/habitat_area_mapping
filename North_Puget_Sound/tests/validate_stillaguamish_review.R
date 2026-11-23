# Read-only regression against the approved, frozen pre-review Phase 3.
# No production graph, watershed clipping, or population aggregation is invoked.
suppressPackageStartupMessages(library(sf))
source("R/project_paths.R")
assert <- function(ok,message) if(!isTRUE(ok)) stop(message,call.=FALSE)
out <- "outputs/phase3_tidal_barriers"
snapshot <- read.csv("spatial_data/stillaguamish_review/previous_phase3/snapshot_manifest.csv",
  stringsAsFactors=FALSE)
snapshot_paths <- nps_resolve_cache_path(snapshot$snapshot_path,must_work=TRUE)
assert(nrow(snapshot)==57L && identical(unname(tools::md5sum(snapshot_paths)),snapshot$md5) &&
  all(file.info(snapshot_paths)$size==snapshot$bytes),"Frozen approved Phase 3 snapshot changed")
old_file <- function(original) {
  value <- gsub("\\\\","/",snapshot$original_path)
  j <- which(endsWith(value,original))
  assert(length(j)==1L,paste("Snapshot source is missing or ambiguous:",original))
  snapshot_paths[j]
}
old_gpkg <- old_file("outputs/phase3_tidal_barriers/North_Puget_Sound_Steelhead_phase3.gpkg")
new_gpkg <- file.path(out,"North_Puget_Sound_Steelhead_phase3.gpkg")
read_old <- function(layer) st_read(old_gpkg,layer=layer,quiet=TRUE)
read_new <- function(layer) st_read(new_gpkg,layer=layer,quiet=TRUE)
has_key <- function(values,key) vapply(strsplit(ifelse(is.na(values),"",values),";",fixed=TRUE),
  function(keys)key %in% keys,logical(1))
geometry <- function(x) if(inherits(x,"sf"))st_geometry(x) else x
length_m <- function(x) {
  g<-geometry(x)
  if(!length(g) || all(st_is_empty(g)))return(0)
  sum(as.numeric(st_length(st_union(g))))
}
area_m2 <- function(x) {
  g<-geometry(x)
  if(!length(g) || all(st_is_empty(g)))return(0)
  sum(as.numeric(st_area(g)))
}
same_lines <- function(a,b) {
  if(identical(st_as_binary(geometry(a)),st_as_binary(geometry(b)))) {
    fields<-c("source_class","graph_edge_id")
    comparable<-fields[fields %in% names(a) & fields %in% names(b)]
    if(!length(comparable) || all(vapply(comparable,function(n)
      identical(as.character(a[[n]]),as.character(b[[n]])),logical(1))))return(TRUE)
  }
  # These IDs identify the immutable physical source edges, not output pieces.
  # Comparing each source-labelled edge is stricter than a whole-network union
  # and avoids quadratic Hausdorff distance across hundreds of thousands of
  # unrelated vertices. Segmentation and row order are intentionally irrelevant.
  if(inherits(a,"sf") && inherits(b,"sf") &&
      all(c("graph_edge_id","source_class") %in% names(a)) &&
      all(c("graph_edge_id","source_class") %in% names(b)) && nrow(a) && nrow(b)) {
    key<-function(x)paste(x$source_class,ifelse(is.na(x$graph_edge_id),"<none>",
      as.character(x$graph_edge_id)),sep="|")
    ak<-key(a);bk<-key(b)
    if(length(unique(c(ak,bk)))>1L) {
      aa<-split(seq_len(nrow(a)),ak);bb<-split(seq_len(nrow(b)),bk)
      if(!setequal(names(aa),names(bb)))return(FALSE)
      return(all(vapply(names(aa),function(k)same_lines(a[aa[[k]],],b[bb[[k]],]),logical(1))))
    }
  }
  am<-length_m(a);bm<-length_m(b)
  if(abs(am-bm)>.01)return(FALSE)
  if(am<.001 && bm<.001)return(TRUE)
  ga<-st_union(geometry(a));gb<-st_union(geometry(b))
  if(isTRUE(st_equals(ga,gb,sparse=FALSE)[1,1]))return(TRUE)
  d<-as.numeric(st_distance(ga,gb,which="Hausdorff"))
  is.finite(d) && d<1e-7
}
missing_m <- function(wanted,available) {
  a<-geometry(wanted);b<-geometry(available)
  if(!length(a)||all(st_is_empty(a)))return(0)
  if(!length(b)||all(st_is_empty(b)))return(length_m(a))
  a<-st_union(a);b<-st_union(b)
  if(isTRUE(st_equals(a,b,sparse=FALSE)[1,1]))return(0)
  # GEOS microbuffers can self-subtract incorrectly at 1e-5 m; the documented
  # 0.1 mm fallback is used only after exact topological equality is checked.
  length_m(suppressWarnings(st_difference(a,st_buffer(b,1e-4))))
}
same_polygons <- function(a,b) area_m2(suppressWarnings(st_sym_difference(
  st_union(geometry(a)),st_union(geometry(b)))))<.01

old_stage<-readRDS(old_file("spatial_data/phase3/barrier_stage.rds"))
new_stage<-readRDS("spatial_data/phase3/barrier_stage.rds")
message("Snapshot intact; comparing complete global source by immutable physical edge")
assert(all(c("network","masks","barriers","ordinary_audit","olney_audit","olney_proof") %in%
  names(new_stage)),"Global pre-tide source/proof cache is incomplete")
for(kind in unique(old_stage$network$source_class)) {
  a<-old_stage$network[old_stage$network$source_class==kind,]
  b<-new_stage$network[new_stage$network$source_class==kind,]
  assert(same_lines(a,b),paste("Global source geometry was changed or filtered:",kind))
  assert(abs(sum(as.numeric(st_length(a)))-sum(as.numeric(st_length(b))))<.001,
    paste("Global source multiplicity/length changed:",kind))
}
message("Global source conserved; checking masks, stable identities and fresh source reports")
old_masks<-read_old("upstream_basin_masks")
masks<-read_new("upstream_basin_masks")
barriers<-read_new("barrier_characteristics")
old_barriers<-read_old("barrier_characteristics")
new_numbers<-c(581L,634L)
new_masks<-masks[masks$review_number %in% new_numbers,]
assert(nrow(new_masks)==2L && !anyDuplicated(masks$review_record_key) && nrow(masks)==129L &&
  all(old_masks$review_record_key %in% masks$review_record_key),"Expected retained masks plus French581 and Pilchuck634")
assert(setequal(setdiff(masks$review_record_key,old_masks$review_record_key),new_masks$review_record_key),
  "An unrequested upstream cutoff was added")
for(i in seq_len(nrow(old_masks))) {
  j<-match(old_masks$review_record_key[i],masks$review_record_key)
  assert(same_polygons(old_masks[i,],masks[j,]),"An earlier approved drainage polygon changed")
}
assert(all(old_barriers$review_number %in% barriers$review_number) && !anyDuplicated(barriers$review_number),
  "Original barrier numbers were lost or duplicated")
for(i in seq_len(nrow(old_barriers))) {
  j<-match(old_barriers$review_number[i],barriers$review_number)
  assert(identical(as.character(old_barriers$review_record_key[i]),as.character(barriers$review_record_key[j])),
    "A review number changed source identity")
}
selected<-barriers[match(new_numbers,barriers$review_number),]
assert(identical(as.character(selected$SiteId),c("890651","890682")) &&
  identical(as.character(selected$StreamName),c("French Cr","Pilchuck Cr")) &&
  all(selected$phase3_applied %in% TRUE) && all(selected$report_blockage=="Total") &&
  all(selected$snap_distance_m<=50) && all(selected$stream_identity_confirmed %in% TRUE) &&
  all(selected$identity_valid_for_cut %in% TRUE) && !any(selected$junction_ambiguous %in% TRUE),
  "New cut identity/explicit Total/strict physical snap criteria failed")
assert(barriers$report_blockage[barriers$review_number==915L]=="Partial" &&
  !barriers$phase3_applied[barriers$review_number==915L] &&
  barriers$snap_distance_m[barriers$review_number==915L]>50 &&
  !any(masks$review_number %in% c(633L,915L)),"A reviewed Partial/out-of-tolerance site was applied")

# Independently inspect original PDF identity, source hashes, and printed report
# classification, without invoking the production natural-barrier parser.
report_check<-function(directory,site,blockage,type) {
  path<-file.path(directory,paste0(site,"_Report.text.json"))
  record<-jsonlite::fromJSON(path,simplifyVector=FALSE)
  pdf<-nps_resolve_cache_path(record$report_pdf,must_work=TRUE)
  assert(identical(as.character(record$SiteId),site) &&
    identical(unname(tools::md5sum(pdf)),record$report_source_md5),"Refreshed report PDF identity/hash mismatch")
  pages<-unlist(record$pages)
  page<-pages[grepl("Natural Barrier Assessment",pages,fixed=TRUE)]
  # One PDF extractor joins "TotalBarrier Criteria:" without whitespace.
  # Accept only the exact known next field label, never arbitrary suffixes.
  assert(length(page)==1L && grepl(paste0("Site ID:\\s*",site,"(?=\\s|Stream:|$)"),page,perl=TRUE) &&
    grepl(paste0("Blockage:\\s*",blockage,"(?=\\s|Barrier Criteria:|$)"),page,perl=TRUE) &&
    grepl(paste0("Type:\\s*",type,"(?=\\s|Blockage:|$)"),page,perl=TRUE),
    paste("Printed source report classification changed:",site))
}
french_reports<-"spatial_data/stillaguamish_review/french/reports"
report_check(french_reports,"890795","Total","Waterfall")
report_check(french_reports,"890651","Total","Gradient")
report_check(french_reports,"890155","Partial","Waterfall")
report_check("spatial_data/stillaguamish_review/pilchuck_sources","890682","Total","Waterfall")

# The drainage polygon must be the whole published drainageBasin, rather than a
# split-only sliver or only the modeled component connected to the waterfall.
edges<-st_read("outputs/review_swifd_connections/North_Puget_Sound_Steelhead.gpkg",
  layer="stream_matching_edges",quiet=TRUE)
new_keys<-new_masks$review_record_key
for(i in seq_len(nrow(new_masks))) {
  mask<-new_masks[i,];key<-mask$review_record_key
  point<-selected[selected$review_record_key==key,]
  assert(all(vapply(c("watershed_validated","stream_identity_validated","total_barrier_approved",
    "coverage_complete","local_outlet_precision_validated"),function(n)isTRUE(mask[[n]][1]),logical(1))) &&
    mask$completeness_method %in% c("full_drainageBasin", "full_drainageBasin_plus_verified_same_lake_catchments"),
    "New upstream mask lacks complete approval")
  field<-intersect(c("source_split_basin_cache_file","mask_cache_file"),names(mask))
  field<-field[vapply(field,function(n) !is.na(mask[[n]][1]) && nzchar(mask[[n]][1]),logical(1))][1]
  assert(!is.na(field),"New mask has no full source polygon cache")
  source_path<-nps_resolve_cache_path(mask[[field]],must_work=TRUE)
  hash_field<-if(field=="source_split_basin_cache_file")"source_split_basin_md5" else "mask_cache_md5"
  assert(identical(unname(tools::md5sum(source_path)),as.character(mask[[hash_field]])),"New source basin hash changed")
  raw_basin<-jsonlite::fromJSON(source_path,simplifyVector=FALSE)
  full<-Filter(function(f)identical(f$id,"drainageBasin"),raw_basin$features)
  assert(length(full)==1L,"New cutoff is based on an incomplete local split catchment")
  source_polygon<-st_make_valid(st_transform(st_read(jsonlite::toJSON(
    list(type="FeatureCollection",features=full),auto_unbox=TRUE,digits=NA),quiet=TRUE),st_crs(mask)))
  source_polygon<-suppressWarnings(st_collection_extract(source_polygon,"POLYGON"))
  if(mask$review_number==634L && "lake_cavanaugh_completed" %in% names(mask) &&
      isTRUE(mask$lake_cavanaugh_completed)) {
    lake_path<-nps_resolve_cache_path(mask$source_lake_basin_cache_file,must_work=TRUE)
    assert(identical(unname(tools::md5sum(lake_path)),as.character(mask$source_lake_basin_md5)),
      "Verified contributing lake basin source changed")
    lake<-st_make_valid(st_transform(st_read(lake_path,quiet=TRUE),st_crs(mask)))
    if("id" %in% names(lake) && any(lake$id=="drainageBasin"))lake<-lake[lake$id=="drainageBasin",]
    assert(nrow(lake)==1L,"The correction needs one complete contributing lake basin")
    source_polygon<-st_sf(geometry=st_union(c(st_geometry(source_polygon),st_geometry(lake))))
  }
  assert(same_polygons(mask,source_polygon),"Applied new basin differs from its complete raw source polygon")
  if(mask$review_number==581L) assert(abs(area_m2(mask)/1e6-9.087444)<1e-5,"French full basin area changed")
  inside<-suppressWarnings(st_intersection(new_stage$network[new_stage$network$measure_habitat &
    !new_stage$network$removed,],st_geometry(mask)))
  protected<-has_key(inside$watershed_downstream_protected_by_barrier,key) &
    has_key(inside$downstream_of_barrier,key)
  assert(length_m(inside[!protected,])<.001,"Unexplained retained upstream habitat remains inside a new basin")
  if(any(protected)) {
    protected_geometry<-inside[protected,]
    at<-st_coordinates(point)[1,1:2]
    xy<-st_coordinates(protected_geometry)[,1:2,drop=FALSE]
    assert(length_m(protected_geometry)<=100.001 && max(sqrt(rowSums(sweep(xy,2,at)^2)))<=100.001,
      "New downstream precision protection exceeds its documented 100 m bound")
    own_edge<-edges[edges$graph_edge_id==point$graph_edge_id,]
    assert(nrow(own_edge)==1L && missing_m(protected_geometry,own_edge)<.001,
      "New downstream precision protection escaped the exact matched physical channel")
  }
}

# Previous special-case downstream proofs must survive full recomputation.
message("New complete masks/outlet protections valid; checking earlier special proofs")
for(number in c(903L,912L,331L)) {
  key<-old_masks$review_record_key[old_masks$review_number==number]
  assert(length(key)==1L,"An earlier special-case mask is missing")
  a<-old_stage$network[has_key(old_stage$network$watershed_downstream_protected_by_barrier,key) &
    has_key(old_stage$network$downstream_of_barrier,key),]
  b<-new_stage$network[has_key(new_stage$network$watershed_downstream_protected_by_barrier,key) &
    has_key(new_stage$network$downstream_of_barrier,key),]
  assert(nrow(a)>0L && same_lines(a,b),paste("An earlier downstream proof/tag was lost:",number))
}

old_all<-read_old("phase3_extent_status");new_all<-read_new("phase3_extent_status")
old_remaining<-read_old("phase3_remaining");new_remaining<-read_new("phase3_remaining")
old_remaining<-old_remaining[old_remaining$measure_habitat,]
new_remaining<-new_remaining[new_remaining$measure_habitat,]
old_table<-read.csv(old_file("outputs/phase3_tidal_barriers/population_phase3_miles.csv"),stringsAsFactors=FALSE)
new_table<-read.csv(file.path(out,"population_phase3_miles.csv"),stringsAsFactors=FALSE)
assert(identical(old_table$pop_id,new_table$pop_id) && nrow(new_table)==17L,"Analysis populations changed")
assert(all(abs(old_table$baseline_untruncated_miles-new_table$baseline_untruncated_miles)<1e-8),
  "Raw population source mileage changed")
allowed<-st_union(c(st_geometry(new_masks),st_geometry(st_buffer(selected,100))))
affected<-integer()
message("Checking 17 population source partitions, no restorations and spatial scope")
for(id in old_table$pop_id) {
  for(kind in unique(old_all$source_class)) assert(same_lines(
    old_all[old_all$pop_id==id & old_all$source_class==kind,],
    new_all[new_all$pop_id==id & new_all$source_class==kind,]),
    paste("Population source was changed or filtered:",id,kind))
  a<-old_remaining[old_remaining$pop_id==id,];b<-new_remaining[new_remaining$pop_id==id,]
  assert(missing_m(b,a)<.001,paste("An unrequested habitat restoration occurred:",id))
  loss<-suppressWarnings(st_difference(st_union(st_geometry(a)),st_buffer(st_union(st_geometry(b)),1e-4)))
  assert(length_m(suppressWarnings(st_difference(loss,allowed)))<.001,
    paste("Habitat was removed outside new basins/outlet precision zones:",id))
  j<-match(id,new_table$pop_id)
  delta<-old_table$phase3_remaining_miles[j]-new_table$phase3_remaining_miles[j]
  assert(delta> -1e-8,"Remaining mileage increased")
  if(delta>1e-8) affected<-c(affected,id) else assert(same_lines(a,b),paste("An unaffected population changed:",id))
  message("Population ",id," verified")
}
assert(all(c(296L,1001L) %in% affected) && length(affected)<=3L && !283L %in% affected,
  "Unexpected affected populations or confusion between Pilchuck Creek and Pilchuck River")

# Original source edge695 must be cut only inside the verified complete
# Pilchuck/lake basin. The prior 'foreign Deer tail' interpretation is superseded:
# its coarse routes were artificial paths within the same Lake Cavanaugh body.
pilchuck<-new_masks[new_masks$review_number==634L,]
foreign_claim<-new_stage$network[new_stage$network$measure_habitat &
  !is.na(new_stage$network$graph_edge_id) & new_stage$network$graph_edge_id==695L &
  has_key(new_stage$network$removed_by_barrier,pilchuck$review_record_key),]
outside_foreign<-suppressWarnings(st_difference(foreign_claim,st_geometry(pilchuck)))
assert(length_m(outside_foreign)<.001,"Pilchuck634 removes source edge695 outside the verified complete basin")

assert(same_polygons(read_old("tidal_clip_mask"),read_new("tidal_clip_mask")),"The approved tidal mask changed")
for(field in c("removed_current_tidal_miles","removed_CFS_tidal_miles","removed_NWI_additional_tidal_miles"))
  assert(all(abs(old_table[[field]]-new_table[[field]])<1e-8),paste("Tidal source attribution changed:",field))
old_points<-read_old("tidal_cutpoints");new_points<-read_new("tidal_cutpoints")
assert(identical(old_points$tidal_id,new_points$tidal_id) &&
  identical(old_points$population_ids,new_points$population_ids) &&
  all(as.numeric(st_distance(old_points,new_points,by_element=TRUE))<1e-7),"Stable tidal review IDs/locations changed")
for(layer in c("noaa_populations","custom_review_watershed")) {
  a<-read_old(layer);b<-read_new(layer)
  assert(identical(a$pop_id,b$pop_id),"Population identity changed")
  for(i in seq_len(nrow(a))) assert(same_polygons(a[i,],b[i,]),"An official/custom analysis boundary changed")
}
preserved<-read.csv(file.path(out,"preserved_source_manifest.csv"),stringsAsFactors=FALSE)
assert(identical(unname(tools::md5sum(nps_resolve_cache_path(preserved$path,must_work=TRUE))),preserved$md5),
  "A preserved baseline/Phase1/Phase2 output changed")
cat(sprintf("PASS: frozen 57-file snapshot intact; full source conserved; only approved581/634 cuts; no restoration; %d populations changed (%s); full basins/protected downstream/old special proofs valid; tidal mask, source mileage and %d cutoff IDs unchanged.\n",
  length(affected),paste(affected,collapse=","),nrow(new_points)))
print(data.frame(pop_id=affected,previous_miles=old_table$phase3_remaining_miles[match(affected,old_table$pop_id)],
  current_miles=new_table$phase3_remaining_miles[match(affected,new_table$pop_id)],
  additional_removed_miles=old_table$phase3_remaining_miles[match(affected,old_table$pop_id)]-
    new_table$phase3_remaining_miles[match(affected,new_table$pop_id)]),digits=12,row.names=FALSE)
