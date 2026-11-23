# Read-only regression against the frozen, approved pre-Lake-Cavanaugh revision.
# Reconstruct source geometry and compare outputs; no production cut/matcher runs.
suppressPackageStartupMessages(library(sf))
source("R/project_paths.R")
assert<-function(ok,message)if(!isTRUE(ok))stop(message,call.=FALSE)
out<-"outputs/phase3_tidal_barriers"
directory<-"spatial_data/lake_cavanaugh_review"
snapshot<-read.csv(file.path(directory,"before_fix/snapshot_manifest.csv"),stringsAsFactors=FALSE)
snapshot_paths<-nps_resolve_cache_path(snapshot$snapshot_path,must_work=TRUE)
assert(nrow(snapshot)==66L && identical(unname(tools::md5sum(snapshot_paths)),snapshot$md5) &&
  all(file.info(snapshot_paths)$size==snapshot$bytes),"Frozen 66-file pre-correction snapshot changed")
old_file<-function(original) {
  j<-which(endsWith(gsub("\\\\","/",snapshot$original_path),original))
  assert(length(j)==1L,paste("Snapshot source missing/ambiguous:",original))
  snapshot_paths[j]
}
old_gpkg<-old_file("outputs/phase3_tidal_barriers/North_Puget_Sound_Steelhead_phase3.gpkg")
new_gpkg<-file.path(out,"North_Puget_Sound_Steelhead_phase3.gpkg")
read_old<-function(layer)st_read(old_gpkg,layer=layer,quiet=TRUE)
read_new<-function(layer)st_read(new_gpkg,layer=layer,quiet=TRUE)
geometry<-function(x)if(inherits(x,"sf"))st_geometry(x) else x
length_m<-function(x) {
  g<-geometry(x)
  if(!length(g)||all(st_is_empty(g)))return(0)
  sum(as.numeric(st_length(st_union(g))))
}
area_m2<-function(x) {
  g<-geometry(x)
  if(!length(g)||all(st_is_empty(g)))return(0)
  sum(as.numeric(st_area(g)))
}
same_lines<-function(a,b) {
  if(identical(st_as_binary(geometry(a)),st_as_binary(geometry(b)))) {
    fields<-intersect(c("source_class","graph_edge_id"),intersect(names(a),names(b)))
    if(!length(fields)||all(vapply(fields,function(n)
      identical(as.character(a[[n]]),as.character(b[[n]])),logical(1))))return(TRUE)
  }
  # Immutable physical-edge and source-class groups avoid an enormous, weakly
  # attributable network-wide Hausdorff comparison.
  if(inherits(a,"sf")&&inherits(b,"sf")&&nrow(a)&&nrow(b)&&
      all(c("graph_edge_id","source_class")%in%names(a))&&
      all(c("graph_edge_id","source_class")%in%names(b))) {
    key<-function(x)paste(x$source_class,ifelse(is.na(x$graph_edge_id),"<none>",x$graph_edge_id),sep="|")
    ak<-key(a);bk<-key(b)
    if(length(unique(c(ak,bk)))>1L) {
      aa<-split(seq_len(nrow(a)),ak);bb<-split(seq_len(nrow(b)),bk)
      if(!setequal(names(aa),names(bb)))return(FALSE)
      return(all(vapply(names(aa),function(k)same_lines(a[aa[[k]],],b[bb[[k]],]),logical(1))))
    }
  }
  am<-length_m(a);bm<-length_m(b)
  if(abs(am-bm)>.01)return(FALSE)
  if(am<.001&&bm<.001)return(TRUE)
  ga<-st_union(geometry(a));gb<-st_union(geometry(b))
  if(isTRUE(st_equals(ga,gb,sparse=FALSE)[1,1]))return(TRUE)
  d<-as.numeric(st_distance(ga,gb,which="Hausdorff"))
  is.finite(d)&&d<1e-7
}
missing_m<-function(wanted,available) {
  a<-geometry(wanted);b<-geometry(available)
  if(!length(a)||all(st_is_empty(a)))return(0)
  if(!length(b)||all(st_is_empty(b)))return(length_m(a))
  a<-st_union(a);b<-st_union(b)
  if(isTRUE(st_equals(a,b,sparse=FALSE)[1,1]))return(0)
  # At UTM magnitudes, GEOS 1e-5 m buffer subtraction produces false residuals
  # even against itself. Exact equality first, documented 0.1 mm fallback next.
  length_m(suppressWarnings(st_difference(a,st_buffer(b,1e-4))))
}
same_polygons<-function(a,b)area_m2(suppressWarnings(st_sym_difference(
  st_union(geometry(a)),st_union(geometry(b)))))<.01
has_key<-function(value,key)vapply(strsplit(ifelse(is.na(value),"",value),";",fixed=TRUE),
  function(x)key%in%x,logical(1))
source_polygon<-function(path,crs,id=NULL) {
  raw<-jsonlite::fromJSON(path,simplifyVector=FALSE)
  features<-if(is.null(id))raw$features else Filter(function(f)identical(f$id,id),raw$features)
  assert(length(features)==1L,"Expected a unique authoritative source polygon")
  result<-st_transform(st_read(jsonlite::toJSON(list(type="FeatureCollection",features=features),
    auto_unbox=TRUE,digits=NA),quiet=TRUE),crs)
  result<-st_make_valid(result)
  if(any(st_geometry_type(result)=="GEOMETRYCOLLECTION"))
    result<-suppressWarnings(st_collection_extract(result,"POLYGON"))
  st_union(st_geometry(result))
}
checked_path<-function(mask,pathfield,hashfield) {
  path<-nps_resolve_cache_path(mask[[pathfield]],must_work=TRUE)
  assert(identical(unname(tools::md5sum(path)),as.character(mask[[hashfield]])),
    paste("Source provenance mismatch:",pathfield))
  path
}
intersection<-function(x,mask)suppressWarnings(st_intersection(x,geometry(mask)))
subtract_selected<-function(x,mask) {
  hit<-lengths(st_intersects(x,mask))>0L
  if(!any(hit))return(x)
  affected<-suppressWarnings(st_difference(x[hit,],geometry(mask)))
  rbind(x[!hit,],affected)
}

message("Frozen snapshot intact; reconstructing authoritative same-lake source union")
old_masks<-read_old("upstream_basin_masks");masks<-read_new("upstream_basin_masks")
assert(nrow(masks)==129L && !anyDuplicated(masks$review_record_key) &&
  setequal(old_masks$review_record_key,masks$review_record_key),"Applied barrier identities/count changed")
old634<-old_masks[old_masks$review_number==634L,];mask<-masks[masks$review_number==634L,]
assert(nrow(old634)==1L&&nrow(mask)==1L&&mask$review_record_key=="6:54208"&&
  isTRUE(mask$lake_cavanaugh_completed)&&
  mask$completeness_method=="full_drainageBasin_plus_verified_same_lake_catchments",
  "Missing explicit, documented same-lake correction")
for(i in which(old_masks$review_number!=634L)) {
  j<-match(old_masks$review_record_key[i],masks$review_record_key)
  assert(same_polygons(old_masks[i,],masks[j,]),"An unrelated approved watershed mask changed")
}
original_path<-checked_path(mask,"source_split_basin_cache_file","source_split_basin_md5")
lake_path<-checked_path(mask,"source_lake_basin_cache_file","source_lake_basin_md5")
waterbody_path<-checked_path(mask,"source_lake_waterbody_cache_file","source_lake_waterbody_md5")
original<-source_polygon(original_path,st_crs(mask),"drainageBasin")
lake_basin<-source_polygon(lake_path,st_crs(mask))
waterbody<-source_polygon(waterbody_path,st_crs(mask))
expected_mask<-st_union(c(original,lake_basin))
extension<-suppressWarnings(st_difference(expected_mask,original))
assert(same_polygons(old634,original)&&same_polygons(mask,expected_mask),
  "#634 correction differs from unchanged original whole basin plus official same-lake basin")
assert(area_m2(suppressWarnings(st_difference(original,geometry(mask))))<.01&&
  abs(area_m2(extension)/1e6-6.90881)<1e-4,"Wrong correction area or original drainage lost")
wb<-jsonlite::fromJSON(waterbody_path,simplifyVector=FALSE)$features[[1]]$properties
assert(as.character(wb$comid)=="24277115"&&wb$gnis_name=="Lake Cavanaugh", "Wrong lake waterbody")
for(comid in c("24274873","24275741")) {
  raw<-jsonlite::fromJSON(file.path(directory,paste0("nhd_flowline_",comid,".json")),simplifyVector=FALSE)
  p<-Filter(function(f)as.character(f$properties$comid)==comid,raw$features)
  assert(length(p)==1L&&as.character(p[[1]]$properties$wbareacomi)=="24277115"&&
    p[[1]]$properties$ftype=="ArtificialPath","Generalized routes do not belong to the same lake")
}
receiver<-jsonlite::fromJSON(file.path(directory,"official_receiving_route.json"))
assert(receiver$source_url=="https://appswr.ecology.wa.gov/docs/WaterRights/ScanToWRTS/HQ4/6802520_DraftReportofExam_50_156.pdf"&&
  receiver$page==5L&&receiver$exact_excerpt==paste("From the outlet weir, water is released to Lake Creek, tributary to Pilchuck Creek,",
    "which flows into the Stillaguamish River.")&&identical(receiver$pdf_archived,FALSE),
  "Independent official receiver evidence or its archival limitation changed")

message("Source union valid; independently checking fine channel samples and genuine Deer negative control")
proof_path<-checked_path(mask,"source_lake_fine_proof_cache_file","source_lake_fine_proof_md5")
proof<-readRDS(proof_path)
for(i in seq_len(nrow(proof$source_hashes))) {
  path<-if(proof$source_hashes$source[i]=="stage")old_file("spatial_data/phase3/barrier_stage.rds") else
    nps_resolve_cache_path(proof$source_hashes$path[i],must_work=TRUE)
  assert(identical(unname(tools::md5sum(path)),proof$source_hashes$md5[i]),"Independent fine-proof input changed")
}
lake<-proof$Lake_Creek_reference;pilchuck<-proof$Pilchuck_Creek_reference
deer<-proof$Deer_Creek_negative_reference
assert(all(as.character(lake$LLID)=="1220573483422")&&all(lake$LLID_STREAMNAME=="Lake Creek")&&
  all(as.character(pilchuck$LLID)=="1222246482085")&&all(pilchuck$LLID_STREAMNAME=="Pilchuck Creek")&&
  all(as.character(deer$LLID)=="1219314482681")&&all(deer$LLID_STREAMNAME=="Deer Creek"),
  "Fine channel identities changed")
reference<-readRDS(nps_resolve_cache_path(proof$source_hashes$path[proof$source_hashes$source=="fine_reference"],must_work=TRUE))
reference<-st_transform(reference,st_crs(mask))
for(x in list(lake,pilchuck,deer))assert(same_lines(x,reference[as.character(reference$LLID)==as.character(x$LLID[1]),]),
  "Proof reference differs from immutable source geometry")
touch<-suppressWarnings(st_intersection(st_union(geometry(lake)),st_union(geometry(pilchuck))))
assert(length(touch)==1L&&!st_is_empty(touch)&&st_geometry_type(touch)=="POINT"&&
  as.numeric(st_distance(touch,geometry(proof$Lake_to_Pilchuck_exact_touch)))<1e-7,
  "Lake Creek no longer positively touches Pilchuck Creek")
assert(length_m(intersection(deer,mask))<.001&&min(as.numeric(st_distance(proof$escaping_tail,deer)))>1900,
  "Correction clips genuine Deer Creek or assigns the lake tail to Deer")

# Independent numeric projections and 10 m chord headings; no production
# stream-identity helper/index/projection is invoked by this validator.
line_xy<-function(x)lapply(suppressWarnings(st_cast(st_geometry(x),"LINESTRING")),function(g)unclass(g)[,1:2,drop=FALSE])
path_length<-function(xy)sum(sqrt(rowSums(diff(xy)^2)))
position<-function(xy,m) {
  d<-sqrt(rowSums(diff(xy)^2));cumulative<-c(0,cumsum(d));m<-max(0,min(m,tail(cumulative,1)))
  j<-which(cumulative[-1]>=m & d>0)[1]
  as.numeric(xy[j,]+diff(xy)[j,]*(m-cumulative[j])/d[j])
}
projection<-function(xy,p) {
  ab<-diff(xy);start<-xy[-nrow(xy),,drop=FALSE];d2<-rowSums(ab^2)
  fraction<-pmax(0,pmin(1,rowSums(sweep(start,2,p,"-")*-ab)/pmax(d2,1e-30)))
  points<-start+ab*fraction;distance<-sqrt(rowSums(sweep(points,2,p,"-")^2));j<-which.min(distance)
  list(distance=distance[j],measure=sum(sqrt(d2)[seq_len(j-1)])+fraction[j]*sqrt(d2[j]))
}
tangent<-function(xy,m)position(xy,m+5)-position(xy,m-5)
own<-line_xy(proof$original_edge)[[1]];routes<-line_xy(lake)
samples<-proof$sample_points;points<-st_coordinates(samples)[,1:2,drop=FALSE]
assert(nrow(samples)==49L&&identical(as.integer(table(samples$window_id)),rep(7L,7)),"Incomplete fixed fine-course sample inventory")
angles<-distances<-numeric(nrow(points))
own_measures<-vapply(seq_len(nrow(points)),function(i)projection(own,points[i,])$measure,numeric(1))
for(i in seq_len(nrow(points))) {
  p<-points[i,];at<-projection(own,p)
  assert(at$distance<1e-7,"Fine-channel sample escaped original physical source")
  hits<-lapply(routes,projection,p=p);j<-which.min(vapply(hits,`[[`,numeric(1),"distance"))
  window<-own_measures[samples$window_id==samples$window_id[i]]
  # Primary local-support headings use a one-sided chord at the 100 m
  # support window endpoints, rather than borrowing geometry beyond it.
  u<-position(own,min(max(window),at$measure+5))-position(own,max(min(window),at$measure-5))
  v<-tangent(routes[[j]],hits[[j]]$measure)
  distances[i]<-hits[[j]]$distance
  angles[i]<-acos(min(1,abs(sum(u*v))/sqrt(sum(u*u)*sum(v*v))))*180/pi
}
assert(all(distances<=25+1e-8)&&all(angles<=35+1e-8)&&
  all(abs(distances-samples$distance_m)<1e-6)&&all(abs(angles-samples$heading_degrees)<1e-5),
  "Independent unchanged fine-course distance/heading check failed")

message("49 fine-course samples independently verified; comparing all immutable raw/global source")
old_stage<-readRDS(old_file("spatial_data/phase3/barrier_stage.rds"))
stage<-readRDS("spatial_data/phase3/barrier_stage.rds")
for(kind in unique(old_stage$network$source_class)) {
  a<-old_stage$network[old_stage$network$source_class==kind,];b<-stage$network[stage$network$source_class==kind,]
  assert(same_lines(a,b)&&abs(sum(as.numeric(st_length(a)))-sum(as.numeric(st_length(b))))<.001,
    paste("Global raw source geometry/multiplicity changed:",kind))
}
raw_added<-intersection(old_stage$network[old_stage$network$measure_habitat,],extension)
raw_added<-raw_added[as.numeric(st_length(raw_added))>1e-5,]
assert(nrow(raw_added)>0L&&all(raw_added$source_class=="prediction")&&all(raw_added$graph_edge_id==695L)&&
  abs(length_m(raw_added)-2158.88033959534)<.001&&missing_m(raw_added,proof$escaping_tail)<.001&&
  length_m(suppressWarnings(st_difference(raw_added,waterbody)))<.001,
  "Correction reaches unrelated raw habitat or misses the verified lake source tail")
assert(all(as.character(proof$raw_model_source_overlap$OBJECTID)=="20406")&&
  all(as.character(proof$raw_model_source_overlap$MAX_UniqueCHID)=="yaak_964")&&
  missing_m(proof$raw_model_source_overlap,raw_added)<.001,
  "Saved raw-source diagnostic identifies a different occupancy-model reach")
# The saved line/line-intersection diagnostic loses 9.13986 m to coincident
# overlay precision. Establish the ENTIRE source identity independently by
# clipping the original feature against the polygon, which has exact matching
# geometry/length (Hausdorff 0 m); do not infer completeness from that diagnostic.
raw_model<-st_transform(readRDS(nps_resolve_cache_path(proof$source_hashes$path[
  proof$source_hashes$source=="raw_model"],must_work=TRUE)),st_crs(mask))
raw_reach<-raw_model[as.character(raw_model$OBJECTID)=="20406" &
  as.character(raw_model$MAX_UniqueCHID)=="yaak_964",]
assert(nrow(raw_reach)==1L&&same_lines(intersection(raw_reach,extension),raw_added),
  "Correction cannot be reproduced directly from the original raw occupancy-model feature")
old_retained<-old_stage$network[old_stage$network$measure_habitat & !old_stage$network$removed,]
retained<-stage$network[stage$network$measure_habitat & !stage$network$removed,]
expected_retained<-subtract_selected(old_retained,extension)
assert(same_lines(expected_retained,retained),"Global remaining source differs from precisely the approved lake correction")
assert(missing_m(retained,old_retained)<.001,"A previously removed global reach was restored")
inside<-intersection(retained,mask)
protected<-has_key(inside$watershed_downstream_protected_by_barrier,mask$review_record_key)&
  has_key(inside$downstream_of_barrier,mask$review_record_key)
assert(length_m(inside[!protected,])<.001,"Unexplained retained habitat remains upstream of the completed #634 mask")

barriers<-read_new("barrier_characteristics");old_barriers<-read_old("barrier_characteristics")
assert(identical(barriers$review_number,old_barriers$review_number)&&
  identical(barriers$review_record_key,old_barriers$review_record_key)&&
  all(as.numeric(st_distance(barriers,old_barriers,by_element=TRUE))<1e-7),"Barrier numbering, source identity or snap location changed")
b634<-barriers[barriers$review_number==634L,]
assert(as.character(b634$SiteId)=="890682"&&b634$report_blockage=="Total"&&
  b634$report_natural_type=="Waterfall"&&b634$phase3_applied&&b634$graph_edge_id==691L&&
  b634$snap_distance_m<=50&&as.character(b634$matched_llid)=="1222246482085",
  "Physical Total waterfall source/strict same-channel snap changed")
for(number in c(903L,912L,331L,634L)) {
  key<-old_masks$review_record_key[old_masks$review_number==number]
  a<-old_stage$network[has_key(old_stage$network$watershed_downstream_protected_by_barrier,key)&
    has_key(old_stage$network$downstream_of_barrier,key),]
  b<-stage$network[has_key(stage$network$watershed_downstream_protected_by_barrier,key)&
    has_key(stage$network$downstream_of_barrier,key),]
  assert(nrow(a)>0L&&same_lines(a,b),paste("Earlier validated downstream proof changed:",number))
}

message("Global correction/no-restoration and prior cut proofs valid; comparing 17 population products")
old_all<-read_old("phase3_extent_status");all<-read_new("phase3_extent_status")
old_remaining<-read_old("phase3_remaining");remaining<-read_new("phase3_remaining")
old_remaining<-old_remaining[old_remaining$measure_habitat,];remaining<-remaining[remaining$measure_habitat,]
old_table<-read.csv(old_file("outputs/phase3_tidal_barriers/population_phase3_miles.csv"),stringsAsFactors=FALSE)
table<-read.csv(file.path(out,"population_phase3_miles.csv"),stringsAsFactors=FALSE)
assert(identical(old_table$pop_id,table$pop_id)&&nrow(table)==17L&&
  abs(old_table$phase3_remaining_miles[old_table$pop_id==296]-232.823399867038)<1e-8,
  "Wrong pre-correction baseline or analysis areas")
affected<-integer()
for(id in old_table$pop_id) {
  for(kind in unique(old_all$source_class))assert(same_lines(
    old_all[old_all$pop_id==id & old_all$source_class==kind,],all[all$pop_id==id & all$source_class==kind,]),
    paste("Population raw source changed:",id,kind))
  a<-old_remaining[old_remaining$pop_id==id,];b<-remaining[remaining$pop_id==id,]
  expected<-subtract_selected(a,extension)
  assert(same_lines(expected,b)&&missing_m(b,a)<.001,paste("Unrequested removal/restoration or missed lake cut:",id))
  j<-match(id,table$pop_id);delta<-old_table$phase3_remaining_miles[j]-table$phase3_remaining_miles[j]
  assert(delta>= -1e-8&&abs(length_m(b)/1609.344-table$phase3_remaining_miles[j])<1e-8,
    paste("Population mileage disagrees with source geometry:",id))
  if(delta>1e-8)affected<-c(affected,id) else assert(same_lines(a,b),paste("Unchanged population geometry changed:",id))
  message("Population ",id," verified")
}
assert(identical(as.integer(affected),296L)&&
  abs((old_table$phase3_remaining_miles[old_table$pop_id==296]-table$phase3_remaining_miles[table$pop_id==296])*
    1609.344-2158.88033959534)<.001,"Only the verified Stillaguamish lake source should lose mileage")

message("Only Stillaguamish changed by the exact verified source; checking stable tidal sources and review IDs")
assert(same_polygons(read_old("tidal_clip_mask"),read_new("tidal_clip_mask")),"Approved tidal mask changed")
assert(same_lines(read_old("phase3_removed_tidal"),read_new("phase3_removed_tidal")),"Approved tidal source geometry changed")
for(field in c("baseline_untruncated_miles","removed_current_tidal_miles","removed_CFS_tidal_miles",
  "removed_NWI_additional_tidal_miles"))assert(all(abs(old_table[[field]]-table[[field]])<1e-8),
    paste("Raw or tidal source mileage changed:",field))
old_points<-read_old("tidal_cutpoints");points<-read_new("tidal_cutpoints")
assert(nrow(points)==140L&&identical(old_points$tidal_id,points$tidal_id)&&
  identical(old_points$population_ids,points$population_ids)&&
  all(as.numeric(st_distance(old_points,points,by_element=TRUE))<1e-7),"Stable 140 tidal cutoff IDs/locations changed")
for(layer in c("noaa_populations","custom_review_watershed")) {
  a<-read_old(layer);b<-read_new(layer)
  assert(identical(a$pop_id,b$pop_id),"Analysis identity changed")
  for(i in seq_len(nrow(a)))assert(same_polygons(a[i,],b[i,]),"Official/custom analysis boundary changed")
}
for(path in c("freshwater_tidal_source_audit.csv","freshwater_tidal_source_vintage.csv","tidal_source_audit.csv",
  "tidal_cutpoint_review.csv"))assert(identical(unname(tools::md5sum(old_file(paste0(out,"/",path)))),
    unname(tools::md5sum(file.path(out,path)))),paste("Unchanged tidal provenance/review table changed:",path))
preserved<-read.csv(file.path(out,"preserved_source_manifest.csv"),stringsAsFactors=FALSE)
assert(identical(unname(tools::md5sum(nps_resolve_cache_path(preserved$path,must_work=TRUE))),preserved$md5),
  "Preserved baseline/Phase1/Phase2 output changed")
cat(sprintf("PASS: 66 frozen files intact; #634 equals documented117.423km2 source union; 49 independent fine-course samples support Lake Creek->Pilchuck, genuine Deer untouched; full raw source conserved, no restorations, precisely2158.88034m removed from296 only; other16 areas,128 earlier masks,local downstream proofs,tidal geometry/mileage and140 IDs unchanged. Stillaguamish remaining %.12f mi.\n",
  table$phase3_remaining_miles[table$pop_id==296]))
