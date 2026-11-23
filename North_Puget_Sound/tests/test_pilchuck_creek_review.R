# Cached integration/safety tests. Production source, bundles and outputs are
# read only; invalid loader fixtures live only in the R process's temp folder.
for(name in c('arcgis','source_classification','barrier_metadata','barrier_stream_identity',
 'stream_edge_identity','stream_reference','network','phase_barrier_cuts','upstream_basins',
 'nldi_delineation','pilchuck_creek_review'))source(file.path('R',paste0(name,'.R')))
suppressPackageStartupMessages(library(sf))
assert<-function(ok,message)if(!isTRUE(ok))stop(message,call.=FALSE)
has_key<-function(values,key)vapply(strsplit(ifelse(is.na(values),'',values),';',fixed=TRUE),
 function(keys)key %in% keys,logical(1))
bundle<-load_pilchuck_creek_review()
point<-bundle$barriers;mask<-bundle$masks;key<-'6:54208'
assert(nrow(point)==1L && point$review_number==634L && as.character(point$SiteId)=='890682' &&
 point$report_blockage=='Total' && point$report_natural_type=='Waterfall' &&
 point$verification=='Not field verified' && point$snap_distance_m<=50 &&
 as.character(point$matched_llid)=='1222246482085' && point$graph_edge_id==691L,
 'The requested Pilchuck Creek waterfall identity or primary snap changed')
assert(!any(bundle$barriers$review_number %in% c(628L,633L)) &&
 identical(as.character(bundle$context$report_blockage),c('Total','Partial','Total')),
 'Lower Total628 or Partial633 was silently added to the waterfall-specific scenario')
report<-nps_read_cache_json(point$current_report_cache_file,simplifyVector=FALSE)
text<-paste(unlist(report$pages),collapse='\n')
assert(grepl('Site ID: 890682',text,fixed=TRUE) && grepl('Type: Waterfall',text,fixed=TRUE) &&
 grepl('Blockage: Total',text,fixed=TRUE),'Current official report does not explicitly identify Waterfall/Total634')
original<-st_read('outputs/review_swifd_connections/North_Puget_Sound_Steelhead.gpkg',
 layer='barriers_snapped',quiet=TRUE)
original<-original[original$review_number %in% 634L,]
assert(length(st_equals_exact(original,point,par=1e-7)[[1L]])==1L,
 'The primary modeled-channel barrier coordinate moved during coarse reference recovery')
source_file<-nps_resolve_cache_path(mask$source_split_basin_cache_file,must_work=TRUE)
assert(identical(unname(tools::md5sum(source_file)),as.character(mask$source_split_basin_md5)),
 'Pilchuck source basin fingerprint changed')
json<-jsonlite::fromJSON(source_file,simplifyVector=FALSE)
full<-Filter(function(f)identical(f$id,'drainageBasin'),json$features)
assert(length(full)==1L,'A local split catchment was substituted for the full upstream basin')
source<-st_transform(st_read(jsonlite::toJSON(list(type='FeatureCollection',features=full),
 auto_unbox=TRUE,digits=NA),quiet=TRUE),st_crs(mask))
source_geometry<-st_make_valid(st_geometry(source))
if(any(st_geometry_type(source_geometry)=='GEOMETRYCOLLECTION'))
 source_geometry<-suppressWarnings(st_collection_extract(source_geometry,'POLYGON'))
source_geometry<-st_union(source_geometry)
lake_source<-pilchuck_source_polygon(mask$source_lake_basin_cache_file,st_crs(mask))
source_union<-st_union(source_geometry,lake_source)
assert(mask$lake_cavanaugh_completed && mask$completeness_method=='full_drainageBasin_plus_verified_same_lake_catchments' &&
 identical(unname(tools::md5sum(mask$source_lake_basin_cache_file)),as.character(mask$source_lake_basin_md5)) &&
 sum(as.numeric(st_area(suppressWarnings(st_sym_difference(source_union,st_union(st_geometry(mask)))))))<1e-4,
 'Corrected mask differs from the exact union of two immutable complete source basins')
waterbody<-nps_read_cache_json(mask$source_lake_waterbody_cache_file,simplifyVector=FALSE)
assert(length(waterbody$features)==1L && waterbody$features[[1L]]$properties$gnis_name=='Lake Cavanaugh' &&
 as.character(waterbody$features[[1L]]$properties$comid)=='24277115','Source waterbody is not actual named Lake Cavanaugh')
for(comid in c('24274873','24275741')) {
 flow<-nps_read_cache_json(file.path('spatial_data/lake_cavanaugh_review',paste0('nhd_flowline_',comid,'.json')),simplifyVector=FALSE)
 f<-Filter(function(x)as.character(x$properties$comid)==comid,flow$features)[[1L]]
 assert(as.character(f$properties$wbareacomi)=='24277115' && f$properties$ftype=='ArtificialPath',
 'The two presumed separated drainages are not both artificial routes in the SAME named lake')
}
assert(mask$local_outlet_precision_validated && mask$local_correction_limit_m==100 &&
 bundle$basin_validation$proven_upstream_outside_mask_m<=1e-5 &&
 bundle$basin_validation$max_local_disagreement_distance_m<=100,
 'Exact upstream/downstream correction exceeded unchanged generic100m bounds')

edges<-st_read('outputs/review_swifd_connections/North_Puget_Sound_Steelhead.gpkg',
 layer='stream_matching_edges',quiet=TRUE)
raw<-readRDS('spatial_data/north_puget_swifd_bridges.rds')$bridges$network
graph<-cut_reviewed_barriers(raw,point,edges,bundle$anchors)$network
affected<-graph$graph_edge_id %in% 695L
graph$removed_by_barrier[affected]<-vapply(graph$removed_by_barrier[affected],
 function(keys)paste(c(keys[nzchar(keys)],'test_other_barrier'),collapse=';'),character(1))
graph$removed[affected]<-TRUE;graph$retained<-!graph$removed
before<-tapply(as.numeric(st_length(graph)),graph$source_class,sum)
checked<-prepare_pilchuck_creek_graph(graph,point,edges)
after<-tapply(as.numeric(st_length(checked$network)),checked$network$source_class,sum)
assert(max(abs(before-after[names(before)]))<1e-4 && checked$audit$source_partition_error_m<1e-4,
 'Foreign-drainage graph correction lost or added source length')
assert(identical(checked$network,graph) && checked$audit$graph_certainty_preserved &&
 checked$audit$other_barrier_causes_preserved && checked$audit$source_partition_error_m==0,
 'Lake correction altered original graph geometry or any upstream/downstream/other barrier cause')
assert(any(has_key(checked$network$removed_by_barrier[affected],key)) &&
 all(has_key(checked$network$removed_by_barrier[affected],'test_other_barrier')),
 'The retired foreign695 exception suppressed valid upstream certainty or another barrier cause')
proof<-readRDS(mask$source_lake_fine_proof_cache_file)
assert(proof$summary$Lake_Creek_LLID=='1220573483422' && all(proof$sample_points$supports_fine_Lake_Creek) &&
 !any(proof$Deer_Creek_negative_identity$identity_valid_for_cut) && proof$summary$lake_to_pilchuck_touch_distance_m<1e-5,
 'Independent fine LakeCreek/Pilchuck proof or genuine Deer negative control failed')
extension<-suppressWarnings(st_difference(source_union,source_geometry))
indexed<-raw;indexed$original_source_row_id<-seq_len(nrow(raw))
added<-upstream_line_parts(suppressWarnings(st_intersection(indexed,extension)))
assert(nrow(added)==1L && added$original_source_row_id==695L && added$source_class=='prediction' &&
 abs(sum(as.numeric(st_length(added)))-2158.88033959534)<1e-4,
 'The corrected mask newly intersects a source other than the individually verified LakeCreek695 tail')
lake_geometry<-pilchuck_source_polygon(mask$source_lake_waterbody_cache_file,st_crs(mask))
assert(sum(as.numeric(st_length(upstream_line_parts(suppressWarnings(st_difference(added,lake_geometry))))))<1e-5 &&
 sum(as.numeric(st_length(upstream_line_parts(suppressWarnings(st_intersection(proof$Deer_Creek_negative_reference,source_union))))))<1e-5,
 'The correction extends outside the actual lake source piece or into a genuine Deer channel')
generic<-validate_nldi_basin_masks(mask,checked$network,point)
assert(nrow(generic$masks)==1L && generic$audit$max_local_disagreement_distance_m<=100,
 'Recovered waterfall no longer passes the unchanged generic mask validator')

# Public loading and graph entry points must refuse changed identities,
# Partial classification, an over-tolerance primary snap, or modified lake
# evidence. These fixtures never replace the production bundle.
fixture<-tempfile('pilchuck_review_negative_');dir.create(fixture)
expect_loader_failure<-function(mutated,label) {
 saveRDS(mutated,file.path(fixture,'pilchuck_creek.rds'))
 failed<-tryCatch({load_pilchuck_creek_review(fixture);FALSE},error=function(e)TRUE)
 assert(failed,paste('Loader accepted',label))
}
bad<-bundle;bad$barriers$report_blockage<-'Partial';expect_loader_failure(bad,'a Partial barrier')
bad<-bundle;bad$barriers$snap_distance_m<-50.01;expect_loader_failure(bad,'a primary snap over50m')
bad<-bundle;bad$graph_audit$graph_certainty_preserved<-FALSE;expect_loader_failure(bad,'suppressed upstream graph certainty')
bad<-bundle;bad$masks$source_lake_basin_md5<-'changed';expect_loader_failure(bad,'a modified lake contribution basin')
bad<-bundle;bad$masks$source_split_basin_md5<-'changed';expect_loader_failure(bad,'a modified source basin')
bad_point<-point;bad_point$graph_edge_id<-695L
failed<-tryCatch({prepare_pilchuck_creek_graph(graph,bad_point,edges);FALSE},error=function(e)TRUE)
assert(failed,'Graph review accepted the lake channel as the waterfall attachment')
unlink(file.path(fixture,'pilchuck_creek.rds'))
cat('PASS: Pilchuck Creek634 current Total waterfall; unchanged primary50m and local100m gates; exact two-source fullbasin union; Lake695 upstream/source/othercause preservation; genuineDeer negative control; Partial/invalidsource safety guards.\n')
