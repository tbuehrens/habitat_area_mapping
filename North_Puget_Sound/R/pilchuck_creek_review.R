# Individually reviewed Pilchuck CREEK waterfall; Pilchuck RIVER is unrelated.
if(!exists('nps_read_cache_json',mode='function'))source(file.path('R','project_paths.R'))

pilchuck_creek_review_inputs <- function(cache_dir='spatial_data/stillaguamish_review') {
  points<-sf::st_read('outputs/phase3_tidal_barriers/North_Puget_Sound_Steelhead_phase3.gpkg',
    layer='barrier_characteristics',quiet=TRUE)
  context<-points[points$review_number %in% c(628L,633L,634L),]
  stopifnot(identical(as.integer(context$review_number),c(628L,633L,634L)),
    identical(as.character(context$SiteId),c('890624','890145','890682')),
    all(as.character(context$StreamName)=='Pilchuck Cr'),
    all(as.character(context$matched_llid)=='1222246482085'),all(context$graph_edge_id==691L))
  current<-nps_read_cache_json(file.path(cache_dir,'pilchuck_sources','current_wdfw_sites.json'),simplifyVector=FALSE)
  for(i in seq_len(nrow(context))) {
    report<-nps_read_cache_json(file.path(cache_dir,'pilchuck_sources',paste0(context$SiteId[i],'_Report.text.json')),
      simplifyVector=FALSE)
    parsed<-parse_natural_barrier_report(unlist(report$pages),context$SiteId[i],report$report_url,
      report$report_retrieved_utc,report$report_source_md5)
    stopifnot(isTRUE(parsed$report_identity_verified))
    for(field in names(parsed))context[[field]][i]<-parsed[[field]][1L]
    context$current_report_cache_file[i]<-file.path(cache_dir,'pilchuck_sources',paste0(context$SiteId[i],'_Report.text.json'))
    context$current_report_pdf[i]<-report$report_pdf
    feature<-Filter(function(f)as.character(f$attributes$SiteId)==as.character(context$SiteId[i]),current$features)
    stopifnot(length(feature)==1L)
    context$current_source_longitude[i]<-feature[[1L]]$attributes$SiteLongitude
    context$current_source_latitude[i]<-feature[[1L]]$attributes$SiteLatitude
  }
  stopifnot(identical(as.character(context$report_blockage),c('Total','Partial','Total')),
    identical(as.character(context$report_natural_type),c('Gradient','Waterfall','Waterfall')))
  target<-context[context$review_number %in% 634L,]
  stopifnot(target$snap_distance_m<=50,target$matching_channel_count==1L,!target$junction_ambiguous,
    all(vapply(c('stream_identity_confirmed','snap_accepted','identity_valid_for_cut'),function(n)isTRUE(target[[n]][1L]),logical(1))))
  list(target=target,context=context)
}

prepare_pilchuck_creek_requests <- function(cache_dir='spatial_data/stillaguamish_review') {
  inputs<-pilchuck_creek_review_inputs(cache_dir)
  records<-nps_read_cache_json('spatial_data/upstream_basins/batch/manifest.json',simplifyVector=FALSE)
  record<-Filter(function(x)x$review_record_key=='6:54208',records)[[1L]]
  route_json<-nps_read_cache_json(file.path(record$cache_folder,'named_flowline.json'),simplifyVector=FALSE)
  feature<-Filter(function(f)as.character(f$properties$comid)=='24274849',route_json$features)
  stopifnot(length(feature)==1L,identical(feature[[1L]]$properties$gnis_name,'Pilchuck Creek'))
  route<-sf::st_read(jsonlite::toJSON(list(type='FeatureCollection',features=feature),auto_unbox=TRUE,digits=NA),quiet=TRUE)
  route<-suppressWarnings(sf::st_cast(sf::st_transform(route,sf::st_crs(inputs$target)),'LINESTRING'))
  stopifnot(nrow(route)==1L)
  xy<-unclass(sf::st_geometry(route)[[1L]])[,1:2,drop=FALSE]
  hit<-stream_identity_project(xy,sf::st_coordinates(inputs$target)[1L,1:2])
  vector_point<-sf::st_sfc(sf::st_point(hit$xy),crs=sf::st_crs(inputs$target))
  geographic<-sf::st_coordinates(sf::st_transform(vector_point,4326))[1L,1:2]
  request<-list(review_number=634L,review_record_key='6:54208',SiteId='890682',
    vector_longitude=geographic[1L],vector_latitude=geographic[2L],vector_shift_m=hit$distance,
    physical_snap_distance_m=inputs$target$snap_distance_m,nldi_comid='24274849',
    source_named_reference_cache_file=file.path(record$cache_folder,'named_flowline.json'))
  jsonlite::write_json(request,file.path(cache_dir,'pilchuck_requests.json'),auto_unbox=TRUE,pretty=TRUE,digits=NA)
  print(request)
  invisible(inputs)
}

load_pilchuck_creek_review <- function(cache_dir='spatial_data/stillaguamish_review') {
  path<-file.path(cache_dir,'pilchuck_creek.rds')
  if(!file.exists(path))stop('Prepare the Pilchuck Creek waterfall review bundle first: ',path)
  result<-nps_read_cache_rds(path)
  stopifnot(nrow(result$barriers)==1L,nrow(result$masks)==1L,
    result$barriers$review_number==634L,result$barriers$review_record_key=='6:54208',
    as.character(result$barriers$SiteId)=='890682',result$barriers$report_blockage=='Total',
    result$barriers$report_natural_type=='Waterfall',result$barriers$snap_distance_m<=50,
    result$masks$review_record_key=='6:54208',result$masks$local_outlet_precision_validated,
    result$masks$local_correction_limit_m<=100,result$masks$lake_cavanaugh_completed,
    result$masks$completeness_method=='full_drainageBasin_plus_verified_same_lake_catchments',
    all(result$graph_audit$graph_certainty_preserved),
    identical(unname(tools::md5sum(result$masks$source_lake_basin_cache_file)),as.character(result$masks$source_lake_basin_md5)),
    identical(unname(tools::md5sum(result$masks$source_lake_waterbody_cache_file)),as.character(result$masks$source_lake_waterbody_md5)),
    identical(unname(tools::md5sum(result$masks$source_split_basin_cache_file)),as.character(result$masks$source_split_basin_md5)))
  result
}

pilchuck_source_polygon <- function(path,crs,feature_id=NULL) {
  raw<-nps_read_cache_json(path,simplifyVector=FALSE)
  features<-if(is.null(feature_id))raw$features else Filter(function(f)identical(f$id,feature_id),raw$features)
  stopifnot(length(features)==1L)
  value<-sf::st_read(jsonlite::toJSON(list(type='FeatureCollection',features=features),auto_unbox=TRUE,digits=NA),quiet=TRUE)
  geometry<-sf::st_make_valid(sf::st_transform(sf::st_geometry(value),crs))
  if(any(sf::st_geometry_type(geometry)=='GEOMETRYCOLLECTION'))geometry<-suppressWarnings(sf::st_collection_extract(geometry,'POLYGON'))
  geometry<-sf::st_union(geometry)
  stopifnot(!sf::st_is_empty(geometry),sf::st_geometry_type(geometry) %in% c('POLYGON','MULTIPOLYGON'))
  geometry
}

# The generalized NHD lake routing assigns two artificial paths in the SAME
# Lake Cavanaugh waterbody to separate downstream routes. Do not suppress the
# independently supported fine Lake Creek -> Pilchuck upstream graph on that
# basis. The explicit source-basin union below fixes completeness, with no
# graph, tolerance, source geometry, or other barrier-cause modifications.
prepare_pilchuck_creek_graph <- function(network,selected,edges,cache_dir='spatial_data/stillaguamish_review') {
  point<-selected[selected$review_record_key %in% '6:54208',]
  stopifnot(nrow(point)==1L,point$review_number==634L,as.character(point$SiteId)=='890682',
    point$graph_edge_id==691L,as.character(point$matched_llid)=='1222246482085',point$snap_distance_m<=50)
  evidence<-pilchuck_lake_cavanaugh_evidence(sf::st_crs(network))
  edge<-edges[edges$graph_edge_id %in% 695L,]
  stopifnot(nrow(edge)==1L)
  audit<-data.frame(review_record_key='6:54208',review_number=634L,original_physical_graph_edge_id=695L,
    graph_certainty_preserved=TRUE,source_partition_error_m=0,other_barrier_causes_preserved=TRUE,
    lake_waterbody_comid='24277115',lake_waterbody_name='Lake Cavanaugh',
    formerly_misclassified_lake_artificial_path_comid='24275741',
    fine_channel_llid='1220573483422',fine_receiving_channel_llid='1222246482085',
    action='Preserve all original upstream graph certainty; retire incorrect foreign-Deer exception after named same-lake and fine-channel verification',
    stringsAsFactors=FALSE)
  list(network=network,audit=audit)
}

pilchuck_lake_cavanaugh_evidence <- function(crs,directory='spatial_data/lake_cavanaugh_review') {
  lake_file<-file.path(directory,'nhd_waterbody_24277115.json')
  basin_file<-file.path(directory,'lake_cavanaugh_24275741_whole_basin.json')
  lake_json<-nps_read_cache_json(lake_file,simplifyVector=FALSE)
  stopifnot(length(lake_json$features)==1L,
    lake_json$features[[1L]]$properties$gnis_name=='Lake Cavanaugh',
    as.character(lake_json$features[[1L]]$properties$comid)=='24277115')
  for(comid in c('24274873','24275741')) {
    flow<-nps_read_cache_json(file.path(directory,paste0('nhd_flowline_',comid,'.json')),simplifyVector=FALSE)
    feature<-Filter(function(f)as.character(f$properties$comid)==comid,flow$features)
    stopifnot(length(feature)==1L,as.character(feature[[1L]]$properties$wbareacomi)=='24277115',
      feature[[1L]]$properties$ftype=='ArtificialPath')
  }
  proof<-nps_read_cache_rds(file.path(directory,'fine_channel_proof.rds'))
  stopifnot(proof$summary$Lake_Creek_LLID=='1220573483422',
    proof$summary$Pilchuck_Creek_LLID=='1222246482085',proof$summary$all_window_coverage>=.8,
    all(proof$window_summary$identity_valid_for_cut),all(proof$sample_points$supports_fine_Lake_Creek),
    proof$summary$lake_to_pilchuck_touch_distance_m<=1e-5,
    !any(proof$Deer_Creek_negative_identity$identity_valid_for_cut))
  list(lake=pilchuck_source_polygon(lake_file,crs),basin=pilchuck_source_polygon(basin_file,crs),
    lake_file=lake_file,basin_file=basin_file,proof=proof)
}

pilchuck_complete_lake_cavanaugh_mask <- function(mask,raw,edges) {
  evidence<-pilchuck_lake_cavanaugh_evidence(sf::st_crs(mask))
  original<-sf::st_union(sf::st_geometry(mask))
  combined<-sf::st_union(original,evidence$basin)
  extension<-suppressWarnings(sf::st_difference(combined,original))
  # Inventory every source piece newly intersected by the full component union.
  # The reviewed data contain precisely one original prediction edge, all of
  # which lies in the actual named lake polygon; no Deer geometry is implicated.
  indexed<-raw;indexed$original_source_row_id<-seq_len(nrow(raw))
  additions<-upstream_line_parts(suppressWarnings(sf::st_intersection(indexed,extension)))
  additions$length_m<-as.numeric(sf::st_length(additions))
  additions<-additions[additions$length_m>1e-5,]
  stopifnot(nrow(additions)==1L,additions$original_source_row_id==695L,
    additions$source_class=='prediction',isTRUE(additions$measure_habitat))
  off_lake<-upstream_line_parts(suppressWarnings(sf::st_difference(additions,evidence$lake)))
  stopifnot(sum(as.numeric(sf::st_length(off_lake)))<=1e-5)
  physical<-edges[edges$graph_edge_id %in% 695L,]
  off_edge<-upstream_line_parts(suppressWarnings(sf::st_difference(additions,sf::st_buffer(sf::st_geometry(physical),1e-5))))
  stopifnot(sum(as.numeric(sf::st_length(off_edge)))<=1e-5)
  deer<-evidence$proof$Deer_Creek_negative_reference
  deer_inside<-upstream_line_parts(suppressWarnings(sf::st_intersection(deer,combined)))
  deer_m<-sum(as.numeric(sf::st_length(deer_inside)))
  stopifnot(deer_m<=1e-5)
  sf::st_geometry(mask)<-combined
  mask$lake_cavanaugh_completed<-TRUE
  mask$completeness_method<-'full_drainageBasin_plus_verified_same_lake_catchments'
  mask$source_lake_basin_cache_file<-evidence$basin_file
  mask$source_lake_basin_md5<-unname(tools::md5sum(evidence$basin_file))
  mask$source_lake_basin_url<-'https://api.water.usgs.gov/nldi/linked-data/comid/24275741/basin?f=json&simplified=false&splitCatchment=false'
  mask$source_lake_waterbody_cache_file<-evidence$lake_file
  mask$source_lake_waterbody_md5<-unname(tools::md5sum(evidence$lake_file))
  mask$source_lake_waterbody_url<-'https://api.water.usgs.gov/fabric/pygeoapi/collections/nhdwaterbody/items?comid=24277115&f=json'
  mask$source_lake_fine_proof_cache_file<-'spatial_data/lake_cavanaugh_review/fine_channel_proof.rds'
  mask$source_lake_fine_proof_md5<-unname(tools::md5sum(mask$source_lake_fine_proof_cache_file))
  mask$source_original_basin_area_km2<-as.numeric(sf::st_area(original))/1e6
  mask$source_lake_basin_area_km2<-as.numeric(sf::st_area(evidence$basin))/1e6
  mask$source_completed_basin_area_km2<-as.numeric(sf::st_area(combined))/1e6
  mask$source_lake_extension_area_km2<-as.numeric(sf::st_area(extension))/1e6
  mask$lake_cavanaugh_completion_note<-paste('Union of unchanged original full634 drainageBasin and unchanged official whole24275741 contributing basin.',
    'Both NHD artificial paths are in named LakeCavanaugh waterbody24277115; fineWDFW LakeCreek connects exactly to PilchuckCreek,',
    'and WAC173-18-330/Ecology investigator corroborate that receiving route. Original generalized downstream routing divided the lake incorrectly.',
    'Every newly intersected raw source segment is audited inside the named lake; genuine fineDeerCreek remains wholly outside.')
  audit<-data.frame(review_number=634L,review_record_key='6:54208',original_source_row_id=additions$original_source_row_id,
    graph_edge_id=695L,source_class=additions$source_class,newly_intersected_source_m=additions$length_m,
    newly_intersected_source_miles=additions$length_m/1609.344,all_new_source_inside_named_lake=TRUE,
    fine_channel_name='Lake Creek',fine_channel_llid='1220573483422',
    original_basin_km2=mask$source_original_basin_area_km2,lake_basin_km2=mask$source_lake_basin_area_km2,
    union_basin_km2=mask$source_completed_basin_area_km2,extension_km2=mask$source_lake_extension_area_km2,
    fine_Deer_negative_llid='1219314482681',fine_Deer_inside_corrected_mask_m=deer_m,
    graph_certainty_preserved=TRUE,previous_classification_corrected='Artificial path within Lake Cavanaugh was incorrectly classified as foreign Deer drainage',
    method=mask$completeness_method,stringsAsFactors=FALSE)
  list(mask=mask,audit=audit)
}

build_pilchuck_creek_review <- function(cache_dir='spatial_data/stillaguamish_review') {
  inputs<-pilchuck_creek_review_inputs(cache_dir)
  target<-inputs$target;lower<-inputs$context[inputs$context$review_number %in% 628L,]
  edges<-sf::st_read('outputs/review_swifd_connections/North_Puget_Sound_Steelhead.gpkg',layer='stream_matching_edges',quiet=TRUE)
  named<-load_stream_identity_reference()
  lower_direction<-derive_downstream_anchors(lower,edges,named,marine_geometry=readRDS('spatial_data/marine_north.rds'))
  stopifnot(nrow(lower_direction$anchors)==1L)
  own<-edges[edges$graph_edge_id %in% 691L,]
  xy<-unclass(sf::st_geometry(own)[[1L]])[,1:2,drop=FALSE]
  measure<-function(point)stream_identity_project(xy,sf::st_coordinates(point)[1L,1:2])$measure
  direction<-sign(measure(lower_direction$anchors)-measure(lower))
  stopifnot(direction!=0,(measure(target)-measure(lower))*direction<0)
  anchor<-lower_direction$anchors
  anchor$review_record_key<-target$review_record_key
  anchor$anchor_id<-'Pilchuck634_same_unbranched_channel_published_root_direction'
  anchor$direction_basis<-paste('Same original unbranched physical edge691 as independently rooted lowerPilchuck reference;',
    'current634 fine named-channel identity; downstream root uniquely corroborates LLIDmouth.',
    '634 is2753.53m upstream of628; anchor50m below634 on the unchanged physical channel.')
  sf::st_geometry(anchor)<-sf::st_sfc(sf::st_point(stream_identity_at(xy,measure(target)+50*direction)),crs=sf::st_crs(target))
  raw<-readRDS('spatial_data/north_puget_swifd_bridges.rds')$bridges$network
  fine_index<-build_stream_identity_index(raw,named)
  fine<-assess_stream_edge_identity(fine_index,target,barrier_stream='Pilchuck Creek')
  stopifnot(fine$stream_identity_valid,as.character(fine$matched_llid)=='1222246482085',fine$support_coverage>=.8)
  source_dir<-file.path(cache_dir,'pilchuck_sources')
  request<-nps_read_cache_json(file.path(cache_dir,'pilchuck_requests.json'),simplifyVector=FALSE)
  confirmed<-nps_read_cache_json(file.path(source_dir,'634_vector_position.json'),simplifyVector=FALSE)
  stopifnot(length(confirmed$features)==1L,as.character(confirmed$features[[1L]]$properties$comid)=='24274849',
    request$vector_shift_m<100,target$snap_distance_m<=50)
  route_json<-nps_read_cache_json(request$source_named_reference_cache_file,simplifyVector=FALSE)
  feature<-Filter(function(f)as.character(f$properties$comid)=='24274849',route_json$features)[[1L]]
  stopifnot(feature$properties$gnis_name=='Pilchuck Creek',tolower(feature$properties$flowdir)=='with digitized')
  basin_file<-file.path(source_dir,'634_vector_split_basin.json')
  complete<-nldi_validate_basin_completeness(nps_read_cache_json(basin_file,simplifyVector=FALSE),
    feature$properties,list(comid='24274849'),sf::st_crs(target))
  stopifnot(isTRUE(complete$valid),complete$diagnostics$completeness_method=='full_drainageBasin')
  mask<-sf::st_sf(review_record_key='6:54208',review_number=634L,graph_edge_id=691L,
    watershed_validated=TRUE,stream_identity_validated=TRUE,total_barrier_approved=TRUE,coverage_complete=TRUE,
    local_outlet_precision_validated=FALSE,protect_local_graph_downstream=TRUE,local_correction_limit_m=100,
    primary_source_snap_distance_m=target$snap_distance_m,coarse_reference_registration_offset_m=request$vector_shift_m,
    nldi_comid='24274849',indexing_method='individual_fine_identity_review_of_same_named_coarse_reference_vector_outlet',
    source_url='https://api.water.usgs.gov/nldi/pygeoapi/processes/nldi-splitcatchment/execution?f=json',
    source_md5=unname(tools::md5sum(basin_file)),source_split_basin_cache_file=basin_file,
    source_split_basin_md5=unname(tools::md5sum(basin_file)),
    source_pourpoint_longitude=request$vector_longitude,source_pourpoint_latitude=request$vector_latitude,
    source_coarse_reference_cache_file=request$source_named_reference_cache_file,
    source_coarse_reference_md5=unname(tools::md5sum(request$source_named_reference_cache_file)),
    coarse_reference_exception_note='Generic NLDIcourse gates unchanged; this individually reviewed site uses independent fineWDFW identity and unchanged100m exactlocalmask gates. Physical barrier remains at its8.77m accepted model snap.',
    geometry=pilchuck_source_polygon(basin_file,sf::st_crs(target),'drainageBasin'))
  for(field in names(complete$diagnostics))mask[[field]]<-complete$diagnostics[[field]]
  lake_completion<-pilchuck_complete_lake_cavanaugh_mask(mask,raw,edges)
  mask<-lake_completion$mask
  target$explicit_total_passage<-target$confirmed_total_for_scenario<-TRUE
  target$phase1_selected<-TRUE
  cuts<-cut_reviewed_barriers(raw,target,edges,anchor)
  corrected<-prepare_pilchuck_creek_graph(cuts$network,target,edges,cache_dir)
  checked<-validate_nldi_basin_masks(mask,corrected$network,target)
  print(checked$audit[,c('review_number','approval_status','approval_reason','proven_upstream_outside_mask_m',
    'proven_downstream_inside_mask_m','max_local_disagreement_distance_m')])
  saveRDS(list(barriers=target,candidate_masks=mask,anchors=anchor,graph=corrected,validation=checked,
    fine_identity=fine,lower_direction=lower_direction),file.path(cache_dir,'pilchuck_candidate_diagnostics.rds'))
  stopifnot(nrow(checked$masks)==1L)
  audit<-checked$audit
  audit$SiteId<-target$SiteId;audit$StreamName<-target$StreamName
  audit$report_blockage<-target$report_blockage;audit$report_natural_type<-target$report_natural_type
  audit$verification<-target$verification;audit$source_report_url<-target$report_url
  audit$primary_source_snap_distance_m<-target$snap_distance_m
  audit$coarse_reference_registration_offset_m<-request$vector_shift_m
  audit$fine_reference_max_distance_m<-fine$max_reference_distance_m
  audit$fine_reference_median_heading_degrees<-fine$median_heading_degrees
  audit$fine_reference_coverage<-fine$support_coverage
  audit$lower628_upstream_distance_to634_m<-(measure(target)-measure(lower))*(-direction)
  audit$lower628_action<-'Context only; lowerGradientTotal628 is not applied by the specific requested waterfall634 review'
  notes<-c('Pilchuck CREEK is Stillaguamish population296, LLID1222246482085; unrelated Pilchuck RIVER population283 is unchanged.',
    '634/WDFW890682 is current reportWaterfall/Total, notfieldverified, unknownheight.633nearby isWaterfall/Partial andexcluded.',
    '628 is a lowerGradientTotal record ~2753.5m below634; retained ascontext only, notautomatically applied in this waterfall-specific review.',
    'The generalized NHD lake routing incorrectly partitions LakeCavanaugh artificial paths24274873/24275741 despite both belonging to the same named waterbody24277115.',
    'The incorrect foreign-Deer exception is retired. FineWDFW and WAC/Ecology corroborate LakeCavanaugh to LakeCreek to PilchuckCreek; graph certainty and all source geometry/other barrier causes are preserved.',
    'The full634 sourcebasin is unioned with the unchanged official whole24275741 same-lake basin. Every newly intersected raw source piece was checked: only prediction physical6952158.88034m, entirely within the named lake; genuine DeerCreek remains outside.',
    'The physicalbarrier snap stays8.77m<=50. A separately documented85.82m generalizedNHDreferenceoffset is checked byfineWDFWidentity and unchanged100m exactlocalmaskvalidation; no generic thresholds change.')
  result<-list(barriers=target,masks=checked$masks,anchors=anchor,audit=audit,notes=notes,
    context=inputs$context,graph_audit=corrected$audit,fine_identity=sf::st_drop_geometry(fine),
    basin_validation=checked$audit,lake_cavanaugh_audit=lake_completion$audit)
  saveRDS(result,file.path(cache_dir,'pilchuck_creek.rds'))
  utils::write.csv(audit,file.path(cache_dir,'pilchuck_creek_audit.csv'),row.names=FALSE)
  utils::write.csv(corrected$audit,file.path(cache_dir,'pilchuck_creek_graph_audit.csv'),row.names=FALSE)
  writeLines(notes,file.path(cache_dir,'pilchuck_creek_notes.txt'))
  invisible(result)
}
