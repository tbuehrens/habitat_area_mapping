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
    result$masks$local_correction_limit_m<=100,result$masks$completeness_method=='full_drainageBasin',
    all(result$graph_audit$unproved_foreign_m<=1e-5),
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

prepare_pilchuck_creek_graph <- function(network,selected,edges,cache_dir='spatial_data/stillaguamish_review') {
  key<-'6:54208'
  point<-selected[selected$review_record_key %in% key,]
  stopifnot(nrow(point)==1L,point$review_number==634L,as.character(point$SiteId)=='890682',
    point$graph_edge_id==691L,as.character(point$matched_llid)=='1222246482085',point$snap_distance_m<=50)
  source_dir<-file.path(cache_dir,'pilchuck_sources')
  upstream<-nps_read_cache_json(file.path(source_dir,'634_upstream_flowlines.json'),simplifyVector=FALSE)
  upstream_ids<-vapply(upstream$features,function(f)as.character(f$properties$nhdplus_comid),character(1))
  foreign_down<-nps_read_cache_json(file.path(source_dir,'24275741_downstream_flowlines.json'),simplifyVector=FALSE)
  foreign_ids<-vapply(foreign_down$features,function(f)as.character(f$properties$nhdplus_comid),character(1))
  local_down<-nps_read_cache_json(file.path(source_dir,'24274849_downstream_flowlines.json'),simplifyVector=FALSE)
  local_ids<-vapply(local_down$features,function(f)as.character(f$properties$nhdplus_comid),character(1))
  stopifnot(all(c('24274849','24274863','24274873') %in% upstream_ids),
    !'24275741' %in% upstream_ids,all(c('24275741','24274777','24274371') %in% foreign_ids),
    !any(c('24274849','24274847') %in% foreign_ids),
    identical(foreign_ids[which(foreign_ids %in% local_ids)[1L]],'24274371'))
  deer<-nps_read_cache_json(file.path(source_dir,'context_24274777_flowline.json'),simplifyVector=FALSE)
  deer_feature<-Filter(function(f)as.character(f$properties$comid)=='24274777',deer$features)
  stopifnot(length(deer_feature)==1L,deer_feature[[1L]]$properties$gnis_name=='Deer Creek',
    tolower(deer_feature[[1L]]$properties$flowdir)=='with digitized')
  foreign_file<-file.path(source_dir,'foreign_24275741_whole_basin.json')
  foreign<-pilchuck_source_polygon(foreign_file,sf::st_crs(network))
  target_mask<-pilchuck_source_polygon(file.path(source_dir,'634_vector_split_basin.json'),sf::st_crs(network),'drainageBasin')
  foreign_area<-as.numeric(sf::st_area(foreign))/1e6
  stopifnot(abs(foreign_area/14.0265-1)<.05)
  rows<-which(network$graph_edge_id %in% 695L & network$measure_habitat %in% TRUE)
  stopifnot(length(rows)>0L)
  # Split only the original strict physical edge695 where the complete target
  # basin excludes it. The independent foreign watershed must contain every
  # corrected meter; geometry and all other barrier causes are preserved.
  affected<-network[rows,]
  outside<-upstream_line_parts(suppressWarnings(sf::st_difference(affected,target_mask)))
  inside<-upstream_line_parts(suppressWarnings(sf::st_intersection(affected,target_mask)))
  stopifnot(nrow(outside)>0L)
  unproved<-upstream_line_parts(suppressWarnings(sf::st_difference(outside,foreign)))
  unproved_m<-sum(as.numeric(sf::st_length(unproved)))
  if(unproved_m>1e-5)stop('Foreign Lake/Deer drainage does not cover the entire proposed graph correction: ',unproved_m,'m')
  edge<-edges[edges$graph_edge_id %in% 695L,]
  stopifnot(nrow(edge)==1L)
  off_edge<-upstream_line_parts(suppressWarnings(sf::st_difference(outside,sf::st_buffer(sf::st_geometry(edge),1e-5))))
  stopifnot(sum(as.numeric(sf::st_length(off_edge)))<=1e-5)
  remove_key<-function(values)vapply(strsplit(ifelse(is.na(values),'',values),';',fixed=TRUE),
    function(keys)paste(setdiff(keys,c('',key)),collapse=';'),character(1))
  initial<-tapply(as.numeric(sf::st_length(network)),network$source_class,sum)
  for(field in c('removed_by_barrier','downstream_of_barrier','unresolved_by_barrier'))
    outside[[field]]<-remove_key(outside[[field]])
  if(!'pilchuck_foreign_drainage_review' %in% names(network))network$pilchuck_foreign_drainage_review<-''
  inside$pilchuck_foreign_drainage_review<-''
  outside$pilchuck_foreign_drainage_review<-'634 graph certainty withheld: independently routed Deer Creek drainage outside full Pilchuck basin'
  corrected<-rbind(network[-rows,,drop=FALSE],inside,outside)
  corrected$removed<-nzchar(corrected$removed_by_barrier)
  corrected$retained<-!corrected$removed
  corrected$unresolved_direction<-nzchar(corrected$unresolved_by_barrier) & corrected$retained
  corrected$length_m<-as.numeric(sf::st_length(corrected))
  corrected$piece_id<-seq_len(nrow(corrected))
  final<-tapply(corrected$length_m,corrected$source_class,sum)
  error<-max(abs(initial-final[names(initial)]))
  stopifnot(error<=max(1e-5,sum(initial)*1e-10))
  audit<-data.frame(review_record_key=key,review_number=634L,original_physical_graph_edge_id=695L,
    corrected_physical_m=sum(as.numeric(sf::st_length(outside))),unproved_foreign_m=unproved_m,
    foreign_basin_comid='24275741',foreign_basin_area_km2=foreign_area,
    foreign_downstream_named_stream='Deer Creek',foreign_downstream_named_comid='24274777',
    first_shared_downstream_comid='24274371',first_shared_downstream_stream='Stillaguamish River',
    actual_lake_cavanaugh_upstream_comid='24274873',actual_lake_creek_mouth_upstream_comid='24274863',
    foreign_source_basin_cache_file=foreign_file,foreign_source_basin_md5=unname(tools::md5sum(foreign_file)),
    foreign_source_basin_url='https://api.water.usgs.gov/nldi/linked-data/comid/24275741/basin?f=json&simplified=false&splitCatchment=false',
    foreign_source_navigation_cache_file=file.path(source_dir,'24275741_downstream_flowlines.json'),
    foreign_source_navigation_md5=unname(tools::md5sum(file.path(source_dir,'24275741_downstream_flowlines.json'))),
    source_partition_error_m=error,other_barrier_causes_preserved=TRUE,
    action='Clear only634 upstream/downstream graph certainty on positively proven foreign edge695 tail; no habitat removed or restored by this review',
    stringsAsFactors=FALSE)
  list(network=corrected,audit=audit)
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
    'Official upstream navigation contains actualLakeCavanaughcenter24274873 andLakeCreekconfluence24274863.',
    'An undirected modeled branch crosses a drainage divide: provenforeign695tail drainsDeerCreek toStillaguamish belowPilchuck; only634graphcertainty iswithheld there. No other barrier causes or sourcegeometry are changed.',
    'The physicalbarrier snap stays8.77m<=50. A separately documented85.82m generalizedNHDreferenceoffset is checked byfineWDFWidentity and unchanged100m exactlocalmaskvalidation; no generic thresholds change.')
  result<-list(barriers=target,masks=checked$masks,anchors=anchor,audit=audit,notes=notes,
    context=inputs$context,graph_audit=corrected$audit,fine_identity=sf::st_drop_geometry(fine),
    basin_validation=checked$audit)
  saveRDS(result,file.path(cache_dir,'pilchuck_creek.rds'))
  utils::write.csv(audit,file.path(cache_dir,'pilchuck_creek_audit.csv'),row.names=FALSE)
  utils::write.csv(corrected$audit,file.path(cache_dir,'pilchuck_creek_graph_audit.csv'),row.names=FALSE)
  writeLines(notes,file.path(cache_dir,'pilchuck_creek_notes.txt'))
  invisible(result)
}
