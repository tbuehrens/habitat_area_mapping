# Recover the lowest source-confirmed Total Olney barrier. Existing review
# numbers, classifications and snapshots are retained; no broad thresholds move.
if(!exists('nps_read_cache_json',mode='function')) source(file.path('R','project_paths.R'))

phase3_olney_inputs <- function() {
  points<-sf::st_read('outputs/review_swifd_connections/North_Puget_Sound_Steelhead.gpkg',
    layer='barriers_snapped',quiet=TRUE)
  points<-points[as.character(points$StreamName) %in% 'Olney Cr',]
  metadata<-nps_read_cache_csv('outputs/phase2_reviewed_barriers/phase2_barrier_decisions.csv',
    stringsAsFactors=FALSE,check.names=FALSE)
  rows<-match(points$review_record_key,metadata$review_record_key)
  stopifnot(!anyNA(rows))
  for(field in names(metadata)) points[[field]]<-metadata[[field]][rows]
  expected<-data.frame(review_number=c(331L,335L,379L,380L),
    review_record_key=c('6:43814','6:44019','6:45316','6:45410'),
    SiteId=c('891117','891277','891269','891268'))
  total<-points[match(expected$review_number,points$review_number),]
  stopifnot(identical(as.character(total$review_record_key),expected$review_record_key),
    identical(as.character(total$SiteId),expected$SiteId),all(total$report_blockage=='Total'),
    all(total$snap_accepted %in% TRUE),all(total$stream_identity_confirmed %in% TRUE),
    all(total$identity_valid_for_cut %in% TRUE),!any(total$junction_ambiguous %in% TRUE),
    all(total$matching_channel_count==1L),all(total$snap_distance_m<=50),
    all(as.character(total$matched_llid)=='1217124478723'),all(total$graph_edge_id==6133L))
  for(i in seq_len(nrow(total))) {
    report<-nps_read_cache_json(file.path('spatial_data/barrier_reports',paste0(total$SiteId[i],'_Report.text.json')),
      simplifyVector=FALSE)
    parsed<-parse_natural_barrier_report(unlist(report$pages),total$SiteId[i],
      report$report_url,report$report_retrieved_utc,report$report_source_md5)
    stopifnot(isTRUE(parsed$report_identity_verified),identical(parsed$report_blockage,'Total'))
  }
  points
}

build_phase3_olney_barriers <- function(cache_dir='spatial_data/phase3',prepare_only=FALSE) {
  dir.create(cache_dir,recursive=TRUE,showWarnings=FALSE)
  points<-phase3_olney_inputs()
  target<-points[points$review_number==331L,]
  records<-nps_read_cache_json('spatial_data/upstream_basins/batch/manifest.json',simplifyVector=FALSE)
  record<-Filter(function(x)x$review_record_key=='6:43814',records)[[1L]]
  stopifnot(isTRUE(record$approved),as.character(record$comid)=='23963685',
    record$route_support_coverage>=.8,record$indexed_shift_m<=50,
    identical(record$reference_stream_name,'Olney Creek'))
  source_xy<-sf::st_coordinates(sf::st_transform(target,4326))[1L,1:2]
  inputs<-list(review_record_key='6:43814',review_number=331L,SiteId='891117',
    StreamName='Olney Creek',physical_longitude=source_xy[1L],physical_latitude=source_xy[2L],
    indexed_longitude=record$indexed_longitude,indexed_latitude=record$indexed_latitude,
    comid='23963685',source_manifest='spatial_data/upstream_basins/batch/manifest.json',
    source_cache_folder=record$cache_folder)
  jsonlite::write_json(inputs,file.path(cache_dir,'olney_inputs.json'),pretty=TRUE,
    auto_unbox=TRUE,na='null',digits=NA)
  if(isTRUE(prepare_only)) return(invisible(inputs))
  edges<-sf::st_read('outputs/review_swifd_connections/North_Puget_Sound_Steelhead.gpkg',
    layer='stream_matching_edges',quiet=TRUE)
  total<-points[points$review_number %in% c(331L,335L,379L,380L),]
  delineations<-load_nldi_delineations(total,edges,cache_dir='spatial_data/upstream_basins/batch')
  anchors<-delineations$downstream_anchors[delineations$downstream_anchors$review_record_key==target$review_record_key,]
  stopifnot(nrow(anchors)==1L)
  edge<-edges[edges$graph_edge_id==6133L,]
  xy<-unclass(sf::st_geometry(edge)[[1L]])[,1:2,drop=FALSE]
  at<-function(point)stream_identity_project(xy,sf::st_coordinates(point)[1L,1:2])$measure
  target_at<-at(target)
  downsign<-sign(at(anchors)-target_at)
  stopifnot(downsign!=0)
  ordering<-data.frame(review_number=total$review_number,review_record_key=total$review_record_key,
    SiteId=total$SiteId,source_total=total$report_blockage,
    source_type=total$report_natural_type,source_verification=total$verification,
    physical_edge_id=total$graph_edge_id,physical_measure_m=vapply(seq_len(nrow(total)),function(i)at(total[i,]),numeric(1)),
    direction_evidence=anchors$direction_basis[1L],direction_source_url=anchors$direction_source_url[1L],
    source_report_url=total$report_url,stringsAsFactors=FALSE)
  ordering$upstream_distance_from331_m<-(target_at-ordering$physical_measure_m)*downsign
  stopifnot(all(ordering$upstream_distance_from331_m[ordering$review_number!=331]>0))
  recovery<-file.path(cache_dir,'olney_recovery')
  navigation<-sf::st_read(file.path(recovery,'upstream_flowlines.json'),quiet=TRUE)
  stopifnot(all(c('23963685','23964021') %in% as.character(navigation$nhdplus_comid)))
  named_json<-nps_read_cache_json(file.path(record$cache_folder,'named_flowline.json'),simplifyVector=FALSE)
  feature<-Filter(function(f)as.character(f$properties$comid)=='23963685',named_json$features)[[1L]]
  stopifnot(identical(tolower(feature$properties$flowdir),'with digitized'))
  trace<-function(path) {
    raw<-nps_read_cache_json(path,simplifyVector=FALSE)
    fields<-Filter(function(f)identical(f$id,'nhdFlowline'),raw$features)
    stopifnot(length(fields)==1L)
    fields[[1L]]$properties
  }
  physical_trace<-trace(file.path(recovery,'physical_flowtrace.json'))
  indexed_trace<-trace(file.path(recovery,'indexed_flowtrace.json'))
  stopifnot(as.character(physical_trace$comid)=='23963685',as.character(indexed_trace$comid)=='23963685',
    identical(physical_trace$gnis_name,'Olney Creek'),
    isTRUE(all.equal(unlist(physical_trace$intersection_point),unlist(indexed_trace$intersection_point))),
    physical_trace$raindrop_pathDist<=50,indexed_trace$raindrop_pathDist<=50)
  outlet<-sf::st_transform(sf::st_sfc(sf::st_point(as.numeric(unlist(physical_trace$intersection_point))),crs=4326),sf::st_crs(target))
  stopifnot(as.numeric(sf::st_distance(outlet,target))<=50)
  basin_file<-file.path(recovery,'flowtraced_split_basin.json')
  raw<-nps_read_cache_json(basin_file,simplifyVector=FALSE)
  complete<-nldi_validate_basin_completeness(raw,feature$properties,record,sf::st_crs(target))
  stopifnot(isTRUE(complete$valid),identical(complete$diagnostics$completeness_method,'full_drainageBasin'))
  basin<-sf::st_read(jsonlite::toJSON(list(type='FeatureCollection',features=complete$feature),auto_unbox=TRUE,digits=NA),quiet=TRUE)
  mask<-delineations$candidate_masks[delineations$candidate_masks$review_record_key==target$review_record_key,]
  stopifnot(nrow(mask)==1L)
  sf::st_geometry(mask)<-sf::st_union(sf::st_make_valid(sf::st_transform(sf::st_geometry(basin),sf::st_crs(target))))
  mask$source_url<-'https://api.water.usgs.gov/nldi/pygeoapi/processes/nldi-splitcatchment/execution?f=json'
  mask$source_md5<-unname(tools::md5sum(basin_file))
  mask$source_split_basin_cache_file<-basin_file
  mask$source_split_basin_md5<-mask$source_md5
  mask$source_flowtrace_cache_file<-file.path(recovery,'physical_flowtrace.json')
  mask$source_flowtrace_md5<-unname(tools::md5sum(mask$source_flowtrace_cache_file))
  mask$source_flowtrace_confirmation_file<-file.path(recovery,'indexed_flowtrace.json')
  mask$source_flowtrace_confirmation_md5<-unname(tools::md5sum(mask$source_flowtrace_confirmation_file))
  mask$source_navigation_cache_file<-file.path(recovery,'upstream_flowlines.json')
  mask$source_navigation_md5<-unname(tools::md5sum(mask$source_navigation_cache_file))
  mask$source_navigation_url<-'https://api.water.usgs.gov/nldi/linked-data/comid/23963685/navigation/UT/flowlines?f=json&distance=1000'
  for(field in names(complete$diagnostics)) mask[[field]]<-complete$diagnostics[[field]]
  network<-readRDS('spatial_data/north_puget_swifd_bridges.rds')$bridges$network
  target$explicit_total_passage<-target$confirmed_total_for_scenario<-TRUE
  target$phase3_selected<-target$phase1_selected<-TRUE
  cuts<-cut_reviewed_barriers(network,target,edges,anchors)
  checked<-validate_nldi_basin_masks(mask,cuts$network,target)
  downstream<-cuts$network[upstream_has_key(cuts$network$downstream_of_barrier,'6:43814'),]
  overlap<-upstream_line_parts(suppressWarnings(sf::st_intersection(downstream,sf::st_geometry(mask))))
  overlap$length_m<-as.numeric(sf::st_length(overlap))
  overlap$distance_from331_m<-as.numeric(sf::st_distance(sf::st_centroid(sf::st_geometry(overlap)),target))
  utils::write.csv(ordering,file.path(cache_dir,'olney_total_barrier_ordering.csv'),row.names=FALSE)
  print(ordering[,c('review_number','physical_measure_m','upstream_distance_from331_m')])
  print(checked$audit[,c('review_number','approval_status','approval_reason','proven_downstream_inside_mask_m','max_local_disagreement_distance_m')])
  print(sf::st_drop_geometry(overlap)[,intersect(c('graph_edge_id','source_class','measure_habitat','length_m','distance_from331_m'),names(overlap))])
  saveRDS(list(barriers=target,masks=mask,anchors=anchors,audit=checked$audit,
    ordering=ordering,network=cuts$network,downstream_overlap=overlap,graph_audit=cuts$barrier_audit),
    file.path(cache_dir,'olney_diagnostics.rds'))
  applied<-apply_phase3_olney_mask(cuts$network,mask,target,edges)
  mask379<-delineations$candidate_masks[delineations$candidate_masks$review_number %in% 379L,]
  redundant_area<-sum(as.numeric(sf::st_area(suppressWarnings(sf::st_difference(
    sf::st_geometry(sf::st_make_valid(mask379)),sf::st_geometry(applied$mask))))))
  audit<-applied$barrier_audit
  audit$SiteId<-target$SiteId
  audit$StreamName<-target$StreamName
  audit$report_blockage<-target$report_blockage
  audit$report_natural_type<-target$report_natural_type
  audit$verification<-target$verification
  audit$report_url<-target$report_url
  audit$original_omission_reason<-'Complete source basin held because 4.341m of known downstream model overlap included a remote boundary sliver ~798m below331'
  audit$upper379_basin_outside331_m2<-redundant_area
  target$phase3_selection_basis<-'Lowest source-confirmed Total on the exact Olney channel; unfield-verified Total included as requested'
  target$phase3_review_action<-'Apply unchanged full upstream basin; preserve only per-piece independently proven same-channel downstream overlap'
  target$phase3_selected<-TRUE
  notes<-c(
    'Olney331 / WDFW891117 is the lowest confirmed Total: Waterfall, not field verified. Partial sites371/374 remain excluded.',
    'Published downstream orientation plus exact unbranched physical-channel measures places335 1413m upstream,380 14249m upstream,and379 17917m upstream of331.',
    'The prior hold was a conservative remote-downstream overlap gate, not missing Total classification or an incomplete watershed.',
    'Physical and indexed official elevation flowtraces independently converge to the same Olney COMID23963685 outlet within6.78m; full source basin remains unchanged.',
    'Every protected inside-mask piece must independently pass exact physical-edge membership, published downstream route order, and seven local reference-course samples. No remote geographic buffer is used.',
    'All other inside-mask model, SWIFD and alignment geometry, including disconnected reaches, is removed; original source lengths are conserved.',
    paste0('Upper379 basin area outside331 basin: ',format(redundant_area,digits=10),'m2. Retain379 as source evidence;331 is the effective lower cutoff.'))
  result<-list(barriers=target,masks=applied$mask,anchors=anchors,audit=audit,
    proof_audit=applied$proof_audit,ordering=ordering,notes=notes,
    previous_generic_validation=checked$audit,graph_audit=cuts$barrier_audit)
  saveRDS(result,file.path(cache_dir,'olney_barriers.rds'))
  utils::write.csv(audit,file.path(cache_dir,'olney_barrier_audit.csv'),row.names=FALSE)
  utils::write.csv(applied$proof_audit,file.path(cache_dir,'olney_downstream_proof_audit.csv'),row.names=FALSE)
  writeLines(notes,file.path(cache_dir,'olney_notes.txt'))
  print(applied$proof_audit[,c('proof_piece_id','protected_length_m','max_distance_from_barrier_m',
    'protection_scope','reference_max_distance_m','reference_max_heading_degrees','route_support_coverage')])
  print(audit[,c('review_number','raw_inside_m','protected_local_downstream_m','protected_remote_downstream_m',
    'remaining_inside_m','unexplained_upstream_remaining_m','source_partition_error_m','upper379_basin_outside331_m2')])
  invisible(result)
}

load_phase3_olney_barriers <- function(cache_dir='spatial_data/phase3') {
  path<-file.path(cache_dir,'olney_barriers.rds')
  if(!file.exists(path)) stop('Build the reviewed Olney bundle first: ',path)
  nps_read_cache_rds(path)
}

# This exception protects no geographic buffer. Every protected piece must be
# on the exact independently named Olney channel and project strictly below331
# in the published WithDigitized reference. The full source basin is unchanged.
apply_phase3_olney_mask <- function(network,mask,barrier_points,stream_edges) {
  key<-'6:43814'
  stopifnot(inherits(network,'sf'),inherits(mask,'sf'),nrow(mask)==1L,
    identical(as.character(mask$review_record_key),key))
  point<-barrier_points[barrier_points$review_record_key %in% key,]
  stopifnot(nrow(point)==1L,point$review_number==331L,as.character(point$SiteId)=='891117',
    identical(as.character(point$report_blockage),'Total'),
    identical(as.character(point$report_natural_type),'Waterfall'),
    point$snap_distance_m<=50,point$graph_edge_id==6133L,
    as.character(point$matched_llid)=='1217124478723',
    !isTRUE(point$junction_ambiguous),point$matching_channel_count==1L,
    all(vapply(c('snap_accepted','stream_identity_confirmed','identity_valid_for_cut'),
      function(n)isTRUE(point[[n]][1L]),logical(1))))
  crs<-sf::st_crs(network)
  stopifnot(!sf::st_is_longlat(network),crs$units_gdal %in% c('metre','meter','m'))
  point<-sf::st_transform(point,crs)
  edge<-sf::st_transform(stream_edges[stream_edges$graph_edge_id==6133L,],crs)
  stopifnot(nrow(edge)==1L)
  xy<-unclass(sf::st_geometry(edge)[[1L]])[,1:2,drop=FALSE]
  edge_length<-sum(sqrt(rowSums(diff(xy)^2)))
  center<-sf::st_coordinates(point)[1L,1:2]
  barrier_at<-stream_identity_project(xy,center)
  stopifnot(barrier_at$distance<=1e-5)
  records<-nps_read_cache_json('spatial_data/upstream_basins/batch/manifest.json',simplifyVector=FALSE)
  record<-Filter(function(x)x$review_record_key==key,records)[[1L]]
  named_file<-file.path(record$cache_folder,'named_flowline.json')
  raw_route<-nps_read_cache_json(named_file,simplifyVector=FALSE)
  features<-Filter(function(f)as.character(f$properties$comid)=='23963685',raw_route$features)
  stopifnot(length(features)==1L,isTRUE(record$approved),record$route_support_coverage>=.8,
    record$indexed_shift_m<=50,identical(features[[1L]]$properties$gnis_name,'Olney Creek'),
    identical(tolower(features[[1L]]$properties$flowdir),'with digitized'))
  route<-sf::st_read(jsonlite::toJSON(list(type='FeatureCollection',features=features),auto_unbox=TRUE,digits=NA),quiet=TRUE)
  route<-suppressWarnings(sf::st_cast(sf::st_transform(route,crs),'LINESTRING'))
  stopifnot(nrow(route)==1L)
  rxy<-unclass(sf::st_geometry(route)[[1L]])[,1:2,drop=FALSE]
  route_length<-sum(sqrt(rowSums(diff(rxy)^2)))
  reference_barrier_at<-stream_identity_project(rxy,center)$measure
  # Independently revalidate current physical/course/source evidence; cached
  # eligibility or protection tags are never sufficient to approve a piece.
  local<-load_nldi_delineations(point,stream_edges,cache_dir='spatial_data/upstream_basins/batch')
  anchor<-local$downstream_anchors[local$downstream_anchors$review_record_key %in% key,]
  stopifnot(nrow(anchor)==1L,nrow(local$candidate_masks)==1L)
  direction<-sign(stream_identity_project(xy,sf::st_coordinates(anchor)[1L,1:2])$measure-barrier_at$measure)
  stopifnot(direction!=0)
  recovery<-'spatial_data/phase3/olney_recovery'
  basin_file<-nps_resolve_cache_path(mask$source_split_basin_cache_file,must_work=TRUE)
  stopifnot(identical(unname(tools::md5sum(basin_file)),as.character(mask$source_split_basin_md5)))
  raw_basin<-nps_read_cache_json(basin_file,simplifyVector=FALSE)
  complete<-nldi_validate_basin_completeness(raw_basin,features[[1L]]$properties,record,crs)
  stopifnot(isTRUE(complete$valid),identical(complete$diagnostics$completeness_method,'full_drainageBasin'))
  polygon_only<-function(geometry) {
    geometry<-sf::st_make_valid(geometry)
    if(any(sf::st_geometry_type(geometry)=='GEOMETRYCOLLECTION'))
      geometry<-suppressWarnings(sf::st_collection_extract(geometry,'POLYGON'))
    geometry<-sf::st_union(geometry)
    stopifnot(length(geometry)==1L,!sf::st_is_empty(geometry),
      sf::st_geometry_type(geometry) %in% c('POLYGON','MULTIPOLYGON'),as.numeric(sf::st_area(geometry))>0)
    geometry
  }
  source_basin<-sf::st_read(jsonlite::toJSON(list(type='FeatureCollection',features=complete$feature),auto_unbox=TRUE,digits=NA),quiet=TRUE)
  source_geometry<-polygon_only(sf::st_transform(sf::st_geometry(source_basin),crs))
  sf::st_geometry(mask)<-polygon_only(sf::st_transform(sf::st_geometry(mask),crs))
  difference<-sum(as.numeric(sf::st_area(suppressWarnings(sf::st_sym_difference(source_geometry,sf::st_geometry(mask))))))
  stopifnot(difference<=1e-4)
  # Every overlay below must use the exact geometry returned for export. Two
  # separately reconstructed rings can have zero area difference yet differ
  # in a sub-meter boundary intersection at floating-point precision.
  source_geometry<-sf::st_geometry(mask)
  # Two independent official elevation traces agree on the same named outlet.
  traces<-lapply(c('physical_flowtrace.json','indexed_flowtrace.json'),function(name) {
    raw<-nps_read_cache_json(file.path(recovery,name),simplifyVector=FALSE)
    f<-Filter(function(f)identical(f$id,'nhdFlowline'),raw$features)
    stopifnot(length(f)==1L,as.character(f[[1L]]$properties$comid)=='23963685',
      identical(f[[1L]]$properties$gnis_name,'Olney Creek'),f[[1L]]$properties$raindrop_pathDist<=50)
    f[[1L]]$properties
  })
  stopifnot(isTRUE(all.equal(unlist(traces[[1L]]$intersection_point),unlist(traces[[2L]]$intersection_point))))
  outlet<-sf::st_transform(sf::st_sfc(sf::st_point(as.numeric(unlist(traces[[1L]]$intersection_point))),crs=4326),crs)
  stopifnot(as.numeric(sf::st_distance(outlet,point))<=50)
  navigation<-sf::st_read(file.path(recovery,'upstream_flowlines.json'),quiet=TRUE)
  stopifnot(all(c('23963685','23964021') %in% as.character(navigation$nhdplus_comid)))
  # Fresh graph proof from the complete immutable raw+SWIFD network checks that
  # no known upstream habitat is missing from the supplied complete basin.
  baseline<-readRDS('spatial_data/north_puget_swifd_bridges.rds')$bridges$network
  proof_point<-point;proof_point$explicit_total_passage<-TRUE
  fresh<-cut_reviewed_barriers(baseline,proof_point,stream_edges,anchor)
  upstream<-fresh$network[upstream_has_key(fresh$network$removed_by_barrier,key),]
  missing<-upstream_line_parts(suppressWarnings(sf::st_difference(upstream,source_geometry)))
  upstream_missing_m<-sum(as.numeric(sf::st_length(missing)))
  stopifnot(upstream_missing_m<=1e-5)
  fine_reference<-load_stream_identity_reference()
  fine_index<-build_stream_identity_index(baseline,fine_reference)
  own_edge<-fine_index$edges[fine_index$edges$graph_edge_id==6133L,]
  stopifnot(nrow(own_edge)==1L,
    identical(sf::st_as_binary(sf::st_geometry(own_edge)),sf::st_as_binary(sf::st_geometry(edge))))

  prove<-function(piece,id) {
    vertices<-sf::st_coordinates(piece)[,1:2,drop=FALSE]
    physical<-lapply(seq_len(nrow(vertices)),function(i)stream_identity_project(xy,vertices[i,]))
    offset<-vapply(physical,`[[`,numeric(1),'distance')
    measures<-vapply(physical,`[[`,numeric(1),'measure')
    if(!isTRUE(piece$measure_habitat[1L]) || is.na(piece$graph_edge_id[1L]) ||
        piece$graph_edge_id[1L]!=6133L || any(offset>1e-5) ||
        !all((measures-barrier_at$measure)*direction>= -1e-5)) return(NULL)
    samples_at<-seq(min(measures),max(measures),length.out=7L)
    sample_xy<-t(vapply(samples_at,function(m)stream_identity_at(xy,m),numeric(2)))
    project<-lapply(seq_len(7L),function(i)stream_identity_project(rxy,sample_xy[i,]))
    distance<-vapply(project,`[[`,numeric(1),'distance')
    reference_at<-vapply(project,`[[`,numeric(1),'measure')
    angle<-vapply(seq_len(7L),function(i) {
      own<-stream_identity_at(xy,min(edge_length,samples_at[i]+25))-stream_identity_at(xy,max(0,samples_at[i]-25))
      ref<-stream_identity_at(rxy,min(route_length,reference_at[i]+25))-stream_identity_at(rxy,max(0,reference_at[i]-25))
      norm<-sqrt(sum(own^2)*sum(ref^2))
      if(norm<=1e-12) return(90)
      acos(min(1,max(0,abs(sum(own*ref))/norm)))*180/pi
    },numeric(1))
    coarse_coverage<-mean(distance<=25 & angle<=35)
    if(any(reference_at<reference_barrier_at-1e-5)) return(NULL)
    # Fine named hydrography establishes local channel identity using the
    # existing unchanged primary matcher. Coarse NHDPlus supplies published
    # downstream order; its displacement is recorded, never used to move a
    # barrier or relax a generic course/index tolerance.
    fine_points<-sf::st_sf(graph_edge_id=rep(6133L,7L),geometry=sf::st_sfc(
      lapply(seq_len(7L),function(i)sf::st_point(sample_xy[i,])),crs=crs))
    fine<-assess_stream_edge_identity(fine_index,fine_points,barrier_stream='Olney Creek',return_samples=TRUE)
    if(!all(fine$stream_identity_valid %in% TRUE) || any(fine$junction_ambiguous %in% TRUE) ||
        !all(as.character(fine$matched_llid)=='1217124478723') ||
        !all(vapply(fine$matched_stream_name,function(name)stream_identity_names_match(name,'Olney Creek'),logical(1)))) return(NULL)
    fine_samples<-attr(fine,'sample_points')
    fine_max_angle<-if(!is.null(fine_samples))max(fine_samples$heading_degrees) else max(fine$median_heading_degrees)
    coverage<-min(fine$support_coverage)
    max_distance<-max(sqrt(rowSums(sweep(vertices,2,center)^2)))
    data.frame(review_record_key=key,review_number=331L,proof_piece_id=id,
      graph_edge_id=6133L,source_class=as.character(piece$source_class[1L]),
      protected_length_m=as.numeric(sf::st_length(piece)),max_distance_from_barrier_m=max_distance,
      protection_scope=if(max_distance>100)'remote_explicit_same_channel_downstream' else 'bounded_local_downstream',
      physical_min_upstream_distance_m=min(-(measures-barrier_at$measure)*direction),
      physical_max_projection_offset_m=max(offset),reference_min_downstream_measure_m=min(reference_at-reference_barrier_at),
      reference_max_distance_m=max(fine$max_reference_distance_m),reference_max_heading_degrees=fine_max_angle,route_support_coverage=coverage,
      coarse_reference_max_distance_m=max(distance),coarse_reference_max_heading_degrees=max(angle),
      coarse_route_support_coverage=coarse_coverage,
      sample_count=7L,heading_halfspan_m=25,primary_heading_halfspan_m=5,primary_local_sample_count=7L,
      primary_evaluation_points=7L,primary_local_support_length_m=min(fine$support_length_m),
      primary_course_source_url='https://geodataservices.wdfw.wa.gov/arcgis/rest/services/FP_Projects/NHDwithLLID/MapServer/4',
      published_flowdir='With Digitized',reference_comid='23963685',
      reference_stream_name='Olney Creek',matched_llid='1217124478723',source_basin_md5=as.character(mask$source_split_basin_md5),
      source_geometry_symmetric_difference_m2=difference,graph_proven_upstream_outside_source_m=upstream_missing_m,
      reference_source_cache_file=named_file,
      reference_source_url='https://api.water.usgs.gov/fabric/pygeoapi/collections/nhdflowline_network/items?comid=23963685&f=json',
      proof_method='exact_physical_channel_plus_seven_fine_named_channel_evaluations_and_published_coarse_downstream_order',
      stringsAsFactors=FALSE)
  }
  lines<-network
  for(field in c('removed_by_barrier','downstream_of_barrier','watershed_removed_by_barrier',
      'watershed_downstream_protected_by_barrier','phase3_remote_downstream_protected_by_barrier'))
    if(!field %in% names(lines)) lines[[field]]<-''
  if(!'removed' %in% names(lines)) lines$removed<-FALSE
  initial<-tapply(as.numeric(sf::st_length(lines)),lines$source_class,sum)
  # Partition the complete current network so every source class participates
  # in the same geometry overlay and length-conservation check.
  nearby<-seq_len(nrow(lines))
  stopifnot(length(nearby)>0L)
  selected<-lines[nearby,]
  inside<-upstream_line_parts(suppressWarnings(sf::st_intersection(selected,source_geometry)))
  stopifnot(nrow(inside)>0L)
  outside<-upstream_line_parts(suppressWarnings(sf::st_difference(selected,source_geometry)))
  proofs<-lapply(seq_len(nrow(inside)),function(i)prove(inside[i,],i))
  protect<-!vapply(proofs,is.null,logical(1))
  # A piece independently classified below331 but failing the strict current
  # reference proof must be held, never silently removed by the basin mask.
  known_downstream<-fresh$network[upstream_has_key(fresh$network$downstream_of_barrier,key),]
  if(any(!protect & upstream_has_key(inside$downstream_of_barrier,key)))
    stop('A graph-proven Olney downstream piece lacks the required current per-piece reference proof')
  if(any(!protect)) {
    conflict<-upstream_line_parts(suppressWarnings(sf::st_intersection(inside[!protect,],sf::st_union(sf::st_geometry(known_downstream)))))
    if(sum(as.numeric(sf::st_length(conflict)))>1e-5) stop('Olney downstream overlap lacks per-piece independent channel proof')
  }
  proof_audit<-if(any(protect))do.call(rbind,proofs[protect]) else data.frame()
  length_inside<-as.numeric(sf::st_length(inside))
  previously_removed<-inside$removed
  inside$downstream_of_barrier[protect]<-upstream_append_key(inside$downstream_of_barrier[protect],key)
  inside$watershed_downstream_protected_by_barrier[protect]<-upstream_append_key(inside$watershed_downstream_protected_by_barrier[protect],key)
  remote<-rep(FALSE,nrow(inside))
  if(any(protect)) remote[which(protect)]<-proof_audit$protection_scope=='remote_explicit_same_channel_downstream'
  inside$phase3_remote_downstream_protected_by_barrier[remote]<-upstream_append_key(inside$phase3_remote_downstream_protected_by_barrier[remote],key)
  inside$removed[!protect]<-TRUE
  inside$removed_by_barrier[!protect]<-upstream_append_key(inside$removed_by_barrier[!protect],key)
  inside$watershed_removed_by_barrier[!protect]<-upstream_append_key(inside$watershed_removed_by_barrier[!protect],key)
  inside$cut_status[!protect]<-'removed_upstream_basin'
  lines<-rbind(lines[-nearby,,drop=FALSE],outside,inside)
  lines$piece_id<-seq_len(nrow(lines))
  lines$length_m<-as.numeric(sf::st_length(lines))
  lines$retained<-!lines$removed
  lines$unresolved_direction<-lines$unresolved_direction & lines$retained
  final<-tapply(lines$length_m,lines$source_class,sum)
  conservation_error<-max(abs(initial-final[names(initial)]))
  stopifnot(is.finite(conservation_error),conservation_error<=max(1e-5,sum(initial)*1e-10))
  retained<-lines[lines$retained & lines$measure_habitat,]
  residual<-upstream_line_parts(suppressWarnings(sf::st_intersection(retained,source_geometry)))
  residual_proofs<-lapply(seq_len(nrow(residual)),function(i)prove(residual[i,],i))
  stopifnot(!any(vapply(residual_proofs,is.null,logical(1))))
  for(field in c('watershed_validated','stream_identity_validated','total_barrier_approved','coverage_complete','local_outlet_precision_validated')) mask[[field]]<-TRUE
  mask$phase3_explicit_same_channel_downstream_proof<-TRUE
  mask$phase3_remote_downstream_proof_method<-'unchanged_full_basin_with_published_same_channel_order_proof_per_protected_piece'
  mask$barrier_x<-center[1L];mask$barrier_y<-center[2L]
  audit<-sf::st_drop_geometry(mask)
  audit$watershed_applied<-TRUE
  audit$approval_status<-'applied_full_basin_with_explicit_same_channel_downstream_proof'
  audit$raw_inside_m<-sum(length_inside[inside$measure_habitat])
  audit$inside_physical_m<-sum(length_inside[inside$measure_habitat & !protect])
  audit$newly_removed_physical_m<-sum(length_inside[inside$measure_habitat & !protect & !previously_removed])
  audit$already_removed_physical_m<-sum(length_inside[inside$measure_habitat & !protect & previously_removed])
  audit$protected_downstream_physical_m<-sum(length_inside[inside$measure_habitat & protect])
  audit$protected_local_downstream_m<-sum(length_inside[inside$measure_habitat & protect & !remote])
  audit$protected_remote_downstream_m<-sum(length_inside[inside$measure_habitat & remote])
  audit$max_protection_distance_m<-if(nrow(proof_audit))max(proof_audit$max_distance_from_barrier_m) else 0
  audit$remaining_inside_m<-sum(as.numeric(sf::st_length(residual)))
  audit$unexplained_upstream_remaining_m<-0
  audit$source_partition_error_m<-conservation_error
  audit$source_geometry_symmetric_difference_m2<-difference
  audit$graph_proven_upstream_outside_source_m<-upstream_missing_m
  list(network=lines,barrier_audit=audit,mask=mask,proof_audit=proof_audit)
}
