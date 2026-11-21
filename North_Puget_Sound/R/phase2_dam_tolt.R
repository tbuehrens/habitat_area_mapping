# Explicit Phase 2 dam/Tolt corrections. Earlier output folders stay read only.
if (!exists("nps_read_cache_json", mode = "function")) source(file.path("R", "project_paths.R"))
phase2_tolt_remove_review_numbers <- function() c(199L,205L,209L,215L,219L,220L)

# A source-mapped dam can terminate the modeled channel while disconnected
# reservoir/headwater linework still lies above it. There is then no immediate
# upstream model edge to invent for the ordinary two-sided graph validator.
validate_phase2_terminal_dam <- function(candidate_masks,graph_network,barrier_points,stream_edges,
    cache_dir='spatial_data/phase2') {
  key<-'user_override:Culmback_Dam'
  candidates<-candidate_masks[candidate_masks$review_record_key==key,]
  audit<-data.frame(review_record_key=key,review_number=912L,mask_approved=FALSE,
    approval_status='pending_terminal_dam_validation',approval_reason='',
    method='explicit_Culmback_terminal_dam_full_basin_plus_published_downstream_direction',
    terminal_distance_m=NA_real_,source_snap_distance_m=NA_real_,
    downstream_support_m=NA_real_,published_direction_monotonic=FALSE,
    full_basin_area_sqmi=NA_real_,official_area_sqmi=68.3,
    basin_source_cache_file=NA_character_,basin_source_md5=NA_character_,
    named_reference_cache_file=NA_character_,source_geometry_symmetric_difference_m2=NA_real_,
    basin_polygon_extracted_from_collection=FALSE,candidate_polygon_area_change_m2=NA_real_,
    downstream_inside_mask_m=NA_real_,max_downstream_overlap_distance_m=NA_real_,
    official_area_source_url='https://waterdata.usgs.gov/nwis/uv?legacy=1&site_no=12137300',
    stringsAsFactors=FALSE)
  accepted<-tryCatch({
    stopifnot(nrow(candidates)==1L)
    polygon_only<-function(geometry) {
      geometry<-sf::st_make_valid(geometry)
      if(any(sf::st_geometry_type(geometry)=='GEOMETRYCOLLECTION'))
        geometry<-suppressWarnings(sf::st_collection_extract(geometry,'POLYGON'))
      geometry<-sf::st_union(geometry)
      if(length(geometry)!=1L || sf::st_is_empty(geometry) ||
          !sf::st_geometry_type(geometry) %in% c('POLYGON','MULTIPOLYGON') ||
          as.numeric(sf::st_area(geometry))<=0) stop('terminal_basin_has_no_positive_polygon')
      geometry
    }
    original_area<-sum(as.numeric(sf::st_area(candidates)))
    audit$basin_polygon_extracted_from_collection<-any(sf::st_geometry_type(
      sf::st_make_valid(sf::st_geometry(candidates)))=='GEOMETRYCOLLECTION')
    sf::st_geometry(candidates)<-polygon_only(sf::st_geometry(candidates))
    audit$candidate_polygon_area_change_m2<-sum(as.numeric(sf::st_area(candidates)))-original_area
    if(abs(audit$candidate_polygon_area_change_m2)>1e-4) stop('terminal_polygon_extraction_changed_positive_area')
    point<-barrier_points[barrier_points$review_record_key==key,]
    stopifnot(nrow(point)==1L,point$review_number==912L,
      all(vapply(c('snap_accepted','stream_identity_confirmed','identity_valid_for_cut','override_future_cut'),
        function(n)isTRUE(point[[n]][1L]),logical(1))),!isTRUE(point$junction_ambiguous),
      point$matching_channel_count==1L,point$snap_distance_m<=50,
      as.character(point$matched_llid)=='1218192478598',
      stream_identity_names_match(point$StreamName,'Sultan River'))
    audit$source_snap_distance_m<-point$snap_distance_m
    edge<-stream_edges[stream_edges$graph_edge_id==point$graph_edge_id,]
    stopifnot(nrow(edge)==1L,point$graph_edge_id==candidates$graph_edge_id)
    xy<-unclass(sf::st_geometry(edge)[[1]])[,1:2,drop=FALSE]
    length_m<-sum(sqrt(rowSums(diff(xy)^2)))
    if(!is.finite(length_m) || length_m<50) stop('selected_terminal_edge_has_less_than50m_support')
    center<-sf::st_coordinates(point)[1L,1:2]
    endpoint_distance<-sqrt(rowSums(sweep(rbind(xy[1L,],xy[nrow(xy),]),2,center)^2))
    terminal<-which.min(endpoint_distance)
    audit$terminal_distance_m<-endpoint_distance[terminal]
    degree<-c(edge$from_degree,edge$to_degree)[terminal]
    if(endpoint_distance[terminal]>1e-5 || degree!=1L) stop('selected_point_not_unique_degree1_physical_terminal')
    records<-nps_read_cache_json(file.path(cache_dir,'dam_tolt_basins','manifest.json'),simplifyVector=FALSE)
    record<-Filter(function(x)identical(x$review_record_key,key),records)
    stopifnot(length(record)==1L)
    record<-record[[1L]]
    audit$basin_source_cache_file<-record$basin_path
    audit$basin_source_md5<-record$basin_md5
    audit$named_reference_cache_file<-file.path(record$cache_folder,'named_flowline.json')
    stopifnot(isTRUE(record$approved),as.character(record$comid)=='23963709',
      record$route_support_coverage>=.8,record$indexed_shift_m<=50,
      identical(tolower(unname(tools::md5sum(record$basin_path))),tolower(record$basin_md5)))
    raw_route<-nps_read_cache_json(file.path(record$cache_folder,'named_flowline.json'),simplifyVector=FALSE)
    features<-Filter(function(x)as.character(x$properties$comid)==as.character(record$comid),raw_route$features)
    stopifnot(length(features)==1L)
    feature<-features[[1L]]
    stopifnot(identical(tolower(feature$properties$flowdir),'with digitized'),
      stream_identity_names_match(feature$properties$gnis_name,'Sultan River'))
    route<-sf::st_read(jsonlite::toJSON(list(type='FeatureCollection',features=features),auto_unbox=TRUE,digits=NA),quiet=TRUE)
    route<-suppressWarnings(sf::st_cast(sf::st_transform(route,sf::st_crs(edge)),'LINESTRING'))
    stopifnot(nrow(route)==1L)
    rxy<-unclass(sf::st_geometry(route)[[1]])[,1:2,drop=FALSE]
    positions<-if(terminal==1L) seq(0,50,length.out=7L) else seq(length_m,length_m-50,length.out=7L)
    samples<-t(vapply(positions,function(m)stream_identity_at(xy,m),numeric(2)))
    projections<-lapply(seq_len(nrow(samples)),function(i)stream_identity_project(rxy,samples[i,]))
    distances<-vapply(projections,`[[`,numeric(1),'distance')
    measures<-vapply(projections,`[[`,numeric(1),'measure')
    if(any(distances>25) || !all(diff(measures)>.25)) stop('published_reference_does_not_prove_terminal_to_interior_downstream')
    ref_length<-sum(sqrt(rowSums(diff(rxy)^2)))
    angles<-vapply(seq_len(nrow(samples)),function(i) {
      own<-stream_identity_at(xy,min(length_m,positions[i]+25))-stream_identity_at(xy,max(0,positions[i]-25))
      reference<-stream_identity_at(rxy,min(ref_length,measures[i]+25))-stream_identity_at(rxy,max(0,measures[i]-25))
      norm<-sqrt(sum(own^2)*sum(reference^2))
      if(norm<=1e-12) return(90)
      acos(min(1,max(0,abs(sum(own*reference))/norm)))*180/pi
    },numeric(1))
    if(mean(distances<=25 & angles<=35)<.8) stop('terminal_course_heading_support_unconfirmed')
    audit$published_direction_monotonic<-TRUE
    audit$downstream_support_m<-50
    source_basin<-nps_read_cache_json(record$basin_path,simplifyVector=FALSE)
    completeness<-nldi_validate_basin_completeness(source_basin,feature$properties,record,sf::st_crs(edge))
    stopifnot(isTRUE(completeness$valid),
      identical(completeness$diagnostics$completeness_method,'full_drainageBasin'))
    full_source<-sf::st_read(jsonlite::toJSON(list(type='FeatureCollection',features=completeness$feature),
      auto_unbox=TRUE,digits=NA),quiet=TRUE)
    full_source<-sf::st_make_valid(sf::st_transform(full_source,sf::st_crs(candidates)))
    full_source_geometry<-polygon_only(sf::st_geometry(full_source))
    symmetric<-suppressWarnings(sf::st_sym_difference(sf::st_geometry(candidates),full_source_geometry))
    audit$source_geometry_symmetric_difference_m2<-sum(as.numeric(sf::st_area(symmetric)))
    if(audit$source_geometry_symmetric_difference_m2>1e-4) stop('candidate_geometry_differs_from_full_cached_source_basin')
    area<-as.numeric(sf::st_area(candidates))/2589988.110336
    audit$full_basin_area_sqmi<-area
    if(abs(area/68.3-1)>.05) stop('full_basin_conflicts_with_independent_Culmback68.3mi2')
    station<-sf::st_read(file.path(cache_dir,'spada_usgs_site.json'),quiet=TRUE)
    stopifnot(nrow(station)==1L,station$identifier=='USGS-12137300',as.character(station$comid)=='23963709')
    station<-sf::st_transform(station,sf::st_crs(point))
    stopifnot(as.numeric(sf::st_distance(station,point))<=50)
    # Every exact physical piece of this unbranched selected edge lies toward
    # its interior from the degree1 dam terminal. Other channels get no tag.
    stopifnot(all(c('measure_habitat','graph_edge_id','downstream_of_barrier') %in% names(graph_network)))
    on_edge<-graph_network$measure_habitat %in% TRUE & !is.na(graph_network$graph_edge_id) &
      graph_network$graph_edge_id==point$graph_edge_id &
      lengths(sf::st_covered_by(graph_network,sf::st_buffer(sf::st_geometry(edge),1e-5)))>0L
    if(!any(on_edge)) stop('no_exact_physical_downstream_edge_support_in_current_graph')
    overlap<-upstream_line_parts(suppressWarnings(sf::st_intersection(graph_network[on_edge,],sf::st_geometry(candidates))))
    local<-upstream_local_geometry_check(overlap,point,100)
    audit$downstream_inside_mask_m<-local$length_m
    audit$max_downstream_overlap_distance_m<-local$max_distance_m
    if(!isTRUE(local$valid)) stop('full_basin_overlaps_proven_downstream_beyond100m')
    graph_network$downstream_of_barrier[on_edge]<-upstream_append_key(graph_network$downstream_of_barrier[on_edge],key)
    candidates$local_outlet_precision_validated<-TRUE
    candidates$protect_local_graph_downstream<-TRUE
    candidates$local_correction_limit_m<-100
    candidates$barrier_x<-center[1L];candidates$barrier_y<-center[2L]
    candidates$terminal_dam_validated<-TRUE
    candidates$terminal_dam_validation_method<-audit$method
    audit$mask_approved<-TRUE
    audit$approval_status<-'approved_explicit_terminal_dam_full_basin_with_proven_downstream_support'
    candidates
  },error=function(e) {
    audit$approval_reason<<-conditionMessage(e)
    candidates[FALSE,]
  })
  list(masks=accepted,audit=audit,network=graph_network)
}

load_phase2_dam_tolt <- function(cache_dir='spatial_data/phase2') {
  path<-file.path(cache_dir,'dam_tolt.rds')
  if(!file.exists(path)) stop('Build the reviewed dam/Tolt cache first: ',path)
  result<-nps_read_cache_rds(path)
  if(!all(c('barriers','masks','anchors','audit','remove_review_numbers','notes') %in% names(result)))
    stop('Dam/Tolt bundle is incomplete')
  if(!identical(sort(as.integer(result$barriers$review_number)),c(912L,913L)))
    stop('Expected distinct appended Culmback912 and SFTFalls913 records')
  if(!all(result$barriers$snap_accepted %in% TRUE) || any(result$barriers$snap_distance_m>50))
    stop('Dam/Tolt points must retain strict primary-channel matching within50m')
  if(anyDuplicated(result$barriers$review_record_key)) stop('Duplicate dam/Tolt cut keys')
  result
}
