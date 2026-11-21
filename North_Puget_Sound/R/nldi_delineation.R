# Prepare/review cached USGS split-basin requests. Reference watercourses are
# used only for classification; no reference geometry becomes habitat mileage.
if (!exists("nps_read_cache_json", mode = "function")) source(file.path("R", "project_paths.R"))
nldi_course_controls <- function() {
  list(heading_halfspan_m=25,sample_count=7L,local_half_length_m=50,
    max_match_distance_m=25,max_heading_degrees=35,min_coverage=.8,
    method_note=paste("Symmetric 50m heading chords within the 100m local course match the",
      "generalized 1:100,000 NHDPlusV2 reference; primary finer-scale WDFW identity is unchanged."))
}

nldi_basin_completeness_defaults <- function() {
  list(completeness_method=NA_character_,headwater_startflag=NA_real_,
    headwater_streamorde=NA_real_,headwater_uphydroseq=NA_real_,
    headwater_areasqkm=NA_real_,headwater_totdasqkm=NA_real_,
    headwater_upstream_reach_count=NA_integer_,headwater_navigation_source_url=NA_character_,
    headwater_navigation_cache_file=NA_character_,headwater_navigation_md5=NA_character_,
    full_basin_area_km2=NA_real_,published_local_area_km2=NA_real_,
    published_total_area_km2=NA_real_,published_upstream_only_area_km2=NA_real_,
    actual_to_published_total_area_ratio=NA_real_,actual_to_published_upstream_only_ratio=NA_real_,
    published_area_check="not_evaluated",area_relative_tolerance=.05,area_absolute_tolerance_km2=.01,
    completeness_documentation_url="https://api.water.usgs.gov/docs/nldi-pygeoapi/splitcatchment/",
    area_documentation_url="https://www.epa.gov/system/files/documents/2023-04/NHDPlusV2_User_Guide.pdf")
}

nldi_validate_basin_completeness <- function(raw,properties,record,crs,
    headwater_cache_dir="spatial_data/upstream_basins/headwater_navigation") {
  diagnostic<-nldi_basin_completeness_defaults()
  number<-function(name) {
    value<-suppressWarnings(as.numeric(properties[[name]]))
    if(length(value)==1L && is.finite(value)) value else NA_real_
  }
  diagnostic$headwater_startflag<-number("startflag")
  diagnostic$headwater_streamorde<-number("streamorde")
  diagnostic$headwater_uphydroseq<-number("uphydroseq")
  diagnostic$headwater_areasqkm<-number("areasqkm")
  diagnostic$headwater_totdasqkm<-number("totdasqkm")
  diagnostic$published_local_area_km2<-number("areasqkm")
  diagnostic$published_total_area_km2<-number("totdasqkm")
  fail<-function(reason) list(valid=FALSE,reason=reason,feature=list(),diagnostics=diagnostic)
  if(!identical(raw$type,"FeatureCollection")) return(fail("basin_response_not_feature_collection"))
  polygon<-Filter(function(x) identical(x$id,"drainageBasin"),raw$features)
  if(length(polygon)==1L) {
    diagnostic$completeness_method<-"full_drainageBasin"
  } else {
    diagnostic$completeness_method<-"incomplete_splitCatchment_only"
    polygon<-Filter(function(x) identical(x$id,"splitCatchment"),raw$features)
    if(length(polygon)!=1L) return(fail("missing_unique_complete_upstream_polygon"))
    local<-number("areasqkm");total<-number("totdasqkm")
    # A local split is not a full watershed on a non-headwater reach. Require
    # positive published VAA headwater evidence and independent upstream
    # navigation; missing attributes or an empty response are not proof.
    headwater<-isTRUE(number("startflag")==1) && isTRUE(number("streamorde")==1) &&
      isTRUE(number("uphydroseq")==0) && isTRUE(local>0) && isTRUE(total>0) &&
      isTRUE(abs(total-local)<=max(.0001,total*1e-6))
    if(!headwater) return(fail("incomplete_upstream_drainage_split_only_non_headwater"))
    id<-as.character(record$comid)
    path<-file.path(headwater_cache_dir,paste0("comid_",id,"_UT_1000.json"))
    diagnostic$headwater_navigation_source_url<-paste0("https://api.water.usgs.gov/nldi/linked-data/comid/",id,
      "/navigation/UT/flowlines?f=json&distance=1000")
    diagnostic$headwater_navigation_cache_file<-path
    if(!file.exists(path)) return(fail("split_only_headwater_navigation_proof_missing"))
    navigation<-tryCatch(nps_read_cache_json(path,simplifyVector=FALSE),error=function(e)NULL)
    if(is.null(navigation) || !identical(navigation$type,"FeatureCollection") || !length(navigation$features))
      return(fail("split_only_headwater_navigation_response_invalid"))
    ids<-vapply(navigation$features,function(f) {
      value<-f$properties$nhdplus_comid
      if(length(value)!=1L || is.null(value) || is.na(value)) NA_character_ else as.character(value)
    },character(1))
    diagnostic$headwater_upstream_reach_count<-length(unique(ids[!is.na(ids)]))
    diagnostic$headwater_navigation_md5<-unname(tools::md5sum(path))
    if(anyNA(ids) || length(unique(ids))!=1L || !identical(unique(ids),id))
      return(fail("split_only_headwater_navigation_contains_other_upstream_reaches"))
    diagnostic$completeness_method<-"published_headwater_single_catchment_split"
  }
  basin<-sf::st_read(jsonlite::toJSON(list(type="FeatureCollection",features=polygon),
    auto_unbox=TRUE,digits=NA),quiet=TRUE)
  basin<-sf::st_make_valid(sf::st_transform(basin,crs))
  area<-sum(as.numeric(sf::st_area(basin)))/1e6
  diagnostic$full_basin_area_km2<-area
  if(!is.finite(area) || area<=0) return(fail("upstream_basin_area_not_positive"))
  local<-number("areasqkm");total<-number("totdasqkm")
  if(isTRUE(local>=0) && isTRUE(total>0) && isTRUE(total>=local)) {
    lower<-max(0,total-local)
    diagnostic$published_upstream_only_area_km2<-lower
    diagnostic$actual_to_published_total_area_ratio<-area/total
    if(lower>0) diagnostic$actual_to_published_upstream_only_ratio<-area/lower
    tolerance<-max(diagnostic$area_absolute_tolerance_km2,diagnostic$area_relative_tolerance*total)
    diagnostic$published_area_check<-if(area<lower-tolerance || area>total+tolerance)
      "outside_published_plausible_range" else "passes_published_plausible_range"
    if(identical(diagnostic$published_area_check,"outside_published_plausible_range"))
      return(fail("upstream_basin_area_conflicts_with_published_drainage_attributes"))
  } else diagnostic$published_area_check<-"published_area_attributes_unavailable"
  list(valid=TRUE,reason="complete_upstream_geometry_confirmed",feature=polygon,diagnostics=diagnostic)
}

nldi_support_index <- function(stream_edges) {
  geometry<-lapply(sf::st_geometry(stream_edges),function(line) unclass(line)[,1:2,drop=FALSE])
  total_nodes<-max(c(stream_edges$from_node,stream_edges$to_node))
  adjacency<-lapply(seq_len(total_nodes),function(node) c(which(stream_edges$from_node==node),which(stream_edges$to_node==node)))
  list(edges=stream_edges,coordinates=geometry,adjacency=adjacency,nodes=data.frame(degree=lengths(adjacency)))
}

prepare_phase1_basin_requests <- function(selected_sf,stream_edges,
    cache_dir="spatial_data/upstream_basins/batch") {
  if(!exists("stream_identity_local_support",mode="function")) stop("Source R/stream_edge_identity.R")
  if(!inherits(selected_sf,"sf") || !inherits(stream_edges,"sf")) stop("Supply sf barrier points and physical stream edges")
  required<-c("graph_edge_id","from_node","to_node","length_m")
  if(!all(required %in% names(stream_edges))) stop("Stream edges require published review graph IDs and topology")
  controls<-nldi_course_controls()
  edges<-stream_edges
  index<-nldi_support_index(edges)
  geometry<-index$coordinates
  points<-sf::st_transform(selected_sf,sf::st_crs(edges))
  geographic<-sf::st_coordinates(sf::st_transform(points,4326))[,1:2,drop=FALSE]
  to_geo<-function(xy) unname(sf::st_coordinates(sf::st_transform(sf::st_sfc(
    lapply(seq_len(nrow(xy)),function(i) sf::st_point(xy[i,])),crs=sf::st_crs(edges)),4326))[,1:2,drop=FALSE])
  requests<-lapply(seq_len(nrow(points)),function(i) {
    request<-list(review_record_key=as.character(points$review_record_key[i]),
      review_number=as.integer(points$review_number[i]),SiteId=as.character(points$SiteId[i]),
      StreamName=as.character(points$StreamName[i]),graph_edge_id=as.integer(points$graph_edge_id[i]),
      longitude=geographic[i,1L],latitude=geographic[i,2L],preparation_status="ready",
      course_tangent_halfspan_m=controls$heading_halfspan_m,
      max_index_distance_m=50,max_match_distance_m=25,max_heading_degrees=35,min_coverage=.8)
    if("override_future_cut" %in% names(points) && isTRUE(as.logical(points$override_future_cut[i]))) {
      request$preparation_status<-"special_gorge_loader"; return(request)
    }
    edge<-match(points$graph_edge_id[i],edges$graph_edge_id)
    if(is.na(edge)) {request$preparation_status<-"unknown_declared_graph_edge";return(request)}
    projection<-stream_identity_project(geometry[[edge]],sf::st_coordinates(points[i,])[1L,1:2])
    support<-stream_identity_local_support(index,edge,projection$measure,50)
    if(support$length<40 || support$topology_ambiguous || support$junction_distance<=25) {
      request$preparation_status<-"insufficient_or_ambiguous_local_course";return(request)
    }
    measures<-seq(0,support$length,length.out=7)
    samples<-t(vapply(measures,function(m) stream_identity_at(support$xy,m),numeric(2)))
    request$support_samples<-to_geo(samples)
    request$support_headings<-lapply(measures,function(m) to_geo(rbind(
      stream_identity_at(support$xy,max(0,m-controls$heading_halfspan_m)),
      stream_identity_at(support$xy,min(support$length,m+controls$heading_halfspan_m)))))
    request$support_length_m<-support$length
    request
  })
  dir.create(cache_dir,recursive=TRUE,showWarnings=FALSE)
  file<-file.path(cache_dir,"requests.json")
  jsonlite::write_json(requests,file,auto_unbox=TRUE,pretty=TRUE,na="null",digits=NA)
  invisible(list(requests=requests,path=file))
}

load_nldi_delineations <- function(selected_sf,stream_edges,
    cache_dir="spatial_data/upstream_basins/batch",refresh=FALSE) {
  if(isTRUE(refresh)) stop("Refresh is explicit: prepare requests, then run scripts/fetch_upstream_basins.py --refresh")
  manifest_file<-file.path(cache_dir,"manifest.json")
  if(!file.exists(manifest_file)) stop("Prepare requests and run scripts/fetch_upstream_basins.py before loading delineations")
  records<-nps_read_cache_json(manifest_file,simplifyVector=FALSE)
  crs<-sf::st_crs(stream_edges)
  selected<-sf::st_transform(selected_sf,crs)
  controls<-nldi_course_controls()
  support_index<-nldi_support_index(stream_edges)
  masks<-anchors<-list(); audits<-list()
  scalar<-function(record,key,default=NA) if(is.null(record[[key]])) default else record[[key]]
  for(record in records) {
    # The single recovery worker may improve an already-completed primary row.
    # Read its per-request manifest rather than retaining a stale batch envelope.
    latest_path<-file.path(record$cache_folder,"manifest.json")
    if(file.exists(latest_path)) {
      latest<-nps_read_cache_json(latest_path,simplifyVector=FALSE)
      if(identical(latest$request_sha256,record$request_sha256) &&
          identical(latest$review_record_key,record$review_record_key)) record<-latest
    }
    key<-as.character(record$review_record_key)
    row<-match(key,selected$review_record_key)
    if(is.na(row)) next
    audit<-data.frame(review_record_key=key,review_number=selected$review_number[row],
      status=as.character(record$status),nldi_reference_stream=as.character(scalar(record,"reference_stream_name",NA_character_)),
      nldi_comid=as.character(scalar(record,"comid",NA_character_)),
      indexed_shift_m=as.numeric(scalar(record,"indexed_shift_m",NA_real_)),
      route_support_coverage=as.numeric(scalar(record,"route_support_coverage",NA_real_)),
      heading_halfspan_m=controls$heading_halfspan_m,
      source_retrieved_utc=as.character(scalar(record,"retrieved_utc",NA_character_)),
      basin_source_md5=as.character(scalar(record,"basin_md5",NA_character_)),
      cache_folder=as.character(record$cache_folder),downstream_anchor_validated=FALSE,
      indexing_method=as.character(scalar(record,"indexing_method","USGS_hydrolocation")),
      request_stage=as.character(scalar(record,"request_stage",NA_character_)),
      local_outlet_precision_validated=FALSE,error=as.character(scalar(record,"error",NA_character_)),
      stringsAsFactors=FALSE)
    for(name in names(nldi_basin_completeness_defaults())) audit[[name]]<-nldi_basin_completeness_defaults()[[name]]
    if(!isTRUE(record$approved)) {audits[[length(audits)+1L]]<-audit;next}
    indexed<-sf::st_sfc(sf::st_point(c(record$indexed_longitude,record$indexed_latitude)),crs=4326)
    indexed<-sf::st_transform(indexed,crs)
    shift<-as.numeric(sf::st_distance(indexed,selected[row,],by_element=TRUE))
    if(shift>50 || !stream_identity_names_match(selected$StreamName[row],record$reference_stream_name)) {
      audit$status<-"projected_distance_or_name_validation_failed";audits[[length(audits)+1L]]<-audit;next
    }
    route_json<-nps_read_cache_json(file.path(record$cache_folder,"named_flowline.json"),simplifyVector=FALSE)
    feature<-Filter(function(x) as.character(x$properties$comid)==as.character(record$comid),route_json$features)[[1L]]
    route<-sf::st_read(jsonlite::toJSON(list(type="FeatureCollection",features=list(feature)),auto_unbox=TRUE,digits=NA),quiet=TRUE)
    route<-suppressWarnings(sf::st_cast(sf::st_transform(route,crs),"LINESTRING"))
    point_xy<-sf::st_coordinates(selected[row,])[1L,1:2]
    edge_row<-match(selected$graph_edge_id[row],stream_edges$graph_edge_id)
    edge_xy<-unclass(sf::st_geometry(stream_edges)[[edge_row]])[,1:2,drop=FALSE]
    barrier_position<-stream_identity_project(edge_xy,point_xy)$measure
    edge_length<-sum(sqrt(rowSums((edge_xy[-1L,,drop=FALSE]-edge_xy[-nrow(edge_xy),,drop=FALSE])^2)))
    route_rows<-sf::st_nearest_feature(selected[row,],route)
    route_xy<-unclass(sf::st_geometry(route)[[route_rows]])[,1:2,drop=FALSE]
    support<-stream_identity_local_support(support_index,edge_row,barrier_position,50)
    if(support$length<40 || support$topology_ambiguous || support$junction_distance<=25 ||
        !stream_identity_names_match(selected$StreamName[row],feature$properties$gnis_name)) {
      audit$status<-"cached_route_identity_or_support_unconfirmed";audits[[length(audits)+1L]]<-audit;next
    }
    measures<-seq(0,support$length,length.out=7)
    samples<-t(vapply(measures,function(m) stream_identity_at(support$xy,m),numeric(2)))
    sample_sf<-sf::st_sfc(lapply(seq_len(7),function(i) sf::st_point(samples[i,])),crs=crs)
    nearest_parts<-sf::st_nearest_feature(sample_sf,route)
    distance<-angle<-numeric(7)
    for(j in seq_len(7)) {
      ref_xy<-unclass(sf::st_geometry(route)[[nearest_parts[j]]])[,1:2,drop=FALSE]
      ref_length<-sum(sqrt(rowSums((ref_xy[-1L,,drop=FALSE]-ref_xy[-nrow(ref_xy),,drop=FALSE])^2)))
      ref_at<-stream_identity_project(ref_xy,samples[j,])
      own_heading<-stream_identity_at(support$xy,min(support$length,measures[j]+controls$heading_halfspan_m))-
        stream_identity_at(support$xy,max(0,measures[j]-controls$heading_halfspan_m))
      ref_heading<-stream_identity_at(ref_xy,min(ref_length,ref_at$measure+controls$heading_halfspan_m))-
        stream_identity_at(ref_xy,max(0,ref_at$measure-controls$heading_halfspan_m))
      norm<-sqrt(sum(own_heading^2)*sum(ref_heading^2))
      distance[j]<-ref_at$distance
      angle[j]<-if(norm>1e-12) acos(min(1,max(0,abs(sum(own_heading*ref_heading))/norm)))*180/pi else 90
    }
    audit$route_support_coverage<-mean(distance<=25+1e-8 & angle<=35+1e-8)
    audit$indexed_shift_m<-shift
    if(audit$route_support_coverage<.8) {
      audit$status<-"cached_local_course_unconfirmed";audits[[length(audits)+1L]]<-audit;next
    }
    # Published WithDigitized authorizes the reference's downstream orientation.
    # Physical-edge vertex order is never assumed to encode flow direction.
    if(identical(tolower(feature$properties$flowdir),"with digitized")) {
      low<-max(0,barrier_position-25);high<-min(edge_length,barrier_position+25)
      positions<-seq(low,high,length.out=7)
      physical_samples<-t(vapply(positions,function(m) stream_identity_at(edge_xy,m),numeric(2)))
      projected<-vapply(seq_len(7),function(i) stream_identity_project(route_xy,physical_samples[i,])$measure,numeric(1))
      changes<-diff(projected)
      direction<-if(all(changes>0.25)) 1 else if(all(changes< -0.25)) -1 else 0
      available<-if(direction==1) edge_length-barrier_position else barrier_position
      anchor_offset<-min(50,available)
      anchor_position<-barrier_position+anchor_offset*direction
      if(direction!=0 && anchor_offset>=40 && anchor_position>=0 && anchor_position<=edge_length) {
        anchor_xy<-stream_identity_at(edge_xy,anchor_position)
        anchor_route_position<-stream_identity_project(route_xy,anchor_xy)$measure
        center_route_position<-stream_identity_project(route_xy,point_xy)$measure
        anchor_separation<-sqrt(sum((anchor_xy-point_xy)^2))
        if(anchor_separation<=25 || anchor_route_position<=center_route_position+10) next_anchor<-FALSE else next_anchor<-TRUE
        if(next_anchor) {
        anchors[[length(anchors)+1L]]<-sf::st_sf(review_record_key=key,
          graph_edge_id=selected$graph_edge_id[row],anchor_role="downstream",
          anchor_id=paste0("NLDI_published_direction_",key),direction_evidence_valid=TRUE,
          anchor_reviewed=TRUE,max_snap_distance_m=1e-5,
          direction_basis="Published WithDigitized orientation; seven monotonic physical-course projections; anchor40-50m along same edge",
          direction_source_url=paste0("https://api.water.usgs.gov/fabric/pygeoapi/collections/nhdflowline_network/items?comid=",record$comid,"&f=json"),
          direction_source="USGS NHDPlusV2 published FlowDir=WithDigitized; monotonic physical-course projection",
          source_url=paste0("https://api.water.usgs.gov/fabric/pygeoapi/collections/nhdflowline_network/items?comid=",record$comid,"&f=json"),
          nldi_comid=as.character(record$comid),geometry=sf::st_sfc(sf::st_point(anchor_xy),crs=crs))
        audit$downstream_anchor_validated<-TRUE
        }
      }
    }
    if(!identical(tolower(unname(tools::md5sum(record$basin_path))),tolower(record$basin_md5))) {
      audit$status<-"cached_basin_md5_mismatch";audits[[length(audits)+1L]]<-audit;next
    }
    raw<-nps_read_cache_json(record$basin_path,simplifyVector=FALSE)
    completeness<-nldi_validate_basin_completeness(raw,feature$properties,record,crs,
      headwater_cache_dir=file.path(dirname(cache_dir),"headwater_navigation"))
    for(name in names(completeness$diagnostics)) audit[[name]]<-completeness$diagnostics[[name]]
    if(!isTRUE(completeness$valid)) {
      audit$status<-completeness$reason;audits[[length(audits)+1L]]<-audit;next
    }
    polygon<-completeness$feature
    mask<-sf::st_read(jsonlite::toJSON(list(type="FeatureCollection",features=polygon),auto_unbox=TRUE,digits=NA),quiet=TRUE)
    mask<-sf::st_make_valid(sf::st_transform(mask,crs))
    new_mask<-sf::st_sf(review_record_key=key,
      review_number=selected$review_number[row],
      graph_edge_id=selected$graph_edge_id[row],
      watershed_validated=TRUE,stream_identity_validated=TRUE,total_barrier_approved=TRUE,
      coverage_complete=TRUE,local_outlet_precision_validated=FALSE,
      protect_local_graph_downstream=TRUE,barrier_x=point_xy[1L],barrier_y=point_xy[2L],
      local_correction_limit_m=100,nldi_comid=as.character(record$comid),indexed_shift_m=shift,
      indexing_method=audit$indexing_method,
      source_url="https://api.water.usgs.gov/nldi/pygeoapi/processes/nldi-splitcatchment/execution?f=json",
      source_md5=record$basin_md5,source_retrieved_utc=record$retrieved_utc,
      heading_halfspan_m=controls$heading_halfspan_m,
      geometry=sf::st_geometry(mask))
    for(name in names(completeness$diagnostics)) new_mask[[name]]<-completeness$diagnostics[[name]]
    masks[[length(masks)+1L]]<-new_mask
    audits[[length(audits)+1L]]<-audit
  }
  empty_masks<-sf::st_sf(review_record_key=character(),graph_edge_id=integer(),
    watershed_validated=logical(),stream_identity_validated=logical(),total_barrier_approved=logical(),
    coverage_complete=logical(),local_outlet_precision_validated=logical(),geometry=sf::st_sfc(crs=crs))
  empty_anchors<-sf::st_sf(review_record_key=character(),graph_edge_id=integer(),anchor_role=character(),
    anchor_id=character(),direction_evidence_valid=logical(),anchor_reviewed=logical(),max_snap_distance_m=numeric(),
    direction_basis=character(),direction_source_url=character(),geometry=sf::st_sfc(crs=crs))
  list(candidate_masks=if(length(masks)) do.call(rbind,masks) else empty_masks,
    audit=if(length(audits)) do.call(rbind,audits) else data.frame(),
    downstream_anchors=if(length(anchors)) do.call(rbind,anchors) else empty_anchors,
    settings=controls)
}

load_phase1_upstream_basins <- load_nldi_delineations
