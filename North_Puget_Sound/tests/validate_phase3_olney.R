# Independent read-only proof of the real Olney export. Deliberately does not
# source the production Olney handler or its eligibility/projection functions.
validate_phase3_olney <- function() {
  previous<-getwd()
  if(!file.exists('R/project_paths.R') && dir.exists('North_Puget_Sound')) setwd('North_Puget_Sound')
  on.exit(setwd(previous),add=TRUE)
  suppressPackageStartupMessages(library(sf))
  source('R/project_paths.R') # Path relocation only; no scientific implementation.
  assert<-function(ok,message)if(!isTRUE(ok))stop(message,call.=FALSE)
  key<-'6:43814'
  out<-'outputs/phase3_tidal_barriers'
  file<-file.path(out,'North_Puget_Sound_Steelhead_phase3.gpkg')
  read_layer<-function(layer)st_read(file,layer=layer,quiet=TRUE)
  has_key<-function(values,key='6:43814')vapply(strsplit(ifelse(is.na(values),'',values),';',fixed=TRUE),
    function(parts)key %in% parts,logical(1))
  lines_only<-function(x) {
    if(!nrow(x))return(x)
    x<-x[!st_is_empty(x),]
    if(!nrow(x))return(x)
    if(any(st_geometry_type(x)=='GEOMETRYCOLLECTION'))x<-suppressWarnings(st_collection_extract(x,'LINESTRING'))
    x<-x[st_geometry_type(x) %in% c('LINESTRING','MULTILINESTRING'),]
    if(!nrow(x))return(x)
    # Homogenize before expanding. Casting a mixed sfc_GEOMETRY directly to
    # LINESTRING can keep only the first part of an existing MULTILINESTRING.
    x<-suppressWarnings(st_cast(x,'MULTILINESTRING'))
    x<-suppressWarnings(st_cast(x,'LINESTRING'))
    x[as.numeric(st_length(x))>1e-8,]
  }
  meters<-function(x)if(!nrow(x))0 else sum(as.numeric(st_length(x)))
  polygon<-function(x) {
    g<-if(inherits(x,'sf'))st_geometry(x) else x
    g<-st_make_valid(g)
    if(any(st_geometry_type(g)=='GEOMETRYCOLLECTION'))g<-suppressWarnings(st_collection_extract(g,'POLYGON'))
    st_union(g)
  }
  positions<-function(xy)c(0,cumsum(sqrt(rowSums(diff(xy)^2))))
  at<-function(xy,m) {
    d<-positions(xy);keep<-!duplicated(d)
    c(approx(d[keep],xy[keep,1L],pmax(0,pmin(tail(d,1L),m)),rule=2)$y,
      approx(d[keep],xy[keep,2L],pmax(0,pmin(tail(d,1L),m)),rule=2)$y)
  }
  project<-function(xy,p) {
    starts<-xy[-nrow(xy),,drop=FALSE];vectors<-diff(xy);squared<-rowSums(vectors^2)
    t<-rowSums(sweep(starts,2,p,function(a,b)b-a)*vectors)/squared
    t[!is.finite(t)]<-0;t<-pmax(0,pmin(1,t))
    projected<-starts+vectors*t
    distance<-sqrt(rowSums(sweep(projected,2,p)^2));j<-which.min(distance)
    list(distance=distance[j],measure=positions(xy)[j]+sqrt(squared[j])*t[j],xy=projected[j,])
  }
  slice<-function(xy,start,end) {
    d<-positions(xy)
    rbind(at(xy,start),xy[d>start & d<end,,drop=FALSE],at(xy,end))
  }
  angle<-function(a,b) {
    norm<-sqrt(sum(a^2)*sum(b^2))
    if(norm<1e-12)return(90)
    acos(min(1,max(0,abs(sum(a*b))/norm)))*180/pi
  }

  manifest<-read.csv(file.path(out,'preserved_source_manifest.csv'),stringsAsFactors=FALSE)
  assert(nrow(manifest)>0L && all(file.exists(nps_resolve_cache_path(manifest$path))) &&
    identical(unname(tools::md5sum(nps_resolve_cache_path(manifest$path))),manifest$md5),
    'An earlier scenario file changed')
  barriers<-read_layer('barrier_characteristics')
  point<-barriers[barriers$review_record_key %in% key,]
  assert(nrow(point)==1L && point$review_number==331L && as.character(point$SiteId)=='891117' &&
    point$phase3_applied && point$phase3_selected && point$report_blockage=='Total' &&
    point$report_natural_type=='Waterfall' && point$verification=='Not field verified' &&
    point$phase3_display_effective_cut && !anyDuplicated(barriers$review_number) &&
    !anyDuplicated(barriers$review_record_key),'Olney331 source identity/classification or effective cut changed')
  baseline_file<-'outputs/review_swifd_connections/North_Puget_Sound_Steelhead.gpkg'
  original<-st_read(baseline_file,layer='barriers_snapped',quiet=TRUE)
  original<-original[original$review_number %in% 331L,]
  assert(length(st_equals_exact(point,original,par=1e-7)[[1L]])==1L &&
    point$snap_distance_m<=50 && point$matching_channel_count==1L && !point$junction_ambiguous &&
    point$graph_edge_id==6133L && as.character(point$matched_llid)=='1217124478723',
    'Olney331 moved or lost its unique original channel match')
  edges<-st_read(baseline_file,layer='stream_matching_edges',quiet=TRUE)
  edge<-edges[edges$graph_edge_id %in% 6133L,]
  xy<-unclass(st_geometry(edge)[[1L]])[,1:2,drop=FALSE]
  crs<-st_crs(edge);center<-st_coordinates(point)[1L,1:2]
  barrier_measure<-project(xy,center)$measure
  edge_length<-tail(positions(xy),1L)
  report_file<-'spatial_data/barrier_reports/891117_Report.text.json'
  report<-nps_read_cache_json(report_file,simplifyVector=FALSE)
  assert(grepl('891117',point$report_url,fixed=TRUE) &&
    identical(as.character(point$report_source_md5),report$report_source_md5),
    'Applied Olney record lost its immutable WDFW report provenance')

  masks<-read_layer('upstream_basin_masks')
  mask<-masks[masks$review_record_key %in% key,]
  assert(nrow(mask)==1L,'Missing unique full Olney331 mask')
  basin_file<-nps_resolve_cache_path(mask$source_split_basin_cache_file,must_work=TRUE)
  assert(identical(unname(tools::md5sum(basin_file)),as.character(mask$source_split_basin_md5)),
    'Full Olney basin cache fingerprint changed')
  raw_basin<-jsonlite::fromJSON(basin_file,simplifyVector=FALSE)
  full<-Filter(function(f)identical(f$id,'drainageBasin'),raw_basin$features)
  assert(length(full)==1L,'A split catchment replaced the complete Olney drainage basin')
  source<-st_transform(st_read(jsonlite::toJSON(list(type='FeatureCollection',features=full),
    auto_unbox=TRUE,digits=NA),quiet=TRUE),crs)
  source_geometry<-polygon(source);mask_geometry<-polygon(mask)
  area_difference<-sum(as.numeric(st_area(suppressWarnings(st_sym_difference(source_geometry,mask_geometry)))))
  boundary_offset<-as.numeric(st_distance(st_boundary(source_geometry),st_boundary(mask_geometry),which='Hausdorff'))
  assert(area_difference<1e-4 && boundary_offset<1e-5,
    'Applied Olney mask differs from the exact positive full-source drainage geometry')
  old_masks<-st_read('outputs/phase2_reviewed_barriers/North_Puget_Sound_Steelhead_phase2.gpkg',
    layer='upstream_basin_masks',quiet=TRUE)
  m379<-masks[masks$review_number %in% 379L,]
  old379<-old_masks[old_masks$review_number %in% 379L,]
  b379<-barriers[barriers$review_number %in% 379L,]
  assert(nrow(m379)==1L && nrow(old379)==1L && b379$phase3_applied && !b379$phase3_display_effective_cut &&
    sum(as.numeric(st_area(suppressWarnings(st_sym_difference(polygon(m379),polygon(old379))))))<1e-4,
    'Upper379 corroborating mask was discarded or altered')

  records<-nps_read_cache_json('spatial_data/upstream_basins/batch/manifest.json',simplifyVector=FALSE)
  record<-Filter(function(x)x$review_record_key==key,records)[[1L]]
  route_json<-jsonlite::fromJSON(file.path(record$cache_folder,'named_flowline.json'),simplifyVector=FALSE)
  rf<-Filter(function(f)as.character(f$properties$comid)=='23963685',route_json$features)
  assert(length(rf)==1L && tolower(rf[[1L]]$properties$flowdir)=='with digitized' &&
    rf[[1L]]$properties$gnis_name=='Olney Creek','Published Olney flow direction is absent')
  route<-suppressWarnings(st_cast(st_transform(st_read(jsonlite::toJSON(
    list(type='FeatureCollection',features=rf),auto_unbox=TRUE,digits=NA),quiet=TRUE),crs),'LINESTRING'))
  assert(nrow(route)==1L,'Ambiguous published Olney reference')
  rxy<-unclass(st_geometry(route)[[1L]])[,1:2,drop=FALSE]
  local_at<-seq(barrier_measure-25,barrier_measure+25,length.out=7L)
  local_projected<-lapply(local_at,function(m)project(rxy,at(xy,m)))
  differences<-diff(vapply(local_projected,`[[`,numeric(1),'measure'))
  assert(all(vapply(local_projected,`[[`,numeric(1),'distance')<=25) &&
    (all(differences>.25)||all(differences< -.25)),'Local downstream physical direction is not independently proven')
  downstream_sign<-sign(sum(differences))
  reference_barrier_measure<-project(rxy,center)$measure
  ordering<-barriers[barriers$review_number %in% c(331L,335L,379L,380L),]
  ordering_measures<-vapply(seq_len(nrow(ordering)),function(i)project(xy,st_coordinates(ordering[i,])[1L,1:2])$measure,numeric(1))
  assert(all((barrier_measure-ordering_measures[ordering$review_number!=331L])*downstream_sign>0),
    '331 is not the lowest published-direction Total on the original unbranched Olney edge')
  nav<-st_read('spatial_data/phase3/olney_recovery/upstream_flowlines.json',quiet=TRUE)
  assert(all(c('23963685','23964021') %in% as.character(nav$nhdplus_comid)),
    'The upper379 reference is absent from the published Olney upstream network')
  traces<-lapply(c('physical_flowtrace.json','indexed_flowtrace.json'),function(name) {
    x<-jsonlite::fromJSON(file.path('spatial_data/phase3/olney_recovery',name),simplifyVector=FALSE)
    f<-Filter(function(f)identical(f$id,'nhdFlowline'),x$features)
    assert(length(f)==1L && as.character(f[[1L]]$properties$comid)=='23963685',
      'An independent official flowtrace selected another stream')
    f[[1L]]$properties
  })
  assert(identical(unlist(traces[[1L]]$intersection_point),unlist(traces[[2L]]$intersection_point)) &&
    all(vapply(traces,`[[`,numeric(1),'raindrop_pathDist')<=50),'Independent official pour-point confirmations disagree')

  downstream_xy<-if(downstream_sign<0)slice(xy,0,barrier_measure) else slice(xy,barrier_measure,edge_length)
  downstream_line<-st_sf(geometry=st_sfc(st_linestring(downstream_xy),crs=crs))
  expected<-lines_only(suppressWarnings(st_intersection(downstream_line,mask_geometry)))
  expected_distance<-vapply(st_geometry(expected),function(g)max(sqrt(rowSums(sweep(unclass(g)[,1:2,drop=FALSE],2,center)^2))),numeric(1))
  expected_remote_m<-sum(as.numeric(st_length(expected))[expected_distance>100])
  expected_local_m<-sum(as.numeric(st_length(expected))[expected_distance<=100])
  assert(abs(expected_remote_m-.592399)<1e-5 && abs(expected_local_m-3.748863)<1e-5,
    'Unexpected physical downstream/basin overlap; review source changes')

  before_tidal<-read_layer('before_tidal_extent_status')
  final<-read_layer('phase3_remaining')
  before290<-before_tidal[before_tidal$pop_id %in% 290L & before_tidal$measure_habitat,]
  remaining290<-final[final$pop_id %in% 290L & final$measure_habitat,]
  fine<-readRDS('spatial_data/north_puget_named_stream_reference.rds')
  fine<-suppressWarnings(st_cast(st_transform(fine[as.character(fine$LLID) %in% '1217124478723',],crs),'LINESTRING'))
  fine_xy<-lapply(st_geometry(fine),function(g)unclass(g)[,1:2,drop=FALSE])
  check_residual<-function(lines,label) {
    residual<-lines_only(suppressWarnings(st_intersection(lines,mask_geometry)))
    assert(abs(meters(residual)-expected_remote_m-expected_local_m)<1e-4,
      paste(label,'has missing or unexplained upstream Olney habitat'))
    assert(all(residual$graph_edge_id==6133L) && all(residual$source_class=='prediction') &&
      all(has_key(residual$watershed_downstream_protected_by_barrier)),
      paste(label,'extended protection to another channel/source or lacks explicit proof tags'))
    remote_total<-0;fine_distances<-fine_angles<-numeric()
    for(i in seq_len(nrow(residual))) {
      coords<-st_coordinates(residual[i,])[,1:2,drop=FALSE]
      located<-lapply(seq_len(nrow(coords)),function(j)project(xy,coords[j,]))
      measures<-vapply(located,`[[`,numeric(1),'measure')
      assert(max(vapply(located,`[[`,numeric(1),'distance'))<1e-5 &&
        all((measures-barrier_measure)*downstream_sign>= -1e-5),
        paste(label,'protected geometry is not exact physical Olney downstream'))
      remote<-max(sqrt(rowSums(sweep(coords,2,center)^2)))>100
      if(remote) {
        remote_total<-remote_total+as.numeric(st_length(residual[i,]))
        assert(has_key(residual$phase3_remote_downstream_protected_by_barrier[i]),
          'Remote protection lacks its distinct audited tag')
      }
      for(m in seq(min(measures),max(measures),length.out=7L)) {
        sample<-at(xy,m)
        assert(project(rxy,sample)$measure>=reference_barrier_measure-1e-5,
          'Published digitized route does not place a protected piece below331')
        for(n in seq(max(0,m-50),min(edge_length,m+50),length.out=7L)) {
          p<-at(xy,n)
          projections<-lapply(fine_xy,function(line)project(line,p))
          nearest<-which.min(vapply(projections,`[[`,numeric(1),'distance'))
          hit<-projections[[nearest]];line<-fine_xy[[nearest]]
          own_heading<-at(xy,min(edge_length,n+5))-at(xy,max(0,n-5))
          ref_heading<-at(line,hit$measure+5)-at(line,hit$measure-5)
          fine_distances<-c(fine_distances,hit$distance)
          fine_angles<-c(fine_angles,angle(own_heading,ref_heading))
          assert(hit$distance<=25 && tail(fine_angles,1L)<=35,
            'Independent fine WDFW course proof failed for retained Olney geometry')
        }
      }
    }
    assert(abs(remote_total-expected_remote_m)<1e-4,'Remote protected length changed')
    c(total_m=meters(residual),remote_m=remote_total,max_fine_distance_m=max(fine_distances),max_fine_angle_degrees=max(fine_angles))
  }
  first<-check_residual(before290[!before290$removed,],'Before-tidal output')
  last<-check_residual(remaining290,'Final output')
  proof<-read.csv(file.path(out,'olney_downstream_proof.csv'),stringsAsFactors=FALSE)
  assert(all(proof$review_number==331L) && all(proof$graph_edge_id==6133L) &&
    abs(sum(proof$protected_length_m)-expected_local_m-expected_remote_m)<1e-4 &&
    abs(sum(proof$protected_length_m[proof$protection_scope=='remote_explicit_same_channel_downstream'])-expected_remote_m)<1e-4,
    'Published proof table and exact retained geometry disagree')

  # All17 clipped analysis areas must still partition the original source by
  # source class; the custom area is checked separately rather than added to16.
  pops<-read_layer('noaa_populations');custom<-read_layer('custom_review_watershed')
  boundaries<-rbind(st_sf(pop_id=pops$pop_id,geometry=st_geometry(pops)),
    st_sf(pop_id=custom$pop_id,geometry=st_geometry(custom)))
  raw<-readRDS('spatial_data/north_puget_swifd_bridges.rds')$bridges$network
  clipped<-lines_only(suppressWarnings(st_intersection(raw,boundaries)))
  totals<-function(x)tapply(as.numeric(st_length(x)),paste(x$pop_id,x$source_class,sep='|'),sum)
  baseline_totals<-totals(clipped);before_totals<-totals(before_tidal)
  full_final<-read_layer('phase3_extent_status');final_totals<-totals(full_final)
  if(!setequal(names(baseline_totals),names(before_totals)) || !setequal(names(baseline_totals),names(final_totals)))
    print(list(missing_before=setdiff(names(baseline_totals),names(before_totals)),
      extra_before=setdiff(names(before_totals),names(baseline_totals)),
      missing_final=setdiff(names(baseline_totals),names(final_totals)),
      extra_final=setdiff(names(final_totals),names(baseline_totals))))
  balance<-data.frame(group=names(baseline_totals),baseline=as.numeric(baseline_totals),
    before=as.numeric(before_totals[names(baseline_totals)]),final=as.numeric(final_totals[names(baseline_totals)]))
  balance$before_error<-balance$before-balance$baseline;balance$final_error<-balance$final-balance$baseline
  if(any(abs(balance$before_error)>.001|abs(balance$final_error)>.001,na.rm=TRUE))
    print(balance[order(-pmax(abs(balance$before_error),abs(balance$final_error))),][seq_len(min(10L,nrow(balance))),])
  assert(setequal(names(baseline_totals),names(before_totals)) &&
    setequal(names(baseline_totals),names(final_totals)) &&
    max(abs(baseline_totals-before_totals[names(baseline_totals)]))<.001 &&
    max(abs(baseline_totals-final_totals[names(baseline_totals)]))<.001,
    'Source conservation failed for one of17 clipped areas or a source class')
  assert(identical(unname(tools::md5sum(nps_resolve_cache_path(manifest$path))),manifest$md5),
    'Read-only validation detected a changed earlier scenario')
  cat(sprintf(paste0('PASS: Olney331 exact full source basin; lower Total ordering;379 preserved; ',
    '%.9fm retained below331 (%.9fm remote), independent49-sample fine-course checks; ',
    '17-area source conservation and all previous scenario hashes unchanged.\n'),last['total_m'],last['remote_m']))
  invisible(list(before_tidal=first,final=last))
}
validate_phase3_olney()
