# Positive stream identity for physical graph edges. No barrier cuts occur here.
# Names are checked only after local line geometry supports one reference route.
stream_identity_name_key <- function(x) {
  key<-barrier_stream_name_key(x)
  # GNIS uses the explicit postfix form 'Stillaguamish River, South Fork'.
  # Reorder that known grammar; do not use fuzzy name similarity.
  sub("^(.+) (south|north|middle) fork$","\\2 fork \\1",key)
}

stream_identity_names_match <- function(a,b) {
  barrier_stream_names_match(stream_identity_name_key(a),stream_identity_name_key(b))
}

stream_identity_project <- function(xy, point) {
  delta <- xy[-1L,,drop=FALSE] - xy[-nrow(xy),,drop=FALSE]
  squared <- rowSums(delta^2)
  fraction <- pmax(0,pmin(1,rowSums((matrix(point,nrow(delta),2L,byrow=TRUE)-
    xy[-nrow(xy),,drop=FALSE])*delta)/ifelse(squared==0,1,squared)))
  projected <- xy[-nrow(xy),,drop=FALSE]+delta*fraction
  distances <- rowSums(sweep(projected,2L,point)^2)
  segment <- which.min(distances)
  list(xy=unname(projected[segment,]),distance=sqrt(distances[segment]),
    measure=sum(sqrt(squared)[seq_len(segment-1L)])+fraction[segment]*sqrt(squared[segment]))
}

stream_identity_at <- function(xy, distance) {
  lengths <- sqrt(rowSums((xy[-1L,,drop=FALSE]-xy[-nrow(xy),,drop=FALSE])^2))
  cumulative <- c(0,cumsum(lengths))
  distance <- max(0,min(distance,tail(cumulative,1L)))
  if (distance==0) return(unname(xy[1L,]))
  if (distance==tail(cumulative,1L)) return(unname(xy[nrow(xy),]))
  segment <- which(cumulative[-1L]>=distance & lengths>0)[1L]
  unname(xy[segment,]+(distance-cumulative[segment])/lengths[segment]*
    (xy[segment+1L,]-xy[segment,]))
}

stream_identity_slice <- function(xy, start, end) {
  cumulative <- c(0,cumsum(sqrt(rowSums((xy[-1L,,drop=FALSE]-xy[-nrow(xy),,drop=FALSE])^2))))
  result <- rbind(stream_identity_at(xy,start),xy[cumulative>start & cumulative<end,,drop=FALSE],
    stream_identity_at(xy,end))
  result[c(TRUE,rowSums(abs(result[-1L,,drop=FALSE]-result[-nrow(result),,drop=FALSE]))>1e-10),,drop=FALSE]
}

stream_identity_groups <- function(xy, tolerance, crs) {
  parent <- seq_len(nrow(xy))
  find <- function(i) { while(parent[i]!=i) i<-parent[i]; i }
  # Sparse spatial search avoids comparing all endpoint pairs.
  geometry <- sf::st_sfc(lapply(seq_len(nrow(xy)),function(i) sf::st_point(xy[i,])),crs=crs)
  neighbors <- sf::st_is_within_distance(geometry,geometry,dist=tolerance)
  for (i in seq_along(neighbors)) for (j in neighbors[[i]][neighbors[[i]]<i]) {
    a<-find(i); b<-find(j)
    if(a!=b) parent[max(a,b)]<-min(a,b)
  }
  roots<-vapply(seq_len(nrow(xy)),find,integer(1))
  match(roots,unique(roots))
}

build_stream_identity_index <- function(physical_lines, swifd_reference, node_tolerance=0.25) {
  if (!requireNamespace("sf",quietly=TRUE)) stop("Install the sf package")
  if (!exists("barrier_stream_name_key",mode="function")) stop("Source R/barrier_stream_identity.R first")
  if (length(node_tolerance)!=1L || !is.finite(node_tolerance) || node_tolerance<0) stop("Invalid node_tolerance")
  if (!inherits(physical_lines,"sf") || is.na(sf::st_crs(physical_lines))) stop("physical_lines must be sf with a CRS")
  crs<-sf::st_crs(physical_lines)
  if (isTRUE(sf::st_is_longlat(physical_lines)) || !crs$units_gdal %in% c("metre","meter","m")) {
    stop("Physical lines must use a projected CRS in meters")
  }
  keep<-!sf::st_is_empty(physical_lines)
  if("source_class" %in% names(physical_lines)) keep<-keep &
    !is.na(physical_lines$source_class) & physical_lines$source_class!="alignment_link"
  if("measure_habitat" %in% names(physical_lines)) keep<-keep &
    !is.na(physical_lines$measure_habitat) & physical_lines$measure_habitat
  if (!any(keep)) stop("No physical stream lines")
  if (any(!sf::st_geometry_type(physical_lines[keep,]) %in% c("LINESTRING","MULTILINESTRING"))) stop("Physical geometry must be lines")
  geometry<-suppressWarnings(sf::st_cast(sf::st_union(sf::st_geometry(physical_lines[keep,])),"LINESTRING"))
  geometry<-geometry[!sf::st_is_empty(geometry) & as.numeric(sf::st_length(geometry))>1e-8]
  coordinates<-lapply(geometry,function(x) unclass(x)[,1:2,drop=FALSE])
  lengths<-as.numeric(sf::st_length(geometry))
  endpoint_xy<-do.call(rbind,lapply(coordinates,function(x) x[c(1L,nrow(x)),,drop=FALSE]))
  groups<-stream_identity_groups(endpoint_xy,node_tolerance,crs)
  from<-groups[seq.int(1L,length(groups),2L)]
  to<-groups[seq.int(2L,length(groups),2L)]
  node_xy<-endpoint_xy[!duplicated(groups),,drop=FALSE]
  adjacency<-lapply(seq_len(max(groups)),function(node) c(which(from==node),which(to==node)))
  degree<-lengths(adjacency)
  # A channel chain passes through degree-two nodes, but never crosses a fork.
  chain_parent<-seq_along(coordinates)
  chain_find<-function(i) { while(chain_parent[i]!=i) i<-chain_parent[i]; i }
  for(node in which(degree==2L)) {
    a<-chain_find(adjacency[[node]][1L]); b<-chain_find(adjacency[[node]][2L])
    if(a!=b) chain_parent[max(a,b)]<-min(a,b)
  }
  chain_root<-vapply(seq_along(coordinates),chain_find,integer(1))
  chains<-match(chain_root,unique(chain_root))
  edges<-sf::st_sf(graph_edge_id=seq_along(coordinates),channel_chain_id=chains,
    from_node=from,to_node=to,length_m=lengths,from_degree=degree[from],to_degree=degree[to],geometry=geometry)
  nodes<-sf::st_sf(node_id=seq_len(nrow(node_xy)),degree=degree,
    geometry=sf::st_sfc(lapply(seq_len(nrow(node_xy)),function(i) sf::st_point(node_xy[i,])),crs=crs))
  if (is.null(swifd_reference)) swifd_reference<-sf::st_sf(LLID=character(),
    LLID_STRM_NAME=character(),geometry=sf::st_sfc(crs=crs))
  if (!inherits(swifd_reference,"sf") || is.na(sf::st_crs(swifd_reference))) stop("Reference must be sf with a CRS")
  reference<-sf::st_transform(swifd_reference,crs)
  reference<-reference[!sf::st_is_empty(reference),]
  if(nrow(reference) && any(!sf::st_geometry_type(reference) %in% c("LINESTRING","MULTILINESTRING"))) stop("Reference geometry must be lines")
  reference<-suppressWarnings(sf::st_cast(reference,"LINESTRING"))
  names_field<-intersect(c("LLID_STRM_NAME","LLID_STREAMNAME","GNIS_STREAMNAME","GNIS_Name","StreamName","stream_name"),names(reference))
  ref_name<-if(length(names_field)) as.character(reference[[names_field[1L]]]) else rep(NA_character_,nrow(reference))
  ref_id<-if("LLID" %in% names(reference)) as.character(reference$LLID) else rep(NA_character_,nrow(reference))
  known_id<-!is.na(ref_id) & nzchar(trimws(ref_id)) & !tolower(ref_id) %in% c("na","unknown","unnamed")
  name_key<-stream_identity_name_key(ref_name)
  key<-paste(ifelse(known_id,ref_id,paste0("missing_id_row_",seq_len(nrow(reference)))),ifelse(is.na(name_key),"<unnamed>",name_key),sep="|")
  unique_keys<-unique(key)
  route_rows<-lapply(unique_keys,function(value) which(key==value))
  route_geometry<-sf::st_sfc(lapply(route_rows,function(rows) {
    sf::st_union(sf::st_geometry(reference)[rows])[[1L]]
  }),crs=crs)
  first<-vapply(route_rows,function(rows) rows[1L],integer(1))
  routes<-sf::st_sf(route_id=seq_along(route_rows),route_key=unique_keys,
    matched_llid=ref_id[first],matched_stream_name=ref_name[first],known_id=known_id[first],
    geometry=route_geometry)
  reference$route_id<-match(key,unique_keys)
  list(edges=edges,nodes=nodes,junction_nodes=nodes[nodes$degree>=3L,],
    coordinates=coordinates,adjacency=adjacency,reference_routes=routes,
    reference_lines=reference,reference_coordinates=lapply(sf::st_geometry(reference),function(x) unclass(x)[,1:2,drop=FALSE]),
    route_rows=route_rows,settings=list(node_tolerance_m=node_tolerance,
      topology="Full physical lines noded at intersections; artificial alignment links excluded",
      identity="Local multi-sample distance and tangent agreement with a unique LLID/name reference route"))
}

stream_identity_local_support <- function(index, edge, measure, half_length) {
  walk<-function(toward_start) {
    current<-edge; position<-measure; remaining<-half_length
    visited<-integer(); result<-matrix(numeric(),ncol=2L)
    junction<-Inf; cycle<-FALSE
    repeat {
      xy<-index$coordinates[[current]]; len<-index$edges$length_m[current]
      if(toward_start) {
        begin<-max(0,position-remaining); end<-position
        piece<-stream_identity_slice(xy,begin,end)
        piece<-piece[nrow(piece):1L,,drop=FALSE]
        used<-position-begin; at_node<-begin<=1e-8
        node<-index$edges$from_node[current]
      } else {
        begin<-position; end<-min(len,position+remaining)
        piece<-stream_identity_slice(xy,begin,end)
        used<-end-position; at_node<-end>=len-1e-8
        node<-index$edges$to_node[current]
      }
      if(nrow(result)) piece<-piece[-1L,,drop=FALSE]
      result<-rbind(result,piece)
      visited<-c(visited,current); remaining<-remaining-used
      if(at_node && index$nodes$degree[node]>=3L) junction<-half_length-remaining
      if(!at_node || remaining<=1e-8 || index$nodes$degree[node]!=2L) break
      next_edges<-setdiff(index$adjacency[[node]],current)
      if(length(next_edges)!=1L || next_edges %in% visited) { cycle<-TRUE; break }
      current<-next_edges
      toward_start<-index$edges$to_node[current]==node
      position<-if(toward_start) index$edges$length_m[current] else 0
    }
    list(xy=result,edges=visited,junction=junction,cycle=cycle)
  }
  left<-walk(TRUE); right<-walk(FALSE)
  overlap<-intersect(setdiff(left$edges,edge),setdiff(right$edges,edge))
  xy<-rbind(left$xy[nrow(left$xy):1L,,drop=FALSE],right$xy[-1L,,drop=FALSE])
  xy<-xy[c(TRUE,rowSums(abs(xy[-1L,,drop=FALSE]-xy[-nrow(xy),,drop=FALSE]))>1e-10),,drop=FALSE]
  list(xy=xy,length=sum(sqrt(rowSums((xy[-1L,,drop=FALSE]-xy[-nrow(xy),,drop=FALSE])^2))),
    junction_distance=min(left$junction,right$junction),
    topology_ambiguous=left$cycle || right$cycle || length(overlap)>0L)
}

assess_stream_edge_identity <- function(index,candidate_points,barrier_stream,
    barrier_tributary_to=NA_character_,local_half_length=50,sample_count=7,
    max_match_distance=25,min_support_length=40,min_coverage=0.8,
    max_heading_degrees=35,junction_distance=25,dominance_margin=5,return_samples=TRUE) {
  if(!inherits(candidate_points,"sf") || !"graph_edge_id" %in% names(candidate_points) ||
      is.na(sf::st_crs(candidate_points)) || any(sf::st_geometry_type(candidate_points)!="POINT") ||
      any(sf::st_is_empty(candidate_points))) stop("Candidates must be nonempty sf POINTs with graph_edge_id and a CRS")
  controls<-c(local_half_length,max_match_distance,min_support_length,junction_distance,dominance_margin)
  if(any(!is.finite(controls)) || any(controls<0) || local_half_length<=0 ||
      length(sample_count)!=1L || sample_count<5 || sample_count!=as.integer(sample_count) ||
      !is.finite(min_coverage) || min_coverage<=0 || min_coverage>1 ||
      !is.finite(max_heading_degrees) || max_heading_degrees<0 || max_heading_degrees>90) stop("Invalid stream identity controls")
  points<-sf::st_transform(candidate_points,sf::st_crs(index$edges))
  count<-nrow(points)
  recycle<-function(x) { if(length(x)==1L) rep(as.character(x),count) else {
    if(length(x)!=count) stop("Barrier name vectors must match candidate rows"); as.character(x) } }
  own_name<-recycle(barrier_stream); receiving_name<-recycle(barrier_tributary_to)
  edge_rows<-match(points$graph_edge_id,index$edges$graph_edge_id)
  if(anyNA(edge_rows)) stop("A candidate graph_edge_id is absent from this identity index")
  result<-data.frame(graph_edge_id=points$graph_edge_id,channel_chain_id=index$edges$channel_chain_id[edge_rows],
    barrier_stream=own_name,barrier_tributary_to=receiving_name,
    identity_status=rep("no_supported_reference",count),own_stream_match=rep(FALSE,count),
    receiving_stream_conflict=rep(FALSE,count),matched_stream_name=rep(NA_character_,count),
    matched_llid=rep(NA_character_,count),matched_route_key=rep(NA_character_,count),
    support_length_m=numeric(count),sample_count=integer(count),support_coverage=numeric(count),
    max_reference_distance_m=rep(NA_real_,count),reference_distance_m=rep(NA_real_,count),
    median_heading_degrees=rep(NA_real_,count),competing_ids=rep("",count),
    dominant_route=rep(FALSE,count),dominance_coverage=numeric(count),
    reference_competitor_count=integer(count),runner_up_coverage=rep(NA_real_,count),
    runner_up_median_distance_m=rep(NA_real_,count),
    best_reference_stream_name=rep(NA_character_,count),best_reference_llid=rep(NA_character_,count),
    distance_to_junction_m=rep(Inf,count),junction_ambiguous=rep(FALSE,count),
    stream_identity_valid=rep(FALSE,count),identity_valid_for_cut=rep(FALSE,count),
    candidate_identity_is_unknown=rep(TRUE,count),stringsAsFactors=FALSE)
  # Projected st_is_within_distance can compare every long reference route
  # against every point. Intersections use a spatial index instead. This
  # circumscribed diamond contains the entire radius needed for local support
  # plus matching tolerance; exact sample distances/headings still determine
  # eligibility below, so extra candidates cannot establish false identity.
  search_radius<-local_half_length+max_match_distance+1e-8
  search_regions<-sf::st_buffer(points,dist=search_radius*sqrt(2),nQuadSegs=1)
  nearby<-if(nrow(index$reference_routes)) sf::st_intersects(search_regions,index$reference_routes)
    else rep(list(integer()),count)
  # Keep the original circular neighborhood exactly, including diagnostics for
  # unsupported matches. Exact distances are evaluated only for indexed hits.
  for(i in seq_len(count)) if(length(nearby[[i]])) {
    rows<-nearby[[i]]
    distance<-as.numeric(sf::st_distance(sf::st_geometry(points)[i],
      sf::st_geometry(index$reference_routes)[rows]))
    nearby[[i]]<-rows[distance<=search_radius]
  }
  sample_audit<-list()
  tangent<-function(xy,measure,total) {
    width<-min(5,total/2)
    stream_identity_at(xy,min(total,measure+width))-stream_identity_at(xy,max(0,measure-width))
  }
  for(i in seq_len(count)) {
    edge<-edge_rows[i]
    projection<-stream_identity_project(index$coordinates[[edge]],sf::st_coordinates(points[i,])[1L,1:2])
    if(projection$distance>index$settings$node_tolerance_m+1e-6) {
      result$identity_status[i]<-"candidate_not_on_declared_edge"; next
    }
    support<-stream_identity_local_support(index,edge,projection$measure,local_half_length)
    result$support_length_m[i]<-support$length
    result$distance_to_junction_m[i]<-support$junction_distance
    result$junction_ambiguous[i]<-is.finite(support$junction_distance) && support$junction_distance<=junction_distance+1e-8
    if(support$topology_ambiguous) { result$identity_status[i]<-"local_topology_ambiguous"; next }
    if(support$length<min_support_length-1e-8 || nrow(support$xy)<2L) {
      result$identity_status[i]<-"insufficient_support_length"; next
    }
    measures<-seq(0,support$length,length.out=sample_count)
    sample_xy<-t(vapply(measures,function(m) stream_identity_at(support$xy,m),numeric(2)))
    headings<-lapply(measures,function(m) tangent(support$xy,m,support$length))
    samples<-sf::st_sfc(lapply(seq_len(sample_count),function(j) sf::st_point(sample_xy[j,])),crs=sf::st_crs(points))
    result$sample_count[i]<-sample_count
    route_candidates<-nearby[[i]]
    if(!length(route_candidates)) { result$identity_status[i]<-"no_nearby_reference"; next }
    scores<-lapply(route_candidates,function(route) {
      rows<-index$route_rows[[route]]
      nearest<-rows[sf::st_nearest_feature(samples,index$reference_lines[rows,])]
      distance<-angle<-numeric(sample_count)
      for(j in seq_len(sample_count)) {
        xy<-index$reference_coordinates[[nearest[j]]]
        at<-stream_identity_project(xy,sample_xy[j,])
        len<-sum(sqrt(rowSums((xy[-1L,,drop=FALSE]-xy[-nrow(xy),,drop=FALSE])^2)))
        ref_heading<-tangent(xy,at$measure,len)
        distance[j]<-at$distance
        norm<-sqrt(sum(headings[[j]]^2)*sum(ref_heading^2))
        angle[j]<-if(norm>1e-12) acos(min(1,max(0,abs(sum(headings[[j]]*ref_heading))/norm)))*180/pi else 90
      }
      supported<-distance<=max_match_distance+1e-8 & angle<=max_heading_degrees+1e-8
      list(route=route,coverage=mean(supported),distance=distance,angle=angle)
    })
    order_score<-order(-vapply(scores,function(x)x$coverage,numeric(1)),
      vapply(scores,function(x)median(x$distance),numeric(1)))
    best<-scores[[order_score[1L]]]
    result$support_coverage[i]<-best$coverage
    result$max_reference_distance_m[i]<-max(best$distance)
    result$reference_distance_m[i]<-median(best$distance)
    result$median_heading_degrees[i]<-median(best$angle)
    result$best_reference_stream_name[i]<-index$reference_routes$matched_stream_name[best$route]
    result$best_reference_llid[i]<-index$reference_routes$matched_llid[best$route]
    qualified<-which(vapply(scores,function(x)x$coverage>=min_coverage-1e-8,logical(1)))
    if(return_samples) sample_audit[[length(sample_audit)+1L]]<-sf::st_sf(candidate_row=i,
      graph_edge_id=points$graph_edge_id[i],sample_index=seq_len(sample_count),
      best_reference_llid=result$best_reference_llid[i],distance_m=best$distance,
      heading_degrees=best$angle,geometry=samples)
    if(!length(qualified)) next
    result$reference_competitor_count[i]<-length(qualified)-1L
    if(length(qualified)>1L) {
      dominance<-vapply(qualified,function(which_score) {
        proposed<-scores[[which_score]]
        runner_distance<-vapply(setdiff(qualified,which_score),function(other) scores[[other]]$distance,
          numeric(sample_count))
        if(is.null(dim(runner_distance))) runner_distance<-matrix(runner_distance,ncol=1L)
        closest_runner<-apply(runner_distance,1L,min)
        mean(proposed$distance+dominance_margin<=closest_runner+1e-8 &
          proposed$distance<=max_match_distance+1e-8 & proposed$angle<=max_heading_degrees+1e-8)
      },numeric(1))
      winners<-which(dominance>=min_coverage-1e-8)
      result$dominance_coverage[i]<-max(dominance)
      if(length(winners)!=1L) {
        result$identity_status[i]<-"competing_reference_routes"
        result$competing_ids[i]<-paste(index$reference_routes$route_key[route_candidates[qualified]],collapse=";")
        runner<-scores[[order_score[2L]]]
        result$runner_up_coverage[i]<-runner$coverage
        result$runner_up_median_distance_m[i]<-median(runner$distance)
        next
      }
      selected<-qualified[winners]
      others<-setdiff(qualified,selected)
      runner<-scores[[others[order(vapply(scores[others],function(x)median(x$distance),numeric(1)))[1L]]]]
      result$runner_up_coverage[i]<-runner$coverage
      result$runner_up_median_distance_m[i]<-median(runner$distance)
    } else {
      selected<-qualified
      result$dominance_coverage[i]<-1
    }
    result$dominant_route[i]<-TRUE
    match<-scores[[selected]]; route<-match$route
    result$support_coverage[i]<-match$coverage
    result$max_reference_distance_m[i]<-max(match$distance)
    result$reference_distance_m[i]<-median(match$distance)
    result$median_heading_degrees[i]<-median(match$angle)
    result$best_reference_stream_name[i]<-index$reference_routes$matched_stream_name[route]
    result$best_reference_llid[i]<-index$reference_routes$matched_llid[route]
    if(return_samples) {
      audit_row<-length(sample_audit)
      sample_audit[[audit_row]]$best_reference_llid<-result$best_reference_llid[i]
      sample_audit[[audit_row]]$distance_m<-match$distance
      sample_audit[[audit_row]]$heading_degrees<-match$angle
    }
    result$matched_stream_name[i]<-index$reference_routes$matched_stream_name[route]
    result$matched_llid[i]<-index$reference_routes$matched_llid[route]
    result$matched_route_key[i]<-index$reference_routes$route_key[route]
    if(!index$reference_routes$known_id[route] || is.na(stream_identity_name_key(result$matched_stream_name[i]))) {
      result$identity_status[i]<-"reference_identity_unknown"; next
    }
    own<-stream_identity_names_match(own_name[i],result$matched_stream_name[i])
    receiving<-stream_identity_names_match(receiving_name[i],result$matched_stream_name[i])
    result$own_stream_match[i]<-own
    result$receiving_stream_conflict[i]<-receiving && !own
    if(own) {
      result$identity_status[i]<-"own_stream_confirmed"
      result$stream_identity_valid[i]<-TRUE
      result$candidate_identity_is_unknown[i]<-FALSE
    } else if(receiving) {
      result$identity_status[i]<-"receiving_stream_conflict"
      result$candidate_identity_is_unknown[i]<-FALSE
    } else if(is.na(stream_identity_name_key(own_name[i]))) {
      result$identity_status[i]<-"barrier_stream_unknown"
    } else {
      result$identity_status[i]<-"different_stream_confirmed"
      result$candidate_identity_is_unknown[i]<-FALSE
    }
    result$identity_valid_for_cut[i]<-result$stream_identity_valid[i] && !result$junction_ambiguous[i]
  }
  attr(result,"sample_points")<-if(length(sample_audit)) do.call(rbind,sample_audit) else
    sf::st_sf(candidate_row=integer(),graph_edge_id=integer(),sample_index=integer(),
      best_reference_llid=character(),distance_m=numeric(),heading_degrees=numeric(),geometry=sf::st_sfc(crs=sf::st_crs(points)))
  attr(result,"settings")<-list(local_half_length_m=local_half_length,sample_count=sample_count,
    max_match_distance_m=max_match_distance,min_support_length_m=min_support_length,
    min_coverage=min_coverage,max_heading_degrees=max_heading_degrees,
    junction_distance_m=junction_distance,dominance_margin_m=dominance_margin,
    node_tolerance_m=index$settings$node_tolerance_m)
  result
}
