source(file.path("R","barrier_stream_identity.R"))
source(file.path("R","stream_edge_identity.R"))
suppressPackageStartupMessages(library(sf))
line <- function(y,x1=0,x2=200) st_linestring(rbind(c(500000+x1,5400000+y),c(500000+x2,5400000+y)))
physical <- st_sf(source_class="prediction",measure_habitat=TRUE,
  geometry=st_sfc(line(0),crs=26910))
parallel_reference <- function(offset) st_sf(LLID=c("own","neighbor"),
  LLID_STRM_NAME=c("Own Creek","Other Creek"),geometry=st_sfc(line(0),line(offset),crs=26910))
index <- build_stream_identity_index(physical,parallel_reference(20))
candidate <- st_sf(graph_edge_id=index$edges$graph_edge_id[1L],
  geometry=st_sfc(st_point(c(500100,5400000)),crs=26910))
dominant <- assess_stream_edge_identity(index,candidate,"Own Cr")
stopifnot(dominant$identity_valid_for_cut,dominant$dominant_route,
  dominant$reference_competitor_count==1L,dominant$dominance_coverage==1,
  abs(dominant$runner_up_median_distance_m-20)<1e-8,
  dominant$runner_up_coverage==1,dominant$matched_llid=="own",dominant$competing_ids=="")
near_tie <- build_stream_identity_index(physical,parallel_reference(3))
ambiguous <- assess_stream_edge_identity(near_tie,candidate,"Own Creek")
stopifnot(!ambiguous$identity_valid_for_cut,!ambiguous$own_stream_match,
  ambiguous$candidate_identity_is_unknown,
  ambiguous$identity_status=="competing_reference_routes",
  ambiguous$dominance_coverage==0,grepl("neighbor",ambiguous$competing_ids))

# Degree-two continuation belongs to one chain and can supply enough local
# support for a short noded piece. A real fork separates the channel chains.
chain <- st_sf(source_class="prediction",measure_habitat=TRUE,
  geometry=st_sfc(line(0,0,70),line(0,70,80),line(0,80,200),crs=26910))
own_reference <- parallel_reference(20)[1,]
chain_index <- build_stream_identity_index(chain,own_reference)
stopifnot(length(unique(chain_index$edges$channel_chain_id))==1L)
short_edge <- which.min(chain_index$edges$length_m)
chain_candidate <- st_sf(graph_edge_id=chain_index$edges$graph_edge_id[short_edge],
  geometry=st_sfc(st_point(c(500075,5400000)),crs=26910))
supported <- assess_stream_edge_identity(chain_index,chain_candidate,"Own Creek")
stopifnot(supported$identity_valid_for_cut,supported$support_length_m>=99.99,
  supported$sample_count==7L,nrow(attr(supported,"sample_points"))==7L)
fork <- st_sf(source_class="prediction",measure_habitat=TRUE,
  geometry=st_sfc(st_linestring(rbind(c(500075,5400000),c(500075,5400100))),crs=26910))
fork_index <- build_stream_identity_index(rbind(chain,fork),own_reference)
stopifnot(nrow(fork_index$junction_nodes)==1L,
  length(unique(fork_index$edges$channel_chain_id))==3L)
touching <- which(as.numeric(st_distance(fork_index$edges,
  st_sfc(st_point(c(500075,5400000)),crs=26910)))<1e-8)[1L]
fork_candidate <- st_sf(graph_edge_id=fork_index$edges$graph_edge_id[touching],
  geometry=st_sfc(st_point(c(500075,5400000)),crs=26910))
blocked <- assess_stream_edge_identity(fork_index,fork_candidate,"Own Creek")
stopifnot(blocked$junction_ambiguous,!blocked$identity_valid_for_cut,
  blocked$graph_edge_id==fork_candidate$graph_edge_id)
settings <- attr(dominant,"settings")
stopifnot(settings$dominance_margin_m==5,settings$max_match_distance_m==25,
  settings$min_support_length_m==40)
postfix_reference <- own_reference
postfix_reference$LLID_STRM_NAME <- "Stillaguamish River, South Fork"
postfix_index <- build_stream_identity_index(physical,postfix_reference)
postfix_own <- assess_stream_edge_identity(postfix_index,candidate,"SF Stillaguamish R")
postfix_receiving <- assess_stream_edge_identity(postfix_index,candidate,"unnamed","SF Stilly R")
stopifnot(postfix_own$identity_valid_for_cut,postfix_receiving$receiving_stream_conflict,
  !postfix_receiving$identity_valid_for_cut,
  stream_identity_names_match("NF Nooksack R","Nooksack River, North Fork"),
  !stream_identity_names_match("Stillaguamish River","Stillaguamish River, South Fork"))
cat("Stream-edge identity dominance, degree-two support, chain separation, and junction tests passed\n")
