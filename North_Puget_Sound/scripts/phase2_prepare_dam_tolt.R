# Locate this region when invoked from the repository root or this folder.
local({
  source_files <- unlist(lapply(sys.frames(), function(frame) frame$ofile), use.names = FALSE)
  cli_files <- sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE))
  starts <- unique(c(dirname(c(source_files, cli_files)), getwd()))
  bootstrap <- NULL
  for (start in starts) {
    current <- normalizePath(start, winslash = "/", mustWork = FALSE)
    repeat {
      candidates <- c(file.path(current, "workflow_bootstrap.R"),
        file.path(current, "North_Puget_Sound", "workflow_bootstrap.R"))
      existing <- candidates[file.exists(candidates)]
      if (length(existing)) { bootstrap <- existing[1L]; break }
      parent <- dirname(current)
      if (identical(parent, current)) break
      current <- parent
    }
    if (!is.null(bootstrap)) break
  }
  if (is.null(bootstrap)) stop("Cannot locate North_Puget_Sound/workflow_bootstrap.R")
  source(bootstrap, local = FALSE)
})
north_puget_workflow({
for(file in c('arcgis.R','source_classification.R','network.R','barrier_stream_identity.R','stream_edge_identity.R',
 'barrier_review.R','stream_reference.R','phase_barrier_cuts.R','upstream_basins.R','nldi_delineation.R',
 'phase2_dam_tolt.R')) source(file.path('R',file))
library(sf)
gpkg<-'outputs/review_swifd_connections/North_Puget_Sound_Steelhead.gpkg'
edges<-st_read(gpkg,layer='stream_matching_edges',quiet=TRUE)
previous<-st_read(gpkg,layer='barriers_snapped',quiet=TRUE)
network<-readRDS('spatial_data/north_puget_swifd_bridges.rds')$bridges$network
named<-load_stream_identity_reference(cache_dir='spatial_data')
site<-st_read('spatial_data/phase2/spada_usgs_site.json',quiet=TRUE)
stopifnot(nrow(site)==1,site$identifier=='USGS-12137300',as.character(site$comid)=='23963709')
fall<-readRDS('spatial_data/phase2/tolt_map_registration.rds')$fall
source_url<-c('https://waterdata.usgs.gov/nwis/uv?legacy=1&site_no=12137300',
 'https://fercrelicensing.seattle.gov/library/assets/docs/SFT_PAD_FINAL.pdf')
new<-st_sf(SiteId=NA_character_,dam_name=c('Culmback Dam','South Fork Tolt Falls'),
 FeatureType=c('Dam','Natural Barrier'),StreamName=c('Sultan River','South Fork Tolt River'),
 TributaryToName=NA_character_,source_layer='user_override',
 barrier_key=c('user_override:Culmback_Dam','user_override:South_Fork_Tolt_Falls'),
 verification=c('Official USGS at-dam station; user-requested cutoff',
  'Official City map-digitized waterfall; user-requested cutoff'),
 source_url=source_url,override_future_cut=TRUE,
 override_reason=c('User explicitly requested Culmback Dam and complete Spada upstream exclusion',
  'User explicitly requested the natural SFT waterfall below the reservoir; do not substitute the dam'),
 source_coordinate_precision=c('Official current NWIS station coordinate, right bank at Culmback Dam',
  'Digitized from City2024 PAD Fig4-86/p318; river-channel registration, approximately12.3m per map pixel'),
 source_coordinate_interpretation=c('Official NWIS longitude/latitude EPSG4326',
  'Registered map falls symbol at pixel949,434 on150% rendering; UTM26910; not a surveyed or GPS coordinate'),
 geometry=c(st_geometry(st_transform(site,26910)),fall))
result<-prepare_barrier_review(new,network,named,
 previous_review_numbers=data.frame(review_record_key=new$barrier_key,review_number=c(912L,913L)))
stopifnot(identical(result$stream_edges$graph_edge_id,edges$graph_edge_id),
 identical(st_as_binary(st_geometry(result$stream_edges)),st_as_binary(st_geometry(edges))))
barriers<-result$points
print(st_drop_geometry(barriers)[,c('review_number','StreamName','graph_edge_id','snap_distance_m','snap_accepted','matched_stream_name','matched_llid','identity_review_status')])
stopifnot(nrow(barriers)==2L,all(barriers$snap_accepted),all(barriers$snap_distance_m<=50))
template<-st_read('outputs/phase1_total_barriers/North_Puget_Sound_Steelhead_phase1.gpkg',
 layer='barrier_characteristics',quiet=TRUE)
names(barriers)[names(barriers)==attr(barriers,'sf_column')]<-attr(template,'sf_column')
attr(barriers,'sf_column')<-attr(template,'sf_column')
barriers<-dplyr::bind_rows(template[FALSE,],barriers)
barriers$phase1_selected<-TRUE
barriers$confirmed_total_for_scenario<-TRUE
barriers$explicit_total_passage<-FALSE
barriers$passability_review_status<-'explicit_user_override'
barriers$total_passage_evidence<-barriers$override_reason
barriers$total_evidence_conflict<-FALSE
barriers$passability_evidence_conflict<-FALSE
barriers$report_blockage_conflict<-FALSE
barriers$report_blockage<-c(NA,'Total')
barriers$total_evidence_basis<-barriers$override_reason
barriers$eligible_for_future_cut<-TRUE
barriers$reported_feature_type<-barriers$FeatureType
barriers$report_natural_type<-c(NA,'Waterfall')
barriers$reported_height_m<-c(NA,73*.3048)
barriers$reported_height_quality<-c('Not used to infer passability','City2024 PAD states73ft; not a WDFW height measurement')
barriers$report_url<-source_url
barriers$report_source_md5<-unname(tools::md5sum(c('spatial_data/phase2/spada_usgs_site.json','spatial_data/phase2/SFT_PAD_FINAL.pdf')))
barriers$report_pdf<-c(NA,'spatial_data/phase2/SFT_PAD_FINAL.pdf')
barriers$report_assessment_pages<-c(NA,'City PAD printed217,317-318 (PDF239,339-340)')
barriers$report_comments<-c('Official at-dam station drainage benchmark68.3mi2; explicit user dam override',
 'City PAD RM7.8,73ft natural falls; dam~0.5mi upstream. Distinct from WDFW170 at dam toe.')
barriers$phase2_source_note<-barriers$report_comments
registration<-readRDS('spatial_data/phase2/tolt_map_registration.rds')
barriers$phase2_map_registration_median_distance_m<-c(NA,median(registration$distances))
barriers$phase2_map_registration_max_distance_m<-c(NA,max(registration$distances))
barriers$phase2_map_registration_falls_to_reference_m<-c(NA,registration$distances[nrow(registration$pixel)-2L])
barriers$phase2_map_registration_note<-c(NA,
 'Map-to-channel registration of42 manually traced river vertices; residuals characterize tracing/reference disagreement, not surveyed positional accuracy. Local falls-symbol-to-reference4.17m. See registration CSV/PNG and sourcefigure.')
saveRDS(barriers,'spatial_data/phase2/dam_tolt_barriers.rds')
saveRDS(result$candidate_matches,'spatial_data/phase2/dam_tolt_channel_matches.rds')
write.csv(st_drop_geometry(result$candidate_matches),'spatial_data/phase2/dam_tolt_channel_matches.csv',row.names=FALSE)
requests_points<-barriers
requests_points$override_future_cut<-FALSE
prepare_phase1_basin_requests(requests_points,edges,cache_dir='spatial_data/phase2/dam_tolt_basins')
})
