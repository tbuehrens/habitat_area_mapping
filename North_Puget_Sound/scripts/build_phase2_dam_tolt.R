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
# Run from repository root. Default reuses existing source/basin caches.
# --prepare-sources fetches missing official site/PDF and repeats the documented
# map registration/strict channel matching. --fetch-basins retrieves only missing
# per-request USGS responses. Earlier output folders are always read only.
arguments<-commandArgs(trailingOnly=TRUE)
if('--prepare-sources' %in% arguments) {
  dir.create('spatial_data/phase2',recursive=TRUE,showWarnings=FALSE)
  download<-function(url,path) {
    if(file.exists(path)&&file.info(path)$size>0) return(invisible(path))
    status<-system2('curl.exe',c('--ssl-no-revoke','--fail','--silent','--show-error','--location',
      '--max-time','120','--output',shQuote(path),shQuote(url)))
    if(status!=0L) stop('Official source retrieval failed: ',url)
    invisible(path)
  }
  download('https://api.water.usgs.gov/nldi/linked-data/nwissite/USGS-12137300?f=json',
    'spatial_data/phase2/spada_usgs_site.json')
  download('https://fercrelicensing.seattle.gov/library/assets/docs/SFT_PAD_FINAL.pdf',
    'spatial_data/phase2/SFT_PAD_FINAL.pdf')
  named<-readRDS('spatial_data/north_puget_named_stream_reference.rds')
  sf::st_write(named[as.character(named$LLID)=='1218201476970',],
    'spatial_data/phase2/sf_tolt_reference.geojson',delete_dsn=TRUE,quiet=TRUE)
  source('scripts/phase2_tolt_map_registration.R')
  source('scripts/phase2_prepare_dam_tolt.R')
}
if('--fetch-basins' %in% arguments) {
  status<-system2('python',c('scripts/fetch_upstream_basins.py','--requests',
    'spatial_data/phase2/dam_tolt_basins/requests.json','--cache-dir',
    'spatial_data/phase2/dam_tolt_basins','--workers','2'))
  if(status!=0L) stop('USGS dam/Tolt basin retrieval failed')
}
for(file in c('arcgis.R','source_classification.R','network.R','barrier_stream_identity.R','stream_edge_identity.R',
 'barrier_review.R','stream_reference.R','phase_barrier_cuts.R','upstream_basins.R','nldi_delineation.R',
 'phase2_dam_tolt.R')) source(file.path('R',file))
library(sf)
barriers<-readRDS('spatial_data/phase2/dam_tolt_barriers.rds')
gpkg<-'outputs/review_swifd_connections/North_Puget_Sound_Steelhead.gpkg'
edges<-st_read(gpkg,layer='stream_matching_edges',quiet=TRUE)
network<-readRDS('spatial_data/north_puget_swifd_bridges.rds')$bridges$network
delineations<-load_nldi_delineations(barriers,edges,cache_dir='spatial_data/phase2/dam_tolt_basins')
stopifnot(nrow(delineations$candidate_masks)==2)
masks<-delineations$candidate_masks
area<-as.numeric(st_area(masks))/2589988.110336
stopifnot(abs(area[masks$review_number==912]/68.3-1)<.05)
# A permanent at-dam station links this split to the independently published
# Culmback drainage area; its current sourceCOMID is checked in preparation.
masks$official_area_sqmi<-ifelse(masks$review_number==912,68.3,NA_real_)
masks$official_area_source_url<-ifelse(masks$review_number==912,
 'https://waterdata.usgs.gov/nwis/uv?legacy=1&site_no=12137300',NA_character_)
named<-load_stream_identity_reference(cache_dir='spatial_data')
direction<-derive_downstream_anchors(barriers,edges,named,
 marine_geometry=readRDS('spatial_data/marine_north.rds'))
anchors<-direction$anchors
extra<-delineations$downstream_anchors[!delineations$downstream_anchors$review_record_key %in% anchors$review_record_key,]
anchors<-dplyr::bind_rows(anchors,extra)
stopifnot(all(barriers$review_record_key %in% anchors$review_record_key))
cuts<-cut_reviewed_barriers(network,barriers,edges,anchors)
saveRDS(list(cuts=cuts,anchors=anchors,masks=masks,delineations=delineations,direction=direction),
 'spatial_data/phase2/dam_tolt_validation_debug.rds')
terminal<-validate_phase2_terminal_dam(masks,cuts$network,barriers,edges)
print(terminal$audit)
checked<-validate_nldi_basin_masks(masks[masks$review_number!=912,],terminal$network,barriers)
print(checked$audit[,c('review_number','mask_approved','approval_status','approval_reason')])
stopifnot(nrow(checked$masks)==1,nrow(terminal$masks)==1)
audit<-dplyr::left_join(delineations$audit,st_drop_geometry(barriers)[,c('review_record_key','source_url','source_coordinate_precision','source_coordinate_interpretation','phase2_source_note')],by='review_record_key')
audit$independent_area_sqmi<-ifelse(audit$review_number==912,68.3,NA_real_)
audit$calculated_area_sqmi<-area[match(audit$review_record_key,masks$review_record_key)]
audit$source_distinction<-ifelse(audit$review_number==913,
 'Actual City-mapped RM7.8 SFT Falls, ~0.5mi belowdam; WDFW170 near dam is separate and not substituted',
 'Culmback at official NWIS right-bank damstation; user explicitly requested damcut')
notes<-c('NF Tolt222 retained as user-reviewed limit; remove all six lower NF Tolt Total masks and graphcuts to restore full raw+SWIFD upstream to222.',
 'SF Tolt913 is the permanent natural waterfall RM7.8,73ft, digitized from authoritative City2024 Fig4-86. City confirms dam about0.5mi upstream. WDFW170 is near dam and remains separate.',
 'Falls point is map-digitized, not surveyed; fine named-stream overlay and strict physical match retained. No generic identity/course distance threshold relaxed.',
 'Culmback912 uses official current NWIS12137300 point described on rightbank at dam; mask agrees within5% of published68.3mi2.',
 'Both masks require unique full drainageBasin, published area range, original50m primary snap, and independent local downstream proof. Root must revalidate against combined Phase2 graph before exporting.')
bundle<-list(barriers=barriers,masks=masks,anchors=anchors,audit=audit,
 remove_review_numbers=phase2_tolt_remove_review_numbers(),notes=notes,
 local_validation=checked$audit,terminal_validation=terminal$audit,direction_audit=direction$audit,
 source_files=c('spatial_data/phase2/SFT_PAD_FINAL.pdf','spatial_data/phase2/spada_usgs_site.json',
 'spatial_data/phase2/tolt_map_registration.rds','spatial_data/phase2/tolt_map_registration.png'))
saveRDS(bundle,'spatial_data/phase2/dam_tolt.rds')
write.csv(audit,'spatial_data/phase2/dam_tolt_audit.csv',row.names=FALSE)
writeLines(notes,'spatial_data/phase2/dam_tolt_notes.txt')
print(st_drop_geometry(anchors)[,intersect(c('review_record_key','anchor_id','direction_basis'),names(anchors))])
print(audit[,c('review_number','status','calculated_area_sqmi','independent_area_sqmi')])
})
