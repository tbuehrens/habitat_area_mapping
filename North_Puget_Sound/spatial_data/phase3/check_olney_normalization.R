for(name in c('arcgis','source_classification','barrier_stream_identity','stream_edge_identity',
 'stream_reference','network','barrier_metadata','phase_barrier_cuts','upstream_basins',
 'nldi_delineation','phase3_olney_barriers')) source(file.path('R',paste0(name,'.R')))
library(sf)
d<-readRDS('spatial_data/phase3/olney_diagnostics.rds')
b<-readRDS('spatial_data/phase3/olney_barriers.rds')
edges<-st_read('outputs/review_swifd_connections/North_Puget_Sound_Steelhead.gpkg',layer='stream_matching_edges',quiet=TRUE)
checked<-validate_nldi_basin_masks(b$masks,d$network,b$barriers)
print(checked$audit[,c('review_number','mask_approved','approval_reason','proven_downstream_inside_mask_m','max_local_disagreement_distance_m')])
print(st_geometry_type(d$masks));print(st_geometry_type(b$masks))
raw<-readRDS('spatial_data/north_puget_swifd_bridges.rds')$bridges$network
before_overlap<-upstream_line_parts(suppressWarnings(st_intersection(d$downstream_overlap,st_geometry(b$masks))))
print(st_drop_geometry(before_overlap)[,c('length_m','distance_from331_m')])
print(st_length(before_overlap))
old379<-st_read('outputs/phase2_reviewed_barriers/North_Puget_Sound_Steelhead_phase2.gpkg',layer='upstream_basin_masks',quiet=TRUE)
old379<-old379[old379$review_number %in% 379L,]
extra<-st_difference(st_geometry(old379),st_geometry(b$masks))
extra_habitat<-upstream_line_parts(suppressWarnings(st_intersection(raw[raw$measure_habitat,],extra)))
cat('Habitat in379 outside331 polygon meters:',sum(as.numeric(st_length(extra_habitat))),'\n')
