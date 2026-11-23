for(name in c('arcgis','source_classification','barrier_stream_identity','stream_edge_identity',
 'stream_reference','network','barrier_metadata','phase_barrier_cuts','upstream_basins','nldi_delineation'))
 source(file.path('R',paste0(name,'.R')))
library(sf)
directory<-'spatial_data/stillaguamish_review'
dir.create(directory,recursive=TRUE,showWarnings=FALSE)
points<-st_read('outputs/phase3_tidal_barriers/North_Puget_Sound_Steelhead_phase3.gpkg',
 layer='barrier_characteristics',quiet=TRUE)
points<-points[points$review_number %in% c(628L,633L,634L),]
edges<-st_read('outputs/review_swifd_connections/North_Puget_Sound_Steelhead.gpkg',
 layer='stream_matching_edges',quiet=TRUE)
total<-points[points$review_number %in% c(628L,634L),]
delineations<-load_nldi_delineations(total,edges)
direction<-derive_downstream_anchors(total,edges,load_stream_identity_reference(),
 marine_geometry=readRDS('spatial_data/marine_north.rds'))
print(direction$audit)
anchors<-dplyr::bind_rows(direction$anchors,
 delineations$downstream_anchors[!delineations$downstream_anchors$review_record_key %in% direction$anchors$review_record_key,])
print(st_drop_geometry(anchors))
network<-readRDS('spatial_data/north_puget_swifd_bridges.rds')$bridges$network
total$explicit_total_passage<-total$confirmed_total_for_scenario<-TRUE
total$phase1_selected<-TRUE
cuts<-cut_reviewed_barriers(network,total,edges,anchors)
checked<-validate_nldi_basin_masks(delineations$candidate_masks,cuts$network,total)
print(checked$audit[,c('review_number','approval_status','approval_reason',
 'proven_upstream_outside_mask_m','proven_downstream_inside_mask_m','max_local_disagreement_distance_m')])
mask<-delineations$candidate_masks[delineations$candidate_masks$review_number %in% 628L,]
upstream<-cuts$network[upstream_has_key(cuts$network$removed_by_barrier,'6:53933'),]
missing<-upstream_line_parts(suppressWarnings(st_difference(upstream,st_geometry(mask))))
missing$length_m<-as.numeric(st_length(missing))
missing$longitude<-st_coordinates(st_transform(st_centroid(st_geometry(missing)),4326))[,1]
missing$latitude<-st_coordinates(st_transform(st_centroid(st_geometry(missing)),4326))[,2]
print(st_drop_geometry(missing)[,c('source_class','graph_edge_id','length_m','longitude','latitude')])
named<-load_stream_identity_reference()
near<-st_nearest_feature(st_centroid(st_geometry(missing)),named)
print(st_drop_geometry(named[near,])[,intersect(c('LLID','reference_stream_name','GNIS_STREAMNAME','LLID_STREAMNAME'),names(named))])
if(length(near))print(st_distance(st_centroid(st_geometry(missing)),named[near,],by_element=TRUE))
png(file.path(directory,'pilchuck_basin_diagnostics.png'),width=1400,height=1000)
plot(st_geometry(mask),col='#ddf2ee',border='#098080',main='Pilchuck Creek: existing628 watershed and graph-upstream discrepancy')
plot(st_geometry(network),col='#b0b0b0',lwd=.6,add=TRUE)
plot(st_geometry(missing),col='red',lwd=3,add=TRUE)
plot(st_geometry(points),pch=21,bg='orange',cex=1.2,add=TRUE)
text(st_coordinates(points),labels=points$review_number,pos=3)
dev.off()
own<-edges[edges$graph_edge_id %in% 691L,]
xy<-unclass(st_geometry(own)[[1L]])[,1:2]
measure<-vapply(seq_len(nrow(points)),function(i)stream_identity_project(xy,st_coordinates(points[i,])[1L,1:2])$measure,numeric(1))
print(data.frame(review_number=points$review_number,measure_m=measure))
saveRDS(list(points=points,edges=edges,delineations=delineations,direction=direction,anchors=anchors,
 cuts=cuts,validation=checked,upstream_outside=missing),file.path(directory,'pilchuck_diagnostics.rds'))
