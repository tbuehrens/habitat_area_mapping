library(sf)
x<-readRDS("spatial_data/phase2/sf_restore_missing_diagnostic.rds")
file<-"outputs/phase2_reviewed_barriers/North_Puget_Sound_Steelhead_phase2.gpkg"
remaining<-st_read(file,layer="phase2_remaining",quiet=TRUE)
remaining<-remaining[remaining$pop_id==292 & remaining$measure_habitat,]
barriers<-st_read(file,layer="barrier_characteristics",quiet=TRUE)
point<-barriers[barriers$review_number==810,]
samples<-st_line_sample(x$missing,sample=seq(0,1,length.out=11))
samples<-suppressWarnings(st_cast(samples,"POINT"))
cat("Sampledistances toremaining",as.numeric(st_distance(samples,st_union(st_geometry(remaining)))),"\n")
cat("Sampledistancesto810",as.numeric(st_distance(samples,point)),"\n")
oldfile<-"outputs/phase1_total_barriers/North_Puget_Sound_Steelhead_phase1.gpkg"
oldremaining<-st_read(oldfile,layer="phase1_remaining",quiet=TRUE)
oldremaining<-oldremaining[oldremaining$pop_id==292 & oldremaining$measure_habitat,]
oldremoved<-st_read(oldfile,layer="phase1_removed_upstream",quiet=TRUE)
oldremoved<-oldremoved[oldremoved$pop_id==292 & oldremoved$measure_habitat,]
cat("Sampledistancestooldremaining",as.numeric(st_distance(samples,st_union(st_geometry(oldremaining)))),"\n")
cat("Sampledistancestooldremoved",as.numeric(st_distance(samples,st_union(st_geometry(oldremoved)))),"\n")
near<-st_nearest_feature(samples,oldremoved)
print(unique(st_drop_geometry(oldremoved[near,c("removed_by_barrier","watershed_removed_by_barrier","graph_edge_id")])) )
print(st_as_text(st_geometry(point)),digits=15)
print(st_as_text(x$missing),digits=15)
