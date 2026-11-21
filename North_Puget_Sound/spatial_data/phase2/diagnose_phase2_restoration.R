try(source("tests/validate_phase2_custom_stillaguamish.R"))
cat("SF target",length_m(expected)/1609.344,"missing",missing_m(expected,sf_remaining),"\n")
missing<-suppressWarnings(st_difference(st_union(st_geometry(expected)),st_buffer(st_union(st_geometry(sf_remaining)),1e-4)))
missing<-suppressWarnings(st_cast(missing,"LINESTRING"))
missing<-missing[!st_is_empty(missing)]
print(st_coordinates(missing))
print(st_as_text(missing))
rm<-removed[removed$pop_id==292 & removed$measure_habitat,]
near<-st_nearest_feature(missing,rm)
print(st_drop_geometry(rm[near,intersect(c("pop_id","source_class","removed_by_barrier","watershed_removed_by_barrier","graph_edge_id","length_m"),names(rm))]))
cat("Distances to removed",as.numeric(st_distance(missing,rm[near,],by_element=TRUE)),"\n")
dec<-read_layer("barrier_characteristics")
print(st_drop_geometry(dec[dec$review_number %in% c(797,798,810,833),intersect(c("review_number","review_record_key","phase2_applied"),names(dec))]))
saveRDS(list(missing=missing,removed=rm[near,]),"spatial_data/phase2/sf_restore_missing_diagnostic.rds")
