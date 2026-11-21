try(source("tests/validate_phase2_custom_stillaguamish.R"))
cat("Custom rows",nrow(custom_extent),"source rows",nrow(source),"\n")
cat("Lengths",length_m(source),length_m(custom_extent),"\n")
cat("Missing source",missing_m(source,custom_extent),"missingoutput",missing_m(custom_extent,source),"\n")
for(tolerance in c(1e-5,.0001,.001,.01,.1,1)) {
 a<-suppressWarnings(st_difference(st_union(st_geometry(source)),st_buffer(st_union(st_geometry(custom_extent)),tolerance)))
 b<-suppressWarnings(st_difference(st_union(st_geometry(custom_extent)),st_buffer(st_union(st_geometry(source)),tolerance)))
 cat("Tolerance",tolerance,"source",length_m(a),"output",length_m(b),"\n")
}
a<-suppressWarnings(st_difference(st_union(st_geometry(source)),st_buffer(st_union(st_geometry(custom_extent)),1e-5)))
saveRDS(list(source=source,extent=custom_extent,missing=a),"spatial_data/phase2/custom_partition_diagnostic.rds")
