source("R/barrier_metadata.R")
source("R/phase1_exports.R")
b <- sf::st_read("outputs/review_swifd_connections/North_Puget_Sound_Steelhead.gpkg", layer="barriers_snapped", quiet=TRUE)
e <- load_barrier_report_evidence()
m <- lapply(c(5,6),function(i) jsonlite::fromJSON(paste0("spatial_data/barrier_layer_",i,".json"),simplifyVector=FALSE))
a <- phase1_select_barriers(audit_barrier_metadata(b,m,e)$decoded)
for (field in names(a)) b[[field]] <- a[[field]]
selected <- b[b$phase1_selected,]
selected$public_explicit_total_passage <- selected$explicit_total_passage
selected$explicit_total_passage <- selected$confirmed_total_for_scenario
saveRDS(selected,"spatial_data/phase1_validation/phase1_selected_totals.rds")
saveRDS(a,"spatial_data/phase1_validation/phase1_all_decisions.rds")
utils::write.csv(sf::st_drop_geometry(selected),"spatial_data/phase1_validation/phase1_selected_totals.csv",row.names=FALSE)
print(table(a$phase1_selected))
print(table(a$verification[a$phase1_selected]))
cat("Gorge number",a$review_number[a$override_future_cut],"\n")
