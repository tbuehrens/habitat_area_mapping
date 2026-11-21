for (file in c("arcgis.R", "noaa_populations.R", "swifd_sources.R", "source_classification.R",
  "barrier_stream_identity.R", "stream_edge_identity.R", "stream_reference.R", "network.R",
  "habitat_workflow.R", "barrier_metadata.R", "phase_barrier_cuts.R", "upstream_basins.R",
  "nldi_delineation.R", "phase1_exports.R", "phase2_targeted_barriers.R", "phase2_dam_tolt.R",
  "phase2_stillaguamish.R", "phase2_exports.R")) source(file.path("R", file))
target <- load_phase2_targeted_barriers()
custom <- load_phase2_stillaguamish()
old <- phase2_geometry(sf::st_read("outputs/phase1_total_barriers/North_Puget_Sound_Steelhead_phase1.gpkg",
  layer = "barrier_characteristics", quiet = TRUE))
target$barriers <- phase2_geometry(target$barriers)
merged <- phase2_bind_rows(old[!old$review_record_key %in% target$barriers$review_record_key, ], target$barriers)
stopifnot(nrow(merged) == 911L, !anyDuplicated(merged$review_number), nrow(target$masks) == 2L,
  all(target$masks$local_outlet_precision_validated), custom$population$pop_id == 1001L)
print(sf::st_drop_geometry(target$barriers)[, c("review_number", "review_record_key", "report_blockage",
  "override_future_cut", "snap_distance_m", "phase2_review_action")], row.names = FALSE)
cat("Targeted geometry/schema/numbering and custom source hashes PASS.\n")
dam <- load_phase2_dam_tolt()
print(data.frame(key = dam$masks$review_record_key, type = sf::st_geometry_type(dam$masks)))
print(data.frame(key = target$masks$review_record_key, type = sf::st_geometry_type(target$masks)))
