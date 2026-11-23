for (file in c("project_paths.R", "habitat_workflow.R", "phase1_exports.R", "phase2_exports.R", "phase3_exports.R")) source(file.path("R", file))
habitat_require_packages()
table_file <- "outputs/phase3_tidal_barriers/population_phase3_miles.csv"
table <- read.csv(table_file, stringsAsFactors = FALSE)
for (field in c("new_barrier_removed_miles", "change_from_phase2_miles")) table[[field]][abs(table[[field]]) < 1e-10] <- 0
write.csv(table, table_file, row.names = FALSE, na = "")
warnings_seen <- character()
withCallingHandlers(refresh_phase3_maps(), warning = function(w) {
  warnings_seen <<- c(warnings_seen, conditionMessage(w))
  invokeRestart("muffleWarning")
})
writeLines(unique(warnings_seen), "spatial_data/phase3/map_render_warnings.txt")
cat("Maps refreshed; upstream/tidal geometry and real mileage unchanged. Render warnings:", length(warnings_seen), "\n")
