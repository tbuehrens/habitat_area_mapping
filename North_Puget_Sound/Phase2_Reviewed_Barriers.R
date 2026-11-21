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
# Reapply approved masks and user-reviewed corrections to the full raw model +
# SWIFD network. Both earlier review folders are read only.
for (file in c("arcgis.R", "noaa_populations.R", "swifd_sources.R",
  "source_classification.R", "barrier_stream_identity.R", "stream_edge_identity.R", "stream_reference.R",
  "network.R", "habitat_workflow.R", "barrier_metadata.R", "phase_barrier_cuts.R",
  "upstream_basins.R", "nldi_delineation.R", "phase1_exports.R", "phase2_targeted_barriers.R",
  "phase2_dam_tolt.R", "phase2_stillaguamish.R", "phase2_exports.R")) source(file.path("R", file))
phase2_results <- run_phase2_reviewed_corrections()
})
