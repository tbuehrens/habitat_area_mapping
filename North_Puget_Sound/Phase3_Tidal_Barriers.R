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
# Preserve prior scientific products and write the tidal/barrier review separately.
for (file in c("arcgis.R", "noaa_populations.R", "swifd_sources.R", "source_classification.R",
  "barrier_stream_identity.R", "stream_edge_identity.R", "stream_reference.R", "network.R",
  "habitat_workflow.R", "barrier_metadata.R", "phase_barrier_cuts.R", "upstream_basins.R",
  "nldi_delineation.R", "phase1_exports.R", "phase2_targeted_barriers.R", "phase2_dam_tolt.R",
  "phase2_stillaguamish.R", "phase2_exports.R", "phase3_woods_barriers.R", "phase3_olney_barriers.R",
  "phase3_tidal_sources.R", "phase3_nwi_tidal.R", "phase3_freshwater_tidal.R",
  "french_creek_review.R", "pilchuck_creek_review.R", "phase3_exports.R")) source(file.path("R", file))
phase3_results <- run_phase3_tidal_barriers()
source(file.path("R", "phase3_tidal_details.R"))
render_phase3_tidal_details(output_dir = phase3_results$output_dir)
source(file.path("R", "stillaguamish_details.R"))
render_stillaguamish_details(output_dir = phase3_results$output_dir)
})
