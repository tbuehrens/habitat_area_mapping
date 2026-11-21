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
# Current review: full raw occupancy model + eligible SWIFD patches, with NO cuts.
# Numbered barrier snaps are supplied for review before any later truncation.
source(file.path("R", "arcgis.R"))
source(file.path("R", "noaa_populations.R"))
source(file.path("R", "swifd_sources.R"))
source(file.path("R", "source_classification.R"))
source(file.path("R", "barrier_stream_identity.R"))
source(file.path("R", "stream_edge_identity.R"))
source(file.path("R", "stream_reference.R"))
source(file.path("R", "network.R"))
source(file.path("R", "swifd_connectors.R"))
source(file.path("R", "habitat_workflow.R"))
source(file.path("R", "barrier_review.R"))
source(file.path("R", "untruncated_review.R"))
swifd_geometry_review <- run_habitat_analysis(
  output_dir = "outputs/review_swifd_connections", review_only = TRUE)
})
