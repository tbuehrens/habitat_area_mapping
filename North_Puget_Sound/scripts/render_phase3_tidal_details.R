# Display-only atlas; use the same final GPKG and T# IDs as the main Phase3 maps.
# Rscript North_Puget_Sound/scripts/render_phase3_tidal_details.R
local({
  source_files <- unlist(lapply(sys.frames(), function(frame) frame$ofile), use.names = FALSE)
  cli_files <- sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE))
  scripts <- c(source_files, cli_files)
  own <- tail(scripts[basename(scripts) == "render_phase3_tidal_details.R"], 1L)
  if (!length(own)) stop("Invoke or source the render_phase3_tidal_details.R script")
  region <- dirname(dirname(normalizePath(own, winslash = "/", mustWork = TRUE)))
  source(file.path(region, "workflow_bootstrap.R"), local = FALSE)
})
north_puget_workflow({
  source("R/habitat_workflow.R")
  source("R/phase3_exports.R")
  source("R/phase3_tidal_details.R")
  render_phase3_tidal_details()
})
