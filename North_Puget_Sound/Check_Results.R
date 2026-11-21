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
# Read-only scientific validation of the committed review scenarios.
checks <- c("test_project_paths.R", "validate_untruncated_review.R", "validate_phase1_total_barriers.R",
  "validate_phase2_reviewed_barriers.R", "validate_phase2_custom_stillaguamish.R",
  "validate_phase2_culmback_terminal.R")
for (check in checks) {
  message("Checking ", check)
  source(file.path("tests", check), local = new.env(parent = globalenv()))
}
message("All saved-result checks passed; no maps or data regenerated.")
})
