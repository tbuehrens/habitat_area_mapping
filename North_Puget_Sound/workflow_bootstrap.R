# Regional entry points run relative to this folder and restore the caller's
# working directory. Scientific cache and output paths remain unchanged.
north_puget_workflow <- local({
  source_files <- unlist(lapply(sys.frames(), function(frame) frame$ofile), use.names = FALSE)
  own_file <- tail(source_files[basename(source_files) == "workflow_bootstrap.R"], 1L)
  if (!length(own_file)) stop("Source workflow_bootstrap.R from a regional entry point")
  region <- dirname(normalizePath(own_file, winslash = "/", mustWork = TRUE))
  function(code) {
    previous <- setwd(region)
    on.exit(setwd(previous), add = TRUE)
    source(file.path("R", "project_paths.R"), local = FALSE)
    eval(substitute(code), envir = parent.frame())
  }
})
