# Resolve recorded cache/provenance paths after moving or cloning this region.
# Source snapshots and validated exports are never rewritten by this module.
nps_project_root <- local({
  source_file <- tryCatch(sys.frame(1L)$ofile, error = function(e) NULL)
  source_root <- if (length(source_file) == 1L && !is.na(source_file))
    dirname(dirname(normalizePath(source_file, winslash = "/", mustWork = FALSE))) else NULL
  function(root = NULL) {
    if (!is.null(root)) return(normalizePath(root, winslash = "/", mustWork = TRUE))
    candidates <- unique(c(getOption("nps.project_root"), source_root,
      file.path(getwd(), "North_Puget_Sound"), getwd()))
    candidates <- candidates[!is.na(candidates) & nzchar(candidates)]
    good <- vapply(candidates, function(x) file.exists(file.path(x, "R", "project_paths.R")), logical(1))
    if (!any(good)) stop("Cannot locate North_Puget_Sound; source its R/project_paths.R first")
    normalizePath(candidates[which(good)[1L]], winslash = "/", mustWork = TRUE)
  }
})

nps_resolve_cache_path <- function(path, root = NULL, must_work = FALSE) {
  region <- nps_project_root(root)
  vapply(as.character(path), function(value) {
    if (is.na(value) || !nzchar(value)) return(value)
    slash <- gsub("\\\\", "/", value)
    if (grepl("^[A-Za-z][A-Za-z0-9+.-]*://", slash)) return(value)
    # These two persistent roots identify a recorded project resource. Match
    # complete path components, so unrelated files and temporary fixtures keep
    # their original locations. Prefer this region even if the old clone exists.
    anchor <- regexpr("(^|/)(spatial_data|outputs)(/|$)", slash, perl = TRUE)
    if (anchor[1L] > 0L) {
      suffix <- substring(slash, anchor[1L])
      suffix <- sub("^/", "", suffix)
      resolved <- file.path(region, suffix)
    } else resolved <- value
    if (isTRUE(must_work) && !file.exists(resolved)) stop("Recorded resource is missing: ", resolved)
    resolved
  }, character(1), USE.NAMES = FALSE)
}

nps_resolve_record_paths <- function(x, root = NULL) {
  path_field <- function(name) grepl("(^|_)(path|paths|file|files|folder)$", name) ||
    name %in% c("report_pdf", "cache_path")
  walk <- function(value, name = "") {
    if (name %in% c("geometry", "coordinates")) return(value)
    if (is.character(value) && path_field(name)) return(nps_resolve_cache_path(value, root))
    if (inherits(value, c("sfc", "sfg"))) return(value)
    if (is.list(value)) {
      labels <- names(value)
      if (is.null(labels)) labels <- rep("", length(value))
      for (i in seq_along(value)) value[i] <- list(walk(value[[i]], labels[i]))
    }
    value
  }
  walk(x)
}

nps_read_cache_json <- function(path, ..., root = NULL) {
  nps_resolve_record_paths(jsonlite::fromJSON(nps_resolve_cache_path(path, root,
    must_work = TRUE), ...), root)
}

nps_read_cache_rds <- function(path, root = NULL) {
  nps_resolve_record_paths(readRDS(nps_resolve_cache_path(path, root, must_work = TRUE)), root)
}

nps_read_cache_csv <- function(file, ..., root = NULL) {
  nps_resolve_record_paths(utils::read.csv(nps_resolve_cache_path(file, root,
    must_work = TRUE), ...), root)
}
