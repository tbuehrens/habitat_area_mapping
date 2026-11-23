# Independent fine-channel audit. This writes audit evidence only; no source,
# cut, mask, mileage or published figure is modified.
local({
  files <- c(unlist(lapply(sys.frames(), function(x) x$ofile), use.names = FALSE),
    sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE)))
  own <- tail(files[basename(files) == "audit_lake_cavanaugh_channel.R"], 1L)
  if (!length(own)) stop("Invoke this script with Rscript or source")
  region <- dirname(dirname(normalizePath(own, winslash = "/", mustWork = TRUE)))
  source(file.path(region, "workflow_bootstrap.R"), local = FALSE)
})
north_puget_workflow({
  for (module in c("source_classification", "barrier_stream_identity", "stream_edge_identity", "network", "upstream_basins"))
    source(file.path("R", paste0(module, ".R")))
  audit_dir <- "spatial_data/lake_cavanaugh_review"
  dir.create(audit_dir, recursive = TRUE, showWarnings = FALSE)
  # This audit describes the missed habitat in the pre-correction revision.
  # Keep that input stable when the published barrier stage is regenerated.
  snapshot <- nps_read_cache_csv(file.path(audit_dir, "before_fix", "snapshot_manifest.csv"),
    stringsAsFactors = FALSE)
  stage_row <- which(endsWith(gsub("\\\\", "/", snapshot$original_path),
    "spatial_data/phase3/barrier_stage.rds"))
  if (length(stage_row) != 1L) stop("The frozen pre-correction barrier stage must be unique")
  frozen_stage <- nps_resolve_cache_path(snapshot$snapshot_path[stage_row], must_work = TRUE)
  if (!identical(unname(tools::md5sum(frozen_stage)), snapshot$md5[stage_row]))
    stop("The frozen pre-correction barrier stage changed")
  paths <- c(stage = frozen_stage,
    physical_edges = "outputs/review_swifd_connections/North_Puget_Sound_Steelhead.gpkg",
    raw_model = "spatial_data/north_puget_steelhead.rds",
    fine_reference = "spatial_data/north_puget_named_stream_reference.rds",
    original_634_basin = "spatial_data/stillaguamish_review/pilchuck_sources/634_vector_split_basin.json",
    original_foreign_basin = "spatial_data/stillaguamish_review/pilchuck_sources/foreign_24275741_whole_basin.json",
    coarse_deer_reference = "spatial_data/stillaguamish_review/pilchuck_sources/context_24274777_flowline.json")
  before <- unname(tools::md5sum(paths))
  edges <- sf::st_read(paths[["physical_edges"]], layer = "stream_matching_edges", quiet = TRUE)
  original_edge <- edges[edges$graph_edge_id == 695L, ]
  if (nrow(original_edge) != 1L) stop("The original reviewed physical edge695 is not unique")
  crs <- sf::st_crs(edges)
  raw_basin <- sf::st_read(paths[["original_634_basin"]], quiet = TRUE)
  basin <- sf::st_transform(raw_basin[raw_basin$id == "drainageBasin", ], crs)
  if (nrow(basin) != 1L) stop("Original634 source must have a unique full drainageBasin")
  escaping <- upstream_line_parts(suppressWarnings(sf::st_difference(original_edge, sf::st_union(basin))))
  escaping$source_class <- "prediction"
  escaping$length_m <- as.numeric(sf::st_length(escaping))
  if (nrow(escaping) != 1L || abs(sum(escaping$length_m) - 2158.88033959534) > .001)
    stop("The reviewed source tail outside the original634 basin changed")
  reference <- sf::st_transform(readRDS(paths[["fine_reference"]]), crs)
  lake <- reference[as.character(reference$LLID) == "1220573483422", ]
  pilchuck <- reference[as.character(reference$LLID) == "1222246482085", ]
  deer <- reference[as.character(reference$LLID) == "1219314482681", ]
  stopifnot(nrow(lake) > 0L, nrow(pilchuck) > 0L, nrow(deer) > 0L,
    all(lake$LLID_STREAMNAME == "Lake Creek"), all(pilchuck$LLID_STREAMNAME == "Pilchuck Creek"),
    all(deer$LLID_STREAMNAME == "Deer Creek"))
  mouth <- suppressWarnings(sf::st_intersection(sf::st_union(sf::st_geometry(lake)),
    sf::st_union(sf::st_geometry(pilchuck))))
  if (length(mouth) != 1L || sf::st_is_empty(mouth) || sf::st_geometry_type(mouth) != "POINT")
    stop("Lake Creek must positively touch Pilchuck Creek at a unique exact fine-reference point")
  index <- build_stream_identity_index(edges, reference)
  xy <- unclass(sf::st_geometry(escaping)[[1L]])[, 1:2, drop = FALSE]
  fractions <- c(.05, .1, .25, .5, .75, .9, .95)
  centers <- sf::st_sf(window_id = seq_along(fractions), fraction = fractions, graph_edge_id = 695L,
    geometry = sf::st_sfc(lapply(fractions, function(f) sf::st_point(stream_identity_at(xy, escaping$length_m * f))), crs = crs))
  result <- assess_stream_edge_identity(index, centers, barrier_stream = "Lake Creek", return_samples = TRUE)
  samples <- attr(result, "sample_points")
  samples$window_id <- samples$candidate_row
  samples$fraction <- centers$fraction[samples$window_id]
  samples$supports_fine_Lake_Creek <- samples$distance_m <= 25 + 1e-8 & samples$heading_degrees <= 35 + 1e-8
  summary <- cbind(sf::st_drop_geometry(centers), result[, setdiff(names(result), "graph_edge_id"), drop = FALSE])
  summary$max_heading_degrees <- vapply(summary$window_id, function(i) max(samples$heading_degrees[samples$window_id == i]), numeric(1))
  geographic <- sf::st_coordinates(sf::st_transform(centers, 4326))
  summary$longitude <- geographic[, 1L]
  summary$latitude <- geographic[, 2L]
  stopifnot(nrow(samples) == 49L, all(summary$stream_identity_valid), all(summary$support_coverage == 1),
    all(summary$matched_llid == "1220573483422"), all(samples$supports_fine_Lake_Creek))
  negative <- assess_stream_edge_identity(index, centers, barrier_stream = "Deer Creek", return_samples = FALSE)
  stopifnot(all(!negative$stream_identity_valid), all(negative$identity_status == "different_stream_confirmed"),
    all(negative$matched_llid == "1220573483422"))
  fine_deer_distance <- min(as.numeric(sf::st_distance(escaping, deer)))
  stage <- readRDS(paths[["stage"]])
  current <- stage$network[stage$network$graph_edge_id == 695L & stage$network$measure_habitat & stage$network$retained, ]
  current_overlap <- upstream_line_parts(suppressWarnings(sf::st_intersection(current, sf::st_union(escaping))))
  model <- sf::st_transform(readRDS(paths[["raw_model"]]), crs)
  hits <- which(lengths(sf::st_intersects(model, escaping)) > 0L)
  source <- model[hits, ]
  source_overlap <- upstream_line_parts(suppressWarnings(sf::st_intersection(source, sf::st_union(escaping))))
  source_overlap$overlap_m <- as.numeric(sf::st_length(source_overlap))
  source_overlap <- source_overlap[source_overlap$overlap_m > 1e-5, ]
  foreign <- sf::st_transform(sf::st_read(paths[["original_foreign_basin"]], quiet = TRUE), crs)
  lake_in_foreign <- upstream_line_parts(suppressWarnings(sf::st_intersection(lake, sf::st_union(foreign))))
  coarse_deer <- sf::st_transform(sf::st_read(paths[["coarse_deer_reference"]], quiet = TRUE), crs)
  endpoints <- sf::st_coordinates(sf::st_transform(escaping, 4326))
  endpoints <- data.frame(endpoint = c("original_tail_start", "original_tail_end"),
    longitude = endpoints[c(1L, nrow(endpoints)), 1L], latitude = endpoints[c(1L, nrow(endpoints)), 2L])
  mouth_xy <- sf::st_coordinates(sf::st_transform(mouth, 4326))[1L, 1:2]
  source_hashes <- data.frame(source = names(paths), path = unname(paths), md5 = before, stringsAsFactors = FALSE)
  overall <- data.frame(graph_edge_id = 695L, source_class = "prediction", original_edge_m = as.numeric(sf::st_length(original_edge)),
    original_basin_excluded_tail_m = sum(escaping$length_m), current_retained_tail_m = sum(as.numeric(sf::st_length(current_overlap))),
    window_count = nrow(summary), fine_sample_count = nrow(samples), all_window_coverage = min(summary$support_coverage),
    max_fine_reference_distance_m = max(summary$max_reference_distance_m), max_fine_heading_degrees = max(samples$heading_degrees),
    Lake_Creek_LLID = "1220573483422", Pilchuck_Creek_LLID = "1222246482085", Deer_Creek_negative_LLID = "1219314482681",
    lake_to_pilchuck_touch_distance_m = min(as.numeric(sf::st_distance(lake, pilchuck))),
    lake_mouth_longitude = mouth_xy[1L], lake_mouth_latitude = mouth_xy[2L], fine_deer_min_distance_m = fine_deer_distance,
    fine_lake_length_in_nominal_foreign_basin_m = sum(as.numeric(sf::st_length(lake_in_foreign))),
    fine_Lake_to_coarse_Deer_min_distance_m = min(as.numeric(sf::st_distance(lake, coarse_deer))),
    fine_reference_source_url = "https://geodataservices.wdfw.wa.gov/arcgis/rest/services/FP_Projects/NHDwithLLID/MapServer/4",
    official_receiver_source_url = "https://app.leg.wa.gov/WAC/default.aspx?cite=173-18-330", stringsAsFactors = FALSE)
  proof <- list(summary = overall, window_summary = summary, sample_points = samples, centers = centers,
    escaping_tail = escaping, original_edge = original_edge, raw_model_source_overlap = source_overlap,
    endpoints = endpoints, Lake_Creek_reference = lake, Pilchuck_Creek_reference = pilchuck,
    Lake_to_Pilchuck_exact_touch = sf::st_sf(geometry = mouth), Deer_Creek_negative_reference = deer,
    Deer_Creek_negative_identity = negative, source_hashes = source_hashes, settings = attr(result, "settings"),
    notes = c("All seven fixed source-tail windows independently pass unchanged primary fine WDFW course controls: 100m support, seven samples, 25m maximum reference distance,35degree heading,80% minimum coverage. Actual coverage100% in all windows.",
      "The original695 prediction tail is positively LakeCreek, with exact fine-reference LakeCreek→PilchuckCreek geometry contact. It is not assigned toDeer merely because a coarse catchment covers it.",
      "Authoritative WAC173-18-330 entry27 states LakeCreek runs from LakeCavanaugh outlet downstream to PilchuckCreek. WAC173-201A-602 locates LakeCavanaugh outlet about48.3126,-121.9803.",
      "This is an independent evidence audit only. It neither modifies the approved basin nor performs any cut, snap, mileage change or output redraw."))
  if (!identical(unname(tools::md5sum(paths)), before)) stop("A protected scientific input changed during read-only channel audit")
  saveRDS(proof, file.path(audit_dir, "fine_channel_proof.rds"))
  utils::write.csv(overall, file.path(audit_dir, "fine_channel_proof.csv"), row.names = FALSE)
  utils::write.csv(summary, file.path(audit_dir, "fine_channel_windows.csv"), row.names = FALSE)
  utils::write.csv(sf::st_drop_geometry(samples), file.path(audit_dir, "fine_channel_samples.csv"), row.names = FALSE)
  utils::write.csv(source_hashes, file.path(audit_dir, "fine_channel_source_hashes.csv"), row.names = FALSE)
  utils::write.csv(sf::st_drop_geometry(source_overlap), file.path(audit_dir, "raw_model_source_overlap.csv"), row.names = FALSE)
  cat("Fine Lake Cavanaugh audit PASS: original edge695 tail is LakeCreek→PilchuckCreek; 49/49 primary-course samples; scientific hashes unchanged\n")
})
