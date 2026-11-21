# Review barrier locations and explicit passage evidence without cutting streams.
# Source R/barrier_stream_identity.R before using SWIFD name checks.
barrier_review_field_definitions <- function(source_metadata = list()) {
  list(
    mode = "Untruncated geometry review; no barrier cuts are applied",
    service_urls = c(
      "https://geodataservices.wdfw.wa.gov/arcgis/rest/services/ApplicationServices/FP_Sites/MapServer/5",
      "https://geodataservices.wdfw.wa.gov/arcgis/rest/services/ApplicationServices/FP_Sites/MapServer/6"),
    FishPassageBarrierStatusCode = list(
      field_meaning = "Whether a barrier is recorded; does not establish complete blockage",
      coded_values = list(`0` = "N/A", `10` = "Yes", `20` = "No", `99` = "Unknown")),
    PercentFishPassableCode = list(
      field_meaning = "Coded percent passable; code 10, not code 0, means zero percent",
      coded_values = list(`0` = "N/A", `10` = "0", `20` = "33", `30` = "67", `40` = "100", `99` = "Unknown")),
    candidate_policy = paste(
      "Only explicit zero-passability code or exact total-blockage descriptions, or an explicit user override,",
      "can identify a future-cut candidate. Unknown passability never establishes complete blockage.",
      "The location must be within 50 m, have a unique positively identified own-stream channel,",
      "and be clear of junction ambiguity. No cuts are applied."),
    identity_policy = paste(
      "Search every physical channel within 50 m; compare its local course using multiple samples and headings",
      "against named hydrography with LLIDs, independent of fish distribution.",
      "Require a unique own-stream channel and unambiguous reference-route support.",
      "Unknown, unnamed, conflicting, competing-channel, or junction-ambiguous matches remain for manual review.",
      "Numbered location-review points are not necessarily approved stream assignments."),
    numbering_policy = "Preserve existing record-key/number assignments; append numbers for newly displayed in-radius points",
    source_metadata = source_metadata)
}

gorge_dam_review_override <- function(crs = 26910) {
  if (!requireNamespace("sf", quietly = TRUE)) stop("Install the sf package")
  # WAC publishes this named location to four decimal places. It is not a WDFW
  # inventory record, and the user override does not supply passability evidence.
  result <- sf::st_sf(
    SiteId = NA_character_, dam_name = "Gorge Dam", FeatureType = "Dam",
    StreamName = "Skagit River", TributaryToName = NA_character_,
    FishPassageBarrierStatusCode = NA_integer_, PercentFishPassableCode = NA_integer_,
    SiteLatitude = 48.6978, SiteLongitude = -121.2082,
    source_layer = "user_override", barrier_key = "user_override:Gorge_Dam",
    verification = "User-requested override; not a WDFW inventory record",
    source_url = "https://app.leg.wa.gov/wac/default.aspx?cite=173-201A-602",
    source_coordinate_precision = "Published to 0.0001 degree; positional accuracy and coordinate datum unspecified",
    source_coordinate_interpretation = "Published longitude/latitude interpreted as EPSG:4326 for review",
    override_future_cut = TRUE,
    override_reason = "User explicitly requested Gorge Dam as a later-cut candidate; no cut applied",
    geometry = sf::st_sfc(sf::st_point(c(-121.2082, 48.6978)), crs = 4326))
  sf::st_transform(result, crs)
}

classify_barrier_passability <- function(barriers, override_site_ids = character()) {
  count <- nrow(barriers)
  value <- function(field) {
    if (field %in% names(barriers)) as.character(barriers[[field]]) else rep(NA_character_, count)
  }
  code <- suppressWarnings(as.numeric(value("PercentFishPassableCode")))
  barrier_code <- suppressWarnings(as.numeric(value("FishPassageBarrierStatusCode")))
  percent <- rep(NA_real_, count)
  for (entry in list(c(10, 0), c(20, 33), c(30, 67), c(40, 100))) {
    percent[!is.na(code) & code == entry[1L]] <- entry[2L]
  }
  total <- !is.na(code) & code == 10
  evidence <- rep("No explicit total-passage evidence", count)
  evidence[total] <- "PercentFishPassableCode=10 (0% passable)"
  # Exact phrases in dedicated descriptive fields only: never search arbitrary
  # comments, URLs, barrier names, or strings such as 'not a total barrier'.
  descriptions <- intersect(c("PercentFishPassableDescription", "PercentPassableDescription",
    "FishPassageBarrierStatusDescription", "PassabilityDescription", "BarrierDescription",
    "BarrierStatusDescription", "Passability", "PassageDescription"), names(barriers))
  total_phrases <- c("0", "0%", "0 percent", "0% passable", "0 percent passable",
    "total", "total barrier", "total blockage", "total fish passage blockage",
    "complete barrier", "complete blockage", "impassable", "not passable")
  descriptive_total <- rep(FALSE, count)
  for (field in descriptions) {
    text <- tolower(trimws(gsub(" +", " ", value(field))))
    matched <- !is.na(text) & text %in% total_phrases
    evidence[matched & !total] <- paste0(field, "=", value(field)[matched & !total])
    descriptive_total <- descriptive_total | matched
  }
  conflict <- (total | descriptive_total) &
    ((!is.na(percent) & percent > 0) | (!is.na(barrier_code) & barrier_code == 20))
  explicit_total <- (total | descriptive_total) & !conflict
  evidence[conflict] <- paste0(evidence[conflict], "; conflicting non-total/no-barrier attribute; manual review required")
  override <- rep(FALSE, count)
  if ("override_future_cut" %in% names(barriers)) {
    override <- tolower(value("override_future_cut")) %in% c("true", "t", "1")
  }
  override <- override | (!is.na(value("SiteId")) & value("SiteId") %in% override_site_ids)
  status <- rep("unknown", count)
  status[!is.na(percent) & percent > 0 & percent < 100] <- "partial_passability_recorded"
  status[!is.na(percent) & percent == 100] <- "fully_passable_recorded"
  status[explicit_total] <- "explicit_total_blockage_recorded"
  status[conflict] <- "conflicting_passability_evidence"
  data.frame(passability_percent = percent, passability_review_status = status,
    explicit_total_passage = explicit_total, total_passage_evidence = evidence,
    passability_evidence_conflict = conflict, override_future_cut = override,
    stringsAsFactors = FALSE)
}

prepare_barrier_review <- function(barriers, augmented_lines, swifd_reference = NULL,
    snap_distance = 50, source_metadata = list(), override_site_ids = character(),
    previous_review_numbers = NULL) {
  if (!requireNamespace("sf", quietly = TRUE)) stop("Install the sf package")
  if (length(snap_distance) != 1L || !is.finite(snap_distance) || snap_distance < 0)
    stop("snap_distance must be one nonnegative distance in meters")
  if (!inherits(barriers, "sf") || is.na(sf::st_crs(barriers)) ||
      any(sf::st_geometry_type(barriers) != "POINT") || any(sf::st_is_empty(barriers)))
    stop("barriers must contain nonempty POINT geometries with a defined CRS")
  if (!exists("build_stream_identity_index", mode = "function"))
    stop("Source R/stream_edge_identity.R before preparing strict barrier matches")
  index <- build_stream_identity_index(augmented_lines, swifd_reference)
  streams <- index$edges
  crs <- sf::st_crs(streams)
  original <- barriers
  points <- sf::st_transform(barriers, crs)
  count <- nrow(points)
  value <- function(field) {
    if (field %in% names(points)) as.character(points[[field]]) else rep(NA_character_, count)
  }
  native_xy <- sf::st_coordinates(original)[, 1:2, drop = FALSE]
  projected_xy <- sf::st_coordinates(points)[, 1:2, drop = FALSE]
  geographic_xy <- sf::st_coordinates(sf::st_transform(original, 4326))[, 1:2, drop = FALSE]
  points$original_source_x <- native_xy[, 1L]
  points$original_source_y <- native_xy[, 2L]
  points$original_source_crs <- rep(if (is.na(sf::st_crs(original)$epsg))
    sf::st_crs(original)$wkt else paste0("EPSG:", sf::st_crs(original)$epsg), count)
  points$original_x <- projected_xy[, 1L]
  points$original_y <- projected_xy[, 2L]
  points$original_longitude <- geographic_xy[, 1L]
  points$original_latitude <- geographic_xy[, 2L]
  points$original_geometry_wkt <- sf::st_as_text(sf::st_geometry(original))
  passage <- classify_barrier_passability(barriers, override_site_ids)
  for (field in names(passage)) points[[field]] <- passage[[field]]
  points$applied <- points$truncation_applied <- rep(FALSE, count)
  key <- value("barrier_key")
  missing_key <- is.na(key) | !nzchar(key)
  key[missing_key] <- paste(value("source_layer"), value("SiteId"),
    value("OBJECTID"), sep = ":")[missing_key]
  points$review_record_key <- key
  if (anyDuplicated(key)) stop("Barrier review record keys must be unique")
  project_pairs <- function(rows, edge_rows) {
    paths <- sf::st_nearest_points(sf::st_geometry(points)[rows],
      sf::st_geometry(streams)[edge_rows], pairwise = TRUE)
    sf::st_sfc(lapply(paths, function(line) sf::st_point(
      unname(tail(unclass(line)[, 1:2, drop = FALSE], 1L)[1L, ]))), crs = crs)
  }
  nearest <- if (count) sf::st_nearest_feature(points, streams) else integer()
  candidate_geometry <- if (count) project_pairs(seq_len(count), nearest) else sf::st_geometry(points)
  points$initial_snap_distance_m <- as.numeric(sf::st_distance(
    sf::st_geometry(points), candidate_geometry, by_element = TRUE))
  points$snap_distance_m <- points$initial_snap_distance_m
  points$nearest_physical_line <- streams$graph_edge_id[nearest]
  points$graph_edge_id <- streams$graph_edge_id[nearest]
  points$channel_chain_id <- streams$channel_chain_id[nearest]
  nearby <- sf::st_is_within_distance(points, streams, dist = snap_distance)
  rows <- rep(seq_len(count), lengths(nearby))
  edge_rows <- unlist(nearby, use.names = FALSE)
  candidate_points <- sf::st_sf(barrier_row = rows,
    review_record_key = key[rows], graph_edge_id = streams$graph_edge_id[edge_rows],
    geometry = if (length(rows)) project_pairs(rows, edge_rows) else sf::st_sfc(crs = crs))
  candidate_points$snap_distance_m <- as.numeric(sf::st_distance(
    sf::st_geometry(points)[rows], sf::st_geometry(candidate_points), by_element = TRUE))
  assessed <- assess_stream_edge_identity(index, candidate_points,
    barrier_stream = value("StreamName")[rows],
    barrier_tributary_to = value("TributaryToName")[rows])
  samples <- attr(assessed, "sample_points")
  identity_settings <- attr(assessed, "settings")
  if (!is.null(samples) && nrow(samples) && "candidate_row" %in% names(samples))
    samples$review_record_key <- key[rows[samples$candidate_row]]
  # graph/chain IDs are already carried by candidate_points; remaining metrics
  # describe the channel course, never a nearest name at one point.
  for (field in setdiff(names(assessed), names(candidate_points)))
    candidate_points[[field]] <- assessed[[field]]
  points$display_location_available <- lengths(nearby) > 0L
  points$candidate_count <- lengths(nearby)
  points$own_stream_candidate_count <- rep(0L, count)
  points$matching_channel_count <- rep(0L, count)
  points$stream_identity_confirmed <- rep(FALSE, count)
  points$snap_accepted <- rep(FALSE, count)
  points$junction_ambiguous <- rep(FALSE, count)
  points$identity_correction_applied <- rep(FALSE, count)
  points$identity_conflict_flag <- rep(FALSE, count)
  points$identity_review_status <- rep("rejected_beyond_tolerance", count)
  points$initial_stream_identity_status <- rep(NA_character_, count)
  points$initial_matched_stream_name <- rep(NA_character_, count)
  points$initial_matched_llid <- rep(NA_character_, count)
  # Allocate evidence fields with their original types, including all threshold
  # and competing-route diagnostics returned by the line-level assessor.
  metric_fields <- setdiff(names(assessed), c("graph_edge_id", "channel_chain_id"))
  for (field in metric_fields) points[[field]] <- assessed[[field]][rep(NA_integer_, count)]
  by_barrier <- split(seq_len(nrow(candidate_points)), candidate_points$barrier_row)
  selected_candidate <- rep(NA_integer_, count)
  for (i in which(points$display_location_available)) {
    candidates <- by_barrier[[as.character(i)]]
    candidates <- candidates[order(candidate_points$snap_distance_m[candidates],
      candidate_points$graph_edge_id[candidates])]
    first <- candidates[1L]
    points$initial_stream_identity_status[i] <- candidate_points$identity_status[first]
    points$initial_matched_stream_name[i] <- candidate_points$matched_stream_name[first]
    points$initial_matched_llid[i] <- candidate_points$matched_llid[first]
    own <- candidates[candidate_points$stream_identity_valid[candidates] %in% TRUE]
    chains <- unique(candidate_points$channel_chain_id[own])
    points$own_stream_candidate_count[i] <- length(own)
    points$matching_channel_count[i] <- length(chains)
    chosen <- if (length(own)) own[1L] else first
    selected_candidate[i] <- chosen
    candidate_geometry[i] <- sf::st_geometry(candidate_points)[chosen]
    points$snap_distance_m[i] <- candidate_points$snap_distance_m[chosen]
    points$graph_edge_id[i] <- candidate_points$graph_edge_id[chosen]
    points$channel_chain_id[i] <- candidate_points$channel_chain_id[chosen]
    for (field in metric_fields) points[[field]][i] <- candidate_points[[field]][chosen]
    points$junction_ambiguous[i] <- isTRUE(candidate_points$junction_ambiguous[chosen])
    points$identity_conflict_flag[i] <- any(candidate_points$receiving_stream_conflict[candidates] %in% TRUE)
    points$stream_identity_confirmed[i] <- length(own) > 0L && length(chains) == 1L
    points$snap_accepted[i] <- points$stream_identity_confirmed[i] && !points$junction_ambiguous[i]
    points$identity_correction_applied[i] <- points$stream_identity_confirmed[i] &&
      candidate_points$graph_edge_id[chosen] != candidate_points$graph_edge_id[first]
    points$identity_review_status[i] <- if (length(own)) {
      if (length(chains) != 1L) "manual_review_multiple_matching_channels"
      else if (points$junction_ambiguous[i]) "manual_review_near_confluence"
      else "confirmed_own_stream"
    } else if (is.na(barrier_stream_name_key(value("StreamName")[i]))) {
      "manual_review_barrier_stream_unknown"
    } else if (points$identity_conflict_flag[i]) {
      "manual_review_receiving_stream_conflict"
    } else if (any(candidate_points$identity_status[candidates] == "different_stream_confirmed", na.rm = TRUE)) {
      "manual_review_stream_name_mismatch"
    } else "manual_review_no_verified_stream_identity"
  }
  # An individual candidate's evidence cannot override competing physical
  # channels found during barrier-level selection.
  points$identity_valid_for_cut <- points$snap_accepted
  points$allow_cut <- points$snap_accepted
  points$snap_status <- points$identity_review_status
  points$stream_identity_status <- points$identity_status
  points$future_cut_requested <- points$explicit_total_passage | points$override_future_cut
  points$eligible_for_future_cut <- points$future_cut_requested & points$snap_accepted
  points$future_cut_candidate <- points$eligible_for_future_cut
  points$future_cut_basis <- ifelse(points$override_future_cut, "explicit_user_override",
    ifelse(points$explicit_total_passage, "explicit_total_passage_evidence", "no_explicit_total_passage_evidence"))
  points$cut_review_status <- ifelse(!points$future_cut_requested, "no_total_passage_evidence_or_override",
    ifelse(points$eligible_for_future_cut, "eligible_for_later_review_no_cut_applied", points$identity_review_status))
  xy <- sf::st_coordinates(candidate_geometry)[, 1:2, drop = FALSE]
  points$snapped_x <- xy[, 1L]
  points$snapped_y <- xy[, 2L]
  points$review_number <- rep(NA_integer_, count)
  highest <- 0L
  if (!is.null(previous_review_numbers) && nrow(previous_review_numbers)) {
    required <- c("review_record_key", "review_number")
    if (!all(required %in% names(previous_review_numbers))) stop("Previous review numbering needs record key and number")
    previous <- previous_review_numbers[!is.na(previous_review_numbers$review_number), required]
    if (anyDuplicated(previous$review_record_key) || anyDuplicated(previous$review_number))
      stop("Previous review keys and numbers must be unique")
    previous$review_number <- as.integer(previous$review_number)
    highest <- if (nrow(previous)) max(previous$review_number) else 0L
    matched <- match(points$review_record_key, previous$review_record_key)
    keep <- points$display_location_available & !is.na(matched)
    points$review_number[keep] <- previous$review_number[matched[keep]]
  }
  added <- which(points$display_location_available & is.na(points$review_number))
  if (length(added)) {
    added <- added[order(key[added], points$original_source_x[added],
      points$original_source_y[added], method = "radix")]
    points$review_number[added] <- highest + seq_along(added)
  }
  candidate_points$review_number <- points$review_number[candidate_points$barrier_row]
  candidate_points$selected_for_location <- seq_len(nrow(candidate_points)) %in% selected_candidate
  candidate_points$barrier_stream <- value("StreamName")[candidate_points$barrier_row]
  candidate_points$barrier_tributary_to <- value("TributaryToName")[candidate_points$barrier_row]
  audit <- points
  sf::st_geometry(audit) <- sf::st_geometry(original)
  all_locations <- points
  sf::st_geometry(all_locations) <- candidate_geometry
  displayed <- all_locations[all_locations$display_location_available, ]
  displayed <- displayed[order(displayed$review_number), ]
  list(points = displayed, audit = audit, source_points = original,
    candidate_points = all_locations, candidate_matches = candidate_points,
    stream_edges = index$edges, identity_samples = samples,
    definitions = barrier_review_field_definitions(source_metadata),
    settings = list(snap_distance_m = snap_distance, truncation_applied = FALSE,
      identity = identity_settings, source_crs = sf::st_crs(original)$wkt, review_crs = crs$wkt))
}
