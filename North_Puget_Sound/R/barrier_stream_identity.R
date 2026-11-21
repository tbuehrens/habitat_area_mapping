# Stream names provide positive evidence against snapping a tributary barrier
# onto its named receiving river. This check does not infer fish passability.
barrier_stream_name_key <- function(x) {
  key <- tolower(trimws(as.character(x)))
  key[is.na(x)] <- NA_character_
  key <- gsub("[^a-z0-9]+", " ", key)
  key <- trimws(gsub(" +", " ", key))
  unknown <- is.na(key) | !nzchar(key) |
    grepl("^(unnamed|unknown|unidentified|not named)( |$)", key) |
    key %in% c("na", "n a", "none", "null", "not available")
  key[unknown] <- NA_character_
  # Explicit abbreviations, without edit-distance or other fuzzy matching.
  key <- gsub("\\b(southfork|south fork|s fork|s f|sf)\\b", "south fork", key)
  key <- gsub("\\b(northfork|north fork|n fork|n f|nf)\\b", "north fork", key)
  key <- gsub("\\b(middlefork|middle fork|m fork|m f|mf)\\b", "middle fork", key)
  key <- gsub("\\b(r|riv)\\b", "river", key)
  key <- gsub("\\b(cr|ck)\\b", "creek", key)
  # Stilly is an explicit local alias, not a general name-similarity rule.
  key <- gsub("\\bstilly\\b", "stillaguamish", key)
  key <- sub("^sfstillaguamish\\b", "south fork stillaguamish", key)
  key <- sub("^nfstillaguamish\\b", "north fork stillaguamish", key)
  key <- sub("^sfstilly\\b", "south fork stillaguamish", key)
  key <- sub("^nfstilly\\b", "north fork stillaguamish", key)
  trimws(gsub(" +", " ", key))
}

barrier_stream_names_match <- function(a, b) {
  a <- barrier_stream_name_key(a)
  b <- barrier_stream_name_key(b)
  known <- !is.na(a) & !is.na(b)
  answer <- rep(FALSE, length(a))
  # An omitted watercourse suffix is permitted, but Bear River and Bear Creek
  # are not equated when both sources specify different watercourse types.
  suffix_a <- grepl(" (river|creek)$", a)
  suffix_b <- grepl(" (river|creek)$", b)
  base_a <- sub(" (river|creek)$", "", a)
  base_b <- sub(" (river|creek)$", "", b)
  answer[known] <- a[known] == b[known] |
    (base_a[known] == base_b[known] & !(suffix_a[known] & suffix_b[known]))
  answer
}

validate_barrier_stream_matches <- function(barriers, match_points_sf, swifd_reference,
                                            max_reference_distance = 100) {
  if (!requireNamespace("sf", quietly = TRUE)) stop("Install the sf package")
  if (length(max_reference_distance) != 1L || !is.finite(max_reference_distance) ||
      max_reference_distance < 0) stop("max_reference_distance must be nonnegative meters")
  count <- nrow(barriers)
  if (is.null(count)) stop("barriers must be a data frame or sf object")
  field <- function(name) {
    if (name %in% names(barriers)) as.character(barriers[[name]]) else rep(NA_character_, count)
  }
  result <- data.frame(
    stream_identity_status = rep("no_swifd_reference", count),
    barrier_stream = field("StreamName"),
    barrier_tributary_to = field("TributaryToName"),
    matched_stream_name = rep(NA_character_, count),
    matched_llid = rep(NA_character_, count),
    reference_distance_m = rep(NA_real_, count),
    allow_cut = rep(TRUE, count), stringsAsFactors = FALSE)
  if (!count) return(result)
  if (!inherits(match_points_sf, c("sf", "sfc")) ||
      length(sf::st_geometry(match_points_sf)) != count) {
    stop("match_points_sf must contain one candidate snapped point per barrier, in the same order")
  }
  crs <- sf::st_crs(match_points_sf)
  if (is.na(crs) || isTRUE(sf::st_is_longlat(match_points_sf)) ||
      !crs$units_gdal %in% c("metre", "meter", "m")) {
    stop("Candidate match points must use a projected CRS with meter units")
  }
  if (any(!sf::st_geometry_type(match_points_sf) %in% c("POINT"))) {
    stop("Candidate match geometries must be POINTs")
  }
  points <- sf::st_sf(geometry = sf::st_geometry(match_points_sf))
  if (is.null(swifd_reference) || !nrow(swifd_reference)) return(result)
  if (!inherits(swifd_reference, "sf") || is.na(sf::st_crs(swifd_reference))) {
    stop("swifd_reference must be sf with a defined CRS")
  }
  reference <- sf::st_transform(swifd_reference, crs)
  reference <- reference[!sf::st_is_empty(reference), ]
  if (!nrow(reference)) return(result)
  if (any(!sf::st_geometry_type(reference) %in% c("LINESTRING", "MULTILINESTRING"))) {
    stop("SWIFD reference must contain stream line geometries")
  }
  empty_points <- sf::st_is_empty(points)
  valid_points <- which(!empty_points)
  result$stream_identity_status[empty_points] <- "empty_candidate_point_unvalidated"
  if (!length(valid_points)) return(result)
  nearest <- sf::st_nearest_feature(points[valid_points, ], reference)
  distance <- as.numeric(sf::st_distance(points[valid_points, ],
    reference[nearest, ], by_element = TRUE))
  result$reference_distance_m[valid_points] <- distance
  name_fields <- intersect(c("LLID_STRM_NAME", "LLID_STREAMNAME", "GNIS_STREAMNAME",
                              "GNIS_Name", "StreamName", "stream_name"), names(reference))
  if (length(name_fields)) result$matched_stream_name[valid_points] <-
    as.character(reference[[name_fields[1L]]][nearest])
  if ("LLID" %in% names(reference)) result$matched_llid[valid_points] <-
    as.character(reference$LLID[nearest])
  result$stream_identity_status[valid_points] <- "no_nearby_swifd_reference"
  nearby <- valid_points[distance <= max_reference_distance + 1e-8]
  if (!length(nearby)) return(result)
  matched_known <- !is.na(barrier_stream_name_key(result$matched_stream_name))
  own_known <- !is.na(barrier_stream_name_key(result$barrier_stream))
  own_matches <- barrier_stream_names_match(result$barrier_stream, result$matched_stream_name)
  receiving_matches <- barrier_stream_names_match(result$barrier_tributary_to,
                                                   result$matched_stream_name)
  result$stream_identity_status[nearby] <- "swifd_stream_name_unavailable"
  known_rows <- nearby[matched_known[nearby]]
  result$stream_identity_status[known_rows] <- "barrier_stream_unknown_unvalidated"
  named_rows <- known_rows[own_known[known_rows]]
  result$stream_identity_status[named_rows] <- "different_stream_name_unvalidated"
  confirmed <- known_rows[own_matches[known_rows]]
  result$stream_identity_status[confirmed] <- "own_stream_name_match"
  conflict <- known_rows[receiving_matches[known_rows] & !own_matches[known_rows]]
  result$stream_identity_status[conflict] <- "receiving_stream_conflict"
  result$allow_cut[conflict] <- FALSE
  result
}
