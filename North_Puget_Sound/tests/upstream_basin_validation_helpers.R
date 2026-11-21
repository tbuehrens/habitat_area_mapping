# Independent read-only checks for exported upstream drainage masks.
validate_upstream_basin_completeness <- function(remaining, removed, masks, barriers,
    length_tolerance_m = .001, local_precision_limit_m = 100,
    gorge_reference = "spatial_data/phase1_validation/gorge_graph_reference_before_basin.rds",
    gorge_isolated = "spatial_data/phase1_validation/gorge_isolated_upstream_expected.rds") {
  require_ok <- function(x, message) if (!isTRUE(x)) stop(message, call. = FALSE)
  contains_key <- function(x, key) vapply(strsplit(ifelse(is.na(x), "", x), ";", fixed = TRUE),
    function(z) key %in% z, logical(1))
  unique_m <- function(x) {
    g <- sf::st_geometry(x)
    if (!length(g) || all(sf::st_is_empty(g))) return(0)
    sum(as.numeric(sf::st_length(sf::st_union(g))))
  }
  linear <- function(x) {
    if (!nrow(x)) return(x)
    x <- x[!sf::st_is_empty(x), ]
    if (!nrow(x)) return(x)
    x <- suppressWarnings(sf::st_collection_extract(x, "LINESTRING"))
    x <- suppressWarnings(sf::st_cast(x, "LINESTRING"))
    x[!sf::st_is_empty(x) & sf::st_dimension(x) == 1L, ]
  }
  flags <- c("watershed_validated", "stream_identity_validated", "total_barrier_approved",
    "coverage_complete", "local_outlet_precision_validated")
  require_ok(all(c("review_record_key", flags) %in% names(masks)), "Exported masks lack validation fields")
  require_ok(!anyDuplicated(masks$review_record_key) && nrow(masks) > 0L, "Masks need unique barrier source keys")
  for (field in flags) require_ok(all(masks[[field]] %in% TRUE), paste("Unvalidated upstream mask:", field))
  require_ok(all(sf::st_is_valid(masks)) && !any(sf::st_is_empty(masks)), "Upstream drainage masks must be nonempty valid polygons")
  crs <- sf::st_crs(remaining)
  require_ok(!is.na(crs) && !sf::st_is_longlat(remaining), "Use projected geometry for upstream completeness checks")
  masks <- sf::st_transform(masks, crs)
  barriers <- sf::st_transform(barriers, crs)
  results <- vector("list", nrow(masks))
  for (i in seq_len(nrow(masks))) {
    key <- masks$review_record_key[i]
    j <- match(key, barriers$review_record_key)
    require_ok(!is.na(j), paste("Upstream mask has no reviewed barrier:", key))
    # Polygon coverage also constrains graph-derived removal. An accidental
    # graph link into an adjacent drainage cannot authorize a distant cut there.
    claimed <- removed[contains_key(removed$removed_by_barrier, key), ]
    outside <- if (nrow(claimed)) linear(suppressWarnings(sf::st_difference(
      claimed, sf::st_geometry(masks)[i]))) else removed[0, ]
    outside_m <- unique_m(outside)
    if (outside_m > length_tolerance_m) {
      at <- sf::st_coordinates(barriers[j, ])[1, 1:2]
      coords <- sf::st_coordinates(outside)[, 1:2, drop = FALSE]
      distance <- max(sqrt(rowSums(sweep(coords, 2, at)^2)))
      require_ok(outside_m <= local_precision_limit_m + length_tolerance_m &&
        distance <= local_precision_limit_m + length_tolerance_m,
        paste("Removal extends outside its upstream drainage into another basin:", key))
      if (key == "user_override:Gorge_Dam" && file.exists(gorge_reference)) {
        graph_reference <- readRDS(gorge_reference)
        unproven <- suppressWarnings(sf::st_difference(sf::st_union(sf::st_geometry(outside)),
          sf::st_buffer(sf::st_union(sf::st_geometry(graph_reference$upstream)), 1e-5)))
        require_ok(sum(as.numeric(sf::st_length(unproven))) <= length_tolerance_m,
          "Gorge outside-mask correction is not independently proven upstream")
      }
    }
    possible <- lengths(sf::st_intersects(remaining, masks[i, ])) > 0L
    residual <- if (any(possible)) linear(suppressWarnings(sf::st_intersection(
      remaining[possible, ], sf::st_geometry(masks)[i]))) else remaining[0, ]
    protected <- rep(FALSE, nrow(residual))
    if (nrow(residual) && all(c("watershed_downstream_protected_by_barrier", "downstream_of_barrier") %in% names(residual))) {
      protected <- contains_key(residual$watershed_downstream_protected_by_barrier, key) &
        contains_key(residual$downstream_of_barrier, key)
    }
    unproven <- unique_m(residual[!protected, ])
    require_ok(unproven <= length_tolerance_m,
      paste("Remaining upstream source geometry inside approved basin:", key, unproven, "meters"))
    protected_m <- unique_m(residual[protected, ])
    max_distance <- 0
    if (any(protected)) {
      at <- sf::st_coordinates(barriers[j, ])[1, 1:2]
      coords <- sf::st_coordinates(residual[protected, ])[, 1:2, drop = FALSE]
      max_distance <- max(sqrt(rowSums(sweep(coords, 2, at)^2)))
      require_ok(max_distance <= local_precision_limit_m + length_tolerance_m &&
        protected_m <= local_precision_limit_m + length_tolerance_m,
        paste("A local downstream protection exceeds its bounded precision zone:", key))
      if (key == "user_override:Gorge_Dam" && file.exists(gorge_reference)) {
        graph_reference <- readRDS(gorge_reference)
        difference <- suppressWarnings(sf::st_difference(sf::st_union(sf::st_geometry(residual[protected, ])),
          sf::st_buffer(sf::st_union(sf::st_geometry(graph_reference$downstream)), 1e-5)))
        require_ok(sum(as.numeric(sf::st_length(difference))) <= length_tolerance_m,
          "Gorge precision protection includes geometry not independently proven downstream")
      }
    }
    results[[i]] <- data.frame(review_record_key = key, remaining_inside_m = unique_m(residual),
      independently_allowed_downstream_m = protected_m, unproven_upstream_residual_m = unproven,
      maximum_local_protection_distance_m = max_distance, bounded_upstream_outside_mask_m = outside_m)
  }
  gorge <- masks[masks$review_record_key == "user_override:Gorge_Dam", ]
  require_ok(nrow(gorge) == 1L, "A complete upstream scenario requires the Gorge basin")
  geo <- sf::st_transform(gorge, 4326)
  total_sqmi <- sum(as.numeric(sf::st_area(gorge))) / 2589988.110336
  bounds <- sf::st_bbox(geo)
  require_ok(bounds["ymax"] > 49.1 && abs(total_sqmi / 1159 - 1) <= .05,
    "Gorge mask fails the independent total-area/Canadian-extent benchmark")
  bounds["ymin"] <- 49
  canada <- suppressWarnings(sf::st_crop(geo, bounds))
  canada_sqmi <- sum(as.numeric(sf::st_area(sf::st_transform(canada, crs)))) / 2589988.110336
  require_ok(abs(canada_sqmi / 400 - 1) <= .1, "Gorge mask omits its documented Canadian contributing area")
  if (file.exists(gorge_isolated)) {
    expected <- readRDS(gorge_isolated)
    nearby <- lengths(sf::st_intersects(remaining, sf::st_buffer(expected$geometry, 1e-5))) > 0L
    survives <- if (any(nearby)) suppressWarnings(sf::st_intersection(
      sf::st_union(sf::st_geometry(expected$geometry)),
      sf::st_buffer(sf::st_union(sf::st_geometry(remaining[nearby, ])), 1e-5))) else sf::st_sfc(crs = crs)
    require_ok(sum(as.numeric(sf::st_length(survives))) <= length_tolerance_m,
      "Previously isolated Gorge-upstream source geometry still remains")
  }
  if (file.exists(gorge_reference) && "removed_by_barrier" %in% names(removed)) {
    graph_reference <- readRDS(gorge_reference)
    claimed <- removed[contains_key(removed$removed_by_barrier, "user_override:Gorge_Dam"), ]
    wrong_side <- if (nrow(claimed)) suppressWarnings(sf::st_intersection(
      sf::st_union(sf::st_geometry(graph_reference$downstream)),
      sf::st_buffer(sf::st_union(sf::st_geometry(claimed)), 1e-5))) else sf::st_sfc(crs = crs)
    require_ok(sum(as.numeric(sf::st_length(wrong_side))) <= length_tolerance_m,
      "Gorge attribution removes independently proven below-dam geometry")
  }
  answer <- do.call(rbind, results)
  attr(answer, "gorge_total_sqmi") <- total_sqmi
  attr(answer, "gorge_canadian_sqmi") <- canada_sqmi
  answer
}
