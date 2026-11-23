# Independent read-only checks of the final Phase 3 maps' scientific geometry.
suppressPackageStartupMessages(library(sf))
source("R/project_paths.R")
source("tests/upstream_basin_validation_helpers.R")
out <- "outputs/phase3_tidal_barriers"
file <- file.path(out, "North_Puget_Sound_Steelhead_phase3.gpkg")
read <- function(layer) st_read(file, layer = layer, quiet = TRUE)
assert <- function(x, text) if (!isTRUE(x)) stop(text, call. = FALSE)
length_m <- function(x) {
  geometry <- if (inherits(x, "sf")) st_geometry(x) else x
  if (!length(geometry) || all(st_is_empty(geometry))) return(0)
  sum(as.numeric(st_length(st_union(geometry))))
}
missing_m <- function(wanted, available, tolerance = .0001) {
  if (!length(wanted) || all(st_is_empty(wanted))) return(0)
  if (!length(available) || all(st_is_empty(available))) return(length_m(wanted))
  length_m(suppressWarnings(st_difference(st_union(wanted), st_buffer(st_union(available), tolerance))))
}
manifest <- read.csv(file.path(out, "preserved_source_manifest.csv"), stringsAsFactors = FALSE)
assert(identical(unname(tools::md5sum(nps_resolve_cache_path(manifest$path))), manifest$md5), "An earlier scenario changed")
old <- read.csv("outputs/phase2_reviewed_barriers/phase2_barrier_decisions.csv", stringsAsFactors = FALSE)
decisions <- read.csv(file.path(out, "phase3_barrier_decisions.csv"), stringsAsFactors = FALSE)
assert(!anyDuplicated(decisions$review_number) && !anyDuplicated(decisions$review_record_key) &&
  all(old$review_number %in% decisions$review_number), "Old barrier numbers lost or duplicated")
assert(identical(old$review_record_key, decisions$review_record_key[match(old$review_number, decisions$review_number)]), "A barrier number changed source identity")
applied <- decisions[decisions$phase3_applied %in% TRUE, ]
assert(all(c(10L, 331L, 222L, 810L, 833L, 903L, 912L, 913L) %in% applied$review_number), "A requested approved cut is absent")
assert(!any(c(797L, 798L, 199L, 205L, 209L, 215L, 219L, 220L, 914L) %in% applied$review_number), "A rejected or held cut was applied")
assert(decisions$report_blockage[decisions$review_number == 10L] == "Total" &&
  abs(decisions$reported_height_m[decisions$review_number == 10L] - 18.3) < .001 &&
  decisions$verification[decisions$review_number == 10L] == "Verified", "Woods evidence lost")
assert(decisions$report_blockage[decisions$review_number == 331L] == "Total", "Lower Olney lacks source Total evidence")
assert(decisions$snap_distance_m[decisions$review_number == 914L] > 50 &&
  !decisions$phase3_applied[decisions$review_number == 914L], "Off-network West Fork Woods was forced onto a channel")

pops <- read("noaa_populations"); custom <- read("custom_review_watershed")
assert(nrow(pops) == 16L && nrow(custom) == 1L && custom$pop_id == 1001L, "Analysis boundaries changed")
old_pops <- st_read("outputs/phase2_reviewed_barriers/North_Puget_Sound_Steelhead_phase2.gpkg", layer = "noaa_populations", quiet = TRUE)
assert(identical(st_as_binary(st_geometry(pops)), st_as_binary(st_geometry(old_pops))), "NOAA polygon bytes changed")
extent <- read("phase3_extent_status")
remaining <- read("phase3_remaining")
barrier_removed <- read("phase3_removed_upstream")
tidal_removed <- read("phase3_removed_tidal")
before_tide <- read("before_tidal_extent_status")
mask <- read("tidal_clip_mask")
assert(all(st_is_valid(mask)) && all(st_geometry_type(mask) %in% c("POLYGON", "MULTIPOLYGON")), "Invalid tidal cut mask")
assert(!any(remaining$removed) && all(tidal_removed$tidal_removed) && !any(tidal_removed$barrier_removed), "Barrier/tidal statuses overlap incorrectly")
residual <- suppressWarnings(st_intersection(st_geometry(remaining[remaining$measure_habitat, ]), st_union(st_geometry(mask))))
assert(length_m(residual) < .001, "Mapped tidal source habitat remains visible")
table <- read.csv(file.path(out, "population_phase3_miles.csv"), stringsAsFactors = FALSE)
old_table <- read.csv("outputs/phase2_reviewed_barriers/population_phase2_miles.csv", stringsAsFactors = FALSE)
assert(nrow(table) == 17L && all(table$new_barrier_removed_miles >= -.00001), "Wrong area count or unintended restoration")
assert(all(abs(table$baseline_untruncated_miles - table$removed_upstream_barriers_miles -
  table$removed_current_tidal_miles - table$phase3_remaining_miles) < 1e-6), "Total source partition failed")
assert(all(abs(table$phase2_remaining_miles - table$new_barrier_removed_miles -
  table$removed_current_tidal_miles - table$phase3_remaining_miles) < 1e-6), "Change-from-Phase2 partition failed")
assert(all(abs(table$phase2_remaining_miles - old_table$phase2_remaining_visible_miles[
  match(table$pop_id, old_table$pop_id)]) < 1e-6), "Wrong Phase2 comparison mileage")
for (id in table$pop_id) {
  current <- extent[extent$pop_id == id & extent$measure_habitat, ]
  row <- table[table$pop_id == id, ]
  assert(abs(length_m(current[current$retained, ]) / 1609.344 - row$phase3_remaining_miles) < 1e-6,
    paste("Remaining mileage does not match GIS", id))
  assert(abs(length_m(current[current$tidal_removed, ]) / 1609.344 - row$removed_current_tidal_miles) < 1e-6,
    paste("Tidal mileage does not match GIS", id))
  assert(abs(length_m(current[current$barrier_removed, ]) / 1609.344 - row$removed_upstream_barriers_miles) < 1e-6,
    paste("Barrier mileage does not match GIS", id))
}

# Ordinary full-basin checks retain the original <=100m outlet safeguard.
# Lower Olney has a separately independently verified same-channel exception.
masks <- read("upstream_basin_masks"); barriers <- read("barrier_characteristics")
coverage <- validate_upstream_basin_completeness(
  before_tide[!before_tide$removed & before_tide$measure_habitat, ],
  before_tide[before_tide$removed & before_tide$measure_habitat, ],
  masks[masks$review_record_key != "6:43814", ], barriers)
assert(all(coverage$unproven_upstream_residual_m < .001), "Unexplained upstream habitat remains in an ordinary basin")

# New removals outside the newly approved basins and current tidal mask would
# indicate an accidental global connectivity/filtering regression.
new_masks <- masks[masks$review_record_key %in% c("5:44379", "6:43814", "6:52652", "6:54208"), ]
changed <- st_union(c(st_geometry(new_masks), st_geometry(mask)))
changed <- st_buffer(changed, 100)
old_remaining <- st_read("outputs/phase2_reviewed_barriers/North_Puget_Sound_Steelhead_phase2.gpkg", layer = "phase2_remaining", quiet = TRUE)
for (id in table$pop_id) {
  a <- st_union(st_geometry(old_remaining[old_remaining$pop_id == id & old_remaining$measure_habitat, ]))
  b <- st_union(st_geometry(remaining[remaining$pop_id == id & remaining$measure_habitat, ]))
  outside_a <- suppressWarnings(st_difference(a, changed))
  outside_b <- suppressWarnings(st_difference(b, changed))
  if (length(st_equals(outside_a, outside_b)[[1L]]) == 1L) next
  assert(missing_m(outside_a, outside_b) + missing_m(outside_b, outside_a) < .01,
    paste("Unrequested stream change outside reviewed extents", id))
}
points <- read("tidal_cutpoints")
point_table <- read.csv(file.path(out, "tidal_cutpoint_review.csv"), stringsAsFactors = FALSE)
assert(!anyDuplicated(points$tidal_id) && identical(points$tidal_id, point_table$tidal_id), "Tidal numbering/table disagreement")
assert(length(list.files(file.path(out, "maps"), pattern = "\\.png$")) == 17L, "Incomplete 17-map set")
assert(file.info(file.path(out, "North_Puget_Sound_Steelhead_population_maps.pdf"))$size > 10000, "Population PDF missing")
cat("PASS: preserved scenarios; stable numbering; requested cuts/holds; 17 GIS/mileage partitions; no remaining mapped tidal habitat; complete ordinary upstream masks; no unrequested spatial changes.\n")
print(table[, c("pop_id", "population", "new_barrier_removed_miles", "removed_current_tidal_miles", "phase3_remaining_miles")], row.names = FALSE)
