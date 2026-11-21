# Independent, read-only checks of the custom full watershed and SF Nooksack
# restoration in exported Phase 2 products. Run after the production export.
suppressPackageStartupMessages(library(sf))
source("R/phase2_stillaguamish.R")
assert <- function(ok, message) if (!isTRUE(ok)) stop(message, call. = FALSE)
out <- "outputs/phase2_reviewed_barriers"
file <- file.path(out, "North_Puget_Sound_Steelhead_phase2.gpkg")
read_layer <- function(layer) st_read(file, layer = layer, quiet = TRUE)
length_m <- function(x) {
  geometry <- if (inherits(x, "sf")) st_geometry(x) else x
  if (!length(geometry) || all(st_is_empty(geometry))) return(0)
  sum(as.numeric(st_length(st_union(geometry))))
}
missing_m <- function(wanted, available) {
  wanted <- if (inherits(wanted, "sf")) st_geometry(wanted) else wanted
  available <- if (inherits(available, "sf")) st_geometry(available) else available
  if (!length(wanted) || all(st_is_empty(wanted))) return(0)
  if (!length(available) || all(st_is_empty(available))) return(length_m(wanted))
  # Repeated GEOS polygon/line splits move a few seam vertices by <0.1 mm;
  # requiring literal coordinate identity would misclassify short seam pieces.
  length_m(suppressWarnings(st_difference(st_union(wanted),
    st_buffer(st_union(available), 1e-4))))
}

bundle <- load_phase2_stillaguamish()
custom <- read_layer("custom_review_watershed")
assert(nrow(custom) == 1L && custom$pop_id == 1001L && st_crs(custom)$epsg == 26910L &&
  all(st_is_valid(custom)) && !any(st_is_empty(custom)), "Invalid custom review watershed")
area_difference <- sum(as.numeric(st_area(suppressWarnings(st_sym_difference(
  st_union(st_geometry(custom)), st_union(st_geometry(bundle$population)))))))
assert(area_difference < .01, "Exported custom polygon differs from the complete validated watershed")
assert(abs(as.numeric(st_area(custom)) / 1e6 - 445.138744564) < 1e-6,
  "Custom watershed is not the complete 445.1387 km2 North Fork basin")
deer <- st_transform(st_read("spatial_data/phase2/deer_creek_basin.json", quiet = TRUE), st_crs(custom))
deer_overlap <- sum(as.numeric(st_area(suppressWarnings(st_intersection(
  st_geometry(custom), st_union(st_geometry(deer)))))))
assert(deer_overlap < 1, "Custom watershed incorrectly includes Deer Creek drainage")
navigation <- st_transform(st_read("spatial_data/phase2/nf_stillaguamish_above_deer_UT.json",
  quiet = TRUE), st_crs(custom))
assert(nrow(navigation) == 108L && "24274669" %in% as.character(navigation$nhdplus_comid) &&
  !any(c("24274775", "24274667") %in% as.character(navigation$nhdplus_comid)),
  "Upstream proof does not identify the complete NF watershed above Deer Creek")
assert(length_m(suppressWarnings(st_difference(st_geometry(navigation),
  st_union(st_geometry(custom))))) < .01, "Contributing upstream channels are missing from the custom basin")

# Establish the expected source from the unclipped cached network, independently
# of any previously clipped NOAA output or production population-mile table.
network <- readRDS("spatial_data/north_puget_swifd_bridges.rds")$bridges$network
source <- suppressWarnings(st_intersection(network[network$measure_habitat, ],
  st_geometry(custom)))
source <- source[!st_is_empty(source) & st_dimension(source) == 1L, ]
source_miles <- length_m(source) / 1609.344
assert(abs(source_miles - 114.646456051) < 1e-8,
  "The full custom source network has changed; independently revalidate its cached provenance")
extent <- read_layer("phase2_extent_status")
custom_extent <- extent[extent$pop_id == 1001L & extent$measure_habitat, ]
assert(nrow(custom_extent) > 0L && missing_m(source, custom_extent) < .01 &&
  missing_m(custom_extent, source) < .01,
  "Custom output does not partition the complete original distribution network")
# A direct bidirectional distance independently proves the small-buffer overlay
# is only a GEOS seam artifact, rather than tolerating a moved source segment.
source_hausdorff_m <- as.numeric(st_distance(st_union(st_geometry(source)),
  st_union(st_geometry(custom_extent)), which = "Hausdorff"))
assert(source_hausdorff_m < 1e-7,
  "Full custom source geometry moved beyond numerical overlay precision")
for (kind in unique(source$source_class)) {
  a <- source[source$source_class == kind, ]
  b <- custom_extent[custom_extent$source_class == kind, ]
  assert(missing_m(a, b) < .01 && missing_m(b, a) < .01,
    paste("Custom source attribution changed:", kind))
}
remaining <- read_layer("phase2_remaining")
removed <- read_layer("phase2_removed_upstream")
custom_remaining <- remaining[remaining$pop_id == 1001L & remaining$measure_habitat, ]
custom_removed <- removed[removed$pop_id == 1001L & removed$measure_habitat, ]
assert(abs(length_m(custom_remaining) + length_m(custom_removed) - length_m(source)) < .01,
  "Custom remaining and removed geometry do not partition the full source")
table <- read.csv(file.path(out, "population_phase2_miles.csv"), stringsAsFactors = FALSE)
row <- table[table$pop_id == 1001L, ]
assert(nrow(row) == 1L && abs(row$baseline_untruncated_miles - source_miles) < 1e-8 &&
  abs(row$phase2_remaining_visible_miles - length_m(custom_remaining) / 1609.344) < 1e-8 &&
  abs(row$removed_upstream_miles - length_m(custom_removed) / 1609.344) < 1e-8,
  "Custom mileage table does not match exported complete-source geometry")
official <- read_layer("noaa_populations")
outside_official <- suppressWarnings(st_difference(st_geometry(source),
  st_union(st_geometry(official))))
assert(missing_m(outside_official, custom_extent) < .01,
  "The custom source was incorrectly limited to NOAA-clipped output")

# Rejected mainstem cuts #797/#798 are nested; both must be removed. Retained
# tributary cuts #810/#833 must still remove only their own upstream drainages.
decisions <- read.csv(file.path(out, "phase2_barrier_decisions.csv"), stringsAsFactors = FALSE)
assert(all(!decisions$phase2_applied[match(c(797L, 798L), decisions$review_number)]) &&
  all(decisions$phase2_applied[match(c(810L, 833L), decisions$review_number)]),
  "The requested SF Nooksack mainstem/tributary decisions changed")
audit <- readRDS("spatial_data/phase2/sf_nooksack_restoration_nesting.rds")
masks <- read_layer("upstream_basin_masks")
sf_remaining <- remaining[remaining$pop_id == 292L & remaining$measure_habitat, ]
expected <- suppressWarnings(st_difference(audit$target_lines, st_union(st_geometry(masks))))
# The older nesting audit used basin polygons. Retained #810 also has a proven
# 6.53 m graph-derived upstream correction just outside its DEM basin. Preserve
# that same pre-existing cut, rather than restoring it with rejected #797/#798.
old_removed <- st_read("outputs/phase1_total_barriers/North_Puget_Sound_Steelhead_phase1.gpkg",
  layer = "phase1_removed_upstream", quiet = TRUE)
old_removed <- old_removed[old_removed$pop_id == 292L & old_removed$measure_habitat, ]
retained_keys <- decisions$review_record_key[match(c(810L, 833L), decisions$review_number)]
retained_cause <- vapply(strsplit(old_removed$removed_by_barrier, ";", fixed = TRUE),
  function(keys) any(keys %in% retained_keys), logical(1))
expected <- suppressWarnings(st_difference(expected,
  st_buffer(st_union(st_geometry(old_removed[retained_cause, ])), 1e-4)))
assert(length_m(expected) / 1609.344 > 11.89,
  "Unexpected active cut blocks the reviewed SF Nooksack restoration")
assert(missing_m(expected, sf_remaining) < .01,
  "Restored SF Nooksack upper mainstem or tributary source segments are missing")
assert(abs(length_m(audit$target_lines) / 1609.344 - audit$target_restoration_miles) < 1e-10,
  "Independent SF Nooksack restoration audit changed")
cat(sprintf(paste0("PASS: complete custom basin %.6f km2, all 108 contributing reaches, Deer excluded; ",
  "full %.9f-mile custom source partitioned; %.9f reviewed SF Nooksack miles present.\n"),
  as.numeric(st_area(custom)) / 1e6, source_miles, length_m(expected) / 1609.344))
cat(sprintf("Custom source beyond original NOAA union: %.6f miles.\n", length_m(outside_official) / 1609.344))
cat(sprintf("Custom source/output Hausdorff distance: %.12g m.\n", source_hausdorff_m))
