# Read-only independent verification of real Phase 2 products.
source("R/project_paths.R")
suppressPackageStartupMessages(library(sf))
source("tests/upstream_basin_validation_helpers.R")
out <- "outputs/phase2_reviewed_barriers"
p1 <- "outputs/phase1_total_barriers"
assert <- function(x, text) if (!isTRUE(x)) stop(text, call. = FALSE)
unique_m <- function(x) if (!nrow(x)) 0 else sum(as.numeric(st_length(st_union(st_geometry(x)))))
read2 <- function(layer) st_read(file.path(out, "North_Puget_Sound_Steelhead_phase2.gpkg"), layer = layer, quiet = TRUE)
read1 <- function(layer) st_read(file.path(p1, "North_Puget_Sound_Steelhead_phase1.gpkg"), layer = layer, quiet = TRUE)
manifest <- read.csv(file.path(out, "preserved_source_manifest.csv"), stringsAsFactors = FALSE)
assert(identical(unname(tools::md5sum(nps_resolve_cache_path(manifest$path))), manifest$md5), "A baseline or Phase 1 file changed")
decisions <- read.csv(file.path(out, "phase2_barrier_decisions.csv"), stringsAsFactors = FALSE)
old_decisions <- read.csv(file.path(p1, "phase1_barrier_decisions.csv"), stringsAsFactors = FALSE)
assert(all(old_decisions$review_number %in% decisions$review_number) &&
  !anyDuplicated(decisions$review_number) && !anyDuplicated(decisions$review_record_key), "Source numbers lost or duplicated")
for (number in old_decisions$review_number) assert(identical(
  decisions$review_record_key[decisions$review_number == number],
  old_decisions$review_record_key[old_decisions$review_number == number]), "Review number changed source identity")
applied <- decisions[decisions$phase2_applied %in% TRUE, ]
assert(all(c(222L, 344L, 663L, 810L, 833L, 903L, 912L, 913L) %in% applied$review_number), "A requested cutoff is missing")
assert(!any(c(199L, 205L, 209L, 215L, 219L, 220L, 797L, 798L) %in% applied$review_number), "A corrected passable/downstream cutoff still applies")
assert(decisions$report_blockage[decisions$review_number == 663L] == "Partial" &&
  grepl("user", decisions$phase2_review_action[decisions$review_number == 663L], ignore.case = TRUE),
  "Deer source Partial evidence must remain distinct from the user's override")
assert(decisions$report_blockage[decisions$review_number == 344L] == "Total", "West Cady must retain its explicit Total report evidence")
pops <- read2("noaa_populations")
old_pops <- read1("noaa_populations")
assert(nrow(pops) == 16L && identical(pops$pop_id, old_pops$pop_id), "Official populations changed")
for (i in seq_len(nrow(pops))) assert(length(st_equals_exact(st_geometry(pops)[i],
  st_geometry(old_pops)[i], par = 0)[[1L]]) == 1L, "Original NOAA polygon changed")
custom <- read2("custom_review_watershed")
assert(nrow(custom) == 1L && custom$pop_id == 1001L && st_is_valid(custom) &&
  !st_is_empty(custom) && as.numeric(st_area(custom)) > 1e6, "Custom watershed is missing or invalid")
table <- read.csv(file.path(out, "population_phase2_miles.csv"), stringsAsFactors = FALSE)
old_table <- read.csv(file.path(p1, "population_phase1_miles.csv"), stringsAsFactors = FALSE)
assert(nrow(table) == 17L && all(c(pops$pop_id, 1001L) %in% table$pop_id), "Expected 16 populations plus one custom watershed")
assert(all(abs(table$baseline_untruncated_miles - table$phase2_remaining_visible_miles - table$removed_upstream_miles) < 1e-6), "Per-population source partition failed")
assert(all(abs(table$baseline_untruncated_miles[match(old_table$pop_id, table$pop_id)] -
  old_table$baseline_untruncated_miles) < 1e-6), "Phase 2 did not start from the same complete baseline")
remaining <- read2("phase2_remaining")
removed <- read2("phase2_removed_upstream")
masks <- read2("upstream_basin_masks")
barriers <- read2("barrier_characteristics")
remaining <- remaining[remaining$measure_habitat, ]
removed <- removed[removed$measure_habitat, ]
coverage <- validate_upstream_basin_completeness(remaining, removed, masks, barriers)
assert(all(coverage$unproven_upstream_residual_m < .001), "Missed upstream habitat remains")
assert(identical(sort(masks$review_record_key), sort(applied$review_record_key)), "Applied points and drainage masks disagree")

# Every old source piece removed solely by a rejected barrier must return,
# unless a retained or newly approved upstream basin independently excludes it.
old_removed <- read1("phase1_removed_upstream")
old_removed <- old_removed[old_removed$measure_habitat, ]
old_masks <- read1("upstream_basin_masks")
dropped_keys <- setdiff(old_masks$review_record_key, masks$review_record_key)
old_causes <- strsplit(ifelse(is.na(old_removed$removed_by_barrier), "", old_removed$removed_by_barrier), ";", fixed = TRUE)
restore <- old_removed[vapply(old_causes, function(z) length(z) > 0L && all(z %in% dropped_keys), logical(1)), ]
expected <- suppressWarnings(st_difference(restore, st_union(st_geometry(masks))))
expected <- expected[!st_is_empty(expected), ]
for (id in unique(expected$pop_id)) {
  wanted <- expected[expected$pop_id == id, ]
  available <- remaining[remaining$pop_id == id, ]
  missing <- suppressWarnings(st_difference(st_union(st_geometry(wanted)), st_buffer(st_union(st_geometry(available)), 1e-5)))
  assert(sum(as.numeric(st_length(missing))) < .01, paste("Requested restoration missing in population", id))
}
assert(table$phase2_remaining_visible_miles[table$pop_id == 292L] >
  old_table$phase1_remaining_visible_miles[old_table$pop_id == 292L], "SF Nooksack restoration did not increase remaining mileage")

# Differences from Phase 1 must occur only in the changed drainage footprints,
# allowing the independently checked <=100m outlet precision zone.
new_masks <- masks[!masks$review_record_key %in% old_masks$review_record_key, ]
dropped_masks <- old_masks[old_masks$review_record_key %in% dropped_keys, ]
changed_geometry <- st_union(c(st_geometry(new_masks), st_geometry(dropped_masks)))
changed_geometry <- st_buffer(changed_geometry, 100)
old_remaining <- read1("phase1_remaining")
old_remaining <- old_remaining[old_remaining$measure_habitat, ]
for (id in pops$pop_id) {
  a <- st_union(st_geometry(old_remaining[old_remaining$pop_id == id, ]))
  b <- st_union(st_geometry(remaining[remaining$pop_id == id, ]))
  outside_a <- suppressWarnings(st_difference(a, changed_geometry))
  outside_b <- suppressWarnings(st_difference(b, changed_geometry))
  # GEOS can leave metre-long seam remnants when intersecting a line with its
  # own 10-micrometre buffer at UTM coordinates. Deer271 reproduced that false
  # difference against itself despite topological equality and <1nm Hausdorff
  # distance. Prefer exact equality; otherwise use a checked 0.1mm overlay
  # tolerance, far below the source and barrier-placement precision.
  if (length(st_equals(outside_a, outside_b)[[1L]]) == 1L) next
  buffer_a <- st_buffer(outside_a, .0001)
  buffer_b <- st_buffer(outside_b, .0001)
  self_a <- suppressWarnings(st_difference(outside_a, buffer_a))
  self_b <- suppressWarnings(st_difference(outside_b, buffer_b))
  assert(sum(as.numeric(st_length(self_a))) + sum(as.numeric(st_length(self_b))) < .01,
    paste("Numerical buffer self-coverage failed in population", id))
  lost <- suppressWarnings(st_difference(outside_a, buffer_b))
  gained <- suppressWarnings(st_difference(outside_b, buffer_a))
  assert(sum(as.numeric(st_length(lost))) + sum(as.numeric(st_length(gained))) < .01,
    paste("Unrequested distribution change outside reviewed basins in population", id))
}
map_files <- list.files(file.path(out, "maps"), pattern = "\\.png$", full.names = TRUE)
assert(length(map_files) == 17L && all(file.info(map_files)$size > 10000), "The 17-map set is incomplete")
assert(file.info(file.path(out, "North_Puget_Sound_Steelhead_population_maps.pdf"))$size > 10000, "Population PDF missing")
cat("PASS: all preserved input files unchanged; stable barrier numbers; requested restores/cuts; 17 mileage partitions; all approved upstream basins complete; no unrequested spatial changes.\n")
print(table[, c("pop_id", "population", "phase2_remaining_visible_miles", "change_from_phase1_miles")], row.names = FALSE)
