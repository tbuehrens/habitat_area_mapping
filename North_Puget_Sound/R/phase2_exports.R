# Phase 2 starts with the complete untruncated model + SWIFD network. Phase 1
# masks are inherited explicitly; reviewed passage corrections remove masks,
# never reconstruct missing habitat from a clipped result.
if (!exists("nps_resolve_cache_path", mode = "function")) source(file.path("R", "project_paths.R"))
phase2_geometry <- function(x) {
  sf::st_sf(sf::st_drop_geometry(x), geometry = sf::st_geometry(x))
}

phase2_bind_rows <- function(...) {
  objects <- list(...)
  # CSV report joins may infer numeric LLIDs where the GeoPackage retains
  # character identifiers. Preserve the source schema's textual fields.
  fields <- unique(unlist(lapply(objects, names)))
  for (field in fields) {
    present <- which(vapply(objects, function(x) field %in% names(x), logical(1)))
    text_field <- any(vapply(objects[present], function(x)
      is.character(x[[field]]) || is.factor(x[[field]]), logical(1)))
    if (text_field) for (i in present) objects[[i]][[field]] <- as.character(objects[[i]][[field]])
  }
  do.call(dplyr::bind_rows, objects)
}

phase2_source_manifest <- function(directories) {
  files <- unlist(lapply(directories, list.files, recursive = TRUE, full.names = TRUE))
  data.frame(path = files, md5 = unname(tools::md5sum(files)), stringsAsFactors = FALSE)
}

phase2_check_preserved <- function(manifest) {
  if (!identical(unname(tools::md5sum(nps_resolve_cache_path(manifest$path))), manifest$md5))
    stop("A preserved baseline or Phase 1 file changed")
  invisible(TRUE)
}

phase2_map <- function(populations, remaining, applied, title, labels = FALSE,
    length_summary = NULL, exclusion_numbers = integer(), notes = NULL) {
  p <- phase1_map(populations, remaining, applied, title, labels = labels,
    length_summary = length_summary, exclusion_numbers = exclusion_numbers)
  p$labels$subtitle <- gsub("PHASE 1: reported Total barriers (verified and unverified) + Gorge Dam",
    "PHASE 2: reviewed passage corrections and additional barriers", p$labels$subtitle, fixed = TRUE)
  p$labels$subtitle <- gsub("PHASE 1: verified and unverified reported Total barriers + Gorge Dam",
    "PHASE 2: reviewed passage corrections and additional barriers", p$labels$subtitle, fixed = TRUE)
  caption <- c(if (length(exclusion_numbers)) paste("Upstream exclusions: review barriers",
    paste(sort(unique(exclusion_numbers)), collapse = ", ")),
    "Red numbered points: applied cuts; numbers match phase2_barrier_decisions.csv.",
    "Blue: remaining occupancy model; orange: qualifying SWIFD patches. Dotted alignment links excluded from mileage.",
    "Full upstream drainage masks include disconnected reaches. Unresolved Phase 1 requests remain pending.",
    notes,
    "Sources: NOAA; WDFW/TerrainWorks; SWIFD; WDFW FPDSI; named hydrography; USGS; local passage review.")
  p$labels$caption <- paste(vapply(caption, function(x) paste(strwrap(x, 125), collapse = "\n"), character(1)), collapse = "\n")
  p
}

phase2_population_table <- function(populations, before, after, phase1_table = NULL) {
  table <- sf::st_drop_geometry(populations)
  physical <- after[after$measure_habitat, ]
  measures <- list(
    habitat_length_by_pop(before[before$measure_habitat, ], populations, "baseline_untruncated_miles"),
    habitat_length_by_pop(physical[!physical$removed & !physical$unresolved_direction, ], populations, "remaining_direction_resolved_miles"),
    habitat_length_by_pop(physical[!physical$removed & physical$unresolved_direction, ], populations, "remaining_direction_unresolved_miles"),
    habitat_length_by_pop(physical[physical$removed, ], populations, "removed_upstream_miles"),
    habitat_length_by_pop(physical[!physical$removed, ], populations, "phase2_remaining_visible_miles"),
    habitat_length_by_pop(physical[!physical$removed & physical$source_class == "prediction", ], populations, "phase2_remaining_model_miles"),
    habitat_length_by_pop(physical[!physical$removed & physical$source_class == "swifd_connector", ], populations, "phase2_remaining_swifd_miles"))
  for (measure in measures) table <- dplyr::left_join(table, measure, by = "pop_id")
  if (any(abs(table$baseline_untruncated_miles - table$removed_upstream_miles -
      table$phase2_remaining_visible_miles) > .00001)) stop("Phase 2 population mileage partition failed")
  table$phase1_remaining_visible_miles <- if (is.null(phase1_table)) NA_real_ else
    phase1_table$phase1_remaining_visible_miles[match(table$pop_id, phase1_table$pop_id)]
  table$change_from_phase1_miles <- table$phase2_remaining_visible_miles - table$phase1_remaining_visible_miles
  table$estimate_scope <- "Phase 2 distribution geometry; custom watershed is an overlapping review area, not an official NOAA population"
  table
}

phase2_render_maps <- function(populations, custom_population, remaining, removed,
    applied, decisions, table, gorge_basin, output_dir) {
  overview <- phase2_map(populations, remaining[remaining$pop_id %in% populations$pop_id, ],
    applied, "North Puget Sound steelhead: Phase 2", labels = TRUE)
  habitat_save_map(overview, file.path(output_dir, "North_Puget_Sound_Steelhead_map.png"), 14, 14)
  all_populations <- phase2_bind_rows(populations, custom_population)
  grDevices::pdf(file.path(output_dir, "North_Puget_Sound_Steelhead_population_maps.pdf"), width = 11, height = 10)
  tryCatch({ for (i in seq_len(nrow(all_populations))) {
    pop <- all_populations[i, ]
    pop_removed <- removed[removed$pop_id == pop$pop_id, ]
    keys <- unique(unlist(strsplit(pop_removed$removed_by_barrier, ";", fixed = TRUE)))
    numbers <- decisions$review_number[match(keys, decisions$review_record_key)]
    numbers <- numbers[!is.na(numbers)]
    map_table <- table[table$pop_id == pop$pop_id, ]
    # phase1_map's length-summary argument has a historical field name only.
    map_table$phase1_remaining_visible_miles <- map_table$phase2_remaining_visible_miles
    notes <- if (pop$pop_id == 1001) paste("Custom watershed upstream of the Deer Creek / North Fork Stillaguamish confluence.",
      "Separate review area; the original Stillaguamish population polygon is retained.") else NULL
    if (pop$pop_id == 271) notes <- "Deer Creek #663: passage override from user review; the original report's Partial classification is preserved in the audit."
    if (pop$pop_id == 281) notes <- "West Cady Creek #344: WDFW-reported Total waterfall; its own-stream name alias is corrected in Phase 2."
    if (pop$pop_id == 290) notes <- "Culmback Dam #912: upstream Spada Lake / Sultan River distribution excluded."
    if (pop$pop_id %in% c(280, 292)) notes <- paste("User passage review: #797 and #798 do not truncate steelhead distribution in this scenario.",
      "Cuts at #810 (Wanlick Creek) and #833 (Bell Creek) remain active.")
    if (pop$pop_id == 297) notes <- paste("North Fork Tolt: distribution restored to #222; lower mainstem cuts disabled.",
      "South Fork Tolt: #913 marks the natural falls below the reservoir, separately from the dam.")
    if (pop$pop_id == 288) notes <- "Purple dashed outline: complete Gorge Dam upstream drainage, including Canadian headwaters."
    p <- phase2_map(pop, remaining[remaining$pop_id == pop$pop_id, ],
      applied[lengths(sf::st_intersects(applied, pop)) > 0L, ], pop$population,
      length_summary = map_table, exclusion_numbers = numbers, notes = notes)
    if (pop$pop_id == 288) p <- p + ggplot2::geom_sf(data = gorge_basin,
      fill = NA, color = "#7e5aa6", linetype = "dashed", linewidth = .45)
    slug <- gsub("[^A-Za-z0-9]+", "_", pop$population)
    habitat_save_map(p, file.path(output_dir, "maps", paste0(pop$pop_id, "_", slug, ".png")), 11, 10)
    print(p)
  } }, finally = grDevices::dev.off())
  nooksack <- populations[grepl("Nooksack", populations$population), ]
  habitat_save_map(phase2_map(nooksack, remaining[remaining$pop_id %in% nooksack$pop_id, ],
    applied[lengths(sf::st_intersects(applied, sf::st_union(nooksack))) > 0L, ], "Nooksack steelhead: Phase 2"),
    file.path(output_dir, "Nooksack_Steelhead_map.png"), 12, 10)
  invisible(overview)
}

refresh_phase2_maps <- function(output_dir = "outputs/phase2_reviewed_barriers") {
  file <- file.path(output_dir, "North_Puget_Sound_Steelhead_phase2.gpkg")
  read <- function(layer) phase2_geometry(sf::st_read(file, layer = layer, quiet = TRUE))
  masks <- read("upstream_basin_masks")
  phase2_render_maps(read("noaa_populations"), read("custom_review_watershed"),
    read("phase2_remaining"), read("phase2_removed_upstream"), read("applied_barriers"),
    utils::read.csv(file.path(output_dir, "phase2_barrier_decisions.csv"), stringsAsFactors = FALSE),
    utils::read.csv(file.path(output_dir, "population_phase2_miles.csv"), stringsAsFactors = FALSE),
    masks[masks$review_record_key == "user_override:Gorge_Dam", ], output_dir)
}

run_phase2_reviewed_corrections <- function(baseline_dir = "outputs/review_swifd_connections",
    phase1_dir = "outputs/phase1_total_barriers", output_dir = "outputs/phase2_reviewed_barriers",
    cache_dir = "spatial_data") {
  habitat_require_packages()
  preserved <- phase2_source_manifest(c(baseline_dir, phase1_dir))
  read1 <- function(layer) phase2_geometry(sf::st_read(file.path(phase1_dir, "North_Puget_Sound_Steelhead_phase1.gpkg"),
    layer = layer, quiet = TRUE))
  read0 <- function(layer) phase2_geometry(sf::st_read(file.path(baseline_dir, "North_Puget_Sound_Steelhead.gpkg"),
    layer = layer, quiet = TRUE))
  populations <- read0("noaa_populations")
  populations$is_custom <- FALSE
  stream_edges <- read0("stream_matching_edges")
  network <- readRDS(file.path(cache_dir, "north_puget_swifd_bridges.rds"))$bridges$network
  barriers <- read1("barrier_characteristics")
  old_masks <- read1("upstream_basin_masks")
  old_anchors <- read1("downstream_anchors")
  targeted <- load_phase2_targeted_barriers(file.path(cache_dir, "phase2"))
  dam_tolt <- load_phase2_dam_tolt(file.path(cache_dir, "phase2"))
  custom <- load_phase2_stillaguamish(file.path(cache_dir, "phase2"))
  for (name in c("barriers", "masks", "anchors")) {
    targeted[[name]] <- phase2_geometry(targeted[[name]])
    dam_tolt[[name]] <- phase2_geometry(dam_tolt[[name]])
  }
  custom$population <- phase2_geometry(custom$population)
  additions <- phase2_bind_rows(targeted$barriers, dam_tolt$barriers)
  # Read replacements from full raw source attributes, without touching their
  # original report classifications. Passage overrides receive separate fields.
  barriers <- phase2_bind_rows(barriers[!barriers$review_record_key %in% additions$review_record_key, ], additions)
  barriers <- barriers[order(barriers$review_number), ]
  if (anyDuplicated(barriers$review_number) || anyDuplicated(barriers$review_record_key)) stop("Duplicate Phase 2 barrier numbering")
  restore_numbers <- unique(c(797L, 798L, dam_tolt$remove_review_numbers))
  restore_keys <- barriers$review_record_key[barriers$review_number %in% restore_numbers]
  barriers$phase2_selected <- barriers$phase1_selected %in% TRUE
  barriers$phase2_selected[barriers$review_record_key %in% additions$review_record_key] <- TRUE
  barriers$phase2_selected[barriers$review_number %in% restore_numbers] <- FALSE
  barriers$phase2_review_action <- ifelse(barriers$phase2_selected, "inherit_phase1_selection", "not_selected")
  barriers$phase2_review_action[barriers$review_number %in% c(797L, 798L)] <- "user_review_passable_for_summer_steelhead_restore_original_distribution"
  barriers$phase2_review_action[barriers$review_number %in% dam_tolt$remove_review_numbers] <- "user_review_North_Fork_Tolt_restore_distribution_to_barrier_222"
  added_index <- match(additions$review_record_key, barriers$review_record_key)
  actions <- if ("phase2_review_action" %in% names(additions)) additions$phase2_review_action else rep(NA_character_, nrow(additions))
  actions[additions$review_number == 912L] <- "Apply user-authorized Culmback Dam cutoff for Spada Lake / Sultan River"
  actions[additions$review_number == 913L] <- "Apply user-authorized natural South Fork Tolt Falls cutoff below reservoir; City source map location"
  actions[is.na(actions) | !nzchar(actions)] <- "Apply reviewed additional barrier"
  barriers$phase2_review_action[added_index] <- actions
  selected <- barriers[barriers$phase2_selected, ]
  selected$public_explicit_total_passage <- selected$explicit_total_passage
  # Source/report Total evidence and a user-authorized scenario exception are
  # distinct gates. In particular Deer663 remains source Partial even in the
  # graph diagnostic; its explicit override authorizes the requested scenario.
  selected$explicit_total_passage <- selected$confirmed_total_for_scenario %in% TRUE
  selected$eligible_for_future_cut <- TRUE
  new_keys <- additions$review_record_key[!additions$review_record_key %in% old_masks$review_record_key]
  anchors <- phase2_bind_rows(old_anchors[!old_anchors$review_record_key %in% additions$review_record_key &
    !old_anchors$review_record_key %in% restore_keys, ], targeted$anchors, dam_tolt$anchors)
  anchors <- anchors[!duplicated(anchors$anchor_id), ]
  message("Phase 2: recomputing exact barrier direction from the full untruncated model + SWIFD network")
  graph <- cut_reviewed_barriers(network, selected, stream_edges, anchors)
  candidates <- phase2_bind_rows(targeted$masks, dam_tolt$masks)
  candidates <- candidates[candidates$review_record_key %in% new_keys, ]
  validation_points <- selected
  # The existing validator names its current-scenario gate phase1_selected.
  # Set that gate only on this local Phase 2 validation copy.
  validation_points$phase1_selected <- TRUE
  terminal <- validate_phase2_terminal_dam(candidates[candidates$review_record_key == "user_override:Culmback_Dam", ],
    graph$network, validation_points, stream_edges)
  graph$network <- terminal$network
  checked <- validate_nldi_basin_masks(candidates[candidates$review_record_key != "user_override:Culmback_Dam", ],
    graph$network, validation_points)
  checked$masks <- phase2_bind_rows(phase2_geometry(checked$masks), phase2_geometry(terminal$masks))
  checked$audit <- phase2_bind_rows(checked$audit, terminal$audit)
  if (nrow(checked$masks) != nrow(candidates)) {
    print(checked$audit)
    stop("An added Phase 2 barrier has not passed complete basin/outlet validation")
  }
  masks <- phase2_bind_rows(old_masks[!old_masks$review_record_key %in% restore_keys, ], checked$masks)
  masks <- masks[masks$review_record_key %in% selected$review_record_key, ]
  if (!all(additions$review_record_key %in% masks$review_record_key)) stop("A requested Phase 2 barrier has no complete approved mask")
  # All removals, including inherited ones, are reapplied from the raw network.
  # Diagnostic graph-only effects must not remove pending-basin habitat.
  graph_network <- graph$network
  approved_keys <- masks$review_record_key
  graph$barrier_audit$graph_direction_diagnostic_only <- TRUE
  graph$barrier_audit$final_basin_cut_applied <- graph$barrier_audit$review_record_key %in% approved_keys
  causes <- strsplit(ifelse(is.na(graph_network$removed_by_barrier), "", graph_network$removed_by_barrier), ";", fixed = TRUE)
  held <- lapply(causes, setdiff, y = c("", approved_keys))
  graph_network$removed_by_barrier <- vapply(lapply(causes, intersect, y = approved_keys), paste, collapse = ";", character(1))
  graph_network$removed <- nzchar(graph_network$removed_by_barrier)
  graph_network$retained <- !graph_network$removed
  unresolved <- strsplit(ifelse(is.na(graph_network$unresolved_by_barrier), "", graph_network$unresolved_by_barrier), ";", fixed = TRUE)
  graph_network$unresolved_by_barrier <- vapply(Map(function(a, b) setdiff(union(a, b), c("", approved_keys)),
    unresolved, held), paste, collapse = ";", character(1))
  graph_network$unresolved_direction <- nzchar(graph_network$unresolved_by_barrier) & graph_network$retained
  graph_network$cut_status <- ifelse(graph_network$removed, "removed_upstream", ifelse(graph_network$unresolved_direction,
    "retained_unresolved", "retained_unaffected"))
  watershed <- apply_upstream_basin_masks(graph_network, masks, barrier_points = selected)
  message("Phase 2: complete upstream masks applied; checking population mileage and restored areas")
  all_populations <- phase2_bind_rows(populations, custom$population)
  before <- habitat_clip(network, all_populations)
  after <- habitat_clip(watershed$network, all_populations)
  table <- phase2_population_table(all_populations, before, after,
    utils::read.csv(file.path(phase1_dir, "population_phase1_miles.csv"), stringsAsFactors = FALSE))
  barriers$phase2_applied <- barriers$phase2_selected & barriers$review_record_key %in% approved_keys
  barriers$phase2_covered_by_downstream_basin <- barriers$phase2_selected &
    barriers$covered_by_downstream_basin %in% TRUE & barriers$covering_basin_key %in% approved_keys
  barriers$phase2_complete_upstream_coverage <- barriers$phase2_applied | barriers$phase2_covered_by_downstream_basin
  barriers$phase2_cut_status <- ifelse(barriers$phase2_applied, "applied_complete_upstream_basin",
    ifelse(barriers$phase2_selected, "pending_complete_upstream_basin", barriers$phase2_review_action))
  barriers$phase2_cut_status[barriers$phase2_covered_by_downstream_basin] <- "covered_by_retained_independently_proven_downstream_basin"
  residual_index <- match(barriers$review_record_key, watershed$barrier_audit$review_record_key)
  for (field in c("raw_inside_m", "remaining_inside_m", "protected_local_downstream_m", "max_protection_distance_m"))
    barriers[[paste0("phase2_", field)]] <- watershed$barrier_audit[[field]][residual_index]
  barriers$phase2_unexplained_upstream_remaining_m <- pmax(0, barriers$phase2_remaining_inside_m -
    barriers$phase2_protected_local_downstream_m)
  applied <- barriers[barriers$phase2_applied, ]
  remaining <- after[!after$removed, ]
  removed <- after[after$removed, ]
  dir.create(file.path(output_dir, "maps"), recursive = TRUE, showWarnings = FALSE)
  utils::write.csv(table, file.path(output_dir, "population_phase2_miles.csv"), row.names = FALSE, na = "")
  decisions <- sf::st_drop_geometry(barriers)
  front <- c("review_number", "SiteId", "StreamName", "population_names", "verification",
    "reported_feature_type", "reported_height_m", "reported_height_quality", "report_blockage",
    "phase2_selected", "phase2_applied", "phase2_complete_upstream_coverage", "phase2_cut_status",
    "phase2_review_action", "report_url", "phase1_applied", "review_record_key")
  front <- intersect(front, names(decisions))
  decisions <- decisions[, c(front, setdiff(names(decisions), front)), drop = FALSE]
  utils::write.csv(decisions, file.path(output_dir, "phase2_barrier_decisions.csv"), row.names = FALSE, na = "")
  utils::write.csv(decisions[decisions$phase2_applied, ], file.path(output_dir, "phase2_cut_barriers.csv"), row.names = FALSE, na = "")
  change_rows <- decisions$review_number %in% c(restore_numbers, additions$review_number)
  change_fields <- intersect(c("review_number", "review_record_key", "SiteId", "StreamName", "verification",
    "report_blockage", "reported_feature_type", "reported_height_m", "phase1_applied", "phase2_selected",
    "phase2_applied", "phase2_review_action", "phase2_cut_status", "report_url", "phase2_source_url"), names(decisions))
  utils::write.csv(decisions[change_rows, change_fields], file.path(output_dir, "phase2_changes.csv"), row.names = FALSE, na = "")
  utils::write.csv(watershed$barrier_audit, file.path(output_dir, "upstream_basin_audit.csv"), row.names = FALSE, na = "")
  completeness_fields <- c("review_number", "review_record_key", "StreamName", "phase2_selected", "phase2_applied",
    "phase2_complete_upstream_coverage", "phase2_covered_by_downstream_basin", "covering_basin_key",
    "phase2_raw_inside_m", "phase2_remaining_inside_m", "phase2_protected_local_downstream_m",
    "phase2_unexplained_upstream_remaining_m", "phase2_cut_status")
  utils::write.csv(decisions[decisions$phase2_selected, completeness_fields],
    file.path(output_dir, "basin_completeness_audit.csv"), row.names = FALSE, na = "")
  utils::write.csv(checked$audit, file.path(output_dir, "new_barrier_basin_validation.csv"), row.names = FALSE, na = "")
  utils::write.csv(graph$barrier_audit, file.path(output_dir, "phase2_graph_direction_audit.csv"), row.names = FALSE, na = "")
  utils::write.csv(phase2_bind_rows(targeted$audit, dam_tolt$audit, custom$audit),
    file.path(output_dir, "targeted_review_audit.csv"), row.names = FALSE, na = "")
  utils::write.csv(preserved, file.path(output_dir, "preserved_source_manifest.csv"), row.names = FALSE)
  output_gpkg <- file.path(output_dir, "North_Puget_Sound_Steelhead_phase2.gpkg")
  sf::st_write(populations, output_gpkg, layer = "noaa_populations", delete_dsn = TRUE, quiet = TRUE)
  gis <- list(custom_review_watershed = custom$population, phase2_extent_status = after,
    phase2_remaining = remaining, phase2_removed_upstream = removed,
    barrier_characteristics = barriers, applied_barriers = applied,
    downstream_anchors = graph$downstream_anchors, upstream_basin_masks = masks)
  for (name in names(gis)) sf::st_write(gis[[name]], output_gpkg, layer = name, quiet = TRUE)
  phase2_render_maps(populations, custom$population, remaining, removed, applied, decisions,
    table, masks[masks$review_record_key == "user_override:Gorge_Dam", ], output_dir)
  jsonlite::write_json(list(phase = "Phase 2: user-reviewed passage corrections and additional barriers",
    baseline_directory = baseline_dir, preserved_phase1_directory = phase1_dir,
    original_population_count = nrow(populations), custom_review_watershed_count = nrow(custom$population),
    removed_cut_numbers = sort(restore_numbers), new_or_reviewed_cut_numbers = sort(additions$review_number),
    applied_barriers = sum(barriers$phase2_applied), upstream_summary = watershed$summary,
    selected_with_complete_upstream_coverage = sum(barriers$phase2_complete_upstream_coverage),
    selected_pending_upstream_coverage = sum(barriers$phase2_selected & !barriers$phase2_complete_upstream_coverage),
    source_length_partition = "full raw model plus qualifying SWIFD; alignment links excluded from mileage",
    custom_watershed_policy = "overlapping review area, not an official NOAA population; do not add to regional totals",
    generated_utc = format(Sys.time(), tz = "UTC", usetz = TRUE)), file.path(output_dir, "run_metadata.json"),
    pretty = TRUE, auto_unbox = TRUE)
  writeLines(c("# Phase 2: reviewed barrier corrections", "",
    "Start with `maps/` for the 16 NOAA populations plus the separate custom Stillaguamish watershed.",
    "`North_Puget_Sound_Steelhead_population_maps.pdf` contains all 17 maps.", "",
    "- Deer Creek #663: user-reviewed full passage barrier above Higgins Creek; the source report's Partial classification remains visible.",
    "- West Cady Creek #344: WDFW-reported Total waterfall near the mouth; exact own-stream alias correction.",
    "- Culmback Dam #912: new numbered cutoff for the Spada Lake / Sultan River watershed.",
    "- South Fork Nooksack: #797 and #798 disabled; #810 (Wanlick Creek) and #833 (Bell Creek) retained.",
    "- North Fork Tolt: lower mainstem cutoffs disabled; #222 retained.",
    "- South Fork Tolt: #913 is the natural falls below the reservoir, distinct from the dam and inventory site #170.",
    "- Custom area #1001: North Fork Stillaguamish watershed above the Deer Creek confluence; excludes Deer Creek itself. Original NOAA polygons remain unchanged.", "",
    "Blue lines are the original occupancy model; orange lines are qualifying SWIFD gap patches.",
    "Red points and numbers identify the barriers actually used. Alignment links do not count toward mileage.", "",
    "`population_phase2_miles.csv` gives before, remaining, removed, and change from Phase 1 for each area.",
    "The custom watershed overlaps official populations; do not add its mileage to population totals.",
    "`phase2_changes.csv` lists reviewed changes; `phase2_barrier_decisions.csv` retains every original barrier number and source evidence.",
    "`targeted_review_audit.csv` and `new_barrier_basin_validation.csv` document the added sites and watershed provenance.",
    "`upstream_basin_audit.csv` records every applied basin's final residual and any independently proven local downstream outlet correction.",
    "`basin_completeness_audit.csv` consolidates current Phase 2 coverage and pending cases. Phase 1 fields retained in the decision table are historical.",
    "Unresolved Phase 1 basin requests remain pending; remaining linework is not certified accessible habitat.", "",
    "The GeoPackage supplies original NOAA polygons, the custom watershed, full extent status, remaining and removed geometry, barrier attributes, applied points, direction anchors, and upstream masks.",
    "Both earlier output folders are preserved byte-for-byte; see `preserved_source_manifest.csv`.", "",
    "Reproduce from the repository root with `Rscript Phase2_Reviewed_Barriers.R` after preparing the cached, validated targeted source bundles.",
    "Validate read-only with `Rscript tests/validate_phase2_reviewed_barriers.R`."), file.path(output_dir, "README.md"))
  capture.output(utils::sessionInfo(), file = file.path(output_dir, "sessionInfo.txt"))
  phase2_check_preserved(preserved)
  message("Phase 2 exported: ", nrow(populations), " original populations + custom Stillaguamish watershed; baseline and Phase 1 unchanged")
  invisible(list(table = table, barriers = barriers, watershed = watershed, output_dir = output_dir))
}
