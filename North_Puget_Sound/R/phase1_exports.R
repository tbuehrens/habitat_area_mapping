# Phase 1 is an evidence-limited scenario, separate from the untruncated review.
# Green confirms a channel assignment; it never supplies passage evidence.
phase1_select_barriers <- function(x) {
  inside <- !is.na(x$population_ids) & nzchar(trimws(x$population_ids))
  strict <- x$snap_accepted %in% TRUE & x$stream_identity_confirmed %in% TRUE &
    x$identity_valid_for_cut %in% TRUE & !(x$junction_ambiguous %in% TRUE)
  verified <- !is.na(x$verification) & x$verification == "Verified"
  total <- x$confirmed_total_for_scenario %in% TRUE
  override <- x$override_future_cut %in% TRUE
  conflict <- x$total_evidence_conflict %in% TRUE
  x$phase1_selected <- inside & strict & !conflict & (override | total)
  x$unverified_total_included <- x$phase1_selected & !verified & !override
  x$later_partial_candidate <- inside & strict &
    tolower(trimws(x$report_blockage)) %in% c("partial", "partial blockage")
  x$phase1_selection_reason <- ifelse(!inside, "Outside NOAA population scope",
    ifelse(!strict, "Hold: stream assignment or junction needs review",
      ifelse(conflict, "Hold: conflicting passage evidence",
        ifelse(override, "Select: explicit Gorge Dam override",
          ifelse(!total, "Hold: no explicit Total classification",
            ifelse(verified, "Select: field-verified reported Total", "Select: unverified reported Total"))))))
  x
}

phase1_map <- function(populations, remaining, applied, title, labels = FALSE,
                       length_summary = NULL, exclusion_numbers = integer()) {
  remaining$line_source <- ifelse(remaining$source_class == "prediction", "Remaining model",
    ifelse(remaining$source_class == "swifd_connector", "Remaining SWIFD patch", "Alignment (not mileage)"))
  remaining$width <- ifelse(remaining$source_class == "swifd_connector", .8,
    ifelse(remaining$source_class == "alignment_link", .25, .45))
  p <- ggplot2::ggplot() +
    ggplot2::geom_sf(data = populations, fill = "#f3f2eb", color = "#414c4b", linewidth = .5) +
    ggplot2::geom_sf(data = remaining,
      ggplot2::aes(color = line_source, linetype = line_source, linewidth = width), show.legend = "line") +
    ggplot2::scale_linewidth_identity() +
    ggplot2::scale_color_manual(name = NULL, values = c("Remaining model" = "#126a91",
      "Remaining SWIFD patch" = "#e66b18", "Alignment (not mileage)" = "#776f68")) +
    ggplot2::scale_linetype_manual(name = NULL, values = c("Remaining model" = "solid",
      "Remaining SWIFD patch" = "solid", "Alignment (not mileage)" = "dotted")) +
    ggplot2::geom_sf(data = applied, shape = 21, fill = "#d72e32", color = "#333333", size = 3) +
    ggplot2::coord_sf(datum = sf::st_crs(4326)) +
    ggplot2::labs(title = title,
      subtitle = "PHASE 1: verified and unverified reported Total barriers + Gorge Dam",
      x = NULL, y = NULL,
      caption = paste("Red numbered points: applied cuts; numbers match phase1_barrier_decisions.csv.",
        "Approved upstream drainage masks remove connected and isolated reaches. Unresolved basin requests remain pending.",
        "Partial, unknown, conflicting and spatially ambiguous barriers are deferred; verification status stays in the audit.",
        "Reported Total concerns adult salmonids; steelhead run-specific passage has not been independently established.",
        "Sources: NOAA; WDFW/TerrainWorks; SWIFD; WDFW FPDSI site reports; named hydrography; USGS/Seattle City Light.", sep = "\n")) +
    ggplot2::theme_bw(base_size = 11) +
    ggplot2::theme(plot.title = ggplot2::element_text(face = "bold", size = 16),
      plot.subtitle = ggplot2::element_text(size = 10), plot.caption = ggplot2::element_text(size = 8, hjust = 0),
      legend.position = "bottom", panel.grid.major = ggplot2::element_line(color = "#e1e7e8", linewidth = .15))
  if (nrow(applied)) {
    applied_labels <- sf::st_sf(review_number = applied$review_number, geometry = sf::st_geometry(applied))
    if (requireNamespace("ggrepel", quietly = TRUE)) p <- p + ggrepel::geom_text_repel(
      data = applied_labels, ggplot2::aes(label = review_number, geometry = geometry), stat = "sf_coordinates",
      size = 2.5, fontface = "bold", seed = 903, box.padding = .25, point.padding = .15,
      min.segment.length = 0, segment.color = "#555555", segment.size = .2,
      max.overlaps = Inf, max.time = 1, max.iter = 10000)
    else p <- p + ggplot2::geom_sf_text(data = applied,
      ggplot2::aes(label = review_number), nudge_y = 100, size = 2.5, fontface = "bold")
  }
  if (labels) p <- p + ggplot2::geom_sf_label(data = suppressWarnings(sf::st_point_on_surface(populations)),
    ggplot2::aes(label = population), size = 2.3, alpha = .8, linewidth = .1)
  if (!is.null(length_summary)) p <- p + ggplot2::labs(subtitle = paste(
    "PHASE 1: reported Total barriers (verified and unverified) + Gorge Dam",
    sprintf("Before %.2f mi | Remaining %.2f mi | Removed %.2f mi", length_summary$baseline_untruncated_miles,
      length_summary$phase1_remaining_visible_miles, length_summary$removed_upstream_miles), sep = "\n"))
  if (length(exclusion_numbers)) {
    text <- paste(head(sort(unique(exclusion_numbers)), 25), collapse = ", ")
    if (length(unique(exclusion_numbers)) > 25) text <- paste0(text, ", ...")
    p <- p + ggplot2::labs(caption = paste(
      paste0("Upstream exclusions are attributed to review barriers ", text,
        ". A downstream barrier may lie outside this population polygon."),
      p$labels$caption, sep = "\n"))
  }
  p$labels$caption <- paste(strwrap(p$labels$caption, width = 120), collapse = "\n")
  p
}

phase1_render_maps <- function(populations, before, remaining, removed, applied,
                                decisions, table, gorge_basin, output_dir) {
  overview <- phase1_map(populations, remaining, applied, "North Puget Sound steelhead: Phase 1", labels = TRUE)
  habitat_save_map(overview, file.path(output_dir, "North_Puget_Sound_Steelhead_map.png"), 14, 14)
  grDevices::pdf(file.path(output_dir, "North_Puget_Sound_Steelhead_population_maps.pdf"), width = 11, height = 10)
  tryCatch({ for (i in seq_len(nrow(populations))) {
    pop <- populations[i, ]
    pop_removed <- removed[removed$pop_id == pop$pop_id, ]
    cause_keys <- unique(unlist(strsplit(pop_removed$removed_by_barrier, ";", fixed = TRUE)))
    cause_numbers <- decisions$review_number[match(cause_keys, decisions$review_record_key)]
    cause_numbers <- cause_numbers[!is.na(cause_numbers)]
    p <- phase1_map(pop, remaining[remaining$pop_id == pop$pop_id, ],
      applied[lengths(sf::st_intersects(applied, pop)) > 0, ], pop$population,
      length_summary = table[table$pop_id == pop$pop_id, ], exclusion_numbers = cause_numbers)
    if (pop$pop_id == 288) p <- p +
      ggplot2::geom_sf(data = gorge_basin, fill = NA, color = "#7e5aa6", linetype = "dashed", linewidth = .45) +
      ggplot2::labs(caption = paste("Purple dashed outline: full Gorge Dam upstream drainage, including Canadian headwaters.",
        "Remaining lines outside that outline drain elsewhere; map latitude alone does not establish upstream position.",
        p$labels$caption, sep = "\n"))
    slug <- gsub("[^A-Za-z0-9]+", "_", pop$population)
    habitat_save_map(p, file.path(output_dir, "maps", paste0(pop$pop_id, "_", slug, ".png")), 11, 10)
    print(p)
  } }, finally = grDevices::dev.off())
  nooksack <- populations[grepl("Nooksack", populations$population), ]
  habitat_save_map(phase1_map(nooksack, remaining[remaining$pop_id %in% nooksack$pop_id, ],
    applied[lengths(sf::st_intersects(applied, sf::st_union(nooksack))) > 0, ], "Nooksack steelhead: Phase 1"),
    file.path(output_dir, "Nooksack_Steelhead_map.png"), 12, 10)
  skagit <- populations[populations$pop_id == 288, ]
  a <- applied[lengths(sf::st_intersects(applied, skagit)) > 0, ]
  outline <- ggplot2::geom_sf(data = gorge_basin, fill = NA, color = "#7e5aa6", linetype = "dashed", linewidth = .45)
  comparison <- gridExtra::arrangeGrob(
    phase1_map(skagit, before[before$pop_id == 288, ], a[0, ], "Before Phase 1") + outline +
      ggplot2::labs(subtitle = "Untruncated model + SWIFD patches",
        caption = "Untruncated baseline; blue = model, orange = SWIFD. Purple dashed outline = Gorge upstream drainage."),
    phase1_map(skagit, remaining[remaining$pop_id == 288, ], a, "After Phase 1") + outline +
      ggplot2::labs(caption = paste("Purple dashed outline: Gorge upstream drainage, including Canada.",
        "All upstream reaches within it have been excluded, with only independently proven local downstream precision corrections.",
        "Remaining reaches outside the outline belong to other drainages. Other Total-barrier basins are also applied in Phase 1.", sep = "\n")), ncol = 2)
  habitat_save_map(comparison, file.path(output_dir, "Skagit_before_after.png"), 20, 11)
  invisible(overview)
}

refresh_phase1_maps <- function(output_dir = "outputs/phase1_total_barriers",
    baseline_dir = "outputs/review_swifd_connections") {
  file <- file.path(output_dir, "North_Puget_Sound_Steelhead_phase1.gpkg")
  read <- function(layer) sf::st_read(file, layer = layer, quiet = TRUE)
  populations <- read("noaa_populations")
  remaining <- read("phase1_remaining")
  removed <- read("phase1_removed_upstream")
  applied <- read("applied_barriers")
  masks <- read("upstream_basin_masks")
  gorge_basin <- masks[masks$review_record_key == "user_override:Gorge_Dam", ]
  before <- sf::st_read(file.path(baseline_dir, "North_Puget_Sound_Steelhead.gpkg"),
    layer = "steelhead_patched_untruncated", quiet = TRUE)
  decisions <- utils::read.csv(file.path(output_dir, "phase1_barrier_decisions.csv"), stringsAsFactors = FALSE)
  table <- utils::read.csv(file.path(output_dir, "population_phase1_miles.csv"), stringsAsFactors = FALSE)
  phase1_render_maps(populations, before, remaining, removed, applied, decisions, table, gorge_basin, output_dir)
}

run_phase1_total_barriers <- function(baseline_dir = "outputs/review_swifd_connections",
    output_dir = "outputs/phase1_total_barriers", cache_dir = "spatial_data") {
  habitat_require_packages()
  if (!requireNamespace("gridExtra", quietly = TRUE)) stop("Install gridExtra for the comparison figure")
  gpkg <- file.path(baseline_dir, "North_Puget_Sound_Steelhead.gpkg")
  if (!file.exists(gpkg)) stop("Generate the strict untruncated review first")
  populations <- sf::st_read(gpkg, layer = "noaa_populations", quiet = TRUE)
  barriers <- sf::st_read(gpkg, layer = "barriers_snapped", quiet = TRUE)
  stream_edges <- sf::st_read(gpkg, layer = "stream_matching_edges", quiet = TRUE)
  before <- sf::st_read(gpkg, layer = "steelhead_patched_untruncated", quiet = TRUE)
  baseline_files <- list.files(baseline_dir, recursive = TRUE, full.names = TRUE)
  baseline_hashes <- tools::md5sum(baseline_files)
  bridges <- readRDS(file.path(cache_dir, "north_puget_swifd_bridges.rds"))$bridges
  evidence <- load_barrier_report_evidence(file.path(cache_dir, "barrier_reports"))
  layers <- lapply(c(5, 6), function(id) jsonlite::fromJSON(
    file.path(cache_dir, paste0("barrier_layer_", id, ".json")), simplifyVector = FALSE))
  metadata_audit <- audit_barrier_metadata(barriers, layers, evidence)
  decisions <- phase1_select_barriers(metadata_audit$decoded)
  # Keep all published/source fields. Only the graph input is enriched with
  # report-backed explicit Total evidence; its scope and provenance stay visible.
  enriched <- barriers
  for (field in names(decisions)) enriched[[field]] <- decisions[[field]]
  selected <- enriched[enriched$phase1_selected, ]
  selected$public_explicit_total_passage <- selected$explicit_total_passage
  selected$explicit_total_passage <- selected$confirmed_total_for_scenario
  selected$total_passage_evidence <- selected$total_evidence_basis
  selected$eligible_for_future_cut <- TRUE
  message("Phase 1: ", nrow(selected), " reported Total candidates, including verified and unverified records")
  named <- load_stream_identity_reference(cache_dir = cache_dir)
  marine <- readRDS(file.path(cache_dir, "marine_north.rds"))
  delineations <- load_nldi_delineations(selected, stream_edges,
    cache_dir = file.path(cache_dir, "upstream_basins", "batch"))
  if (!all(selected$review_record_key %in% delineations$audit$review_record_key))
    stop("The upstream delineation batch is incomplete; finish the cached requests before exporting")
  message("Phase 1: cached drainage requests loaded; ", nrow(delineations$candidate_masks), " candidate full basins")
  direction <- derive_downstream_anchors(selected[!selected$override_future_cut, ], stream_edges,
    named, marine_geometry = marine)
  anchors <- dplyr::bind_rows(direction$anchors, gorge_downstream_anchor())
  extra_anchors <- delineations$downstream_anchors[
    !delineations$downstream_anchors$review_record_key %in% anchors$review_record_key, ]
  anchors <- dplyr::bind_rows(anchors, extra_anchors)
  cuts <- cut_reviewed_barriers(bridges$network, selected, stream_edges, anchors)
  message("Phase 1: local graph direction checked; validating complete upstream drainage masks")
  graph_audit <- cuts$barrier_audit
  checked <- validate_nldi_basin_masks(delineations$candidate_masks, cuts$network, selected)
  gorge <- selected[selected$override_future_cut, ]
  gorge_basin <- load_gorge_upstream_basin(gorge, cuts$network,
    cache_dir = file.path(cache_dir, "upstream_basins"))
  approved_masks <- dplyr::bind_rows(gorge_basin, checked$masks)
  approved_keys <- approved_masks$review_record_key
  message("Phase 1: ", length(approved_keys), " complete drainage masks approved; checking nested upstream coverage")
  navigation <- load_nldi_upstream_membership(approved_masks,
    cache_dir = file.path(cache_dir, "upstream_basins", "navigation"), barrier_points = selected)
  coverage <- assess_downstream_basin_coverage(selected, approved_masks,
    file.path(cache_dir, "upstream_basins", "batch", "manifest.json"), navigation$membership,
    stream_edges = stream_edges)
  # An incomplete delineation cannot masquerade as a complete upstream cut.
  # Keep graph results as direction evidence, but hold removals attributed only
  # to barriers without an approved full basin. The approved graph corrections
  # still locate each exact cut inside the DEM outlet-precision zone.
  graph_network <- cuts$network
  old_causes <- strsplit(ifelse(is.na(graph_network$removed_by_barrier), "",
    graph_network$removed_by_barrier), ";", fixed = TRUE)
  kept_causes <- lapply(old_causes, intersect, y = approved_keys)
  held_causes <- lapply(old_causes, setdiff, y = approved_keys)
  graph_network$removed_by_barrier <- vapply(kept_causes, paste, collapse = ";", character(1))
  graph_network$removed <- lengths(kept_causes) > 0L
  graph_network$retained <- !graph_network$removed
  old_unresolved <- strsplit(ifelse(is.na(graph_network$unresolved_by_barrier), "",
    graph_network$unresolved_by_barrier), ";", fixed = TRUE)
  pending_causes <- Map(function(a, b) setdiff(union(a, b), c("", approved_keys)), old_unresolved, held_causes)
  graph_network$unresolved_by_barrier <- vapply(pending_causes, paste, collapse = ";", character(1))
  graph_network$unresolved_direction <- lengths(pending_causes) > 0L & graph_network$retained
  graph_network$cut_status <- ifelse(graph_network$removed, "removed_upstream",
    ifelse(graph_network$unresolved_direction, "retained_unresolved", "retained_unaffected"))
  watershed <- apply_upstream_basin_masks(graph_network, approved_masks, barrier_points = selected)
  message("Phase 1: all approved upstream masks applied and checked for remaining upstream linework")
  cuts$network <- watershed$network
  cuts$retained <- watershed$retained
  cuts$removed <- watershed$removed
  cuts$unresolved <- watershed$unresolved
  index <- match(decisions$review_record_key, graph_audit$review_record_key)
  decisions$upstream_basin_approved <- decisions$review_record_key %in% approved_keys
  decisions$phase1_applied <- decisions$phase1_selected & decisions$upstream_basin_approved
  decisions$phase1_cut_status <- decisions$phase1_selection_reason
  decisions$phase1_cut_status[!is.na(index)] <- graph_audit$cut_status[index[!is.na(index)]]
  decisions$phase1_cut_status[decisions$phase1_selected & !decisions$upstream_basin_approved] <-
    "held_pending_complete_validated_upstream_basin"
  decisions$phase1_cut_status[decisions$phase1_applied] <- "applied_complete_upstream_basin_with_local_outlet_correction"
  coverage_index <- match(decisions$review_record_key, coverage$audit$review_record_key)
  decisions$complete_upstream_coverage <- !is.na(coverage_index) &
    coverage$audit$complete_upstream_coverage[coverage_index] %in% TRUE
  decisions$covered_by_downstream_basin <- !is.na(coverage_index) &
    coverage$audit$covered_by_downstream_basin[coverage_index] %in% TRUE
  decisions$covering_basin_key <- coverage$audit$covering_basin_key[coverage_index]
  decisions$coverage_status <- coverage$audit$coverage_status[coverage_index]
  decisions$phase1_cut_status[decisions$covered_by_downstream_basin] <-
    "upstream_habitat_already_covered_by_approved_downstream_Total_basin"
  basin_index <- match(decisions$review_record_key, watershed$barrier_audit$review_record_key)
  for (field in c("raw_inside_m", "remaining_inside_m", "protected_local_downstream_m",
    "max_protection_distance_m")) decisions[[field]] <- watershed$barrier_audit[[field]][basin_index]
  decisions$unexplained_upstream_remaining_m <- pmax(0,
    decisions$remaining_inside_m - decisions$protected_local_downstream_m)
  for (field in c("downstream_anchor_id", "downstream_direction_basis", "downstream_direction_source_url",
    "anchor_snap_distance_m", "removed_model_m", "removed_swifd_m", "removed_alignment_m"))
    decisions[[paste0("phase1_", field)]] <- graph_audit[[field]][index]
  # Per-barrier effects overlap and must never be added to obtain population
  # totals. Count the final complete basin effect, including disconnected lines.
  for (entry in list(c("prediction", "model"), c("swifd_connector", "swifd"), c("alignment_link", "alignment"))) {
    decisions[[paste0("phase1_removed_", entry[2L], "_m")]] <- vapply(decisions$review_record_key, function(key) {
      rows <- upstream_has_key(cuts$network$removed_by_barrier, key) & cuts$network$source_class == entry[1L]
      sum(cuts$network$length_m[rows])
    }, numeric(1))
  }
  decisions$per_barrier_effects_overlap <- TRUE
  graph_audit$graph_direction_diagnostic_only <- TRUE
  graph_audit$final_basin_cut_applied <- graph_audit$review_record_key %in% approved_keys
  decisions$reported_feature_type <- decisions$report_natural_type
  decisions$reported_height_m <- decisions$report_waterfall_height_m
  decisions$reported_height_quality <- decisions$report_waterfall_height_quality
  # A reported zero is retained in raw evidence but cannot be presented as a
  # reliable measured height. It does not itself change a Total classification.
  decisions$reported_height_m[decisions$reported_height_quality %in% "reported_zero_review"] <- NA_real_
  front <- c("review_number", "SiteId", "StreamName", "population_names", "verification",
    "reported_feature_type", "reported_height_m", "reported_height_quality", "report_blockage",
    "phase1_selected", "phase1_applied", "upstream_basin_approved", "complete_upstream_coverage",
    "coverage_status", "phase1_cut_status", "report_url", "report_assessment_pages")
  decisions <- decisions[, c(front, setdiff(names(decisions), front)), drop = FALSE]
  for (field in names(decisions)) enriched[[field]] <- decisions[[field]]
  applied <- enriched[enriched$phase1_applied, ]
  after <- habitat_clip(cuts$network, populations)
  remaining <- after[!after$removed, ]
  removed <- after[after$removed, ]
  unresolved <- after[after$unresolved_direction, ]
  physical <- after[after$measure_habitat, ]
  table <- sf::st_drop_geometry(populations)
  measures <- list(habitat_length_by_pop(before[before$measure_habitat, ], populations, "baseline_untruncated_miles"),
    habitat_length_by_pop(physical[!physical$removed & !physical$unresolved_direction, ], populations, "remaining_direction_resolved_miles"),
    habitat_length_by_pop(physical[physical$unresolved_direction, ], populations, "remaining_direction_unresolved_miles"),
    habitat_length_by_pop(physical[physical$removed, ], populations, "removed_upstream_miles"),
    habitat_length_by_pop(physical[!physical$removed, ], populations, "phase1_remaining_visible_miles"),
    habitat_length_by_pop(physical[!physical$removed & physical$source_class == "prediction", ], populations, "phase1_remaining_model_miles"),
    habitat_length_by_pop(physical[!physical$removed & physical$source_class == "swifd_connector", ], populations, "phase1_remaining_swifd_miles"))
  for (measure in measures) table <- dplyr::left_join(table, measure, by = "pop_id")
  balance <- table$baseline_untruncated_miles - table$remaining_direction_resolved_miles -
    table$remaining_direction_unresolved_miles - table$removed_upstream_miles
  if (any(abs(balance) > .001)) stop("Phase 1 population length partition failed")
  table$estimate_scope <- "Phase 1 geometry scenario; remaining linework is not certified accessible habitat"
  dir.create(file.path(output_dir, "maps"), recursive = TRUE, showWarnings = FALSE)
  message("Phase 1: writing updated figures and per-basin completeness audits")
  utils::write.csv(table, file.path(output_dir, "population_phase1_miles.csv"), row.names = FALSE)
  utils::write.csv(decisions, file.path(output_dir, "barrier_characteristics.csv"), row.names = FALSE, na = "")
  utils::write.csv(decisions, file.path(output_dir, "phase1_barrier_decisions.csv"), row.names = FALSE, na = "")
  utils::write.csv(decisions[decisions$phase1_applied, ], file.path(output_dir, "phase1_cut_barriers.csv"), row.names = FALSE, na = "")
  utils::write.csv(graph_audit, file.path(output_dir, "phase1_graph_cut_audit.csv"), row.names = FALSE, na = "")
  utils::write.csv(metadata_audit$field_inventory, file.path(output_dir, "barrier_field_inventory.csv"), row.names = FALSE)
  utils::write.csv(metadata_audit$code_inventory, file.path(output_dir, "barrier_code_inventory.csv"), row.names = FALSE)
  utils::write.csv(metadata_audit$summary, file.path(output_dir, "barrier_metadata_summary.csv"), row.names = FALSE)
  utils::write.csv(attr(evidence, "fetch_audit"), file.path(output_dir, "report_fetch_audit.csv"), row.names = FALSE)
  utils::write.csv(direction$audit, file.path(output_dir, "downstream_direction_audit.csv"), row.names = FALSE, na = "")
  utils::write.csv(delineations$audit, file.path(output_dir, "nldi_delineation_audit.csv"), row.names = FALSE, na = "")
  utils::write.csv(checked$audit, file.path(output_dir, "upstream_basin_validation.csv"), row.names = FALSE, na = "")
  utils::write.csv(watershed$barrier_audit, file.path(output_dir, "upstream_basin_audit.csv"), row.names = FALSE, na = "")
  utils::write.csv(coverage$audit, file.path(output_dir, "upstream_coverage_audit.csv"), row.names = FALSE, na = "")
  utils::write.csv(navigation$audit, file.path(output_dir, "upstream_navigation_audit.csv"), row.names = FALSE, na = "")
  completeness_fields <- c("review_record_key", "review_number", "SiteId", "StreamName", "verification",
    "report_blockage", "phase1_selected", "phase1_applied", "upstream_basin_approved",
    "complete_upstream_coverage", "covered_by_downstream_basin", "covering_basin_key", "coverage_status",
    "raw_inside_m", "remaining_inside_m", "protected_local_downstream_m", "max_protection_distance_m",
    "unexplained_upstream_remaining_m", "phase1_cut_status")
  utils::write.csv(decisions[decisions$phase1_selected, completeness_fields],
    file.path(output_dir, "basin_completeness_audit.csv"), row.names = FALSE, na = "")
  overview <- phase1_render_maps(populations, before, remaining, removed, applied,
    decisions, table, gorge_basin, output_dir)
  output_gpkg <- file.path(output_dir, "North_Puget_Sound_Steelhead_phase1.gpkg")
  sf::st_write(populations, output_gpkg, layer = "noaa_populations", delete_dsn = TRUE, quiet = TRUE)
  gis <- list(phase1_extent_status = after, phase1_remaining = remaining,
    phase1_removed_upstream = removed, phase1_unresolved_direction = unresolved,
    barrier_characteristics = enriched, applied_barriers = applied,
    downstream_anchors = cuts$downstream_anchors, upstream_basin_masks = approved_masks)
  for (name in names(gis)) sf::st_write(gis[[name]], output_gpkg, layer = name, quiet = TRUE)
  manifest <- data.frame(baseline_file = substring(baseline_files, nchar(baseline_dir) + 2L),
    md5 = unname(baseline_hashes), stringsAsFactors = FALSE)
  utils::write.csv(manifest, file.path(output_dir, "baseline_file_manifest.csv"), row.names = FALSE)
  metadata <- list(phase = "Phase 1: verified and unverified reported Total natural barriers plus explicit Gorge Dam override",
    baseline_directory = baseline_dir, output_directory = output_dir,
    selection = "Inside NOAA populations; confirmed stream match, unambiguous junction; explicit Total without conflicting passage evidence (verified or unverified), or Gorge override; Partial excluded",
    direction_policy = "Apply only validated full upstream drainage masks, including disconnected reaches; preserve independently proven local downstream outlet geometry and unrelated basins",
    passage_scope = "Qualitative adult-salmonid classification; neither height nor green stream matching establishes steelhead run-specific passability",
    selected_barriers = sum(decisions$phase1_selected), applied_barriers = sum(decisions$phase1_applied),
    graph_diagnostic_summary = cuts$summary, graph_settings = cuts$settings,
    nldi_course_controls = delineations$settings,
    watershed_summary = watershed$summary, watershed_settings = watershed$settings,
    approved_upstream_basins = nrow(approved_masks), pending_upstream_basins = sum(decisions$phase1_selected & !decisions$upstream_basin_approved),
    selected_with_complete_upstream_coverage = sum(decisions$phase1_selected & decisions$complete_upstream_coverage),
    selected_covered_by_downstream_basin = sum(decisions$covered_by_downstream_basin),
    selected_pending_upstream_coverage = sum(decisions$phase1_selected & !decisions$complete_upstream_coverage),
    reference_direction_settings = direction$settings,
    manual_source = "https://wdfw.wa.gov/publications/02061",
    generated_utc = format(Sys.time(), tz = "UTC", usetz = TRUE))
  jsonlite::write_json(metadata, file.path(output_dir, "run_metadata.json"), pretty = TRUE, auto_unbox = TRUE)
  capture.output(utils::sessionInfo(), file = file.path(output_dir, "sessionInfo.txt"))
  if (!identical(unname(tools::md5sum(baseline_files)), unname(baseline_hashes))) stop("Baseline files changed")
  message("Phase 1 exported separately: ", sum(decisions$phase1_applied), " applied cuts; untruncated baseline unchanged")
  invisible(list(table = table, decisions = decisions, cuts = cuts, habitat = after, map = overview, output_dir = output_dir))
}
