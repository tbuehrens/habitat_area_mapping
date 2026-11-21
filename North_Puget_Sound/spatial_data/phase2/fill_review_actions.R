# Metadata-only finalization: preserve every geometry and source attribute.
out <- "outputs/phase2_reviewed_barriers"
actions <- c("912" = "Apply user-authorized Culmback Dam cutoff for Spada Lake / Sultan River",
  "913" = "Apply user-authorized natural South Fork Tolt Falls cutoff below reservoir; City source map location")
for (name in c("phase2_barrier_decisions.csv", "phase2_cut_barriers.csv", "phase2_changes.csv",
    "phase2_graph_direction_audit.csv")) {
  path <- file.path(out, name)
  x <- read.csv(path, stringsAsFactors = FALSE, colClasses = "character", check.names = FALSE)
  rows <- which(x$review_number %in% names(actions))
  x$phase2_review_action[rows] <- unname(actions[x$review_number[rows]])
  write.csv(x, path, row.names = FALSE, na = "")
}
file <- file.path(out, "North_Puget_Sound_Steelhead_phase2.gpkg")
for (layer in c("barrier_characteristics", "applied_barriers")) {
  x <- sf::st_read(file, layer = layer, quiet = TRUE)
  original_geometry <- sf::st_as_binary(sf::st_geometry(x))
  rows <- which(x$review_number %in% names(actions))
  x$phase2_review_action[rows] <- unname(actions[as.character(x$review_number[rows])])
  sf::st_write(x, file, layer = layer, delete_layer = TRUE, quiet = TRUE)
  checked <- sf::st_read(file, layer = layer, quiet = TRUE)
  stopifnot(identical(original_geometry, sf::st_as_binary(sf::st_geometry(checked))))
}
cat("Filled new dam/falls review actions; both GIS layer geometries byte-identical.\n")
