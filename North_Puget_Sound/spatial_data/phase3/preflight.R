for (file in list.files("R", pattern = "\\.R$", full.names = TRUE)) source(file)
habitat_require_packages()
w <- load_phase3_woods_barriers()
o <- load_phase3_olney_barriers()
t <- load_phase3_tidal_sources()
stopifnot(nrow(w$masks) == 1L, nrow(o$masks) == 1L, all(sf::st_is_valid(t$mask)))
print(data.frame(target = c("Woods", "Olney"), number = c(w$barriers$review_number[
  w$barriers$review_record_key %in% w$masks$review_record_key],
  o$barriers$review_number[o$barriers$review_record_key %in% o$masks$review_record_key])))
cat("Source records", nrow(t$all_polygons), "selected tidal polygons", nrow(t$tidal_polygons), "\n")
cat("All source bundles load; mask validity and existing/new barrier identifiers PASS.\n")
