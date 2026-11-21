# Source provenance survives noding and numerical interpolation without overlay.
source(file.path("R", "source_classification.R"))
library(sf)

line <- function(...) st_linestring(rbind(...))
sources <- st_sf(
  source_class = c("prediction", "swifd_connector", "alignment_link"),
  bridge_id = c("should_be_ignored", "bridge_1", "bridge_1"),
  geometry = st_sfc(
    line(c(500000.123, 5400000.789), c(500800.345, 5400500.567),
         c(501500.789, 5400800.123)),
    line(c(501500.789, 5400800.123), c(501700, 5401000), c(502000, 5400900)),
    line(c(502000, 5400900), c(502050, 5400905)), crs = 26910))

parts <- st_cast(st_union(st_geometry(sources)), "LINESTRING")
classified <- classify_network_sources(parts, sources)
stopifnot(setequal(classified$source_class, sources$source_class),
  all(classified$measure_habitat == (classified$source_class != "alignment_link")),
  all(is.na(classified$bridge_id[classified$source_class == "prediction"])),
  all(classified$bridge_id[classified$source_class != "prediction"] == "bridge_1"))

# Prediction takes priority on truly coincident supports; it is never measured
# as new SWIFD habitat or attributed to an arbitrary bridge ID.
duplicate <- sources[1, ]
duplicate$source_class <- "swifd_connector"
duplicate$bridge_id <- "coincident_bridge"
coincident <- classify_network_sources(st_geometry(sources)[1], rbind(sources, duplicate))
stopifnot(coincident$source_class == "prediction", coincident$measure_habitat,
          is.na(coincident$bridge_id))

# A diagonal sub-piece reconstructed by interpolation must still classify.
xy <- unclass(st_geometry(sources)[[1]])
fractional <- st_sfc(line(xy[1, ] + (xy[2, ] - xy[1, ]) * .173,
                         xy[1, ] + (xy[2, ] - xy[1, ]) * .719), crs = 26910)
stopifnot(classify_network_sources(fractional, sources)$source_class == "prediction")

# Noded crossings retain their own source classes on either side of the node.
crossing <- st_sf(source_class = c("prediction", "swifd_connector"),
  bridge_id = c(NA, "cross"), geometry = st_sfc(
    line(c(0, 0), c(20, 20)), line(c(0, 20), c(20, 0)), crs = 26910))
cross_parts <- st_cast(st_union(st_geometry(crossing)), "LINESTRING")
cross_class <- classify_network_sources(cross_parts, crossing)
stopifnot(sum(cross_class$source_class == "prediction") == 2L,
          sum(cross_class$source_class == "swifd_connector") == 2L)

# Unsupported paths fail rather than silently disappearing or inheriting a
# source from a nearby but spatially distinct stream.
remote <- st_sfc(line(c(502000, 5401900), c(502050, 5401905)), crs = 26910)
failed <- tryCatch({classify_network_sources(remote, sources); FALSE},
  error = function(e) grepl("Cannot classify", conditionMessage(e)))
stopifnot(failed)
no_ids <- sources[, "source_class"]
stopifnot(all(is.na(classify_network_sources(parts, no_ids)$bridge_id)))
empty <- classify_network_sources(st_sfc(crs = 26910), sources)
stopifnot(nrow(empty) == 0L, identical(names(empty), c("source_class", "measure_habitat", "bridge_id")))
cat("Noded source classification, coincident priority, diagonal precision, and alignment accounting passed.\n")
