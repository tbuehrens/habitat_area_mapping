# Load packages -----------------------------------------------------------

pacman::p_load(
  dplyr, tidyr, ggplot2, sf, httr, jsonlite, units, nhdplusTools
)


# Download SWIFD records --------------------------------------------------
# Retrieves matching IDs first, avoiding an arbitrary upper OBJECTID limit.
# GeoJSON preserves multipart stream geometry.

get_swifd <- function(
    speciesrun = "Winter Steelhead",
    usetype = c("Presence", "Spawning", "Rearing"),
    aoi,
    batch_size = 500
) {
  
  url <- paste0(
    "https://geodataservices.wdfw.wa.gov/arcgis/rest/services/",
    "MapServices/SWIFD/MapServer/0/query"
  )
  
  sql_quote <- function(x) {
    paste0("'", gsub("'", "''", x, fixed = TRUE), "'")
  }
  
  where <- paste0(
    "SPECIESRUN = ", sql_quote(speciesrun),
    " AND USETYPE_DESC IN (",
    paste(sql_quote(usetype), collapse = ", "),
    ")"
  )
  
  # Limit the download to the watershed bounding box.
  # Exact clipping to the watershed occurs below.
  bb <- st_bbox(st_transform(aoi, 4326))
  
  envelope <- jsonlite::toJSON(
    list(
      xmin = unname(bb["xmin"]),
      ymin = unname(bb["ymin"]),
      xmax = unname(bb["xmax"]),
      ymax = unname(bb["ymax"]),
      spatialReference = list(wkid = 4326)
    ),
    auto_unbox = TRUE
  )
  
  get_response <- function(params) {
    response <- httr::RETRY(
      "GET",
      url,
      query = params,
      httr::timeout(120),
      times = 3,
      quiet = TRUE
    )
    
    httr::stop_for_status(response)
    
    txt <- httr::content(
      response, as = "text", encoding = "UTF-8"
    )
    
    parsed <- jsonlite::fromJSON(txt, simplifyVector = FALSE)
    
    if (!is.null(parsed$error)) {
      stop(
        "ArcGIS query failed: ",
        paste(unlist(parsed$error), collapse = "; ")
      )
    }
    
    list(text = txt, parsed = parsed)
  }
  
  id_response <- get_response(list(
    where = where,
    geometry = envelope,
    geometryType = "esriGeometryEnvelope",
    inSR = 4326,
    spatialRel = "esriSpatialRelIntersects",
    returnIdsOnly = "true",
    f = "json"
  ))
  
  ids <- sort(unique(unlist(id_response$parsed$objectIds)))
  
  if (length(ids) == 0) {
    stop("No matching SWIFD records found.")
  }
  
  batches <- split(
    ids,
    ceiling(seq_along(ids) / batch_size)
  )
  
  features <- lapply(batches, function(batch_ids) {
    
    result <- get_response(list(
      objectIds = paste(batch_ids, collapse = ","),
      outFields = paste(
        c(
          "OBJECTID", "LLID", "LLID_STRM_NAME",
          "SPECIES", "SPECIESRUN", "RUNTIME_DESC",
          "DISTTYPE_DESC", "USETYPE_DESC"
        ),
        collapse = ","
      ),
      returnGeometry = "true",
      returnM = "false",
      returnZ = "false",
      outSR = 4326,
      f = "geojson"
    ))
    
    if (isTRUE(result$parsed$exceededTransferLimit)) {
      stop("Download exceeded the service limit; reduce batch_size.")
    }
    
    batch_sf <- sf::st_read(result$text, quiet = TRUE)
    
    if (nrow(batch_sf) != length(batch_ids)) {
      stop("Incomplete SWIFD batch returned; retry the download.")
    }
    
    batch_sf
  })
  
  do.call(rbind, features)
}


# 1. Nooksack watershed ---------------------------------------------------
# HUC8 17110004 includes the Nooksack's mainstem and forks.
# EPSG:26910 = NAD83 / UTM zone 10N; coordinates are in meters.

NooksackWatershed <- nhdplusTools::get_huc(
  type = "huc08",
  id = "17110004"
) %>%
  st_make_valid() %>%
  st_transform(26910) %>%
  mutate(watershed = "Nooksack") %>%
  group_by(watershed) %>%
  summarise(.groups = "drop") %>%
  mutate(
    area_sq_km = as.numeric(
      units::set_units(st_area(.), "km^2")
    )
  )


# 2. Download and clip winter steelhead distribution ----------------------

swifd_raw <- get_swifd(
  speciesrun = "Winter Steelhead",
  usetype = c("Presence", "Spawning", "Rearing"),
  aoi = NooksackWatershed
)

# Optional: restrict distribution status if that matches your objective.
# Leaving this commented retains the status categories in the original code.
#
# swifd_raw <- swifd_raw %>%
#   filter(DISTTYPE_DESC %in% c("Documented", "Presumed"))

swifd_clipped <- swifd_raw %>%
  st_transform(st_crs(NooksackWatershed)) %>%
  st_intersection(NooksackWatershed)

# Remove empty geometries and any point-only boundary intersections.
swifd_clipped <- swifd_clipped[
  !st_is_empty(swifd_clipped), 
]

swifd_clipped <- swifd_clipped[
  st_dimension(swifd_clipped) == 1,
]

if (nrow(swifd_clipped) == 0) {
  stop("No matching stream segments intersect the Nooksack watershed.")
}

# Inspect the distribution categories represented in the clipped data.
print(
  swifd_clipped %>%
    st_drop_geometry() %>%
    count(DISTTYPE_DESC, USETYPE_DESC)
)

# Dissolve within use type before measuring.
# This avoids counting coincident linework more than once within a category.
sf_swifd_winter_steelhead <- swifd_clipped %>%
  filter(!USETYPE_DESC %in%c("Artificial - Documented","Gradient Accessible Presence"))%>%
  group_by(watershed, area_sq_km) %>%
  summarise(do_union = TRUE, .groups = "drop") %>%
  mutate(
    length_km = as.numeric(
      units::set_units(st_length(.), "km")
    )
  )



# 3. Map -----------------------------------------------------------------
# Facet by use type so overlapping categories remain visible.

WinterSteelhead <- ggplot() +
  geom_sf(
    data = NooksackWatershed,
    color = "grey40",
    fill = "grey97",
    linewidth = 0.4
  ) +
  geom_sf(
    data = sf_swifd_winter_steelhead,
    aes(color = "blue"),
    linewidth = 0.6
  ) +
  coord_sf(expand = FALSE) +
  labs(
    title = "Winter steelhead distribution — Nooksack watershed",
    subtitle = "SWIFD distribution within HUC8 17110004",
    x = NULL,
    y = NULL,
    caption = paste(
      "Source: WDFW / NWIFC SWIFD.",
      "Use categories may overlap; lengths are not additive."
    )
  ) +
  theme_bw() +
  theme(
    legend.position = "none",
    panel.grid.major = element_line(
      color = "grey90", linewidth = 0.2
    ),
    strip.background = element_rect(fill = "grey95")
  )

print(WinterSteelhead)

ggsave(
  filename = "Nooksack_Winter_Steelhead_map.png",
  plot = WinterSteelhead,
  width = 12,
  height = 5,
  units = "in",
  dpi = 300
)


# 4. Export habitat lengths -----------------------------------------------

hab_lengths <- sf_swifd_winter_steelhead %>%
  st_drop_geometry() %>%
  select(watershed, area_sq_km, USETYPE_DESC, length_km) %>%
  mutate(
    USETYPE_DESC = factor(
      USETYPE_DESC,
      levels = c("Presence", "Spawning", "Rearing")
    )
  ) %>%
  pivot_wider(
    id_cols = c(watershed, area_sq_km),
    names_from = USETYPE_DESC,
    values_from = length_km,
    names_glue = "{USETYPE_DESC}_km",
    names_expand = TRUE
  ) %>%
  mutate(species = "Winter steelhead", .after = watershed)

write.csv(
  hab_lengths,
  "Nooksack_Winter_Steelhead_hab_lengths.csv",
  row.names = FALSE
)

print(hab_lengths)