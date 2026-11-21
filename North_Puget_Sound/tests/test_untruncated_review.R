# A future-cut candidate must not hide or remove any raw model or SWIFD geometry
# during the untruncated review. Keep all writes inside a temporary workspace.
source(file.path("R", "arcgis.R"))
source(file.path("R", "noaa_populations.R"))
source(file.path("R", "swifd_sources.R"))
source(file.path("R", "source_classification.R"))
source(file.path("R", "barrier_stream_identity.R"))
source(file.path("R", "stream_edge_identity.R"))
source(file.path("R", "network.R"))
source(file.path("R", "swifd_connectors.R"))
source(file.path("R", "habitat_workflow.R"))
source(file.path("R", "barrier_review.R"))
source(file.path("R", "untruncated_review.R"))
suppressPackageStartupMessages(library(sf))

check_untruncated_review <- function() {
  previous <- getwd()
  on.exit(setwd(previous), add=TRUE)
  directory <- tempfile("untruncated_review_")
  dir.create(directory, recursive=TRUE)
  setwd(directory)
  dir.create(file.path("outputs","north_puget_sound"), recursive=TRUE)
  guards <- c("Nooksack_Winter_Steelhead_map.png",
    "Nooksack_Winter_Steelhead_hab_lengths.csv",
    file.path("outputs","north_puget_sound","North_Puget_Sound_Steelhead_hab_lengths.csv"))
  for (file in guards) writeBin(charToRaw(paste("legacy sentinel",file)),file)
  guard_hashes <- tools::md5sum(guards)

  offset <- c(500000,5400000)
  xy <- function(coordinates) sweep(coordinates,2,offset,"+")
  line <- function(coordinates) st_linestring(xy(coordinates))
  point <- function(coordinates) st_point(coordinates+offset)
  network <- st_sf(
    source_class=c("prediction","prediction","swifd_connector","alignment_link"),
    measure_habitat=c(TRUE,TRUE,TRUE,FALSE),
    bridge_id=c(NA_character_,NA_character_,"patch_1","patch_1"),
    geometry=st_sfc(
      line(rbind(c(0,0),c(100,0))),
      line(rbind(c(300,0),c(400,0))),
      line(rbind(c(100,10),c(200,5),c(300,0))),
      line(rbind(c(100,0),c(100,10))),crs=26910))
  network$length_m <- as.numeric(st_length(network))
  raw <- network[network$source_class=="prediction",]
  swifd <- st_sf(LLID="review_stream",LLID_STRM_NAME="Review Creek",
    geometry=st_geometry(network[network$measure_habitat,]))
  populations <- st_sf(pop_id=1L,population="Synthetic steelhead",run_timing="su",
    geometry=st_sfc(st_polygon(list(xy(rbind(c(-20,-20),c(420,-20),
      c(420,30),c(-20,30),c(-20,-20))))),crs=26910))
  barrier <- st_sf(SiteId="explicit_total",source_layer="5",barrier_key="5:explicit_total",
    StreamName="Review Cr",TributaryToName="Other R",verification="Verified",
    FishPassageBarrierStatusCode=10L,PercentFishPassableCode=10L,
    geometry=st_sfc(point(c(200,5)),crs=26910))
  endpoints <- st_sf(anchor_id=1:2,snap_distance_m=c(10,0),
    eligible=TRUE,match_status="used_by_connector",
    geometry=st_sfc(point(c(100,0)),point(c(300,0)),crs=26910))
  bridges <- list(network=network,
    connectors=network[network$source_class=="swifd_connector",],
    alignment_links=network[network$source_class=="alignment_link",],
    endpoint_audit=endpoints,
    bridge_audit=data.frame(bridge_id="patch_1",status="retained_in_physical_forest"),
    summary=data.frame(retained_bridges=1L),settings=list(snap_distance_m=100))
  output <- file.path(directory,"outputs","review_swifd_connections")
  result <- habitat_export_untruncated_review(populations,raw,swifd,bridges,
    barrier,output,metadata=list(synthetic_check=TRUE,
      barrier_field_definitions=barrier_review_field_definitions()))

  # Both original pieces, including the piece beyond the proposed barrier,
  # remain blue, exported in full, and counted in the model mileage.
  lengths <- read.csv(file.path(output,"population_distribution_miles.csv"))
  expected_model <- sum(as.numeric(st_length(raw)))/1609.344
  expected_patch <- sum(as.numeric(st_length(bridges$connectors)))/1609.344
  expected_alignment <- sum(as.numeric(st_length(bridges$alignment_links)))/1609.344
  stopifnot(nrow(lengths)==1L,
    abs(lengths$raw_model_miles-expected_model)<1e-9,
    abs(lengths$preserved_model_miles-expected_model)<1e-9,
    abs(lengths$swifd_added_miles-expected_patch)<1e-9,
    abs(lengths$patched_untruncated_miles-expected_model-expected_patch)<1e-9,
    abs(lengths$alignment_link_miles_not_counted-expected_alignment)<1e-9,
    !lengths$barrier_truncation_applied,
    !any(c("retained","removed","unresolved") %in% names(result$habitat)))
  gpkg <- file.path(output,"North_Puget_Sound_Steelhead.gpkg")
  exported <- st_read(gpkg,layer="steelhead_patched_untruncated",quiet=TRUE)
  exported_model <- exported[exported$source_class=="prediction",]
  missing <- st_difference(st_union(st_geometry(raw)),st_union(st_geometry(exported_model)))
  stopifnot(sum(as.numeric(st_length(missing)))<1e-8,
    length(st_equals(st_geometry(raw[2,]),st_geometry(exported_model))[[1]])==1L,
    !any(c("retained","removed","unresolved") %in% names(exported)),
    !"predicted_extent_status" %in% st_layers(gpkg)$name)

  numbered <- read.csv(file.path(output,"barrier_review.csv"))
  stopifnot(nrow(numbered)==1L,numbered$review_number==1L,
    numbered$future_cut_candidate,numbered$explicit_total_passage,
    !numbered$truncation_applied,!numbered$applied,
    abs(numbered$snap_distance_m)<1e-8)
  metadata <- jsonlite::fromJSON(file.path(output,"run_metadata.json"))
  stopifnot(isTRUE(metadata$review_checkpoint),!metadata$barrier_truncation_applied,
    metadata$barrier_summary$actual_cuts==0,
    metadata$barrier_summary$proposed_later_cuts==1,
    file.exists(file.path(output,"barrier_field_definitions.json")),
    identical(unname(tools::md5sum(guards)),unname(guard_hashes)),
    !file.exists(file.path(output,"natural_barrier_audit.csv")),
    !length(list.files(output,pattern="hab_lengths\\.csv$",recursive=TRUE)))

  # Inspect the actual ggplot scales: all raw prediction is blue even beyond
  # the barrier; SWIFD patches are orange and alignment is dotted.
  index <- which(vapply(result$map$layers,function(layer)
    inherits(layer$data,"sf") && "source_class" %in% names(layer$data),logical(1)))
  stopifnot(length(index)==1L)
  source <- result$map$layers[[index]]$data$source_class
  rendered <- ggplot2::ggplot_build(result$map)$data[[index]]
  blue <- unique(rendered$colour[source=="prediction"])
  orange <- unique(rendered$colour[source=="swifd_connector"])
  stopifnot(length(blue)==1L,length(orange)==1L,blue!=orange,
    grDevices::col2rgb(blue)[3L,1L]>grDevices::col2rgb(blue)[1L,1L],
    grDevices::col2rgb(orange)[1L,1L]>grDevices::col2rgb(orange)[2L,1L],
    grDevices::col2rgb(orange)[2L,1L]>grDevices::col2rgb(orange)[3L,1L],
    all(rendered$linetype[source=="alignment_link"]=="dotted"))
  cat("Untruncated review regression passed: complete model plus SWIFD geometry, zero cuts, numbered future candidate, distinct colors, legacy files untouched.\n")
}

check_untruncated_review()
