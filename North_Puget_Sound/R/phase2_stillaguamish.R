# Custom analysis unit: North Fork Stillaguamish above the Deer Creek junction.
# This is a complete USGS upstream basin, not a clipped NOAA polygon or a
# connected subset of modeled streams. Deer Creek itself is excluded.
if (!exists("nps_read_cache_rds", mode = "function")) source(file.path("R", "project_paths.R"))

phase2_stillaguamish_sources <- function() {
  data.frame(file=c("deer_confluence_fabric.json","nf_stillaguamish_oso_site.json",
      "nf_stillaguamish_above_deer_basin.json","deer_creek_basin.json",
      "nf_stillaguamish_above_deer_UT.json"),
    source_url=c(
      "https://api.water.usgs.gov/fabric/pygeoapi/collections/nhdflowline_network/items?bbox=-121.95,48.25,-121.91,48.29&limit=100&f=json",
      "https://api.water.usgs.gov/nldi/linked-data/nwissite/USGS-12166495?f=json",
      "https://api.water.usgs.gov/nldi/linked-data/comid/24274669/basin?simplified=false",
      "https://api.water.usgs.gov/nldi/linked-data/comid/24274775/basin?simplified=false",
      "https://api.water.usgs.gov/nldi/linked-data/comid/24274669/navigation/UT/flowlines?f=json&distance=1000"),
    stringsAsFactors=FALSE)
}

fetch_phase2_stillaguamish_sources <- function(cache_dir="spatial_data/phase2",refresh=FALSE) {
  dir.create(cache_dir,recursive=TRUE,showWarnings=FALSE)
  curl<-Sys.which(if(.Platform$OS.type=="windows") "curl.exe" else "curl")
  if(!nzchar(curl)) stop("curl is required to fetch the public USGS source snapshots")
  sources<-phase2_stillaguamish_sources()
  for(i in seq_len(nrow(sources))) {
    path<-file.path(cache_dir,sources$file[i])
    if(!refresh && file.exists(path) && file.info(path)$size>0) next
    arguments<-c(if(.Platform$OS.type=="windows") "--ssl-no-revoke", "--fail", "--silent",
      "--show-error","--location","--connect-timeout","20","--max-time","120",
      "--output",shQuote(path),shQuote(sources$source_url[i]))
    status<-system2(curl,arguments)
    if(status!=0 || !file.exists(path) || file.info(path)$size==0)
      stop("Could not retrieve the public USGS source: ",sources$source_url[i])
  }
  invisible(sources)
}

build_phase2_stillaguamish <- function(cache_dir="spatial_data/phase2",
    network_file="spatial_data/north_puget_swifd_bridges.rds",
    raw_model_file="spatial_data/north_puget_steelhead.rds",
    named_reference_file="spatial_data/north_puget_named_stream_reference.rds") {
  if(!requireNamespace("sf",quietly=TRUE)) stop("Install sf")
  sources<-phase2_stillaguamish_sources()
  paths<-file.path(cache_dir,sources$file)
  if(!all(file.exists(paths)) || any(file.info(paths)$size==0))
    stop("Cached USGS sources are missing; run fetch_phase2_stillaguamish_sources() explicitly first")
  crs<-26910
  named<-sf::st_transform(sf::st_read(paths[1L],quiet=TRUE),crs)
  get_reach<-function(id) {
    row<-named[as.character(named$comid)==id,]
    if(nrow(row)!=1L) stop("USGS reference reach is missing or ambiguous: ",id)
    row
  }
  upper<-get_reach("24274669")
  deer<-get_reach("24274775")
  lower<-get_reach("24274667")
  if(!identical(as.character(upper$gnis_name),"North Fork Stillaguamish River") ||
      !identical(as.character(lower$gnis_name),"North Fork Stillaguamish River") ||
      !identical(as.character(deer$gnis_name),"Deer Creek") ||
      !all(tolower(c(upper$flowdir,deer$flowdir,lower$flowdir))=="with digitized"))
    stop("Published stream identities or flow direction changed; review the custom outlet")
  junction_node<-as.character(upper$tonode)
  if(!identical(as.character(deer$tonode),junction_node) ||
      !identical(as.character(lower$fromnode),junction_node))
    stop("The upstream NF, Deer Creek and downstream NF do not share the published confluence node")
  drainage_balance<-as.numeric(upper$totdasqkm)+as.numeric(deer$totdasqkm)+
    as.numeric(lower$areasqkm)-as.numeric(lower$totdasqkm)
  if(!is.finite(drainage_balance) || abs(drainage_balance)>.001)
    stop("Published confluence drainage-area accounting does not close")
  lines<-suppressWarnings(sf::st_cast(sf::st_geometry(upper),"LINESTRING"))
  if(length(lines)!=1L) stop("The published upper NF reach has multiple ambiguous geometry parts")
  xy<-unclass(lines[[1L]])
  outlet<-sf::st_sfc(sf::st_point(unname(xy[nrow(xy),1:2])),crs=crs)
  outlet_geographic<-sf::st_coordinates(sf::st_transform(outlet,4326))[1L,1:2]
  basin<-sf::st_make_valid(sf::st_transform(sf::st_read(paths[3L],quiet=TRUE),crs))
  deer_basin<-sf::st_make_valid(sf::st_transform(sf::st_read(paths[4L],quiet=TRUE),crs))
  geometry<-sf::st_union(sf::st_geometry(basin))
  if(!length(geometry) || any(sf::st_is_empty(geometry)) ||
      any(!sf::st_geometry_type(geometry) %in% c("POLYGON","MULTIPOLYGON")))
    stop("The complete upstream basin is empty or not a polygon")
  area_km2<-sum(as.numeric(sf::st_area(geometry)))/1e6
  if(!is.finite(area_km2) || abs(area_km2/as.numeric(upper$totdasqkm)-1)>.02)
    stop("Custom upstream basin does not match the published full NF drainage area")
  deer_overlap_km2<-sum(as.numeric(sf::st_area(suppressWarnings(sf::st_intersection(
    geometry,sf::st_union(sf::st_geometry(deer_basin)))))))/1e6
  if(!is.finite(deer_overlap_km2) || deer_overlap_km2>1e-5)
    stop("The custom basin incorrectly includes Deer Creek drainage")
  upstream<-sf::st_transform(sf::st_read(paths[5L],quiet=TRUE),crs)
  ids<-as.character(upstream$nhdplus_comid)
  if(!"24274669" %in% ids || any(c("24274775","24274667") %in% ids) || nrow(upstream)<50L)
    stop("USGS upstream navigation is incomplete or includes Deer/downstream reaches")
  outside<-suppressWarnings(sf::st_difference(sf::st_geometry(upstream),geometry))
  upstream_outside_m<-sum(as.numeric(sf::st_length(outside)))
  if(!is.finite(upstream_outside_m) || upstream_outside_m>.01)
    stop("Some published contributing upstream channels fall outside the custom basin")
  site<-sf::st_transform(sf::st_read(paths[2L],quiet=TRUE),crs)
  if(nrow(site)!=1L || as.character(site$identifier)!="USGS-12166495" ||
      as.character(site$comid)!="24274669" || !lengths(sf::st_intersects(site,geometry)))
    stop("The official Oso gauge above Deer Creek is not in the selected NF basin")
  reference<-sf::st_transform(readRDS(named_reference_file),crs)
  deer_named<-reference[reference$LLID=="1219314482681",]
  nf_named<-reference[reference$LLID=="1221262482038",]
  named_junction<-suppressWarnings(sf::st_intersection(sf::st_union(sf::st_geometry(deer_named)),
    sf::st_union(sf::st_geometry(nf_named))))
  if(length(named_junction)!=1L || sf::st_geometry_type(named_junction)!="POINT")
    stop("Independent WDFW named hydrography does not identify a unique Deer/NF junction")
  junction_offset_m<-as.numeric(sf::st_distance(outlet,named_junction))
  if(!is.finite(junction_offset_m) || junction_offset_m>100)
    stop("USGS confluence is too far from the independent named-stream junction")
  raw<-readRDS(raw_model_file)
  download_bbox<-attr(raw,"query_bbox")
  bounds<-sf::st_bbox(sf::st_transform(geometry,4326))
  if(length(download_bbox)!=4L || bounds["xmin"]<download_bbox[1L] ||
      bounds["ymin"]<download_bbox[2L] || bounds["xmax"]>download_bbox[3L] ||
      bounds["ymax"]>download_bbox[4L])
    stop("The custom watershed extends outside the cached raw-model download")
  network<-readRDS(network_file)$bridges$network
  source<-suppressWarnings(sf::st_intersection(network,geometry))
  source<-source[!sf::st_is_empty(source) & sf::st_dimension(source)==1,]
  physical_miles<-sum(as.numeric(sf::st_length(sf::st_union(sf::st_geometry(
    source[source$measure_habitat,])))))/1609.344
  if(!is.finite(physical_miles) || physical_miles<=0) stop("No original distribution linework is available in the custom basin")
  sources$md5<-unname(tools::md5sum(paths))
  sources$retrieved_utc<-format(file.info(paths)$mtime,tz="UTC",usetz=TRUE)
  population<-sf::st_sf(pop_id=1001L,population="North Fork Stillaguamish above Deer Creek (custom)",
    run_timing="custom",wcr_pop_id="CUSTOM_NF_STILLAGUAMISH_ABOVE_DEER",mpg="North Cascades",
    species="Steelhead",is_custom=TRUE,
    population_source="USGS NLDI full upstream basin for the NF reach ending at Deer Creek confluence",
    basin_source_url=sources$source_url[3L],outlet_comid="24274669",confluence_node=junction_node,
    excluded_deer_creek_comid="24274775",outlet_longitude=unname(outlet_geographic[1L]),
    outlet_latitude=unname(outlet_geographic[2L]),area_sq_km=area_km2,
    published_area_sq_km=as.numeric(upper$totdasqkm),contributing_reaches=nrow(upstream),
    outlet_definition="NF immediately upstream of Deer Creek; excludes Deer Creek itself",
    coverage_complete=TRUE,geometry=geometry)
  audit<-data.frame(check=c("published_confluence_node","published_drainage_area_balance",
      "full_basin_area","Deer_drainage_excluded","all_upstream_reaches_included",
      "independent_named_junction_offset","official_upstream_Oso_gauge_included",
      "raw_model_download_covers_whole_watershed","full_augmented_source_linework_present"),
    observed=c(junction_node,format(drainage_balance,digits=12),format(area_km2,digits=12),
      format(deer_overlap_km2,digits=12),format(upstream_outside_m,digits=12),
      format(junction_offset_m,digits=12),"TRUE","TRUE",format(physical_miles,digits=12)),
    units=c("node","km2","km2","km2","m outside","m","boolean","boolean","miles"),
    passed=TRUE,stringsAsFactors=FALSE)
  notes<-c("This is an additional custom analysis unit, not a NOAA population replacement.",
    "The geographic definition is the North Fork Stillaguamish immediately above the Deer Creek confluence; Deer Creek drainage is excluded.",
    "The USGS COMID upstream-basin endpoint supplies the complete watershed, including all contributing tributaries and disconnected model reaches.",
    "Use full cached raw/augmented network geometry for clipping; do not restrict the source to previously clipped NOAA outputs.",
    "The USGS and WDFW confluence locations differ within the documented hydrography precision; provenance and checks are retained.")
  bundle<-list(population=population,audit=audit,notes=notes,sources=sources,
    outlet=sf::st_sf(outlet_comid="24274669",geometry=outlet),
    validation=list(published_upstream_area_km2=as.numeric(upper$totdasqkm),
      published_deer_area_km2=as.numeric(deer$totdasqkm),
      published_downstream_area_km2=as.numeric(lower$totdasqkm),
      full_augmented_physical_miles=physical_miles,upstream_reach_count=nrow(upstream),
      deer_overlap_km2=deer_overlap_km2,upstream_outside_m=upstream_outside_m),
    generated_utc=format(Sys.time(),tz="UTC",usetz=TRUE))
  saveRDS(bundle,file.path(cache_dir,"stillaguamish_custom.rds"))
  utils::write.csv(audit,file.path(cache_dir,"stillaguamish_custom_audit.csv"),row.names=FALSE)
  jsonlite::write_json(list(notes=notes,sources=sources,validation=bundle$validation,
    generated_utc=bundle$generated_utc),file.path(cache_dir,"stillaguamish_custom_provenance.json"),
    auto_unbox=TRUE,pretty=TRUE,digits=NA)
  bundle
}

load_phase2_stillaguamish <- function(cache_dir="spatial_data/phase2",rebuild=FALSE) {
  path<-file.path(cache_dir,"stillaguamish_custom.rds")
  if(rebuild || !file.exists(path)) return(build_phase2_stillaguamish(cache_dir=cache_dir))
  bundle<-nps_read_cache_rds(path)
  if(!inherits(bundle$population,"sf") || nrow(bundle$population)!=1L ||
      bundle$population$pop_id!=1001L || !isTRUE(bundle$population$coverage_complete) ||
      any(sf::st_is_empty(bundle$population))) stop("Custom Stillaguamish bundle is invalid")
  paths<-file.path(cache_dir,bundle$sources$file)
  if(!all(file.exists(paths)) || !identical(unname(tools::md5sum(paths)),bundle$sources$md5))
    stop("Custom watershed source snapshots changed; rebuild and revalidate the bundle")
  bundle
}
