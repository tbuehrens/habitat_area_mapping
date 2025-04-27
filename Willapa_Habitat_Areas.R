#Load packages 
pacman::p_load(shiny, tidyverse, devtools, ggplot2, leaflet,sf,rnaturalearth,httr,jsonlite, 
               dplyr, RODBC, curl,odbc,DBI,tidyverse,janitor,fuzzyjoin,ggplot2,
               lubridate,kableExtra,sf,rnaturalearth,ggmap,httr,here,units,nhdplusTools)


get_swifd<-function(species,usetype){
  url <- "https://geodataservices.wdfw.wa.gov/arcgis/rest/services/MapServices/SWIFD/MapServer/0/query"
  total_records = 100000
  batch_size = 1000
  dat <- data.frame()
  
  for (offset in seq(0, total_records, by = batch_size)) {
    query_params <- list(
      where = paste0("SPECIES = '",species,"' AND (",
                     paste0("USETYPE_DESC = '", usetype, "'", collapse = " OR "),
                     ") AND OBJECTID > ", offset, " AND OBJECTID <= ", offset + batch_size
      ),
      outFields = "*",
      outSR = 4326,
      f = "json"
    )
    response <- GET(url, query = query_params)
    json_data <- content(response, "text")
    # Parse and process the JSON data
    parsed_data <- fromJSON(json_data)
    # Convert the parsed data to a data frame
    batch_data <- as.data.frame(parsed_data$features)
    # Append the batch to the overall data frame
    dat <- bind_rows(dat, batch_data)
  }
  
  # Assuming all_data is your list with attributes and geometry
  # Extract paths from the geometry list
  paths <- lapply(dat$geometry$paths, function(path) st_linestring(matrix(as.numeric(path), ncol = 2, byrow = F)))
  
  # Create an sf object
  sf_swifd <- st_sf(
    # attributes
    attributes = dat$attributes,
    # geometry
    geometry = st_sfc(paths),
    # set coordinate reference system (CRS) - replace EPSG:4326 with the appropriate CRS
    crs = st_crs(4326)
  )%>%
    setNames(gsub("attributes\\.", "", colnames(.)))%>%
    st_set_crs(st_crs("+proj=longlat +datum=WGS84 +units=m"))
}


########
#Part 0: create polygons for NF Lewis that include upper watershed to modify NOAA boundaries which haven't been updated after passage of adults above merwin resumed
WillapaWatershed<-nhdplusTools::get_huc(type="huc12",id="171001060401")%>%mutate(watershed="Palix")%>%
  bind_rows(nhdplusTools::get_huc(type="huc10",id="1710010603")%>%mutate(watershed="Willapa"),
            nhdplusTools::get_huc(type="huc10",id="1710010602")%>%mutate(watershed="North_Smith"),
            nhdplusTools::get_huc(type="huc10",id="1710010601")%>%mutate(watershed="North_Smith"),
            nhdplusTools::get_huc(type="huc12",id="171001060403")%>%mutate(watershed="Nemah"),
            nhdplusTools::get_huc(type="huc12",id="171001060402")%>%mutate(watershed="Nemah"),
            nhdplusTools::get_huc(type="huc12",id="171001060503")%>%mutate(watershed="Naselle"),
            nhdplusTools::get_huc(type="huc12",id="171001060504")%>%mutate(watershed="Naselle"),
            nhdplusTools::get_huc(type="huc12",id="171001060502")%>%mutate(watershed="Naselle"),
            nhdplusTools::get_huc(type="huc12",id="171001060501")%>%mutate(watershed="Naselle"),
            nhdplusTools::get_huc(type="huc12",id="171001060505")%>%mutate(watershed="Naselle"),
            nhdplusTools::get_huc(type="huc12",id="171001060505")%>%mutate(watershed="Bear")
  )%>%
  group_by(watershed)%>%
  summarise()%>%
  dplyr::rename(SHAPE=geometry)%>%
  st_set_crs(st_crs("+proj=longlat +datum=NAD83 +units=m"))



sf_swifd_chum <- st_transform(get_swifd(species="CHUM SALMON", usetype = c("Presence","Spawning","Rearing")), st_crs(WillapaWatershed))%>%
  st_intersection(WillapaWatershed%>%
                    mutate(area=st_area(SHAPE))%>%
                    group_by(watershed)%>%
                    summarise(area_sq_km=set_units(sum(area), km^2))
  )%>%
  group_by(watershed,area_sq_km,USETYPE_DESC)%>%
  summarise(
  )%>%
  mutate(length_km = set_units(st_length(geometry),km))


sf_swifd_coho <- st_transform(get_swifd("COHO SALMON",usetype = c("Presence","Spawning","Rearing")), st_crs(WillapaWatershed))%>%
  #st_join(sf_pops)%>%
  st_intersection(WillapaWatershed%>%
                    mutate(area=st_area(SHAPE))%>%
                    group_by(watershed)%>%
                    summarise(area_sq_km=set_units(sum(area), km^2))
  )%>%
  group_by(watershed,area_sq_km,USETYPE_DESC)%>%
  summarise(
  )%>%
  mutate(length_km = set_units(st_length(geometry),km))

sf_swifd_chinook <- st_transform(get_swifd("CHINOOK SALMON",usetype = c("Presence","Spawning","Rearing")), st_crs(WillapaWatershed))%>%
  #st_join(sf_pops)%>%
  st_intersection(WillapaWatershed%>%
                    mutate(area=st_area(SHAPE))%>%
                    group_by(watershed)%>%
                    summarise(area_sq_km=set_units(sum(area), km^2))
  )%>%
  group_by(watershed,area_sq_km,USETYPE_DESC)%>%
  summarise(
  )%>%
  mutate(length_km = set_units(st_length(geometry),km))


#########################################################################################
#Step 4: Render maps by NOAA population

#Render map
state_map <- ne_states (country = 'United States of America', returnclass = 'sf')%>% 
  filter (name %in% c('Washington','Oregon'))

state_map <- st_transform(state_map, st_crs(WillapaWatershed))

Chum<-ggplot() +
  geom_sf(data=state_map,color="grey",fill=NA)+
  geom_sf(data = WillapaWatershed,color="black",fill=NA)+
  geom_sf(data = sf_swifd_chum%>%sf::st_cast("MULTILINESTRING"), aes(color=USETYPE_DESC),size=1.5,fill=NA)+
  coord_sf(xlim = c(-124.5, -122.5), ylim = c(46, 47), expand = FALSE)+
  theme_bw()

Coho<-ggplot() +
  geom_sf(data=state_map,color="grey",fill=NA)+
  geom_sf(data = WillapaWatershed,color="black",fill=NA)+
  geom_sf(data = sf_swifd_coho%>%sf::st_cast("MULTILINESTRING"), aes(color=USETYPE_DESC),size=1.5,fill=NA)+
  coord_sf(xlim = c(-124.5, -122.5), ylim = c(46, 47), expand = FALSE)+
  theme_bw()

Chinook<-ggplot() +
  geom_sf(data=state_map,color="grey",fill=NA)+
  geom_sf(data = WillapaWatershed,color="black",fill=NA)+
  geom_sf(data = sf_swifd_chinook%>%sf::st_cast("MULTILINESTRING"), aes(color=USETYPE_DESC),size=1.5,fill=NA)+
  coord_sf(xlim = c(-124.5, -122.5), ylim = c(46, 47), expand = FALSE)+
  theme_bw()



ggsave(Chum,filename="Willapa_Chum_map.png",dpi=300)
ggsave(Coho,filename="Willapa_Coho_map.png",dpi=300)
ggsave(Chinook,filename="Willapa_Chinook_map.png",dpi=300)




#######################################################################################
#Combine lengths into a single output file and export. 
hab_lengths <- rbind(sf_swifd_coho%>%
                       as.data.frame()%>%
                       pivot_wider(names_from = USETYPE_DESC,values_from = length_km, id_cols=c(watershed,area_sq_km) )%>%
                       mutate(species="coho"),
                     sf_swifd_chum%>%
                       as.data.frame()%>%
                       pivot_wider(names_from = USETYPE_DESC,values_from = length_km,id_cols=c(watershed,area_sq_km))%>%
                       mutate(species="chum"),
                     sf_swifd_chinook%>%
                       as.data.frame()%>%
                       pivot_wider(names_from = USETYPE_DESC,values_from = length_km,id_cols=c(watershed,area_sq_km))%>%
                       mutate(species="chinook")
)%>%
  mutate(geometry = NULL)

write.csv(hab_lengths,"willapa_hab_lengths.csv",row.names = F)
