#Load packages 
pacman::p_load(shiny, tidyverse, devtools, ggplot2, leaflet,sf,rnaturalearth,httr,jsonlite, 
               dplyr, RODBC, curl,odbc,DBI,tidyverse,janitor,fuzzyjoin,ggplot2,
               lubridate,kableExtra,sf,rnaturalearth,ggmap,httr,here,units,nhdplusTools)



########
#Part 0: create polygons for NF Lewis that include upper watershed to modify NOAA boundaries which haven't been updated after passage of adults above merwin resumed
WillapaWatershed<-nhdplusTools::get_huc(type="huc08",id="17100106")%>%
  #bind_rows(nhdplusTools::get_huc(type="huc10",id="1708000204"),
  #           nhdplusTools::get_huc(type="huc10",id="1708000203"),
  #           nhdplusTools::get_huc(type="huc10",id="1708000202"),
  #           nhdplusTools::get_huc(type="huc10",id="1708000201")
  # )%>%
  summarise()%>%
  dplyr::rename(SHAPE=geometry)%>%
  st_set_crs(st_crs("+proj=longlat +datum=NAD83 +units=m"))





#######################################################################################
#Part 2: Download JSON of SWIFD data and convert to SF object 
#(note this is done in batches because the API is set up to have max record count)
#WINTER STEELHEAD
url <- "https://geodataservices.wdfw.wa.gov/arcgis/rest/services/MapServices/SWIFD/MapServer/0/query"
total_records = 100000
batch_size = 1000
dat <- data.frame()

for (offset in seq(0, total_records, by = batch_size)) {
  query_params <- list(
    where = paste0("SPECIES = 'STEELHEAD TROUT' 
                   AND OBJECTID > ", offset, " AND OBJECTID <= ", offset + batch_size,
                   "AND RUNTIME_DESC = 'Winter'"
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

sf_swifd <- st_transform(sf_swifd, st_crs(WinterSteelhead))

#Calculate total length of habitat within each NOAA polygon
sf_swifd_pops <- sf_swifd %>%
  #st_join(sf_pops)%>%
  st_difference(estuary_polygons)%>%
  st_intersection(WinterSteelhead%>%
                    group_by(NWFSC_POP_ID)%>%
                    mutate(area=st_area(SHAPE))%>%
                    summarise(area=sum(area))
  )%>%
  filter(!is.na(NWFSC_POP_ID) 
         & DISTTYPE_DESC == "Modeled"
         & LLID_STRM_NAME!="Columbia River"
         & !(LLID_STRM_NAME=="Cowlitz River" & NWFSC_POP_ID == 234)
         & !(LLID_STRM_NAME%in%c("Hemlock Creek", "Sucker Creek") & NWFSC_POP_ID == 238)
         & !(LLID %in% c("1227158463282",	"1229186463107") & NWFSC_POP_ID == 238) #Toutle mainstem, NF Toutle mainstem--estimates dont include
  )%>%
  group_by(NWFSC_POP_ID,area)%>%
  summarise()%>%
  mutate(length = st_length(geometry))%>%
  mutate(length_km = set_units(length, km),area_sq_km = set_units(area, km^2))%>%
  dplyr::select(-length,-area)

print(sf_swifd_pops)


#########################################################################################
#Step 4: Render maps by NOAA population

#Render map
state_map <- ne_states (country = 'United States of America', returnclass = 'sf')%>% 
  filter (name %in% c('Washington','Oregon'))

state_map <- st_transform(state_map, st_crs(WillapaWatershed))

Willapa_Watershed_map<-ggplot() +
  geom_sf(data=state_map,color="red",fill=NA)+
  geom_sf(data = WillapaWatershed,color="green",fill=NA)+
  #geom_sf(data = winter_steelhead_lengths,color="blue")+
  #coord_sf(xlim = c(-124.5, -121.25), ylim = c(45.5, 47), expand = FALSE)+
  theme_bw()+
  ggtitle("Winter Steelhead")

print(WI_SH_map)
ggsave(WI_SH_map,filename="WI_SH_map.png")
#########################################################################################
#STEP 5: Repeat for other runs and species
#SUMMER STEELHEAD
url <- "https://geodataservices.wdfw.wa.gov/arcgis/rest/services/MapServices/SWIFD/MapServer/0/query"
total_records = 100000
batch_size = 1000
dat <- data.frame()

for (offset in seq(0, total_records, by = batch_size)) {
  query_params <- list(
    where = paste0("SPECIES = 'STEELHEAD TROUT' 
                   AND OBJECTID > ", offset, " AND OBJECTID <= ", offset + batch_size,
                   "AND RUNTIME_DESC = 'Summer'"
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
  crs = st_crs("+proj=longlat +datum=NAD83 +units=m")
)%>%
  setNames(gsub("attributes\\.", "", colnames(.)))%>%
  st_set_crs(st_crs("+proj=longlat +datum=WGS84 +units=m"))

sf_swifd <- st_transform(sf_swifd, st_crs(SummerSteelhead))

#Calculate total length of habitat within each NOAA polygon
sf_swifd_pops <- sf_swifd %>%
  #st_join(sf_pops)%>%
  st_difference(estuary_polygons)%>%
  st_intersection(SummerSteelhead%>%
                    group_by(NWFSC_POP_ID)%>%
                    mutate(area=st_area(SHAPE))%>%
                    summarise(area=sum(area))
                  )%>%
  filter(!is.na(NWFSC_POP_ID) 
         & DISTTYPE_DESC == "Modeled"
         & LLID_STRM_NAME!="Columbia River"
           )%>%
  group_by(NWFSC_POP_ID,area)%>%
  summarise()%>%
  mutate(length = st_length(geometry))%>%
  mutate(length_km = set_units(length, km),area_sq_km = set_units(area, km^2))%>%
  dplyr::select(-length,-area)

summer_steelhead_lengths <- sf_swifd_pops%>%
  left_join(populations%>%
              dplyr::select(NWFSC_POP_ID=NMFS_POPID,ESAPOPNAME),
            by=join_by(NWFSC_POP_ID)
  )

print(summer_steelhead_lengths)

SU_SH_map<-ggplot() +
  geom_sf(data=state_map,color="red",fill=NA)+
  geom_sf(data=estuary_polygons,color="black",fill=NA)+
  geom_sf(data = SummerSteelhead,color="green",fill=NA)+
  geom_sf(data = summer_steelhead_lengths,color="blue")+
  coord_sf(xlim = c(-123, -121.4), ylim = c(45.5, 46.2), expand = FALSE)+
  ggtitle("Summer Steelhead")

print(SU_SH_map)
ggsave(SU_SH_map,filename="SU_SH_map.png")

#########################################################################################
#SPRING CHINOOK
url <- "https://geodataservices.wdfw.wa.gov/arcgis/rest/services/MapServices/SWIFD/MapServer/0/query"
total_records = 100000
batch_size = 1000
dat <- data.frame()

for (offset in seq(0, total_records, by = batch_size)) {
  query_params <- list(
    where = paste0("SPECIES = 'CHINOOK SALMON' 
                   AND OBJECTID > ", offset, " AND OBJECTID <= ", offset + batch_size,
                   "AND RUNTIME_DESC = 'Spring'"
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
  crs = st_crs("+proj=longlat +datum=NAD83 +units=m")
)%>%
  setNames(gsub("attributes\\.", "", colnames(.)))%>%
  st_set_crs(st_crs("+proj=longlat +datum=WGS84 +units=m"))

sf_swifd <- st_transform(sf_swifd, st_crs(SpringChinook))

#Calculate total length of habitat within each NOAA polygon
sf_swifd_pops <- sf_swifd %>%
  #st_join(sf_pops)%>%
  st_intersection(SpringChinook%>%
                    group_by(NWFSC_POP_ID)%>%
                    mutate(area=st_area(SHAPE))%>%
                    summarise(area=sum(area))
  )%>%
  filter(!is.na(NWFSC_POP_ID) 
         & !DISTTYPE_DESC %in% c("Gradient Accessible","Modeled")
         & !LLID_STRM_NAME %in% c("Columbia River", "Cedar Creek")
  )%>%
  group_by(NWFSC_POP_ID,area)%>%
  summarise()%>%
  mutate(length = st_length(geometry))%>%
  mutate(length_km = set_units(length, km),area_sq_km = set_units(area, km^2))%>%
  dplyr::select(-length,-area)

spring_chinook_lengths <- sf_swifd_pops%>%
  left_join(populations%>%
              dplyr::select(NWFSC_POP_ID=NMFS_POPID,ESAPOPNAME),
            by=join_by(NWFSC_POP_ID)
  )

print(spring_chinook_lengths)

SP_CK_map<-ggplot() +
  geom_sf(data=state_map,color="red")+
  geom_sf(data = SpringChinook,color="green")+
  geom_sf(data = spring_chinook_lengths,color="blue")+
  coord_sf(xlim = c(-124.2, -121), ylim = c(45.5, 47), expand = FALSE)+
  ggtitle("Spring Chinook")


print(SP_CK_map)
ggsave(SP_CK_map,filename="SP_CK_map.png")

#########################################################################################
#FALL CHINOOK
url <- "https://geodataservices.wdfw.wa.gov/arcgis/rest/services/MapServices/SWIFD/MapServer/0/query"
total_records = 100000
batch_size = 1000
dat <- data.frame()

for (offset in seq(0, total_records, by = batch_size)) {
  query_params <- list(
    where = paste0("SPECIES = 'CHINOOK SALMON' 
                   AND OBJECTID > ", offset, " AND OBJECTID <= ", offset + batch_size,
                   "AND RUNTIME_DESC = 'Fall'"
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
  crs = st_crs("+proj=longlat +datum=NAD83 +units=m")
)%>%
  setNames(gsub("attributes\\.", "", colnames(.)))%>%
  st_set_crs(st_crs("+proj=longlat +datum=WGS84 +units=m"))

sf_swifd <- st_transform(sf_swifd, st_crs(FallChinook))

#Calculate total length of habitat within each NOAA polygon
sf_swifd_pops <- sf_swifd %>%
  #st_join(sf_pops)%>%
  st_intersection(FallChinook%>%
                    group_by(NWFSC_POP_ID)%>%
                    mutate(area=st_area(SHAPE))%>%
                    summarise(area=sum(area))
  )%>%
  filter(!is.na(NWFSC_POP_ID) 
         & !DISTTYPE_DESC %in% c("Gradient Accessible", "Potential","Historic - Documented","Artificial - Potential","Transported - Potential","Modeled")
         & LLID_STRM_NAME!="Columbia River"
         & !(LLID_STRM_NAME %in% c("Cowlitz River","Cispus River","Yellowjacket Creek","North Fork Cispus River","Skate Creek","Silver Creek","Ohanapecosh River","Clear Fork Cowlitz River","Muddy Fork Cowlitz River") & NWFSC_POP_ID ==27)
  )%>%
  group_by(NWFSC_POP_ID,area)%>%
  summarise()%>%
  mutate(length = st_length(geometry))%>%
  mutate(length_km = set_units(length, km),area_sq_km = set_units(area, km^2))%>%
  dplyr::select(-length,-area)

fall_chinook_lengths <- sf_swifd_pops%>%
  left_join(populations%>%
              dplyr::select(NWFSC_POP_ID=NMFS_POPID,ESAPOPNAME),
            by=join_by(NWFSC_POP_ID)
  )

print(fall_chinook_lengths)


FA_CK_map<-ggplot() +
  geom_sf(data=state_map,color="red")+
  geom_sf(data = FallChinook,color="green")+
  geom_sf(data = fall_chinook_lengths,color="blue")+
  coord_sf(xlim = c(-124.2, -121), ylim = c(45.4, 46.9), expand = FALSE)+
  ggtitle("Fall Chinook")

  print(FA_CK_map)
ggsave(FA_CK_map,filename="FA_CK_map.png")

#########################################################################################
#LATE FALL CHINOOK
#Section commented out because SWIFD doesn't contain unique late fall data. 
# url <- "https://geodataservices.wdfw.wa.gov/arcgis/rest/services/MapServices/SWIFD/MapServer/0/query"
# total_records = 100000
# batch_size = 1000
# dat <- data.frame()
# 
# for (offset in seq(0, total_records, by = batch_size)) {
#   query_params <- list(
#     where = paste0("SPECIES = 'CHINOOK SALMON' 
#                    AND OBJECTID > ", offset, " AND OBJECTID <= ", offset + batch_size
#                 #   "AND RUNTIME_DESC = 'Late fall'"
#     ),
#     outFields = "*",
#     outSR = 4326,
#     f = "json"
#   )
#   response <- GET(url, query = query_params)
#   json_data <- content(response, "text")
#   # Parse and process the JSON data
#   parsed_data <- fromJSON(json_data)
#   # Convert the parsed data to a data frame
#   batch_data <- as.data.frame(parsed_data$features)
#   # Append the batch to the overall data frame
#   dat <- bind_rows(dat, batch_data)
# }
# 
#lfall_test <- sort(unique(dat$attributes$SPECIESRUN))
# 
# # Assuming all_data is your list with attributes and geometry
# # Extract paths from the geometry list
# paths <- lapply(dat$geometry$paths, function(path) st_linestring(matrix(as.numeric(path), ncol = 2, byrow = F)))
# 
# # Create an sf object
# sf_swifd <- st_sf(
#   # attributes
#   attributes = dat$attributes,
#   # geometry
#   geometry = st_sfc(paths),
#   # set coordinate reference system (CRS) - replace EPSG:4326 with the appropriate CRS
#   crs = st_crs("+proj=longlat +datum=NAD83 +units=m")
# )%>%
#   setNames(gsub("attributes\\.", "", colnames(.)))%>%
#   st_set_crs(st_crs("+proj=longlat +datum=WGS84 +units=m"))
# 
# sf_swifd <- st_transform(sf_swifd, st_crs(LateFallChinook))
# 
# #Calculate total length of habitat within each NOAA polygon
# sf_swifd_pops <- sf_swifd %>%
#   #st_join(sf_pops)%>%
#   st_intersection(LateFallChinook)%>%
#   filter(!is.na(NWFSC_POP_ID) 
#          & !DISTTYPE_DESC %in% c("Gradient Accessible", "Potential","Historic - Documented","Artificial - Potential","Transported - Potential","Modeled")
#   )%>%
#   group_by(NWFSC_POP_ID)%>%
#   summarise()%>%
#   mutate(length = st_length(geometry), area=st_area(geometry))%>%
#   mutate(length_km = set_units(length, km),area_sq_km = set_units(area, km^2))%>%
#   dplyr::select(-length,-area)
# 
# lfall_chinook_lengths <- sf_swifd_pops%>%
#   left_join(populations%>%
#               dplyr::select(NWFSC_POP_ID=NMFS_POPID,ESAPOPNAME),
#             by=join_by(NWFSC_POP_ID)
#   )
# 
# print(lfall_chinook_lengths)
# 
# LF_CK_map<-ggplot() +
#   geom_sf(data = LateFallChinook,color="green")+
#   geom_sf(data = lfall_chinook_lengths,color="blue")
# 
# print(LF_CK_map)

#########################################################################################
#FALL COHO
url <- "https://geodataservices.wdfw.wa.gov/arcgis/rest/services/MapServices/SWIFD/MapServer/0/query"
total_records = 100000
batch_size = 1000
dat <- data.frame()

for (offset in seq(0, total_records, by = batch_size)) {
  query_params <- list(
    where = paste0("SPECIES = 'COHO SALMON' 
                   AND OBJECTID > ", offset, " AND OBJECTID <= ", offset + batch_size
      #             "AND RUNTIME_DESC = 'Fall'"
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
  crs = st_crs("+proj=longlat +datum=NAD83 +units=m")
)%>%
  setNames(gsub("attributes\\.", "", colnames(.)))%>%
  st_set_crs(st_crs("+proj=longlat +datum=WGS84 +units=m"))

sf_swifd <- st_transform(sf_swifd, st_crs(FallCoho))

#Calculate total length of habitat within each NOAA polygon
sf_swifd_pops <- sf_swifd %>%
  #st_join(sf_pops)%>%
  st_difference(estuary_polygons)%>%
  st_intersection(FallCoho%>%
                    group_by(NWFSC_POP_ID)%>%
                    mutate(area=st_area(SHAPE))%>%
                    summarise(area=sum(area))
  )%>%
  filter(!is.na(NWFSC_POP_ID) 
         & DISTTYPE_DESC == "Modeled"
         & LLID_STRM_NAME!="Columbia River"
         & !(LLID_STRM_NAME=="Cowlitz River" & NWFSC_POP_ID == 137)
         & !(LLID %in% c("1227158463282",	"1229186463107") & NWFSC_POP_ID == 142) #Toutle mainstem, NF Toutle mainstem--estimates dont include
  )%>%
  group_by(NWFSC_POP_ID,area)%>%
  summarise()%>%
  mutate(length = st_length(geometry))%>%
  mutate(length_km = set_units(length, km),area_sq_km = set_units(area, km^2))%>%
  dplyr::select(-length,-area)

fall_coho_lengths <- sf_swifd_pops%>%
  left_join(populations%>%
              dplyr::select(NWFSC_POP_ID=NMFS_POPID,ESAPOPNAME),
            by=join_by(NWFSC_POP_ID)
  )

print(fall_coho_lengths)

FA_CO_map<-ggplot() +
  geom_sf(data=state_map,color="red",fill=NA)+
  geom_sf(data =estuary_polygons,color="green",fill=NA)+
  geom_sf(data = FallCoho,color="green",fill=NA)+
  geom_sf(data = fall_coho_lengths,color="blue")+
  coord_sf(xlim = c(-124.2, -121), ylim = c(45.4, 46.9), expand = FALSE)+
  ggtitle("Fall Coho")

print(FA_CO_map)
ggsave(FA_CO_map,filename="FA_CO_map.png")

#########################################################################################
#FALL CHUM
url <- "https://geodataservices.wdfw.wa.gov/arcgis/rest/services/MapServices/SWIFD/MapServer/0/query"
total_records = 100000
batch_size = 1000
dat <- data.frame()

for (offset in seq(0, total_records, by = batch_size)) {
  query_params <- list(
    where = paste0("SPECIES = 'CHUM SALMON' 
                   AND OBJECTID > ", offset, " AND OBJECTID <= ", offset + batch_size,
                                "AND RUNTIME_DESC = 'Fall'"
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
  crs = st_crs("+proj=longlat +datum=NAD83 +units=m")
)%>%
  setNames(gsub("attributes\\.", "", colnames(.)))%>%
  st_set_crs(st_crs("+proj=longlat +datum=WGS84 +units=m"))

sf_swifd <- st_transform(sf_swifd, st_crs(FallChum))

#Calculate total length of habitat within each NOAA polygon
sf_swifd_pops <- sf_swifd %>%
  #st_join(sf_pops)%>%
  st_intersection(FallChum%>%
                    group_by(NWFSC_POP_ID)%>%
                    mutate(area=st_area(SHAPE))%>%
                    summarise(area=sum(area))
  )%>%
  filter(!is.na(NWFSC_POP_ID) 
         & !DISTTYPE_DESC %in% c("Gradient Accessible", "Potential","Historic - Documented","Artificial - Potential","Transported - Potential","Modeled")
         & LLID_STRM_NAME!="Columbia River"
  )%>%
  group_by(NWFSC_POP_ID,area)%>%
  summarise()%>%
  mutate(length = st_length(geometry))%>%
  mutate(length_km = set_units(length, km),area_sq_km = set_units(area, km^2))%>%
  dplyr::select(-length,-area)

fall_chum_lengths <- sf_swifd_pops%>%
  left_join(populations%>%
              dplyr::select(NWFSC_POP_ID=NMFS_POPID,ESAPOPNAME),
            by=join_by(NWFSC_POP_ID)
  )

print(fall_chum_lengths)

FA_CH_map<-ggplot() +
  geom_sf(data=state_map,color="red")+
  geom_sf(data = FallChum,color="green")+
  geom_sf(data = fall_chum_lengths,color="blue")+
  coord_sf(xlim = c(-124.2, -120.8), ylim = c(45.4, 46.9), expand = FALSE)+
  ggtitle("Fall Chum")

print(FA_CH_map)
ggsave(FA_CH_map,filename="FA_CH_map.png")

#########################################################################################
#SUMMER CHUM
url <- "https://geodataservices.wdfw.wa.gov/arcgis/rest/services/MapServices/SWIFD/MapServer/0/query"
total_records = 100000
batch_size = 1000
dat <- data.frame()

for (offset in seq(0, total_records, by = batch_size)) {
  query_params <- list(
    where = paste0("SPECIES = 'CHUM SALMON' 
                   AND OBJECTID > ", offset, " AND OBJECTID <= ", offset + batch_size,
                   "AND RUNTIME_DESC = 'Summer'"
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
  crs = st_crs("+proj=longlat +datum=NAD83 +units=m")
)%>%
  setNames(gsub("attributes\\.", "", colnames(.)))%>%
  st_set_crs(st_crs("+proj=longlat +datum=WGS84 +units=m"))

sf_swifd <- st_transform(sf_swifd, st_crs(SummerChum))

#Calculate total length of habitat within each NOAA polygon
sf_swifd_pops <- sf_swifd %>%
  #st_join(sf_pops)%>%
  st_intersection(SummerChum%>%
                    group_by(NWFSC_POP_ID)%>%
                    mutate(area=st_area(SHAPE))%>%
                    summarise(area=sum(area))
  )%>%
  filter(!is.na(NWFSC_POP_ID) 
         & !DISTTYPE_DESC %in% c("Gradient Accessible", "Potential","Historic - Documented","Artificial - Potential","Transported - Potential","Modeled")
         & LLID_STRM_NAME!="Columbia River"
  )%>%
  group_by(NWFSC_POP_ID,area)%>%
  summarise()%>%
  mutate(length = st_length(geometry))%>%
  mutate(length_km = set_units(length, km),area_sq_km = set_units(area, km^2))%>%
  dplyr::select(-length,-area)

summer_chum_lengths <- sf_swifd_pops%>%
  left_join(populations%>%
              dplyr::select(NWFSC_POP_ID=NMFS_POPID,ESAPOPNAME),
            by=join_by(NWFSC_POP_ID)
  )

print(summer_chum_lengths)

SU_CH_map<-ggplot() +
  geom_sf(data=state_map,color="red")+
  geom_sf(data = SummerChum,color="green")+
  geom_sf(data = summer_chum_lengths,color="blue")+
  coord_sf(xlim = c(-124.2, -120.8), ylim = c(45.4, 46.9), expand = FALSE)+
  ggtitle("Summer Chum")

print(SU_CH_map)
ggsave(SU_CH_map,filename="SU_CH_map.png")
#######################################################################################
#Combine lengths into a single output file and export. 
hab_lengths <- rbind(winter_steelhead_lengths,summer_steelhead_lengths,fall_coho_lengths,
      spring_chinook_lengths,fall_chinook_lengths,fall_chum_lengths,summer_chum_lengths)
hab_lengths$geometry <- NULL
print(hab_lengths)
write.csv(hab_lengths,"hab_lengths.csv",row.names = F)


# read_csv("hab_lengths_10.4.2024_with comments.csv")%>%
#   group_by(Species)%>%
#   summarize(length=quantile(length_km,c(0,0.1,0.25,0.5,0.75,0.9,1)),quants=c(0,0.1,0.25,0.5,0.75,0.9,1))%>%
#   pivot_wider(names_from = quants,values_from = length)

#=========================================================
# Manual Substitutions and Edits
hab_lengths<-read_csv("hab_lengths.csv")

hab_lengths_edit<-hab_lengths%>%
  mutate(length_km=ifelse(ESAPOPNAME=="Steelhead (Lower Columbia River DPS) Kalama River - winter",
                          hab_lengths%>%
                            filter(ESAPOPNAME=="Steelhead (Lower Columbia River DPS) Kalama River - summer")%>%
                            dplyr::select(length_km)%>%
                            pull(),
                          length_km), #use summer steelhead frame to account for winters above KFH
         area_sq_km=ifelse(ESAPOPNAME=="Salmon, Chinook (Lower Columbia River ESU) Lower Cowlitz River - fall",
                            area_sq_km 
                            +
                            hab_lengths%>%
                            filter(ESAPOPNAME=="Salmon, Chinook (Lower Columbia River ESU) Upper Cowlitz River - fall")%>%
                            dplyr::select(area_sq_km)%>%
                            pull() 
                          ,area_sq_km #use watershed area + upstream watershed area
                          )
         )%>% 
  bind_rows(
    tibble(
      NWFSC_POP_ID = 13,
      ESAPOPNAME = "Salmon, Chinook (Lower Columbia River ESU) Lewis River - late fall",
      length_km = hab_lengths%>%
        filter(ESAPOPNAME=="Salmon, Chinook (Lower Columbia River ESU) Lewis River - fall")%>%
        dplyr::select(length_km)%>%
        pull(),
      area_sq_km = hab_lengths%>%
        filter(ESAPOPNAME=="Salmon, Chinook (Lower Columbia River ESU) Lewis River - fall")%>%
        dplyr::select(area_sq_km)%>%
        pull()
    )
  )%>%
  arrange(ESAPOPNAME)%>%
  write.csv("hab_lengths_edited.csv",row.names = F)

