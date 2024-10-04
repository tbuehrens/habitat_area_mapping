#Lower Columbia River Habitat Lengths

#Script designed to generate total habitat in river kilometers for each NOAA 
#designated salmon and steelhead population in the LCR. 

#Toby Harbison & Thomas Buehrens 
#8/7/24

#Load packages 
pacman::p_load(shiny, tidyverse, devtools, ggplot2, leaflet,sf,rnaturalearth,httr,jsonlite, 
               dplyr, RODBC, curl,odbc,DBI,tidyverse,janitor,fuzzyjoin,ggplot2,
               lubridate,kableExtra,sf,rnaturalearth,ggmap,httr,here,units)

########################################################################################
#Part 1: Pull habitat area polygons for each NOAA designated population in the LCR from
#NOAA website: 
#Create a geodatabase placeholder for spatial data. 
fgdb <- "spatial_data/WCR_Salmon_Steelhead_gdb_2015.gdb"
if (file.exists(fgdb)) {
  print("Geodatabase already downloaded!")
} else {
  dir_path <- here::here("spatial_data")
  if (!dir.exists(dir_path)) {
    # If not, create the directory
    dir.create(dir_path, recursive = TRUE)
  }
  print("Attempting to download Geodatabase...may take a few mins!")
  url<-"https://www.webapps.nwfsc.noaa.gov/portal/sharing/rest/content/items/097239ff29b44a8b87acc048f0363229/data"
  response <- GET(url, timeout(600))
  content <- content(response, as = "raw")
  writeBin(content, "spatial_data/WCR_Salmon_Steelhead_gdb_2015.zip")
  #download.file(, destfile = "data/WCR_Salmon_Steelhead_gdb_2015.zip", mode = "wb",timeout = 300)
  unzip("spatial_data/WCR_Salmon_Steelhead_gdb_2015.zip", exdir = "spatial_data")
}

noaa_polygons<-st_read(fgdb, layer = "fish")%>%
  #Set the coordinate system for the NOAA polygons
  st_set_crs(st_crs("+proj=longlat +datum=NAD83 +units=m"))

#Create separate polygons for each species in the LCR
FallCoho <- noaa_polygons%>%
  filter(
    DPS %in% c(
      "Salmon, coho (Lower Columbia River ESU)",
      "Salmon, coho (Lower Columbia River ESU) - Outside legal area"
    ),
    SPECIES == "CO" & !is.na(NWFSC_POP_ID)
  ) %>%
  group_by(NWFSC_POP_ID)%>%
  summarise()

WinterSteelhead <- noaa_polygons%>%
  filter(
    DPS %in% c(
      "Steelhead (Lower Columbia River DPS)",
      "Steelhead (Lower Columbia River DPS) - Outside legal area"
      #could add in Steelhead (Middle Columbia River DPS) and su wi for klickitat and white salmon
    )
  ) %>%
  filter(SPECIES == "ST" & RUN_TIMING == "wi") %>%
  group_by(NWFSC_POP_ID)%>%
  summarise()%>%
  bind_rows(noaa_polygons%>%
              filter(
                DPS %in% c(
                  "Salmon, coho (Lower Columbia River ESU)",
                  "Salmon, coho (Lower Columbia River ESU) - Outside legal area"
                ),
                SPECIES == "CO" & NWFSC_POP_ID%in%c(134,135,140)
              ) %>%
              group_by(NWFSC_POP_ID)%>%
              mutate(NWFSC_POP_ID=ifelse(NWFSC_POP_ID==135,9991,NWFSC_POP_ID),#Grays
                     NWFSC_POP_ID=ifelse(NWFSC_POP_ID==134,9992,NWFSC_POP_ID),#elochoman
                     NWFSC_POP_ID=ifelse(NWFSC_POP_ID==140,9993,NWFSC_POP_ID),#Mill -abernathy-germany
              )%>%
              summarise()
  )

SummerSteelhead <- noaa_polygons%>%
  filter(
    DPS %in% c(
      "Steelhead (Lower Columbia River DPS)",
      "Steelhead (Lower Columbia River DPS) - Outside legal area"
    )
  ) %>%
  filter(SPECIES == "ST" & RUN_TIMING == "su") %>%
  group_by(NWFSC_POP_ID)%>%
  summarise()

SpringChinook <- noaa_polygons%>%
  filter(
    DPS %in% c(
      "Salmon, Chinook (Lower Columbia River ESU)",
      "Salmon, Chinook (Lower Columbia River ESU) - Outside legal area"
    )
  ) %>%
  filter(SPECIES == "CK" & RUN_TIMING %in% c("sp","ss")) %>%
  group_by(NWFSC_POP_ID)%>%
  summarise()

FallChinook <- noaa_polygons%>%
  filter(
    DPS %in% c(
      "Salmon, Chinook (Lower Columbia River ESU)",
      "Salmon, Chinook (Lower Columbia River ESU) - Outside legal area"
    )
  ) %>%
  filter(SPECIES == "CK" & RUN_TIMING == "fa") %>%
  group_by(NWFSC_POP_ID)%>%
  summarise()

LateFallChinook <- noaa_polygons%>%
  filter(
    DPS %in% c(
      "Salmon, Chinook (Lower Columbia River ESU)",
      "Salmon, Chinook (Lower Columbia River ESU) - Outside legal area"
    )
  ) %>%
  filter(SPECIES == "CK" & RUN_TIMING == "lf") %>%
  group_by(NWFSC_POP_ID)%>%
  summarise()

FallChum <- noaa_polygons%>%
  filter(
    DPS %in% c(
      "Salmon, chum (Columbia River ESU)",
      "Salmon, chum (Columbia River ESU) - Outside legal area"
    )
  ) %>%
  filter(SPECIES == "CM" & RUN_TIMING == "fa") %>%
  group_by(NWFSC_POP_ID)%>%
  summarise()

SummerChum <- noaa_polygons%>%
  filter(
    DPS %in% c(
      "Salmon, chum (Columbia River ESU)",
      "Salmon, chum (Columbia River ESU) - Outside legal area"
    )
  ) %>%
  filter(SPECIES == "CM" & RUN_TIMING == "su") %>%
  group_by(NWFSC_POP_ID)%>%
  summarise()

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
  st_intersection(WinterSteelhead)%>%
  filter(!is.na(NWFSC_POP_ID) 
         & DISTTYPE_DESC == "Modeled"
         & LLID_STRM_NAME!="Columbia River"
  )%>%
  group_by(NWFSC_POP_ID)%>%
  summarise()%>%
  mutate(length = st_length(geometry))%>%
  mutate(length_km = set_units(length, km))%>%
  dplyr::select(-length)

print(sf_swifd_pops)

#######################################################################################
#Step 3: Relate stream lengths to NOAA population names. 
# NOAA POP LUT:  https://cax.streamnet.org/
#populations<-read_csv("data/populations.csv")%>%
url <- "https://cax.streamnet.org/download/ca-data-all%2011-27-2023%2010%2015.xls"
temp_file <- tempfile(fileext = ".xls")
download.file(url, temp_file, mode = "wb")
populations<-readxl::read_excel(temp_file, sheet = "Populations")
unlink(temp_file)

populations<-populations%>%
  dplyr::rename(run=RUN,species=SPECIES)%>%
  mutate(species=gsub(" salmon","",species),
         run=ifelse(run%in%c("Late","Both early & late","Early","Early (Type S)","Late (Type N)"),"Fall",run),
         across(c(species,run),~tolower(.)),
         ESAPOPNAME=ifelse(POPULATIONNAME=="Grays/Chinook winter Steelhead","Steelhead (Southwest Washington DPS) Grays and Chinook Rivers - winter",ESAPOPNAME),
         NMFS_POPID=ifelse(POPULATIONNAME=="Grays/Chinook winter Steelhead",9991,NMFS_POPID),
         ESAPOPNAME=ifelse(POPULATIONNAME=="Elochoman/Skamokawa winter Steelhead","Steelhead (Southwest Washington DPS) Elochoman River - winter",ESAPOPNAME),
         NMFS_POPID=ifelse(POPULATIONNAME=="Elochoman/Skamokawa winter Steelhead",9992,NMFS_POPID),
         ESAPOPNAME=ifelse(POPULATIONNAME=="Mill/Abernathy/Germany winter Steelhead","Steelhead (Southwest Washington DPS) Mill Creek - winter",ESAPOPNAME),
         NMFS_POPID=ifelse(POPULATIONNAME=="Mill/Abernathy/Germany winter Steelhead",9993,NMFS_POPID)
  )%>%
  filter(!is.na(NMFS_POPID))

winter_steelhead_lengths <- sf_swifd_pops%>%
  left_join(populations%>%
              dplyr::select(NWFSC_POP_ID=NMFS_POPID,ESAPOPNAME),
            by=join_by(NWFSC_POP_ID)
            )

print(winter_steelhead_lengths)

#########################################################################################
#Step 4: Render maps by NOAA population

#Render map
state_map <- ne_states (country = 'United States of America', returnclass = 'sf')%>% 
  filter (name %in% c('Washington','Oregon'))

state_map <- st_transform(state_map, st_crs(WinterSteelhead))

WI_SH_map<-ggplot() +
  geom_sf(data=state_map,color="red")+
  geom_sf(data = WinterSteelhead,color="green")+
  geom_sf(data = winter_steelhead_lengths,color="blue")+
  coord_sf(xlim = c(-124.5, -121.25), ylim = c(45.5, 47), expand = FALSE)+
  ggtitle("Winter Steelhead")

print(WI_SH_map)

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
  st_intersection(SummerSteelhead)%>%
  filter(!is.na(NWFSC_POP_ID) 
         & DISTTYPE_DESC == "Modeled"
         & LLID_STRM_NAME!="Columbia River"
           )%>%
  group_by(NWFSC_POP_ID)%>%
  summarise()%>%
  mutate(length = st_length(geometry))%>%
  mutate(length_km = set_units(length, km))%>%
  dplyr::select(-length)

summer_steelhead_lengths <- sf_swifd_pops%>%
  left_join(populations%>%
              dplyr::select(NWFSC_POP_ID=NMFS_POPID,ESAPOPNAME),
            by=join_by(NWFSC_POP_ID)
  )

print(summer_steelhead_lengths)

SU_SH_map<-ggplot() +
  geom_sf(data=state_map,color="red")+
  geom_sf(data = SummerSteelhead,color="green")+
  geom_sf(data = summer_steelhead_lengths,color="blue")+
  coord_sf(xlim = c(-123, -121.4), ylim = c(45.5, 46.2), expand = FALSE)+
  ggtitle("Summer Steelhead")

print(SU_SH_map)

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
  st_intersection(SpringChinook)%>%
  filter(!is.na(NWFSC_POP_ID) 
         & !DISTTYPE_DESC %in% c("Gradient Accessible", "Potential","Historic - Documented","Artificial - Potential","Transported - Potential","Modeled")
         & LLID_STRM_NAME!="Columbia River"
  )%>%
  group_by(NWFSC_POP_ID)%>%
  summarise()%>%
  mutate(length = st_length(geometry))%>%
  mutate(length_km = set_units(length, km))%>%
  dplyr::select(-length)

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
  st_intersection(FallChinook)%>%
  filter(!is.na(NWFSC_POP_ID) 
         & !DISTTYPE_DESC %in% c("Gradient Accessible", "Potential","Historic - Documented","Artificial - Potential","Transported - Potential","Modeled")
  
  )%>%
  group_by(NWFSC_POP_ID)%>%
  summarise()%>%
  mutate(length = st_length(geometry))%>%
  mutate(length_km = set_units(length, km))%>%
  dplyr::select(-length)

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
#   mutate(length = st_length(geometry))%>%
#   mutate(length_km = set_units(length, km))%>%
#   dplyr::select(-length)
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
  st_intersection(FallCoho)%>%
  filter(!is.na(NWFSC_POP_ID) 
         & !DISTTYPE_DESC %in% c("Gradient Accessible", "Potential","Historic - Documented","Artificial - Potential","Transported - Potential","Modeled")
  )%>%
  group_by(NWFSC_POP_ID)%>%
  summarise()%>%
  mutate(length = st_length(geometry))%>%
  mutate(length_km = set_units(length, km))%>%
  dplyr::select(-length)

fall_coho_lengths <- sf_swifd_pops%>%
  left_join(populations%>%
              dplyr::select(NWFSC_POP_ID=NMFS_POPID,ESAPOPNAME),
            by=join_by(NWFSC_POP_ID)
  )

print(fall_coho_lengths)

FA_CO_map<-ggplot() +
  geom_sf(data=state_map,color="red")+
  geom_sf(data = FallCoho,color="green")+
  geom_sf(data = fall_coho_lengths,color="blue")+
  coord_sf(xlim = c(-124.2, -121), ylim = c(45.4, 46.9), expand = FALSE)+
  ggtitle("Fall Coho")

print(FA_CO_map)

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
  st_intersection(FallChum)%>%
  filter(!is.na(NWFSC_POP_ID) 
         & !DISTTYPE_DESC %in% c("Gradient Accessible", "Potential","Historic - Documented","Artificial - Potential","Transported - Potential","Modeled")
  )%>%
  group_by(NWFSC_POP_ID)%>%
  summarise()%>%
  mutate(length = st_length(geometry))%>%
  mutate(length_km = set_units(length, km))%>%
  dplyr::select(-length)

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
  st_intersection(SummerChum)%>%
  filter(!is.na(NWFSC_POP_ID) 
         & !DISTTYPE_DESC %in% c("Gradient Accessible", "Potential","Historic - Documented","Artificial - Potential","Transported - Potential","Modeled")
  )%>%
  group_by(NWFSC_POP_ID)%>%
  summarise()%>%
  mutate(length = st_length(geometry))%>%
  mutate(length_km = set_units(length, km))%>%
  dplyr::select(-length)

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

#######################################################################################
#Combine lengths into a single output file and export. 
hab_lengths <- rbind(winter_steelhead_lengths,summer_steelhead_lengths,fall_coho_lengths,
      spring_chinook_lengths,fall_chinook_lengths,fall_chum_lengths,summer_chum_lengths)
hab_lengths$geometry <- NULL
print(hab_lengths)
write.csv(hab_lengths,"hab_lengths.csv")
