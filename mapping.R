library(sf)
library(tidyverse)
library(mapview)
library(googlesheets4)
library(tidygeocoder)
library(leaflet)

gs4_auth()


sites <- googlesheets4::read_sheet(ss = "https://docs.google.com/spreadsheets/d/1-OshZpXIJ_OvSX2ZuJ1x5eb3u4g8W5f2U1AB_cGBjWU/edit?gid=0#gid=0",
            sheet = "main") %>% mutate(
              phl = "Philadelphia",
              pa = "PA"
            )

# Geocode the addresses
geocoded_addresses <- sites %>%
  geocode(method = "osm", street = address, county = phl, state = pa) 

sites_sf <- geocoded_addresses %>% st_as_sf(coords = c("long", "lat"), crs = 4326)
  

# Create a leaflet map
map <- leaflet(sites_sf) %>%
  addTiles() %>%
  addCircleMarkers(
    radius = ~(`number of pcs`/ 5),
    popup = ~site
  ) 

map

council_districts <- read_sf("https://opendata.arcgis.com/api/v3/datasets/1ba5a5d68f4a4c75806e78b1d9245924_0/downloads/data?format=geojson&spatialRefId=4326&where=1%3D1")
### static map

ggplot()+
  geom_sf(data = council_districts)+
  geom_sf(data = sites_sf, aes(size = `number of pcs`, color = `operator type`))
