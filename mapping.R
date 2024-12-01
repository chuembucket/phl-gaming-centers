library(sf)
library(tidyverse)
library(mapview)
library(googlesheets4)
library(tidygeocoder)
library(leaflet)
library(gt)

source("https://raw.githubusercontent.com/urbanSpatial/Public-Policy-Analytics-Landing/master/functions.r")


gs4_auth()


sites <- googlesheets4::read_sheet(ss = "https://docs.google.com/spreadsheets/d/1-OshZpXIJ_OvSX2ZuJ1x5eb3u4g8W5f2U1AB_cGBjWU/edit?gid=0#gid=0",
            sheet = "main") %>% mutate(
              phl = "Philadelphia",
              pa = "PA"
            )

# Geocode the addresses
geocoded_addresses <- sites %>%
  geocode(method = "osm", street = Address, county = phl, state = pa) 

sites_sf <- geocoded_addresses %>% st_as_sf(coords = c("long", "lat"), crs = 4326)
  

# Create a leaflet map
map <- leaflet(sites_sf) %>%
  addTiles() %>%
  addCircleMarkers(
    radius = ~(`Number of PCs`/ 5),
    popup = ~site
  ) 

map

council_districts <- read_sf("https://opendata.arcgis.com/api/v3/datasets/1ba5a5d68f4a4c75806e78b1d9245924_0/downloads/data?format=geojson&spatialRefId=4326&where=1%3D1")
sites_sf <- st_join(sites_sf, select(council_districts,`Council District` = DISTRICT))

### static map

ggplot()+
  geom_sf(data = council_districts)+
  geom_sf(data = sites_sf, aes(size = `Number of PCs`, color = `operator type`) )+
  mapTheme()



sites_sf %>% st_drop_geometry() %>% 
  select(site, `operator type`, `Number of PCs`, Address, `Council District`) %>% 
  arrange(factor(`operator type`, levels = c('Commercial Operator', 'Community Operator', 'City - Parks & Rec'))) %>% 
  group_by(`operator type`) %>% 
  gt(groupname_col = 'operator type', rowname_col = "site") %>% tab_stub_indent(
    rows = everything(),
    indent = 4
  ) %>% 
  cols_align(columns = `Number of PCs`, "left") %>% 
  tab_header(
    title = "Philadelphia Current and Future Gaming Centers"
  ) %>% tab_stubhead(label="Gaming Center") 

