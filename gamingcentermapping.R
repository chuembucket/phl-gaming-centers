library(sf)
library(tidyverse)
library(mapview)
library(googlesheets4)
library(tidygeocoder)
library(leaflet)

mapTheme <- theme(plot.title =element_text(size=12),
                  plot.subtitle = element_text(size=8),
                  plot.caption = element_text(size = 6),
                  axis.line=element_blank(),
                  axis.text.x=element_blank(),
                  axis.text.y=element_blank(),
                  axis.ticks=element_blank(),
                  axis.title.x=element_blank(),
                  axis.title.y=element_blank(),
                  panel.background=element_blank(),
                  panel.border=element_blank(),
                  panel.grid.major=element_line(colour = 'transparent'),
                  panel.grid.minor=element_blank(),
                  legend.direction = "vertical", 
                  legend.position = "right",
                  plot.margin = margin(1, 1, 1, 1, 'cm'),
                  legend.key.height = unit(1, "cm"), legend.key.width = unit(0.2, "cm"))
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
  

# # Create a leaflet map
# map <- leaflet(sites_sf) %>%
#   addTiles() %>%
#   addCircleMarkers(
#     radius = ~(`Number of PCs`/ 6),
#     popup = ~site
#   ) 
# 
# map

council_districts <- read_sf("https://opendata.arcgis.com/api/v3/datasets/1ba5a5d68f4a4c75806e78b1d9245924_0/downloads/data?format=geojson&spatialRefId=4326&where=1%3D1")
### static map

ggplot()+
  geom_sf(data = council_districts)+
  geom_sf(data = sites_sf, aes(size = `Number of PCs`, fill = `operator type`), color ='black',pch=21)+
  scale_fill_manual(values = c(
    "City - PPR" = "green",
    "Commercial" = "orange",
    "Faith-based" = "purple"
  ))+
  mapTheme
