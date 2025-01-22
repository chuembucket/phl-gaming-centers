library(sf)
library(tidyverse)
library(mapview)
library(googlesheets4)
library(tidygeocoder)
library(leaflet)
library(gt)

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


# map

## dowload extra details
council_districts <- read_sf("https://opendata.arcgis.com/api/v3/datasets/1ba5a5d68f4a4c75806e78b1d9245924_0/downloads/data?format=geojson&spatialRefId=4326&where=1%3D1")
septa_lines <- read_sf("https://opendata.arcgis.com/api/v3/datasets/1e7754ca5f7d47e480a628e282466428_0/downloads/data?format=geojson&spatialRefId=4326")

# Filter for just BSL and MFL
rapid_transit <- septa_lines %>%
  filter(Route %in% c("Broad Street Line", "Market-Frankford Line"))
### static map

ggplot()+
  geom_sf(data = council_districts)+
  #geom_sf(data = rapid_transit, aes(color = Route), lwd = 1.7, alpha = .5) +
  geom_sf(data = sites_sf , aes(size = as.character(`Number of PCs`), fill = `operator type`), color ='black', pch = 21)+
    scale_fill_manual(values = c(
    "Public - SDP" = "#4471c1",
    "Public - PPR" = "#3a833c",
    "Public - JJSC" = "grey",
    "Commercial" = "#f3c613",
    "Community" = "#9400c6"
  ))+
  scale_color_manual(values = c(
    "Broad Street Line" = "#F26722",
    "Market-Frankford Line" = "#1C9AD6"
  ))+
  scale_size_manual(values = c(
    "10" = 3,
    "26" = 6,
    "70" = 9,
    "140" = 12
  ))+
  mapTheme+
  guides(size = 'none',
         color = 'none',
         shape = guide_legend())

## gt
  
sites %>% select(-opened, -phl, -pa) %>% 
  mutate(size_category = case_when(
    `Number of PCs` >= 70 ~ "Large (> 70 PCs)",
    `Number of PCs` >= 25 ~ "Medium (~25 PCs)",
    TRUE ~ "Small (< 10 PCs)"
  )) %>% 
  mutate(site = ifelse(site == "Rev. Leon H. Sullivan Community Impact Center*",
                              "Rev. Leon H. Sullivan<br />Community Impact Center*",
                              site)) %>% 
  mutate(site = ifelse(status == "Opening soon", paste0(site, "*"), site)) %>%
  # Add ordering for small sites
  mutate(order_weight = case_when(
    `Number of PCs` > 10 ~ 1,  # Keep larger sites at top
    `operator type` == "Community" ~ 2,
    `operator type` == "Public - SDP" ~ 3,
    `operator type` == "Public - PPR" ~ 4,
    `operator type` == "Public - JJSC" ~ 5,
    
    TRUE ~ 5
  )) %>%
  arrange(order_weight, desc(`Number of PCs`)) %>%
  select(-order_weight, -status) %>%  # Remove helper column
  group_by(size_category) %>%
  gt() %>% 
  opt_table_font(
    font = list(
      google_font("Inter")
    )
  ) %>%
  cols_label(
    site = "Location Name",
    address = "Address",
    operator = "Operator",
    `operator type` = "Type",
    `Number of PCs` = "PCs",
    size_category = "Size Category"
  ) %>%
  tab_style(
    style = list(
      #cell_text(weight = "bold"),
      cell_text(size = "12px")
    ),
    locations = cells_body()
  ) %>%
  tab_style(
    style = list(
      cell_text(weight = "bold"),
      cell_text(size = "12px")
    ),
    locations = cells_column_labels()
  ) %>%
  # Add color fills for each operator type
  tab_style(
    style = cell_text(color = "#f3c613"),
    locations = cells_body(
      columns = `operator type`,
      rows = `operator type` == "Commercial"
    )
  ) %>%
  tab_style(
    style = cell_text(color = '#9400c6'),  
    locations = cells_body(
      columns = `operator type`,
      rows = `operator type` == "Community"
    )
  ) %>%
  tab_style(
    style = cell_text(color = "#4471c1"),
    locations = cells_body(
      columns = `operator type`,
      rows = `operator type` == "Public - SDP"
    )
  ) %>%
  tab_style(
    style = cell_text(color = "#3a833c"), 
    locations = cells_body(
      columns = `operator type`,
      rows = `operator type` == "Public - PPR"
    )
    
  ) %>%
  tab_style(
    style = cell_text(color = "grey"), 
    locations = cells_body(
      columns = `operator type`,
      rows = `operator type` == "Public - JJSC"
    )
    
  ) %>%
  tab_options(
    row_group.background.color = "#f8f9fa",
    row_group.font.weight = "bold",
    table.font.size = "12px"
  ) %>%
  fmt_number(
    columns = `Number of PCs`,
    decimals = 0
  ) %>%
  cols_align(
    align = "left",
    columns = everything()
  ) %>%
  cols_align(
    align = "center",
    columns = `Number of PCs`
  ) %>%
  cols_width(columns = site ~ px(180)) %>% 
  tab_footnote(
    footnote = "* Opening in 2025",
    locations = cells_column_labels(columns = site)
  ) %>% tab_footnote(
    footnote = "Not an all encompassing list. If your esports center isn't here please reach out!",
    locations = cells_column_labels(columns = site)
  ) %>% fmt_markdown(columns = everything())
