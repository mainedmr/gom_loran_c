# Make Loran-C map layer for the Gulf of Maine by passing intersections of
# lines to the NOAA Loran converter and the connecting the dots.
# 
# Created by Bill DeVoe @ Maine Department of Marine Resources, November 2020
# For questions William.DeVoe@maine.gov

# Set WD to current location
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

## Install/load packages
if (!require(pacman)) {install.packages('pacman')}
pacman::p_load(tidyverse, rvest, sf, smoothr, 
               leaflet, leaflet.esri, leaflet.extras,
               htmltools, htmlwidgets)

# Import NOAA converter batch
source('noaa_converter_batch.R')

## WX grid
if (!file.exists('wx_grid_points.csv')) {
  wx <- expand.grid(w = seq(11800, 14000, by = 10),
                    x = seq(25000, 26000, by = 10)) %>%
    mutate(chain = 'Loran9960')
  
  # Will take this many requests to converter page
  nrow(wx) / 10
  
  # Broken up into chunks of 5000 records in case something goes wrong
  wx_grid <- batch_loran(wx[1:5000,], 'w', 'x', 'chain')
  write_csv(wx_grid, 'wx_grid.csv')
  wx_grid2 <- batch_loran(wx[5001:10000,], 'w', 'x', 'chain')
  write_csv(wx_grid2, 'wx_grid2.csv')
  wx_grid3 <- batch_loran(wx[10001:15000,], 'w', 'x', 'chain')
  write_csv(wx_grid3, 'wx_grid3.csv')
  wx_grid4 <- batch_loran(wx[15001:nrow(wx),], 'w', 'x', 'chain')
  write_csv(wx_grid4, 'wx_grid4.csv')
  
  wx_combined <- rbind(wx_grid, wx_grid2, wx_grid3, wx_grid4)
  
  wx_missed <- wx_combined %>%
    filter(latitude == 0 | longitude == 0) %>%
    batch_loran(., 'w', 'x', 'chain')
  
  # Extra in Casco Bay
  wx_extra <- expand.grid(w = seq(12800, 14000, by = 10),
                    x = seq(26010, 26200, by = 10)) %>%
    mutate(chain = 'Loran9960') %>%
    batch_loran(., 'w', 'x', 'chain')
  
  wx_final <- wx_combined %>%
    filter(!is.na(latitude) & !is.na(longitude)) %>%
    filter(!(latitude == 0 | longitude == 0)) %>%
    rbind(wx_missed) %>%
    rbind(wx_extra) %>%
    mutate(longitude = longitude * -1)
  
  write_csv(wx_final, 'wx_grid_points.csv')
} else {
  wx_final <- read.csv('wx_grid_points.csv')
}

if (!file.exists('wy_grid_points.csv')) {
    ## WY grid
    wy <- expand.grid(w = seq(11800, 14000, by = 10),
                      y = seq(44000, 44575, by = 5)) %>%
      mutate(chain = 'Loran9960')
    
    wy_grid <- batch_loran(wy[1:5000,], 'w', 'y', 'chain')
    write_csv(wy_grid, 'wy_grid.csv')
    wy_grid2 <- batch_loran(wy[5001:10000,], 'w', 'y', 'chain')
    write_csv(wy_grid2, 'wy_grid2.csv')
    wy_grid3 <- batch_loran(wy[10001:15000,], 'w', 'y', 'chain')
    write_csv(wy_grid3, 'wy_grid3.csv')
    wy_grid4 <- batch_loran(wy[15001:20000,], 'w', 'y', 'chain')
    write_csv(wy_grid4, 'wy_grid4.csv')
    wy_grid5 <- batch_loran(wy[20001:nrow(wy),], 'w', 'y', 'chain')
    write_csv(wy_grid4, 'wy_grid5.csv')
    
    wy_combined <- rbind(wy_grid, wy_grid2, wy_grid3, wy_grid4, wy_grid5)
    
    wy_missed <- wy_combined %>%
      filter(latitude == 0 | longitude == 0) %>%
      batch_loran(., 'w', 'y', 'chain')
    
    wy_final <- wy_combined %>%
      filter(!is.na(latitude) & !is.na(longitude)) %>%
      filter(!(latitude == 0 | longitude == 0)) %>%
      rbind(wy_missed) %>%
      mutate(longitude = longitude * -1)
    
    write_csv(wy_final, 'wy_grid_points.csv')
} else {
  wy_final <- read.csv('wy_grid_points.csv')
}

### Now make the lines

x_lines <- wx_final %>%
  arrange(x, w) %>%
  sf::st_as_sf(coords = c("longitude", "latitude"), crs = 4326) %>%
  dplyr::group_by(x) %>%
  dplyr::summarise(do_union = FALSE) %>%
  sf::st_cast("LINESTRING") %>%
  ungroup() %>%
  # Smooth lines to get rid of "kinks" from converter
  smoothr::smooth(method = "ksmooth", smoothness = 10) %>%
  rename(td = x) %>%
  mutate(station = 'X-W')

w1_lines <- wx_final %>%
  arrange(w, x) %>%
  sf::st_as_sf(coords = c("longitude", "latitude"), crs = 4326) %>%
  dplyr::group_by(w) %>%
  dplyr::summarise(do_union = FALSE) %>%
  sf::st_cast("LINESTRING") %>%
  ungroup() %>%
  smoothr::smooth(method = "ksmooth", smoothness = 10) %>%
  rename(td = w) %>%
  mutate(station = 'W-X')

y_lines <- wy_final %>%
  arrange(y, w) %>%
  sf::st_as_sf(coords = c("longitude", "latitude"), crs = 4326) %>%
  dplyr::group_by(y) %>%
  dplyr::summarise(do_union = FALSE) %>%
  sf::st_cast("LINESTRING") %>%
  ungroup() %>%
  smoothr::smooth(method = "ksmooth", smoothness = 10) %>%
  rename(td = y) %>%
  mutate(station = 'Y-W')

w2_lines <- wy_final %>%
  arrange(w, y) %>%
  sf::st_as_sf(coords = c("longitude", "latitude"), crs = 4326) %>%
  dplyr::group_by(w) %>%
  dplyr::summarise(do_union = FALSE) %>%
  sf::st_cast("LINESTRING") %>%
  ungroup() %>%
  smoothr::smooth(method = "ksmooth", smoothness = 10) %>%
  rename(td = w) %>%
  mutate(station = 'W-Y')

load('ne_coast.Rda')
coast_simp <- coast %>%
  rmapshaper::ms_simplify(.05) %>%
  st_combine() %>%
  st_union() %>%
  st_make_valid()


st_erase = function(x, y) {
  y <- y %>%
    st_combine() %>%
    #st_make_valid() %>%
    st_union()
  st_difference(x, y)
}

# Bind everything together to make final loran lines layer
loran_lines <- rbind(x_lines, y_lines, w1_lines, w2_lines) %>%
  st_crop(c(xmin = -75, xmax = -50, ymin = 41, ymax = 50)) %>%
  st_make_valid() %>%
  # Project to UTM
  st_transform(26919) %>%
  # Erase lines over land
  st_erase(coast_simp) %>%
  # Project back to WGS84
  st_transform(4326) %>%
  # Add a field for the TD interval
  mutate(interval = case_when(
    td %% 100 == 0 ~ 100,
    td %% 50 == 0 ~ 50,
    td %% 25 == 0 ~ 25,
    td %% 10 == 0 ~ 10,
    td %% 5 == 0 ~ 5,
    TRUE ~ 0
  ))


mapview::mapview(loran_lines)

sf::st_write(loran_lines, 'loran_lines.shp', append = F)

## Leaflet
library(glue)
make_labs <- function(data) {
  lab_str <- ''
  for (fld in colnames(data)) {
    if (is.character(data[[fld]]) | is.numeric(data[[fld]])) {
      fld_name <- gsub('_', ' ', fld) %>%
        stringr::str_to_title()
      lab_str <- paste0(lab_str, "<strong>", fld_name,
                        ": </strong> {data[['", fld, "']]}<br>")
    }
  }
  glue(lab_str) %>% lapply(htmltools::HTML)
}

library(leaflet)
library(leaflet.esri)
library(leaflet.extras)

loran_leaflet <- loran_lines %>%
  filter(station != 'W-Y')

loran_100 <- loran_leaflet %>%
  filter(interval >= 100)

loran_100_labs = make_labs(loran_100)

loran_50 <- loran_leaflet %>%
  filter(interval == 50)

loran_50_labs = make_labs(loran_50)

loran_10 <- loran_leaflet %>%
  filter(interval <= 10)

loran_10_labs = make_labs(loran_10)

lines_pal <- colorFactor(c('red', 'green', 'black'), c('X-W', 'Y-W', 'W-X'))

map <- leaflet(options = leafletOptions(title = "Wind Map")) %>%
  setView(lat = 44, lng = -69 , zoom = 8) %>%
  # Basemap options
  addProviderTiles(providers$Esri.NatGeoWorldMap, group = "National Geographic") %>%
  addProviderTiles(providers$Esri.OceanBasemap, group = "World Oceans") %>%
  addProviderTiles(providers$Esri.WorldImagery, group = "World Imagery") %>%
  addEsriImageMapLayer(url = "https://seamlessrnc.nauticalcharts.noaa.gov/arcgis/rest/services/RNC/NOAA_RNC/ImageServer",
                       layerId = "chart",
                       group = "Nautical Charts") %>%
  addPolylines(data = loran_100, color = ~lines_pal(station),
               label = loran_100_labs, group = '100') %>%
  addPolylines(data = loran_50, color = ~lines_pal(station),
               label = loran_50_labs, group = '50') %>%
  addPolylines(data = loran_10, color = ~lines_pal(station),
               label = loran_10_labs, group = '10') %>%
  addLayersControl(
    position = "topleft",
    baseGroups = c("World Oceans", "World Imagery", "National Geographic", "Nautical Charts"),
    #overlayGroups = overlays,
    options = layersControlOptions(collapsed = FALSE)
  ) %>%
  groupOptions('100', zoomLevels = 1:20) %>%
  groupOptions('50', zoomLevels = 8:20) %>%
  groupOptions('10', zoomLevels = 9:20) %>%
  addLegend("bottomright", pal = lines_pal, values = unique(loran_100$station),
            title = "LORAN Station",
            opacity = 1
  )

map

# Save leaflet map
htmlwidgets::saveWidget(map, file = 'loran_lines.html')
