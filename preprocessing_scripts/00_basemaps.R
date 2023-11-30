############################################'
##
## Create ggplot2 basemaps
##
############################################'
############################################'
rm(list=ls())

library(sf)
library(ggplot2)
library(tidyverse)

sf::sf_use_s2(FALSE)

# projection info
proj_lambert_conic <- "+proj=lcc +lat_1=43 +lat_2=62 +lat_0=30 +lon_0=10 +x_0=0 +y_0=0 +ellps=intl +units=m no_defs"

# basemap
wrld <- read_sf("./data/raw/ne_10m_admin_0_countries/ne_10m_admin_0_countries.shp")

bbox_europe <- c(xmin=-25, xmax=40, ymin=25, ymax=72)
bbox_canary <- c(xmin=-25, xmax=-10, ymin=25, ymax=30)
europe <- wrld %>% 
  st_crop(bbox_europe)
europe_proj <- st_transform(europe, proj_lambert_conic)

canary_islands_proj <- st_crop(wrld, bbox_canary) %>% 
  st_transform(proj_lambert_conic) %>%
  filter(ADMIN == "Spain")

basemap <- ggplot() + 
  geom_sf(data = wrld, fill="white", col="gray90") +
  coord_sf(default = TRUE) + 
  ylim(25,70) + xlim (-25,40) 

basemap_proj <- ggplot() + 
  geom_sf(data = europe_proj, fill="white", col="gray90") +
  coord_sf(default = TRUE) 

add_buff <- 5e4
map_extent_canary <- as.data.frame(st_coordinates(canary_islands_proj)) %>%
  summarize(min_x = min(X) - add_buff,
            max_x = max(X) + add_buff,
            min_y = min(Y) - add_buff,
            max_y = max(Y) + add_buff)

basemap_canary <- ggplot() +
  geom_sf(data=canary_islands_proj, fill="white", col="gray90") +
  coord_sf(default = TRUE, 
           ylim = c(map_extent_canary$min_y, map_extent_canary$max_y),
           xlim = c(map_extent_canary$min_x, map_extent_canary$max_x))

save(basemap, basemap_proj, map_extent_canary, file="./outputs/basemaps_europe.RData")

st_write(europe_proj, "./outputs/europe_proj_natearth.gpkg")
