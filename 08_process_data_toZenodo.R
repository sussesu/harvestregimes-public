####################################################################'
##
## Harvest grids processing to be added to Zenodo
##
####################################################################'

library(tidyverse)
library(sf)

# load data ----

harvest_values <- read.csv("./data/processed/harvest_grids/harvest_grid_29-05-24.csv",
                            row.names = 1)

grid_wgs84 <- read_sf("./data/processed/harvest_grids/grid_1degree_wgs84_weights_28-05-24.gpkg") 
grid_projected <- read_sf("./data/processed/harvest_grids/grid_1degree_projected_weights_28-05-24.gpkg") 

# select columns ----

harvest_columns <- c("lat",
                     "lon",
                     "country",
                     "n_plots",
                     "n_harvest",
                     "ba_perc_HARVEST",
                     "harvest_pros",
                     "harvest_percent_ba_mean",
                     "harvest_percent_ba_sd",
                     "harvest_p_0_25",
                     "harvest_p_25_50",
                     "harvest_p_50_75",
                     "harvest_p_75_100")

harvest_values <- harvest_values %>%
  select(all_of(harvest_columns))

# fractions to percentages ----
# (as presented in the MS figures)
# harvest_pros, ba_perc_HARVEST

harvest_values <- harvest_values %>%
  mutate(harvest_pros = harvest_pros * 100,
         ba_perc_HARVEST = ba_perc_HARVEST * 100)

# check grid cells with too few observations ----
# (needs to be n_plots >= 20, same as in the MS)
# --> no grid cells with fewer plots, all ok

harvest_values %>% filter(n_plots < 20) # should be an empty data frame

# rename columns ----

rename_columns <- c("lat" = "lat",
                     "lon" = "lon",
                     "country" = "country",
                     "n_plots" = "n_plots",
                     "n_harvest" = "n_harvest",
                     "ba_perc_HARVEST" = "harvest_rate",
                     "harvest_pros" = "harvest_freq",
                     "harvest_percent_ba_mean" = "harvest_intensity_mean",
                     "harvest_percent_ba_sd" = "harvest_intensity_sd",
                     "harvest_p_0_25" = "p_harvests_in_0_25",
                     "harvest_p_25_50" = "p_harvests_in_25_50",
                     "harvest_p_50_75" = "p_harvests_in_50_75",
                     "harvest_p_75_100" = "p_harvests_in_75_100")

harvest_values <- harvest_values %>% 
  rename_at(vars(names(rename_columns)), ~ rename_columns)

# check and process grid ----

# add centroid coordinates to grid cells

grid_coords <- grid_wgs84 %>%
  st_centroid() %>%
  st_coordinates() %>%
  round()

grid_wgs84$lat <- grid_coords[,"Y"]
grid_wgs84$lon <- grid_coords[,"X"]

grid_wgs84 <- grid_wgs84 %>% select(id, lat, lon, geom)

# project
proj_lambert_conic <- "+proj=lcc +lat_1=43 +lat_2=62 +lat_0=30 +lon_0=10 +x_0=0 +y_0=0 +ellps=intl +units=m no_defs"

grid_lambert <- grid_wgs84 %>%
  st_transform(proj_lambert_conic)

# ggplot(grid_lambert) + geom_sf()
# ggplot(grid_lambert) + geom_sf(col = 2, lwd = 2) +
#   geom_sf(data = grid_projected, lty = 2, col = 1, alpha = 0.5)
# basemap_proj + geom_sf(data = grid_lambert)

# write files -----

write_csv(harvest_values, file = "./data/processed/harvest_grids/variables_harvest_regimes_zenodo1.csv")

st_write(grid_lambert, dsn = "./data/processed/harvest_grids/grid_harvest_regimes_zenodo1.gpkg")

# # check ----
# rm(list=ls())
# 
# h1 <- read_csv("./data/processed/harvest_grids/variables_harvest_regimes_zenodo1.csv")
# g1 <- st_read("./data/processed/harvest_grids/grid_harvest_regimes_zenodo1.gpkg")


