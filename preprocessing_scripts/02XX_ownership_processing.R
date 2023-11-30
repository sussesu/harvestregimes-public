#################################################################'
##
## Extract values to plot data from Pulla forest ownership data
##
#################################################################'

library(tidyverse)
library(sf)

fl_date <-format(Sys.Date(), "%d-%m-%y")
load("./outputs/basemaps_europe.RData")

# Read data and define things --------------------------------------------------
outfile_name <- "./data/processed/plots_w_PullaOwnership.csv"

# ownership data
ownership <- read_sf("./data/raw/Pulla_ForesOwnership/Shapefile/forestownership_europe.shp")

# a few polygons have values of 9999 in the %s. 
# Removing these here so that the plots in these polygons (total 4 polygons, 2 plots) will be linked to the other nearest polygon
ownership %>% filter(StatPct == 9999)
ownership <- ownership %>%
  filter(StatPct != 9999)

# plots
census_stats_final <- readRDS("./data/processed/census_stats_final_15-12-22.rds")

nfi_plots <- census_stats_final %>% 
  dplyr::select(tmt.plot.id, latitude, longitude) %>%
  st_as_sf(coords=c("longitude", "latitude"), crs="+proj=longlat +datum=WGS84 +no_defs ", remove=FALSE) %>%
  st_transform(crs = st_crs(ownership))

#join
plots_with_ownership <- nfi_plots %>%
  st_join(ownership) %>%
  select(tmt.plot.id, PrivPct, StatPct, OtherPct)

# deal with NAs - join to closest NUTS polygon
plotsNA_distance <- plots_with_ownership %>%
  filter(is.na(StatPct)) %>%
  st_distance(ownership)

idx_mindist <- apply(plotsNA_distance, 1, which.min)

# # check that correct polygons found
# ggplot() +
#   geom_sf(data = ownership[unique(idx_mindist),]) +
#   geom_sf(data = plots_with_ownership %>% filter(is.na(StatPct) ), col=2)

# replace the NA values with nearest polygon values
plots_with_ownership$wasNA <- is.na(plots_with_ownership$StatPct)
plots_with_ownership[is.na(plots_with_ownership$StatPct), c("PrivPct", "StatPct", "OtherPct")] <- ownership[idx_mindist,c("PrivPct", "StatPct", "OtherPct")] %>%
  st_drop_geometry()

summary(plots_with_ownership)

plots_with_ownership <- plots_with_ownership %>%
  st_drop_geometry() %>%
  select(!wasNA)

# Write outputs -----------------------------------------------------------

write.csv(plots_with_ownership, file = outfile_name)
