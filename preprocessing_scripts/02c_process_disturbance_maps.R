####################################################################'
##
## Processing the disturbance maps ----
##
## Input: Cornelius's disturbance files
## Output: grid cell level fraction of each disturbance class
##  - 50 km hexagon grid
##  - 1 degree lat/lon grid
##
####################################################################'

library(tidyverse)
library(sf)
library(viridis)

load("./outputs/basemaps_europe.RData")

# output file
outfile_hexa <- "./data/processed/disturbance_grids/disturbances_hexagrid_UPDATE.RData"
outfile_degree <- "./data/processed/disturbance_grids/disturbances_degreegrid_UPDATE.RData"

# read disturbance data
in_dir <- "./data/raw/disturbance_attribution_CS/update/"
fls <- list.files(in_dir, pattern=".csv", full.names=TRUE)

# read grid
hexa_grid <- st_read("./data/raw/disturbance_attribution_CS/grids/hexagon_50km.shp")

# measurement years by country
census_stats_final <- readRDS("./data/processed/census_stats_final_15-12-22.rds")

yrs_by_country <- census_stats_final %>% 
  mutate(year0 = floor(census.date0),
         year1 = floor(census.date)) %>%
  select(database.code, country, year0, year1) %>%
  distinct()

# Calculate fractions per cell & time window ----

#check years
dist_lst <- lapply(fls, read.csv)
years_max <- sapply(dist_lst, function(x) max(x$year))
years_min <- sapply(dist_lst, function(x) min(x$year))

## extract country name from path
countries <- str_to_title(sapply(strsplit(gsub(pattern=".csv", replacement = "", x = fls), split="_"), "[[", 6))
countries <- ifelse(countries == "Czechia", "Czech Republic", countries)

dist_country_lst <- list()
dist_country_degree_lst <- list()
for(i in 1:length(fls)) {
  if(!(countries[i] %in% yrs_by_country$country)) {
    next()
  }
  print(paste("Start:", countries[i]))
  dist_data <- as.data.frame(dist_lst[i]) #read.csv(fls[i])
  dist_data$country <- countries[i]
  
  dist_data <- dist_data %>%
    st_as_sf(coords = c("x", "y"), crs = "EPSG:3035") %>%
    st_join(hexa_grid %>% rename(grid.id = id) %>% select(grid.id)) %>%
    st_transform(crs = "EPSG:4326")
  dist_data[, c("longitude", "latitude")] <- st_coordinates(dist_data)
  
  dist_data <- dist_data %>% st_drop_geometry()
  
  # for each time window, calculate % of natural dist from all dist in grid cells
  yrs_i <- yrs_by_country %>% filter(country == countries[i])
  print(yrs_i)
  dist_y_lst <- list()
  dist_y_degree_lst <- list()
  for(j in 1:nrow(yrs_i)) {
    print(paste("Now starting years:", yrs_i$year0[j], "to", yrs_i$year1[j]))
    # HEXAGON GRID
    dist_y_lst[[j]] <- dist_data %>%
      filter(year >= yrs_i$year0[j] & year <= yrs_i$year1[j]) %>%
      group_by(grid.id, prediction_class) %>%
      summarise(area_dist = sum(area),
                n_dist = n()) %>%
      mutate(area_tot = sum(area_dist),
             p_disttype = area_dist / area_tot,
             year0 = yrs_i$year0[j],
             year1 = yrs_i$year1[j],
             n_dist_all = sum(n_dist)) 
    # DEGREE GRID
    dist_y_degree_lst[[j]] <- dist_data %>%
      filter(year >= yrs_i$year0[j] & year <= yrs_i$year1[j]) %>%
      mutate(lat = round(latitude),
             lon = round(longitude)) %>%
      group_by(lat, lon, prediction_class) %>%
      summarise(area_dist = sum(area),
                n_dist = n()) %>%
      mutate(area_tot = sum(area_dist),
             p_disttype = area_dist / area_tot,
             year0 = yrs_i$year0[j],
             year1 = yrs_i$year1[j],
             n_dist_all = sum(n_dist))
  }
  # combine and widen
  dist_country_lst[[i]] <- do.call(rbind, dist_y_lst)  %>%
    select(grid.id, year0, year1, n_dist_all, prediction_class, p_disttype) %>%
    pivot_wider(id_cols = grid.id:n_dist_all, values_from = p_disttype, names_from=prediction_class)
  
  dist_country_degree_lst[[i]] <- do.call(rbind, dist_y_degree_lst)  %>%
    select(lat, lon, year0, year1, n_dist_all, prediction_class, p_disttype) %>%
    pivot_wider(id_cols = lat:n_dist_all, values_from = p_disttype, names_from=prediction_class)
  
  #make sure all dataframes have all columns
  all_cols_hexa <- c("grid.id", "year0", "year1", "n_dist_all", "harvest", "barkbeetle_windthrow", "fire", "country")
  missing_cols <- setdiff(all_cols_hexa, colnames(dist_country_lst[[i]]))
  dist_country_lst[[i]][, missing_cols] <- 0
  dist_country_lst[[i]] <- dist_country_lst[[i]][, all_cols_hexa]

  all_cols_degree <- c("lat", "lon", "year0", "year1", "n_dist_all", "harvest", "barkbeetle_windthrow", "fire", "country")
  missing_cols <- setdiff(all_cols_degree, colnames(dist_country_degree_lst[[i]]))
  dist_country_degree_lst[[i]][, missing_cols] <- 0
  dist_country_degree_lst[[i]] <- dist_country_degree_lst[[i]][, all_cols_degree] 
  
  # add country
  dist_country_lst[[i]]$country <- countries[i]
  dist_country_degree_lst[[i]]$country <- countries[i]
}

dist_hexagrid <- do.call(rbind, dist_country_lst)
dist_degreegrid <- do.call(rbind, dist_country_degree_lst)

#### check & viz ----

grid_av <- hexa_grid %>%
  left_join(dist_hexagrid %>%
              group_by(grid.id) %>%
              summarise(harvest = mean(harvest, na.rm=TRUE),
                        barkbeetle_windthrow = mean(barkbeetle_windthrow, na.rm=TRUE),
                        fire = mean(fire, na.rm=TRUE)),
            by= c("id" = "grid.id"))


pdf(file="./data/processed/disturbance_grids/disturbances_hexagrid_UPDATE.pdf")
ggplot() + geom_sf(data = grid_av, aes(fill=harvest)) + scale_fill_viridis() + theme(legend.position = "bottom")
ggplot() + geom_sf(data = grid_av, aes(fill=barkbeetle_windthrow)) + scale_fill_viridis() + theme(legend.position = "bottom")
ggplot() + geom_sf(data = grid_av, aes(fill=fire)) + scale_fill_viridis() + theme(legend.position = "bottom")
dev.off()

# Write files -------------------------------------------------------------

saveRDS(dist_hexagrid, file=outfile_hexa)
saveRDS(dist_degreegrid, file=outfile_degree)
