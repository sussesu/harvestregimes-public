##########################################################'
##
## Additional data preparations for RFs
## - combine TMt data with info extracted from rasters
## - add sub-national admin info for Spain and Germany
##
##########################################################'

library(ggplot2)
library(dplyr)
library(sf)
library(viridis)
library(tidyr)

# Read data & define things -----------------------------------------------

fl_date <-format(Sys.Date(), "%d-%m-%y")

#input files
input_fl_annual <- "./data/processed/data_annual_22-08-23.rds"
input_fl_census <- "./data/processed/census_stats_final_22-08-23.rds"

#output files

out_annual <- paste0("./data/processed/data_annual_", fl_date, "_RFready.rds") # paste0(gsub(".rds", "", input_fl_annual), "_RFready.rds")
out_census <- paste0("./data/processed/census_stats_final_", fl_date, "_RFready.rds")# paste0(gsub(".rds", "", input_fl_census), "_RFready.rds")

out_annual_weights <- paste0(gsub(".rds", "", input_fl_annual), "_weights_RFready.rds")

# read data
data_annual <- readRDS(input_fl_annual)
data_census <- readRDS(input_fl_census)


# Join raster extracted variables ------------------------------------------
# from script 02b_extract_from_rasters.R

extract_covs <- readRDS("./data/processed/plots_w_extracted_values_16-01-23.rds")

dim(data_annual)
data_annual <- data_annual %>%
  left_join(extract_covs)
dim(data_annual)

dim(data_census)
data_census <- data_census %>%
  left_join(extract_covs)
dim(data_census)


# Join NUTS level ownership information ----------------------------------------
# from script 

ownership_pulla <-  read.csv("./data/processed/plots_w_PullaOwnership.csv", row.names = 1)
colnames(ownership_pulla)[2:length(ownership_pulla)] <- paste0("Owner", colnames(ownership_pulla)[2:length(ownership_pulla)])


dim(data_annual)
data_annual <- data_annual %>%
  left_join(ownership_pulla)
dim(data_annual)

dim(data_census)
data_census <- data_census %>%
  left_join(ownership_pulla)
dim(data_census)

summary(data_annual$OwnerStatPct)
summary(data_census$OwnerStatPct)


# Sp classes to factor -----------------------------------------------------
sp_class_levels <- c("PlantationSp", "Picea", "Pinus", "FagusQuercus", 
                     "OtherConifer", "OtherBroadleaved")

data_annual$species_classes <- factor(data_annual$species_classes,
                                      levels = sp_class_levels)

data_census$species_classes <- factor(data_census$species_classes,
                                      levels = sp_class_levels)

# Join disturbance attribution information ---------------------------------
grid_hexa <- st_read("./data/raw/disturbance_attribution_CS/grids/hexagon_50km.shp")
dist_grid <- readRDS("./data/processed/disturbance_grids/disturbances_hexagrid_UPDATE.RData") 

dist_grid <- grid_hexa %>%
  left_join(dist_grid, by = c("id" = "grid.id")) %>%
  filter(!is.na(n_dist_all)) %>%
  mutate(harvest = ifelse(is.na(harvest), 0, harvest),
         barkbeetle_windthrow = ifelse(is.na(barkbeetle_windthrow), 0, barkbeetle_windthrow),
         fire = ifelse(is.na(fire), 0, fire) ) 

## TMt data to spatial
data_annual_sf <- data_annual %>% 
  st_as_sf(coords = c("longitude", "latitude"), crs="EPSG:4326", remove = FALSE) %>%
  st_transform(crs = "EPSG:3035") %>%
  mutate(year0 = floor(census.date0),
         year1 = floor(census.date) ) %>% 
  st_join(grid_hexa %>% select(id, geometry))

data_census_sf <- data_census  %>% 
  st_as_sf(coords = c("longitude", "latitude"), crs="EPSG:4326", remove = FALSE) %>%
  st_transform(crs = "EPSG:3035") %>%
  mutate(year0 = floor(census.date0),
         year1 = floor(census.date) ) %>% 
  st_join(grid_hexa %>% select(id, geometry))

# check if grid cells 
dist_grid <- dist_grid %>%
  filter(id %in% data_census_sf$id)

# deal with cells with few or none obs
n_dist_thold <- 10

dist_grid$NAs <- (dist_grid$n_dist_all < n_dist_thold) | is.na(dist_grid$n_dist_all)

buff_neighbours <- st_buffer(dist_grid %>% filter(NAs == TRUE), dist = 5000)

nbours <- dist_grid %>%
  st_join(buff_neighbours, left=FALSE) %>%
  filter(year0.x == year0.y & year1.x == year1.y & country.x == country.y) %>%
  group_by(id.y, year0.y, year1.y, country.y) %>%
  summarise(harvest = sum(harvest.x * n_dist_all.x)/sum(n_dist_all.x),
            barkbeetle_windthrow = sum(barkbeetle_windthrow.x * n_dist_all.x)/sum(n_dist_all.x),
            fire = sum(fire.x * n_dist_all.x)/sum(n_dist_all.x))

dist_grid2 <- dist_grid %>%
  left_join(nbours %>% st_drop_geometry(), 
            by=c("id" = "id.y", "year0" = "year0.y", 
                 "year1" = "year1.y", "country" = "country.y")) %>%
  mutate(harvest = ifelse(n_dist_all < n_dist_thold, harvest.y, harvest.x),
         barkbeetle_windthrow = ifelse(n_dist_all < n_dist_thold, barkbeetle_windthrow.y, barkbeetle_windthrow.x),
         fire = ifelse(n_dist_all < n_dist_thold, fire.y, fire.x) )
  

ggplot() +  geom_sf(data=dist_grid %>%
                      mutate(fire = ifelse(n_dist_all < n_dist_thold, NA, fire)),
                    aes(fill=fire)) +
  scale_fill_viridis(direction = -1)

ggplot() + geom_sf(data=dist_grid2, aes(fill=fire)) + scale_fill_viridis(direction = -1)
  

dist_grid2 <- dist_grid2 %>%
  select(year0, year1, country, id, harvest, barkbeetle_windthrow, fire, geometry) %>%
  rename(dist_harvest = harvest,
         dist_barkbeetle_windthrow = barkbeetle_windthrow,
         dist_fire = fire) %>%
  ungroup()

# combine with tmt data 
data_annual_sf <- data_annual_sf  %>%
  left_join(dist_grid2 %>% st_drop_geometry())

data_census_sf <- data_census_sf  %>%
  left_join(dist_grid2 %>% st_drop_geometry())

# check NAs and set dist in Canary islands to average of plots in Spain
# (currently canary islands not included in the RFs so this not really used)
data_census_sf %>% filter(is.na(dist_fire)) %>%
  ggplot(aes(latitude)) + geom_density()

dist_spain <- data.frame(harvest = mean(data_annual_sf$dist_harvest[data_annual_sf$country == "Spain"], na.rm=TRUE),
                barkbeetle_windthrow = mean(data_annual_sf$dist_barkbeetle_windthrow[data_annual_sf$country == "Spain"], na.rm=TRUE),
                fire = mean(data_annual_sf$dist_fire[data_annual_sf$country == "Spain"], na.rm=TRUE))

data_census_sf[data_census_sf$latitude < 30, c("dist_harvest", "dist_barkbeetle_windthrow", "dist_fire")] <- dist_spain
data_annual_sf[data_annual_sf$latitude < 30, c("dist_harvest", "dist_barkbeetle_windthrow", "dist_fire")] <- dist_spain


# other NAs
# some plots in coastal Norway and Sweden, setting these to harvest = 1, others = 0

# data_census_sf %>%
#   filter(is.na(dist_fire)) %>%
#   select(database.code, tmt.census.id, 
#          dist_fire, dist_barkbeetle_windthrow, dist_harvest) %>%
#   View()
# 
ggplot() + geom_sf(data = dist_grid2) +
  geom_sf(data = data_census_sf %>% filter(is.na(dist_fire)),
          col=2)

data_census_sf <- data_census_sf %>%
  mutate(dist_harvest = ifelse(is.na(dist_harvest), 1, dist_harvest),
         dist_barkbeetle_windthrow = ifelse(is.na(dist_barkbeetle_windthrow), 0, dist_barkbeetle_windthrow),
         dist_fire = ifelse(is.na(dist_fire), 0, dist_fire))

data_annual_sf <- data_annual_sf %>%
  mutate(dist_harvest = ifelse(is.na(dist_harvest), 1, dist_harvest),
         dist_barkbeetle_windthrow = ifelse(is.na(dist_barkbeetle_windthrow), 0, dist_barkbeetle_windthrow),
         dist_fire = ifelse(is.na(dist_fire), 0, dist_fire))

# Administrative units Spain & Germany ------------------------------------

## Autonomous provinces Spain
spain_prov <- read.csv("./data/raw/Spain_paloma/management23.csv", row.names=1) %>%
  select(plotcode, Provincia)

spain_codes <- read.csv("./data/raw/Spain_paloma/provinces_codes_2R.csv")

spain_prov <- spain_prov %>% 
  left_join(spain_codes, by = c("Provincia" = "CODIGO_PROVINCIA_INE")) %>%
  mutate(comunidad_autonoma = NOMBRE_COMUNIDAD_AUTONOMA,
         comunidad_autonoma = ifelse(comunidad_autonoma == "Datos espaciales (IFNXX_MCA)",   #fix name for Castile-Leon
                                     "Castile Leon", comunidad_autonoma),
         comunidad_autonoma_code = paste0("Spain_", CODIGO_COMUNIDAD_AUTONOMA)) %>%
  select(plotcode, comunidad_autonoma, comunidad_autonoma_code) 

## States Germany

ger_shp <- st_read("./data/raw/Germany_shapefile/vg5000_01-01.utm32s.shape.ebenen/vg5000_ebenen_0101/VG5000_LAN.shp") %>%
  st_transform(crs = st_crs(data_census_sf)) %>%
  mutate(state = gsub(pattern=" \\(Bodensee\\)", replacement="", x = GEN),
         state_code = paste0("Germany_", ARS)) %>%
  select(state, state_code)

# join

dim(data_census_sf)
data_census_out <- data_census_sf %>%
  left_join(spain_prov,
            by=c("plot.id" = "plotcode")) %>%
  st_join(ger_shp, join = st_nearest_feature) %>%
  mutate(country_or_region = case_when(
    country == "Germany" ~ paste0("Germany_", state),
    country == "Spain" ~ paste0("Spain_", comunidad_autonoma),
    TRUE ~ country),
    country_or_region_old = case_when(
      country == "Germany" ~ state_code,
      country == "Spain" ~ comunidad_autonoma_code,
      TRUE ~ country))

dim(data_census_out)

data_annual_out <- data_annual_sf %>%
  left_join(data_census_out %>% st_drop_geometry() %>%
              select(tmt.census.id, country_or_region))

dim(data_annual_sf)
dim(data_annual_out)

table(data_census_out$country_or_region, useNA = "always")
table(data_annual_out$country_or_region, useNA = "always")
ggplot() + geom_sf(data = data_census_out %>% slice_sample(n=10000), aes(col=country_or_region))


# Species classifications -------------------------------------------------

# sp classes to factor
sp_class_levels <- c("PlantationSp", "Picea", "Pinus", "FagusQuercus", "OtherConifer",
                     "OtherBroadleaved")


data_census$species_classes <- factor(data_census$species_classes,
                                      levels = sp_class_levels)

data_annual$species_classes <- factor(data_annual$species_classes,
                                      levels = sp_class_levels)

# alternative sp-variable, separate PlantationSp to Eucalyptus and PinusPinaster

sp_class_levels2 <- c("Eucalyptus", "PinusPinaster", "Picea", "Pinus", "FagusQuercus", "OtherConifer",
                     "OtherBroadleaved")

data_census_out <- data_census_out %>%
  mutate(species_classes2 = case_when(
    genus_dom0 %in% c("Picea") ~ "Picea",
    genus_dom0 %in% c("Pinus") & species_dom0 != "Pinus pinaster"  ~ "Pinus",
    genus_dom0 %in% c("Fagus", "Quercus") ~ "FagusQuercus",
    genus_dom0 %in% c("Eucalyptus") ~ "Eucalyptus",
    species_dom0 == "Pinus pinaster" ~ "PinusPinaster",
    TRUE & !conifer0 ~ "OtherBroadleaved",
    TRUE & conifer0 ~ "OtherConifer"),
    species_classes2 = factor(species_classes2, levels = sp_class_levels2)) %>%
  st_drop_geometry()

data_annual_out <- data_annual_out %>%
  mutate(species_classes2 = case_when(
    genus_dom0 %in% c("Picea") ~ "Picea",
    genus_dom0 %in% c("Pinus") & species_dom0 != "Pinus pinaster" ~ "Pinus",
    genus_dom0 %in% c("Fagus", "Quercus") ~ "FagusQuercus",
    genus_dom0 %in% c("Eucalyptus") ~ "Eucalyptus",
    species_dom0 == "Pinus pinaster" ~ "PinusPinaster",
    TRUE & !conifer0 ~ "OtherBroadleaved",
    TRUE & conifer0 ~ "OtherConifer"),
    species_classes2 = factor(species_classes2, levels = sp_class_levels2)) %>%
  st_drop_geometry()

summary(data_census_out$species_classes2)


# Rep-forest-area for Sweden ----------------------------------------------
# these are missing for some reason???

swe_counties <- st_read("./data/raw/Sweden_lan_shp/alla_lan/alla_lan.shp")
swe_forest_area <- read.csv("./data/raw/Sweden_lan_shp/forestarea_sweden_by_lan.csv")

swe_counties <- swe_counties %>%
  mutate(county_name = gsub("s län", replacement = "", x = LAN_NAMN),
         county_name = gsub(" län", replacement = "", x = county_name)) %>%
  st_transform(crs="EPSG:4326") %>%
  left_join(swe_forest_area, by=c("county_name" = "Lan"))

data_census_NSW <- data_census_out %>%
  filter(database.code == "NSW") %>%
  st_as_sf(coords = c("longitude", "latitude"), crs="EPSG:4326") %>%
  st_join(swe_counties, join = st_nearest_feature) %>%
  group_by(county_name, census.n) %>%
  mutate(n_plots_county = n(),
         forest_rep_area_NSW_county = Skogsmark_total / n_plots_county) %>%
  group_by(region, census.n) %>%
  mutate(n_plots_region = n(),
         Skogsmark_region = sum(Skogsmark_total),
         forest_rep_area_NSW_region = Skogsmark_region / n_plots_region) %>%
  ungroup()
  

ggplot() + 
  geom_sf(data = swe_counties, aes(fill=Skogsmark_total, col=factor(region))) #+
  # geom_sf(data = data_census_NSW %>% slice_sample(n=1000), aes(col=forest_rep_area_NSW_region))


data_census_out2 <- data_census_out %>%
  left_join(data_census_NSW %>% 
              select(tmt.census.id, forest_rep_area_NSW_county) %>%
              st_drop_geometry() ) %>%
  mutate(forest_area_rep_new = ifelse(database.code == "NSW", forest_rep_area_NSW_county, forest_area_rep))

# data_census_out2 %>%
#   slice_sample(n=1e4) %>%
#   ggplot(aes(longitude, latitude, col=forest_area_rep)) +
#   geom_point()
# 
# data_census_out2 %>%
#   ggplot(aes(db_country, forest_area_rep_new)) +
#   geom_violin(lwd=2)

data_census_out <- data_census_out2

data_annual_out <- data_annual_out %>%
  left_join(data_census_out %>% select(tmt.census.id, forest_area_rep_new))

summary(data_census_out$forest_area_rep_new)
summary(data_annual_out$forest_area_rep_new)


# # Remove POL from 11/8/2017 - NOT DOING THIS ANYMORE SINCE THE UPDATED DISTURBANCE DATA COVERS ALL YEARS!
# # Big storm in Poland in 11-12 Aug 2017, not covered by Senf data (ends in 2016)
# # --> remove data points with 2nd meas after storm from the RFs, at least until figured out how to deal with it
# 
# storm_date <- lubridate::decimal_date(as.Date("2017/08/11"))
# data_census_out <- data_census_out %>%
#   filter(country != "Poland" | (country == "Poland" & census.date < storm_date) )
# data_annual_out <- data_annual_out %>%
#   filter(country != "Poland" | (country == "Poland" & census.date < storm_date) )
# 
# # Update forest_rep_area by dividing the total forest area with the new number of plots in Poland
# forestarea2015_poland <- read.csv("./data/raw/FORESTEUROPE2020/panEuropean-forestArea_forests_2015.csv", skip = 1) %>%
#   filter(X == "Poland")
# 
# forest_rep_Poland <- (forestarea2015_poland$Area * 1000) / sum(data_census_out$country == "Poland")
# 
# unique(data_census_out$forest_area_rep_new[data_census_out$country == "Poland"]) # check old value
# 
# data_census_out <- data_census_out %>%
#   mutate(forest_area_rep_new = ifelse(country == "Poland", forest_rep_Poland, forest_area_rep_new))
# 
# data_annual_out <- data_annual_out %>%
#   mutate(forest_area_rep_new = ifelse(country == "Poland", forest_rep_Poland, forest_area_rep_new))
# 
# unique(data_census_out$forest_area_rep_new[data_census_out$country == "Poland"]) # check new value

# Remove Canary Islands (no NPP or disturbance data) ----------------------
data_census_out <- data_census_out %>% filter(latitude > 30)
data_annual_out <- data_annual_out %>% filter(latitude > 30)

# Weights -----------------------------------------------------------------
# weights based on 
#  - represented forest area
#   (( - the annualization (for data_annual only) ))

data_census_out$rep_area_weights <- data_census_out$forest_area_rep_new / sum(data_census_out$forest_area_rep_new) * nrow(data_census_out)
data_annual_out$rep_area_weights <- data_annual_out$forest_area_rep_new / sum(data_annual_out$forest_area_rep_new) * nrow(data_annual_out)

## data_annual to weights ----

dim(data_annual_out)

data_annual_weights <- data_annual_out %>%
  group_by(tmt.census.id, status) %>%
  mutate(n_annual = n()) %>%
  select(!any_of(c("ann.id"))) %>%
  distinct() %>%
  ungroup()

data_annual_weights$rep_area_weights <- (data_annual_weights$n_annual * data_annual_weights$forest_area_rep_new) / sum(data_annual_weights$forest_area_rep_new) * nrow(data_annual_weights)

dim(data_annual_weights)


# Additional soc-eco stats ------------------------------------------------
# - Ownership stats for Norway (2010 as 2015 is missing)
# - % of small-holdings from the private owners

soceco <- read.csv("./data/raw/FORESTEUROPE2020/panEuropean-forestHoldings_update.csv", header=FALSE)

colnames(soceco)[1] <- "country"

# extract info from 1st row
year <- stringr::str_sub(soceco[1,2:ncol(soceco)], -4, -1)
ownertype <- case_when(
  grepl(pattern="public", x=soceco[1,2:ncol(soceco)]) ~ "Public",
  grepl(pattern="private", x=soceco[1,2:ncol(soceco)]) ~ "Private",
  grepl(pattern="Other", x=soceco[1,2:ncol(soceco)]) ~ "Other")

colnames(soceco)[2:length(soceco)] <- paste(ownertype, year, sep="_")

#extract info from 2nd row
size <- sapply(strsplit(unlist(soceco[2,2:ncol(soceco)]), split=":", fixed=TRUE), "[", 1)
size <- ifelse(grepl("Total", x = size), "Total", size)
size <- gsub(pattern = "≤|≥| ", replacement = "", x = size)
table(size)

type_unit <- case_when(
  grepl(pattern="area|Area", x=unlist(soceco[2,2:ncol(soceco)])) ~ "Area",
  grepl(pattern="holdings", x=unlist(soceco[2,2:ncol(soceco)])) ~ "Number",
)

colnames(soceco)[2:length(soceco)]  <- paste(ownertype, type_unit, size, year, sep="_")

soceco <- soceco %>%
  filter(!row_number() %in% 1:2) %>%
  pivot_longer(names_to = c("ownertype", "type_unit", "size", "year"), names_sep = "_", cols=Public_Area_Total_2015:Other_Number_500ha_1990) %>%
  mutate(value = as.numeric(value),
         country = ifelse(country == "Czechia", "Czech Republic", country)) %>%
  filter(country %in%data_census_out$country) 

soceco %>%
  filter(type_unit == "Area" & size == "Total") %>%
  ggplot(aes(year, value ,group=ownertype, col = ownertype)) + geom_point() + geom_line() +
  facet_wrap(country~.)

soceco %>%
  filter(type_unit == "Number" & size == "10ha") %>%
  ggplot(aes(year, value,group=ownertype, col = ownertype)) + geom_point() + geom_line() +
  facet_wrap(country~.)

soceco %>%
  filter(type_unit == "Area" & size == "Total") %>%
  group_by(country, year) %>%
  mutate(total_ha = sum(value),
         share = value / total_ha) %>%
  ggplot(aes(year, share ,group=country, col = country, lty = country)) + geom_point() + geom_line() +
  facet_wrap(ownertype~.) 

# Info from Switzerland and Spain missing to make a variable out of this (for Area more countries missing)
soceco %>%
  filter(type_unit == "Number" & size == "10ha", ownertype == "Private") %>%
  group_by(country, ownertype) %>%
  summarise(anyValues = any(!is.na(value))) %>%
  as.data.frame()

# share of public ownership
public_share <- soceco %>%
  filter(type_unit == "Area" & size == "Total") %>%
  group_by(country, year) %>%
  mutate(total_ha = sum(value, na.rm=TRUE),
         share = value / total_ha) %>%
  filter(ownertype == "Public" & !is.na(share)) %>%
  arrange(country, desc(year) ) %>%
  group_by(country) %>%
  filter(row_number() == 1) %>%
  rename(owner_share_public = share) %>%
  select(country, owner_share_public)
  

data_census_out <- data_census_out %>%
  select(!owner_share_public) %>%
  left_join(public_share) 

data_annual_out <- data_annual_out %>%
  select(!owner_share_public) %>%
  left_join(public_share) 

# Write files -------------------------------------------------------------

saveRDS(data_annual_out, file = out_annual)
saveRDS(data_census_out, file = out_census)

# saveRDS(data_annual_weights, file = out_annual_weights)


