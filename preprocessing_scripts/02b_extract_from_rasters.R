#################################################################'
##
## Extract values to plot data from rasters
##
#################################################################'

library(tidyverse)
library(sf)
library(terra)
library(viridis)
library(ggcorrplot)

fl_date <-format(Sys.Date(), "%d-%m-%y")
load("./outputs/basemaps_europe.RData")

# Read data and define things --------------------------------------------------

# plots
census_stats_final <- readRDS("./data/processed/census_stats_final_15-12-22.rds")

nfi_plots <- census_stats_final %>% 
  dplyr::select(tmt.plot.id, latitude, longitude) %>%
  st_as_sf(coords=c("longitude", "latitude"), crs="+proj=longlat +datum=WGS84 +no_defs ", remove=FALSE)

# raster files
earthenv_fls <- c(roughness_1KMmd = "./data/raw/EarthEnv/roughness_1KMmd_GMTEDmd.tif",
                roughness_1KMma = "./data/raw/EarthEnv/roughness_1KMma_GMTEDmd.tif",
                elevation_1KMmd = "./data/raw/EarthEnv/elevation_1KMmd_GMTEDmd.tif",
                elevation_1KMma = "./data/raw/EarthEnv/elevation_1KMma_GMTEDma.tif",
                slope_1KMmd = "./data/raw/EarthEnv/slope_1KMmd_GMTEDmd.tif",
                slope_1KMma = "./data/raw/EarthEnv/slope_1KMma_GMTEDmd.tif",
                tri_1KMmd = "./data/raw/EarthEnv/tri_1KMmd_GMTEDmd.tif",
                tri_1KMma = "./data/raw/EarthEnv/tri_1KMma_GMTEDmd.tif")

popdens_fls <- c("./data/raw/Population_GHSL/GHS_POP_E2015_GLOBE_R2019A_54009_1K_V1_0/GHS_POP_E2015_GLOBE_R2019A_54009_1K_V1_0.tif")

# Extract -----------------------------------------------------------------

## Topography ----

ee_rast <- terra::rast(earthenv_fls)
ee_extract <- terra::extract(ee_rast, vect(nfi_plots))
rm(ee_rast)

## Population - aggregate first to 10x10 grid ----
rast_popdens <- rast(popdens_fls)

rast_popdens_10km <- terra::aggregate(rast_popdens,
                                      fact=10, fun=mean, na.rm=TRUE)
names(rast_popdens_10km) <- "POPDENS_2015_mean10km"

rast_popdens_10km_max <- terra::aggregate(rast_popdens,
                                      fact=10, fun=max, na.rm=TRUE)
names(rast_popdens_10km_max) <- "POPDENS_2015_max10km"

rast_popdens_50km <- terra::aggregate(rast_popdens,
                                      fact=50, fun=mean, na.rm=TRUE)
names(rast_popdens_50km) <- "POPDENS_2015_mean50km"

rast_popdens_50km_max <- terra::aggregate(rast_popdens,
                                          fact=50, fun=max, na.rm=TRUE)
names(rast_popdens_50km_max) <- "POPDENS_2015_max50km"

# terra::writeRaster(rast_popdens_10km, file ="./data/processed/gis_processing/pop_density_10km_mean.tif")
# terra::writeRaster(rast_popdens_10km_max, file ="./data/processed/gis_processing/pop_density_10km_max.tif")


nfi_plots_ESRI54009 <- vect(nfi_plots %>%
                              st_transform(crs = "ESRI:54009"))

pop_mean_extract <-  terra::extract(rast_popdens_10km,
                                    nfi_plots_ESRI54009)

pop_mean_extract_max <-  terra::extract(rast_popdens_10km_max,
                                        nfi_plots_ESRI54009)

pop_mean50_extract <-  terra::extract(rast_popdens_50km,
                                    nfi_plots_ESRI54009)

pop_mean50_extract_max <-  terra::extract(rast_popdens_50km_max,
                                        nfi_plots_ESRI54009)


## WorldClim ---------------------------------------------------------------
# BIO1 = Annual Mean Temperature
# BIO2 = Mean Diurnal Range (Mean of monthly (max temp - min temp))
# BIO3 = Isothermality (BIO2/BIO7) (×100)
# BIO4 = Temperature Seasonality (standard deviation ×100)
# BIO5 = Max Temperature of Warmest Month
# BIO6 = Min Temperature of Coldest Month
# BIO7 = Temperature Annual Range (BIO5-BIO6)
# BIO8 = Mean Temperature of Wettest Quarter
# BIO9 = Mean Temperature of Driest Quarter
# BIO10 = Mean Temperature of Warmest Quarter
# BIO11 = Mean Temperature of Coldest Quarter
# BIO12 = Annual Precipitation
# BIO13 = Precipitation of Wettest Month
# BIO14 = Precipitation of Driest Month
# BIO15 = Precipitation Seasonality (Coefficient of Variation)
# BIO16 = Precipitation of Wettest Quarter
# BIO17 = Precipitation of Driest Quarter
# BIO18 = Precipitation of Warmest Quarter
# BIO19 = Precipitation of Coldest Quarter

wclim_fls <- list.files("./data/raw/WorldClim/wc2.1_2.5m_bio/", full.names = TRUE)
wclim_rast <- terra::rast(wclim_fls)

wclim_extract <- terra::extract(wclim_rast, vect(nfi_plots))

# deal with NAs by taking the values from 5k buffer (10 plots)
idx_na <- !complete.cases(wclim_extract)
na_buffers <- st_buffer(nfi_plots[idx_na,], dist = 5e3)

wclim_extract_buffer <- terra::extract(wclim_rast, vect(na_buffers), 
                                       fun=mean, na.rm=TRUE)

wclim_extract_buffer$ID <- wclim_extract[idx_na,"ID"]

wclim_extract[idx_na,] <- wclim_extract_buffer


## CHELSA - gdd5 ------------------------------------------------------------

chelsa_fls <- "./data/raw/CHELSA/CHELSA_gdd5_1981-2010_V.2.1.tif"
rast_gdd5 <- rast(chelsa_fls)

#aggregate to 2.5 arc minutes (same as WorldClim) as the plot coordinates are not exact either
rast_gdd5_2.5min <- terra::aggregate(rast_gdd5, fact=5, fun=mean, na.rm=TRUE)
names(rast_gdd5_2.5min) <- "gdd5_2.5min_CHELSA"

gdd5_extract <- terra::extract(rast_gdd5_2.5min, vect(nfi_plots))



## Global accessibility ----------------------------------------------------

access_fls <- list.files("./data/raw/Global_accessibility/7638134/", 
                         pattern="*to_cities_[1-9]*.tif",
                         full.names = TRUE)
rast_access <- rast(access_fls)


# # aggregate to 2.5 arc minutes (same as WorldClim) as the plot coordinates are not exact either
# # not done, this weirdly results in a cor=1 between variables, not sure what happens here...
# rast_access_2.5min <- sapp(rast_access, aggregate, fact=5, median, na.rm=TRUE)  #terra::aggregate(rast_access, fact=c(5, 1), fun=mean, na.rm=TRUE)

access_extract <- terra::extract(rast_access, vect(nfi_plots)) %>% select(!ID)

# set outlier values of > 5e4 to max of non-outliers (119-127 obs, depending on layer)
for(i in 1:ncol(access_extract)) {
  idx_over50k <- access_extract[,i] > 5e4
  max_i <- max(access_extract[!idx_over50k,i])
  access_extract[idx_over50k,i] <- max_i
  print(i); print(sum(idx_over50k))
}

cor_mat <- cor(access_extract)
ggcorrplot(cor_mat)

ggplot(access_extract, aes(travel_time_to_cities_1, travel_time_to_cities_9)) +
  geom_point()

access_extract <- access_extract %>%
  rowwise() %>%
  mutate(access_1M = min(travel_time_to_cities_1, travel_time_to_cities_2),
         access_100k = min(access_1M,
                           travel_time_to_cities_3, travel_time_to_cities_4,
                           travel_time_to_cities_5),
         access_50k = min(access_100k, travel_time_to_cities_6),
         access_20k = min(access_50k, travel_time_to_cities_7))

ggcorrplot(cor(access_extract), type = "lower",  lab = TRUE)

# NPP Europe (Morena/Neumann) ---------------------------------------------
npp <- rast("./data/raw/npp_europe/npp.tif")

npp_extract <- terra::extract(npp, vect(nfi_plots))

plot(npp) 

# deal with NAs by taking the values from buffers with increasing size
buffer_r <- c(1e3, 5e3, 1e4, 5e4)
for(i in 1:length(buffer_r)) {
  buffer_i <- buffer_r[i]
  idx_na <- !complete.cases(npp_extract)
  print(paste("Total NAs:", sum(idx_na), "Buffer:", buffer_i, "m"))
  na_buffers <- st_buffer(nfi_plots[idx_na,], dist = buffer_i)
  
  npp_extract_buffer <- terra::extract(npp, vect(na_buffers), 
                                       fun=function(x) mean(x, na.rm=TRUE),
                                       cells=TRUE)
  
  # npp_extract_buffer$ID <- npp_extract[idx_na,"ID"]
  
  npp_extract[idx_na,"npp"] <- npp_extract_buffer$npp
}

summary(npp_extract) # NAs are plots in Canary Islands, the NPP raster doesn't extend there

# write buffers to file to check in qgis
idx_na <- !complete.cases(npp_extract)
na_buffers <- st_buffer(nfi_plots[idx_na,], dist = buffer_i)
write_sf(na_buffers, dsn="./outputs/npp_NA_buffers.gpkg")

# ggplot() + geom_raster(data=as.data.frame(npp, xy=TRUE), aes(x, y, fill=npp)) +
#   geom_sf(data = na_buffers, col=2, fill=NA)


# Protected areas ---------------------------------------------------------
sf::sf_use_s2(FALSE)

protected_polygons <- read_sf("./data/raw/ProtectedAreas/CDDA_2022_v01_public.gpkg",
                              layer="ProtectedSite")

protected_info <- read_sf("./data/raw/ProtectedAreas/CDDA_2022_v01_public.gpkg",
                          layer="DesignatedArea")

country_codes <- c(Finland = "FI", Sweden = "SE",
                   Poland = "PL", Germany = "DE",
                   France = "FR", Netherlands = "NL",
                   Belgium = "BE", Norway = "NO",
                   Spain = "ES", Switzerland = "CH",
                   Czechia = "CZ")

# iucn_levels <- c("Ia" , "Ib", "II", "III", "IV", "V", "notApplicable",
#                  "notAssigned", "notReported")

protected_join <- protected_polygons %>%
  filter(cddaCountryCode %in% country_codes) %>% 
  left_join(protected_info %>% select(cddaId, designatedAreaType, 
                                      designationTypeCode, iucnCategory, 
                                      siteArea, majorEcosystemType)) %>%
  filter(majorEcosystemType == "terrestrial") %>%
  st_transform(crs = "EPSG:3035")

natura2000_polygons <- read_sf("./data/raw/ProtectedAreas/Natura2000_end2018.gpkg",
                               layer = "NaturaSite_polygon")

natura2000_habitatclass <- read_sf("./data/raw/ProtectedAreas/Natura2000_end2018.gpkg",
                                   layer = "HABITATCLASS")

forest_habitatcodes <- c("N26", "N17", "N16", "N20", "N19", "N18")

natura2000_join <- natura2000_polygons %>%
  filter(MS %in% country_codes) %>%
  left_join(natura2000_habitatclass) %>%
  filter(HABITATCODE %in% forest_habitatcodes) %>%
  # st_transform(crs = "EPSG:4326") %>%
  select(SITECODE, geom) %>%
  rename(natura_sitecode = SITECODE)

protected_join_all <- protected_join  %>% 
  mutate(site_id = paste0("cdda_"), cddaId) %>%  select(site_id, geom) %>%
  rbind(natura2000_join %>% mutate(site_id = paste0("natura_", natura_sitecode)) %>% select(site_id, geom))

# 
plots_buffer500 <- nfi_plots %>%
  st_transform(crs = "EPSG:3035") %>%
  st_buffer(dist = 500)

# the extraction gets quite heavy, so splitting the task into smaller ones 
# to manage it better when computing locally, will anyway take quite long
plots_intersection_lst <- list()
idx <- seq(1, nrow(plots_buffer500)+1, length=101)
for(i in 1:100) {
  idx_min <- floor(idx[i])
  idx_max <- floor(idx[i + 1] -1)
  plots_buffer500_i <- plots_buffer500[idx_min:idx_max,]
  print(paste("row index", idx_min, "to", idx_max))
  plots_intersection_lst[[i]] <- plots_buffer500_i %>%
    st_intersection(protected_join_all)
}

plots_protected <- do.call(rbind, plots_intersection_lst)

plots_protected <- plots_protected %>% # this takes a loooong time (maybe 1-2h locally on my macbook)
  group_by(tmt.plot.id) %>%
  summarise(geometry = st_union(geometry))

dim(plots_protected)
plots_protected$area <- st_area(plots_protected$geometry)
plots_protected$protected_share <- as.numeric(plots_protected$area) / (pi*500^2)
plots_protected$ProtectedArea <- plots_protected$protected_share >= 0.5

st_write(plots_protected, dsn = "./data/processed/plot_protected_buffers_w_areaShares2.gpkg")

# Combine -----------------------------------------------------------------

##  combine ----
nfi_plots2 <- cbind(nfi_plots,
                    ee_extract %>% dplyr::select(!ID),
                    pop_mean_extract %>% dplyr::select(!ID),
                    pop_mean_extract_max %>% dplyr::select(!ID),
                    pop_mean50_extract %>% dplyr::select(!ID),
                    pop_mean50_extract_max %>% dplyr::select(!ID),
                    wclim_extract %>% dplyr::select(!ID),
                    gdd5_extract %>% dplyr::select(!ID),
                    access_extract, 
                    npp_extract %>% dplyr::select(!ID))

nfi_plots2 <- left_join(nfi_plots2, plots_protected %>% st_drop_geometry() %>% select(!any_of(c("area", "protected_share", "natura_sitecode", "natura_sitecode_exact"))))
nfi_plots2$ProtectedArea <- ifelse(is.na(nfi_plots2$ProtectedArea), FALSE, nfi_plots2$ProtectedArea)

summary(nfi_plots2$ProtectedArea)
colnames(nfi_plots2)

# check NPP NAs - only Canary islands
nfi_plots2 %>%
  arrange(is.na(npp)) %>%
  ggplot(aes(longitude, latitude, col=is.na(npp))) +
  geom_point()

#save with geometries
st_write(nfi_plots2, dsn = "./data/processed/plot_w_all_extracted_variables.gpkg")

# remove geometries
nfi_plots2 <- nfi_plots2 %>%
  as.data.frame() %>% select(!geometry)

## check correlations ----
data_cor <- nfi_plots2 %>% filter(!is.na(npp)) %>% select(!any_of(c("tmt.plot.id", "protected_share"))) %>% mutate(ProtectedArea = as.numeric(ProtectedArea))
data_cor <- sapply(data_cor, as.numeric)
cor_mat <- cor(data_cor)
p_mat <- cor_pmat(data_cor)

pdf(file="./outputs/figures/raster_extract_cormatrix.pdf",
    width=20, height=20)
ggcorrplot(cor_mat, type = "lower", p.mat = p_mat, lab = TRUE, hc.order=TRUE)
dev.off()

traveltime_cor <- cor(nfi_plots2[, grepl(pattern="travel_time|access", 
                       colnames(nfi_plots2))])

ggcorrplot(traveltime_cor, type = "lower",  lab = TRUE)

## write to file ----
saveRDS(nfi_plots2, 
        file=paste0("./data/processed/plots_w_extracted_values_", 
                    fl_date, ".rds") )

#checks
summary(nfi_plots2) # no NAs

# # the map code only work if geometry not removed!
# basemap +
#   geom_sf(data=nfi_plots2 %>%
#             slice_sample(n=1e4) %>%
#             arrange(travel_time_to_cities_9),
#           aes(col=travel_time_to_cities_9 > 6e4),
#           size=.5) +
#   scale_color_viridis(discrete = TRUE)
# 
# ggplot(data=nfi_plots2 %>%
#          slice_sample(n=1e4),
#        aes(travel_time_to_cities_1, travel_time_to_cities_12)) + geom_point()
