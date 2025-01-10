################################################################'
##
## Mapping harvest, figures to MS
## Update 5/2024: include weights based on represented forest area per plot
##
################################################################'

# rm(list=ls())

##########################################'
# run if working in Puhti!
.libPaths(c("/projappl/project_2008416/project_rpackages_4.3/", .libPaths())) 
libpath <- .libPaths()[1]
##########################################'

library(tidyverse)
library(sf)
library(viridis)
library(patchwork)
library(ggcorrplot)

load("./outputs/basemaps_europe.RData")

## Set things ----

# Read files
data_annual <- readRDS("./data/processed/data_annual_24-05-24_RFready.rds")
census_stats_final <- readRDS("./data/processed/census_stats_final_24-05-24_RFready.rds")

# exclude data sets
exclude_db <- c("FUN.Fin", "NFL.Bel", "NPO.Pol", "NPO2.Pol")

table(census_stats_final$database.code)
table(data_annual$database.code)

dim(census_stats_final)
dim(data_annual)

# check weights
# rep_area_weights
# forest_area_rep

summary(census_stats_final$rep_area_weights)
summary(census_stats_final$forest_area_rep)

# Filter thresholds - how many plots/harvest events required per grid cell

thold_nplot <- 20
thold_harvest <- 5

# Definitions

fl_date <-format(Sys.Date(), "%d-%m-%y")


# Themes and map definitions
set_theme <- theme_bw() +
  theme(strip.background = element_blank(),
        strip.text = element_text(vjust=0.5))

map_theme <- theme(legend.position = "bottom",
        strip.text = element_text(vjust=0.5),
        panel.border = element_rect(fill=NA, colour="gray50"),
        axis.title = element_blank())

basemap_proj <- basemap_proj +
  # coord_sf(ylim = c(map_extent$min_y, map_extent$max_y),
  #          xlim = c(map_extent$min_x, map_extent$max_x)) +
  map_theme

map_guide <- guide_colourbar(
  direction = "horizontal",
  title.position = "top",
  label.hjust = 0,
  label.position = "bottom",
  label.theme = element_text(angle = 0, size=8),
  barwidth = 10, barheight = 0.5)

# projection def
proj_lambert_conic <- "+proj=lcc +lat_1=43 +lat_2=62 +lat_0=30 +lon_0=10 +x_0=0 +y_0=0 +ellps=intl +units=m no_defs"

# exclude databases
data_annual <- data_annual %>%
  filter(!db_country%in%exclude_db)

census_stats_final <- census_stats_final %>%
  filter(!db_country%in%exclude_db)

# check
summary(census_stats_final$db_country)
summary(data_annual$db_country)

# Redefine species classes
sp_class_levels <- c("PlantationSp", "Picea", "Pinus", "FagusQuercus", "OtherConifer",
                     "OtherBroadleaved")

data_annual <- data_annual %>%
  mutate(conifer_broadleaved = ifelse(conifer, "Conifer", "Broadleaved"),
         species_classes = factor(species_classes, levels = sp_class_levels))

census_stats_final <- census_stats_final %>%
  mutate(conifer_broadleaved = ifelse(conifer, "Conifer", "Broadleaved"),
         species_classes = factor(species_classes, levels = sp_class_levels))


col_harvest <- viridis(4, direction = -1)[1:3]


### Lat/Lon + database points ----
# edit: added weights based on represented forest area
map_data_annual <- data_annual %>%
  mutate(lat = round(latitude),
         lon = round(longitude),
         harvest_intensity_class = cut(harvest_percent_ba, 
                                       breaks=c(0, 0.25, 0.5, 0.75, 1),
                                       labels=c("0_25", "25_50", "50_75", "75_100"))) %>%
  group_by(lat, lon) %>%
  summarize(n_plots_annual = n(),
            n_obs = n_distinct(tmt.plot.id),
            harvest_pros_noWeight = sum(harvest_any)/n_plots_annual,
            harvest_pros = sum(harvest_any * forest_area_rep)/sum(forest_area_rep)
  ) %>%
  filter(n_obs >= thold_nplot)

map_data_org <- census_stats_final %>%
  mutate(lat = round(latitude),
         lon = round(longitude),
         harvest_percent_ba_partial = ifelse(harvest_percent_ba == 1, NA, harvest_percent_ba),
         harvest_percent_stems_partial = ifelse(harvest_percent_stems == 1, NA, harvest_percent_stems),
         harvest_intensity_class = cut(harvest_percent_ba, 
                                       breaks=c(0, 0.25, 0.5, 0.75, 1),
                                       labels=c("0_25", "25_50", "50_75", "75_100")),
         forest_area_weights = rep_area_weights) %>%
  group_by(lat, lon) %>%
  dplyr::summarize(n_plots = n(),
            n_harvest = sum(harvest_any),
            n_countries = n_distinct(country),
            
            harvest_percent_ba_sd_noWeight = sd(harvest_percent_ba * 100, na.rm=TRUE),
            harvest_percent_ba_SEM_noWeight = harvest_percent_ba_sd_noWeight/sqrt(n_plots),
            harvest_percent_ba_mean_noWeight = mean(harvest_percent_ba * 100, na.rm=TRUE),
            harvest_p_0_25_noWeight = sum(harvest_intensity_class == "0_25", na.rm=TRUE)/n_harvest,
            harvest_p_25_50_noWeight = sum(harvest_intensity_class == "25_50", na.rm=TRUE)/n_harvest,
            harvest_p_50_75_noWeight = sum(harvest_intensity_class == "50_75", na.rm=TRUE)/n_harvest,
            harvest_p_75_100_noWeight = sum(harvest_intensity_class == "75_100", na.rm=TRUE)/n_harvest,
            harvest_p_100_noWeight = sum(harvest_percent_ba == 1, na.rm=TRUE)/n_harvest,
            ba0_mean = mean(ba0_m2),
            ba0_max = max(ba0_m2),
            ba_harvest_mean = mean(ba_total.HARVEST, na.rm=TRUE),
            d0_mean = mean(d0_cm),
            d_harvest_mean = mean(d_mean_ht.HARVEST, na.rm=TRUE),
            d_diff_mean = mean(harvest_size_diff, na.rm=TRUE),
            stems_perc_HARVEST = sum(n_stems_ha.HARVEST/ census_interval, na.rm = TRUE) / sum(n_stems_ha0),
            ba_perc_HARVEST = sum(ba_total.HARVEST / census_interval, na.rm=TRUE) / sum(ba_total0),
            
            
            harvest_percent_ba_var = ifelse(n_harvest > 1,
                                            Hmisc::wtd.var(harvest_percent_ba * 100, weights = forest_area_weights, normwt=TRUE, na.rm=TRUE),
                                            NA),
            harvest_percent_ba_sd = sqrt(harvest_percent_ba_var),
            harvest_percent_ba_SEM = harvest_percent_ba_sd/n_harvest, # following https://stats.stackexchange.com/a/615418
            harvest_percent_ba_mean = Hmisc::wtd.mean(harvest_percent_ba * 100, weights = forest_area_weights, na.rm=TRUE),
            intensity_ba_partial = Hmisc::wtd.mean(harvest_percent_ba_partial * 100, weights = forest_area_weights, na.rm=TRUE),
            intensity_stems_partial = Hmisc::wtd.mean(harvest_percent_stems_partial, weights = forest_area_weights, na.rm=TRUE),
            
            harvest_p_0_25 = sum((harvest_intensity_class == "0_25") *forest_area_weights, na.rm=TRUE)/sum(harvest_any * forest_area_weights),
            harvest_p_25_50 = sum((harvest_intensity_class == "25_50") *forest_area_weights, na.rm=TRUE)/sum(harvest_any * forest_area_weights),
            harvest_p_50_75 = sum((harvest_intensity_class == "50_75") *forest_area_weights, na.rm=TRUE)/sum(harvest_any * forest_area_weights),
            harvest_p_75_100 = sum((harvest_intensity_class == "75_100") *forest_area_weights, na.rm=TRUE)/sum(harvest_any * forest_area_weights),
            harvest_p_100 = sum((harvest_percent_ba == 1) *forest_area_weights, na.rm=TRUE)/sum(harvest_any * forest_area_weights),
            ba0_mean = Hmisc::wtd.mean(ba0_m2, weights = forest_area_weights),
            ba0_max = max(ba0_m2),
            ba_harvest_mean = Hmisc::wtd.mean(ba_total.HARVEST, na.rm=TRUE, weights = forest_area_weights),
            d0_mean = Hmisc::wtd.mean(d0_cm, weights = forest_area_weights),
            d_harvest_mean = Hmisc::wtd.mean(d_mean_ht.HARVEST, na.rm=TRUE, weights = forest_area_weights),
            d_diff_mean = Hmisc::wtd.mean(harvest_size_diff, na.rm=TRUE, weights = forest_area_weights),
            stems_perc_HARVEST = sum((n_stems_ha.HARVEST*forest_area_weights)/ census_interval, na.rm = TRUE) / sum(n_stems_ha0*forest_area_weights),
            ba_perc_HARVEST = sum((ba_total.HARVEST*forest_area_weights) / census_interval, na.rm=TRUE) / sum(ba_total0*forest_area_weights)
            ) %>%
  filter(n_plots >= thold_nplot)

idx_few_harvest <- map_data_org$n_harvest < thold_harvest
colnames_harvest <- c("harvest_percent_ba", "intensity_ba_partial", 
                      "intensity_stems_partial", "ba_harvest_mean",
                      "d_harvest_mean", "d_diff_mean")
map_data_org[idx_few_harvest, colnames_harvest] <- NA

map_data_org

map_data_merge <- map_data_annual %>%
  left_join(map_data_org)

#### Check effects from weights ----

# check sd NAs - these are ones where sum(weights) < 1 ??
map_data_merge %>%
  filter(is.na(harvest_percent_ba_sd)) %>%
  select(lat, lon, n_obs, n_harvest)

census_stats_final %>%
  filter(round(latitude) == 55 & round(longitude) == 14 & harvest_any) %>% 
  select(tmt.plot.id, harvest_any, harvest_percent_ba, rep_area_weights)

# check weighted vs. non-weighted values
ggplot(map_data_merge, aes(harvest_pros_noWeight, harvest_pros,
                           col = n_countries == 1)) +
  geom_point() +
  geom_abline(slope = 1, intercept = 0)

ggplot(map_data_merge, aes(harvest_percent_ba_mean_noWeight, harvest_percent_ba_mean, 
                           col = n_countries == 1)) +
  geom_point() +
  geom_abline(slope = 1, intercept = 0)

ggplot(map_data_merge, aes(harvest_percent_ba_SEM_noWeight, harvest_percent_ba_SEM, 
                           col = n_countries == 1)) +
  geom_point() +
  geom_abline(slope = 1, intercept = 0)

# harvest intensity classes
ggplot(map_data_merge, aes(harvest_p_0_25_noWeight, harvest_p_0_25 ,
                           col = n_countries == 1)) +
  geom_point() +
  geom_abline(slope = 1, intercept = 0)

ggplot(map_data_merge, aes(harvest_p_25_50_noWeight, harvest_p_25_50,
                           col = n_countries == 1)) +
  geom_point() +
  geom_abline(slope = 1, intercept = 0)

ggplot(map_data_merge, aes(harvest_p_50_75_noWeight, harvest_p_50_75,
                           col = n_countries == 1)) +
  geom_point() +
  geom_abline(slope = 1, intercept = 0)

ggplot(map_data_merge, aes(harvest_p_75_100_noWeight, harvest_p_75_100,
                           col = n_countries == 1)) +
  geom_point() +
  geom_abline(slope = 1, intercept = 0)

# ggplot(map_data_merge, aes(n_harvest, sum_harvest_weights, 
#                            col = n_countries == 1)) +
#   geom_point() +
#   geom_abline(slope = 1, intercept = 0)


### Map - projected ----

map_data_wgs <- st_as_sf(map_data_merge, coords=c("lon", "lat"),
         crs="EPSG:4326", remove=FALSE) 

map_data_sf <-  st_as_sf(map_data_merge, coords=c("lon", "lat"),
                         crs="EPSG:4326", remove=FALSE) %>%
  st_transform(proj_lambert_conic)

map_extent <- as.data.frame(st_coordinates(map_data_sf %>% filter(lat > 30))) %>%
  summarize(min_x = min(X) - 5e4,
            max_x = max(X) + 1e4,
            min_y = min(Y) - 2e4,
            max_y = max(Y) + 2e4)

### Create grid ----
cellsize <- 1
grid_annual <- st_make_grid(
  st_bbox(map_data_wgs) + 
    c(-cellsize/2, -cellsize/2,
      cellsize/2, cellsize/2),
  what="polygons", cellsize=cellsize)

grid_harvest <- data.frame(id = 1:length(grid_annual)) %>%
  mutate(geometry = grid_annual) %>%
  st_as_sf(crs="EPSG:4326") %>%
  st_transform(proj_lambert_conic) %>%
  mutate(cell_area = st_area(geometry)) %>%
  st_join(map_data_sf)
  
idx_few_harvest <- !is.na(grid_harvest$n_harvest) & grid_harvest$n_harvest < thold_harvest
colnames_harvest <- c("harvest_percent_ba", "intensity_ba_partial",
                      "intensity_stems_partial", "ba_harvest_mean",
                      "d_harvest_mean", "d_diff_mean")
grid_harvest[idx_few_harvest, colnames_harvest] <- NA

###############################################'
##
## Correlations ----
##
###############################################'


colnames(grid_harvest)

cor_data <- grid_harvest %>% 
  select(ba_perc_HARVEST, 
         harvest_pros, 
         harvest_percent_ba_mean) %>%
  st_drop_geometry() %>%
  na.omit()

cor_mat_harv <- cor(cor_data, method = "pearson", use = "complete.obs")

p_mat_harv <- cor_pmat(cor_data, method = "pearson", use = "complete.obs")

ggcorrplot(cor_mat_harv, p.mat = p_mat_harv, type = "lower", lab=TRUE, insig = "blank")


# scatter plots
cor1 <- cor.test(cor_data$ba_perc_HARVEST*100, cor_data$harvest_pros*100)
cor_gg1 <- ggplot(cor_data, aes(ba_perc_HARVEST*100, harvest_pros*100)) + 
  geom_point() +
  geom_smooth(method="lm", formula = 'y ~ x', se=TRUE) +
  ggtitle(paste0("r = ", round(cor1$estimate, 2), " (p", 
                ifelse(round(cor1$p.value, 3) == 0, " < 0.001", 
                       paste("=", round(cor1$p.value, 3))), ")")) +
  ylab("Frequency of harvest events") +
  xlab("Total harvest rate") +
  theme_bw() 

cor2 <- cor.test(cor_data$ba_perc_HARVEST*100, cor_data$harvest_percent_ba_mean)
cor_gg2 <- ggplot(cor_data, aes(ba_perc_HARVEST*100, harvest_percent_ba_mean)) + 
  geom_point() +
  geom_smooth(method="lm", formula = 'y ~ x', se=TRUE) +
  ggtitle(paste0("r = ", round(cor2$estimate, 2), " (p", 
                 ifelse(round(cor2$p.value, 3) == 0, " < 0.001", 
                        paste(" =", round(cor2$p.value, 3))), ")")) +
  ylab("Intensity of harvest events") +
  xlab("Total harvest rate") +
  theme_bw()

cor3 <- cor.test(cor_data$harvest_pros*100, cor_data$harvest_percent_ba_mean)
cor_gg3 <- ggplot(cor_data, aes(harvest_pros*100, harvest_percent_ba_mean)) + 
  geom_point() +
  geom_smooth(method="lm", formula = 'y ~ x', se=TRUE) +
  ggtitle(paste0("r = ", round(cor3$estimate, 2), " (p", 
                 ifelse(round(cor3$p.value, 3) == 0, " < 0.001", 
                        paste("=", round(cor3$p.value, 3))), ")")) +
  ylab("Intensity of harvest events") +
  xlab("Frequency of harvest events") +
  theme_bw()

# cor_gg1 + cor_gg2 + cor_gg3

###############################################'
##
## Draw maps ----
## Grid maps, 1 degree
##
###############################################'

## map  details
map_guide2 <- map_guide
map_guide2$title <- "% plots harvested"

div_colors <- c("#d7191c",  "#fdae61",  "#ffffbf", "#abdda4","#2b83ba")

### n obs ----

grid_harvest$n_obs_bins = cut(grid_harvest$n_obs, 
                                   breaks=c(20, 50, 100, 200, 500, 1000, 10000))

gg_obs <- basemap_proj + geom_sf(data=grid_harvest %>% filter(!is.na(harvest_pros)), 
                                 aes(fill=n_obs_bins), 
                                 alpha=.7, lwd = 0, na.rm=TRUE) +
  scale_fill_viridis(direction=-1, discrete=TRUE, guide =  guide_coloursteps(
    title="number of plots",
    direction = "horizontal",
    title.position = "left",
    # label.hjust = 1,
    label.position = "bottom",
    label.theme = element_text(angle = 0, size=8),
    barwidth = 10, barheight = 0.5),
    option="inferno") +
  coord_sf(ylim = c(map_extent$min_y, map_extent$max_y),
           xlim = c(map_extent$min_x, map_extent$max_x)) +
  map_theme +
  ggtitle("Number of plots per cell")

# n harvest obs 

grid_harvest$n_harv_obs_bins = cut(grid_harvest$n_harvest, 
                              breaks=c(0, 5, 10, 50, 100, 500, 5000))

gg_harv_obs <- basemap_proj + geom_sf(data=grid_harvest %>% filter(!is.na(harvest_pros)), 
                                 aes(fill=n_harv_obs_bins), 
                                 alpha=.7, lwd = 0, na.rm=TRUE) +
  scale_fill_viridis(direction=-1, discrete=TRUE, guide =  guide_coloursteps(
    title="number of plots",
    direction = "horizontal",
    title.position = "left",
    # label.hjust = 1,
    label.position = "bottom",
    label.theme = element_text(angle = 0, size=8),
    barwidth = 10, barheight = 0.5),
    option="inferno") +
  coord_sf(ylim = c(map_extent$min_y, map_extent$max_y),
           xlim = c(map_extent$min_x, map_extent$max_x)) +
  map_theme +
  ggtitle("Number of plots with harvest")

# gg_obs + gg_harv_obs

### Harvest ----

# Total %BA harvested in grid cell

map_guide_totalBA <- map_guide
map_guide_totalBA$title <- "%BA harvested"

gg_harvest_totalBA <- basemap_proj + geom_sf(data=grid_harvest %>% filter(!is.na(harvest_pros)), 
                                             aes(fill=ba_perc_HARVEST*100), 
                                             alpha=.7, lwd = 0, na.rm=TRUE) +
  scale_fill_viridis(direction=-1, na.value = "white", 
                     option= "mako", #"viridis", 
                     guide = map_guide_totalBA) +
  coord_sf(ylim = c(map_extent$min_y, map_extent$max_y),
           xlim = c(map_extent$min_x, map_extent$max_x)) +
  # annotate(geom = 'text', label = 'A. Total intensity', x = -Inf, y = Inf, hjust = 0, vjust = 1, size=6) +
  map_theme +
  ggtitle("A. Total harvest rate")

# % harvested plots (All)
map_guide_FREQ <- map_guide
map_guide_FREQ$title <- "% of plots harvested per year"

gg_harvest_FREQ <- basemap_proj + geom_sf(data=grid_harvest %>% filter(!is.na(harvest_pros)), 
                                     aes(fill=harvest_pros*100), 
                                     alpha=.7, lwd = 0, na.rm=TRUE) +
  scale_fill_viridis(direction=-1, na.value = "white", guide = map_guide_FREQ,
                     option="magma") +
  coord_sf(ylim = c(map_extent$min_y, map_extent$max_y),
           xlim = c(map_extent$min_x, map_extent$max_x)) +
  # annotate(geom = 'text', label = 'B. Frequency', x = -Inf, y = Inf, hjust = 0, vjust = 1, size=6) +
  map_theme +
  theme(axis.text.y = element_blank()) +
  ggtitle("B. Frequency of harvest events")


gg_harvest_FREQ2 <- basemap_proj + geom_sf(data=grid_harvest %>% filter(!is.na(harvest_pros)), 
                                          aes(fill=harvest_pros*100), 
                                          alpha=.7, lwd = 0, na.rm=TRUE) +
  scale_fill_viridis(direction=-1, na.value = "white", guide = map_guide_FREQ,
                     option="mako") +
  coord_sf(ylim = c(map_extent$min_y, map_extent$max_y),
           xlim = c(map_extent$min_x, map_extent$max_x)) +
  # annotate(geom = 'text', label = 'B. Frequency', x = -Inf, y = Inf, hjust = 0, vjust = 1, size=6) +
  map_theme +
  # theme(axis.text.y = element_blank()) +
  ggtitle("B. Frequency of harvest events")


# Harvest intensity (%BA harvested) - ALL PLOTS (incl. ALL_CUT)
map_guide_INT <- map_guide
map_guide_INT$title <- "Average %BA removed in plot"

gg_harvest_INT <- basemap_proj + geom_sf(data=grid_harvest %>% filter(!is.na(harvest_pros)), 
                                      aes(fill=harvest_percent_ba_mean), 
                                      alpha=.7, lwd = 0, na.rm=TRUE) +
  scale_fill_viridis(direction=-1, na.value = "white", guide = map_guide_INT,
                     option="viridis") +
  coord_sf(ylim = c(map_extent$min_y, map_extent$max_y),
           xlim = c(map_extent$min_x, map_extent$max_x)) +
  # annotate(geom = 'text', label = 'C. Intensity', x = -Inf, y = Inf, hjust = 0, vjust = 1, size=6) +
  map_theme +
  theme(axis.text.y = element_blank()) +
  ggtitle("C. Mean intensity of harvest events")

gg_harvest_INT2 <- basemap_proj + geom_sf(data=grid_harvest %>% filter(!is.na(harvest_pros)), 
                                         aes(fill=harvest_percent_ba_mean), 
                                         alpha=.7, lwd = 0, na.rm=TRUE) +
  scale_fill_viridis(direction=-1, na.value = "white", guide = map_guide_INT,
                     option="mako") +
  coord_sf(ylim = c(map_extent$min_y, map_extent$max_y),
           xlim = c(map_extent$min_x, map_extent$max_x)) +
  # annotate(geom = 'text', label = 'C. Intensity', x = -Inf, y = Inf, hjust = 0, vjust = 1, size=6) +
  map_theme +
  theme(axis.text.y = element_blank()) +
  ggtitle("C. Intensity of harvest events")


gg_harvest_totalBA + gg_harvest_FREQ2 + gg_harvest_INT


### SEM of mean intensity 

gg_IntSEM <- basemap_proj + geom_sf(data=grid_harvest %>% filter(!is.na(harvest_pros)),
                                      aes(fill=harvest_percent_ba_SEM),
                                      alpha=.7, lwd = 0, na.rm=TRUE) +
  scale_fill_viridis(direction=-1, discrete=FALSE, guide =  guide_coloursteps(
    title="SEM",
    direction = "horizontal",
    title.position = "left",
    # label.hjust = 1,
    label.position = "bottom",
    label.theme = element_text(angle = 0, size=8),
    barwidth = 10, barheight = 0.5),
    option="inferno") +
  coord_sf(ylim = c(map_extent$min_y, map_extent$max_y),
           xlim = c(map_extent$min_x, map_extent$max_x)) +
  map_theme +
  ggtitle("St. error of the mean harvest intensity")

gg_IntSEM

### St.dev of mean intensity 

gg_IntStDev <- basemap_proj + geom_sf(data=grid_harvest %>% filter(!is.na(harvest_pros)),
                                    aes(fill=harvest_percent_ba_sd),
                                    alpha=.7, lwd = 0, na.rm=TRUE) +
  scale_fill_viridis(direction=-1, discrete=FALSE, guide =  guide_coloursteps(
    title="St.dev.",
    direction = "horizontal",
    title.position = "left",
    # label.hjust = 1,
    label.position = "bottom",
    label.theme = element_text(angle = 0, size=8),
    barwidth = 10, barheight = 0.5),
    option="inferno") +
  coord_sf(ylim = c(map_extent$min_y, map_extent$max_y),
           xlim = c(map_extent$min_x, map_extent$max_x)) +
  map_theme +
  ggtitle("St. deviation of harvest intensity")

gg_IntStDev

### Freq by intensity classes -----

map_guide_IntClasses <- map_guide
map_guide_IntClasses$title <- "% of harvest events in intensity class"
map_guide_IntClasses$direction <- "vertical"
map_guide_IntClasses$label.position <- "right"
map_guide_IntClasses$barwidth = map_guide_IntClasses$barheight
map_guide_IntClasses$barheight = map_guide_IntClasses$barwidth

col_lims <- c(0, 100)
col_option <- "mako" #"magma"

gg_harvest_0_25 <- basemap_proj + geom_sf(data=grid_harvest %>% filter(!is.na(harvest_pros)), 
                                          aes(fill=harvest_p_0_25*100), 
                                          alpha=.7, lwd = 0, na.rm=TRUE) +
  scale_fill_viridis(direction=-1, na.value = "white", #guide = map_guide_IntClasses,
                     limits=col_lims, option=col_option) +
  guides(fill=guide_legend("% harvest events")) +
  coord_sf(ylim = c(map_extent$min_y, map_extent$max_y),
           xlim = c(map_extent$min_x, map_extent$max_x)) +
  # annotate(geom = 'text', label = 'B. Frequency', x = -Inf, y = Inf, hjust = 0, vjust = 1, size=6) +
  map_theme +
  theme(legend.position = "right") +
  ggtitle("A. < 25% harvested")

gg_harvest_25_50 <- basemap_proj + geom_sf(data=grid_harvest %>% filter(!is.na(harvest_pros)), 
                                          aes(fill=harvest_p_25_50*100), 
                                          alpha=.7, lwd = 0, na.rm=TRUE) +
  scale_fill_viridis(direction=-1, na.value = "white", #guide = map_guide_IntClasses,
                     limits=col_lims, option=col_option) +
  coord_sf(ylim = c(map_extent$min_y, map_extent$max_y),
           xlim = c(map_extent$min_x, map_extent$max_x)) +
  # annotate(geom = 'text', label = 'B. Frequency', x = -Inf, y = Inf, hjust = 0, vjust = 1, size=6) +
  map_theme +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        legend.position = "none") +
  ggtitle("B. 25 - 50% harvested")

gg_harvest_50_75 <- basemap_proj + geom_sf(data=grid_harvest %>% filter(!is.na(harvest_pros)), 
                                           aes(fill=harvest_p_50_75*100), 
                                           alpha=.7, lwd = 0, na.rm=TRUE) +
  scale_fill_viridis(direction=-1, na.value = "white", #guide = map_guide_IntClasses,
                     limits=col_lims, option=col_option) +
  coord_sf(ylim = c(map_extent$min_y, map_extent$max_y),
           xlim = c(map_extent$min_x, map_extent$max_x)) +
  # annotate(geom = 'text', label = 'B. Frequency', x = -Inf, y = Inf, hjust = 0, vjust = 1, size=6) +
  map_theme +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        legend.position = "none") +
  ggtitle("C. 50 - 75% harvested")

gg_harvest_75_100 <- basemap_proj + geom_sf(data=grid_harvest %>% filter(!is.na(harvest_pros)), 
                                           aes(fill=harvest_p_75_100*100), 
                                           alpha=.7, lwd = 0, na.rm=TRUE) +
  scale_fill_viridis(direction=-1, na.value = "white",# guide = map_guide_IntClasses,
                     limits=col_lims, option=col_option) +
  coord_sf(ylim = c(map_extent$min_y, map_extent$max_y),
           xlim = c(map_extent$min_x, map_extent$max_x)) +
  # annotate(geom = 'text', label = 'B. Frequency', x = -Inf, y = Inf, hjust = 0, vjust = 1, size=6) +
  map_theme +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        legend.position = "none") +
  ggtitle("D. > 75% harvested")

gg_harvest_100 <- basemap_proj + geom_sf(data=grid_harvest %>% filter(!is.na(harvest_pros)), 
                                            aes(fill=harvest_p_100*100), 
                                            alpha=.7, lwd = 0, na.rm=TRUE) +
  scale_fill_viridis(direction=-1, na.value = "white", #guide = map_guide_IntClasses,
                     limits=col_lims, option=col_option) +
  coord_sf(ylim = c(map_extent$min_y, map_extent$max_y),
           xlim = c(map_extent$min_x, map_extent$max_x)) +
  # annotate(geom = 'text', label = 'B. Frequency', x = -Inf, y = Inf, hjust = 0, vjust = 1, size=6) +
  map_theme +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        legend.position = "right") +
  ggtitle("E. 100%")


# Write to files ----

## Fig. 2 ---- 
# print to file - harvest maps
png(paste0("./outputs/figures/MS/harvest_maps_MS_", fl_date, ".png"),
    height=5.5, width=10.5, unit="in", res=300)

gg_harvest_totalBA +
  gg_harvest_FREQ2 + 
  gg_harvest_INT2

dev.off()

# print to file - harvest maps, 2 maps
png(paste0("./outputs/figures/MS/harvest_event2_maps_MS_", fl_date, ".png"),
    height=5.5, width=8, unit="in", res=300)

  gg_harvest_FREQ2 + 
  gg_harvest_INT2

dev.off()

## Fig. 4----
# maps by intensity
png(paste0("./outputs/figures/MS/harvest_maps_byINT2_MS_", fl_date, ".png"),
    height=4, width=10.5, unit="in", res=300)

gg_harvest_0_25 + gg_harvest_25_50 + gg_harvest_50_75 + gg_harvest_75_100 +
  plot_layout(ncol = 4, guides = "collect") 

dev.off()

# print to file - number of observations (to supplement)
png(paste0("./outputs/figures/MS/harvest_maps_nObs_", fl_date, ".png"),
    height=5, width=10.5, unit="in", res=300)

gg_obs + gg_harv_obs

dev.off()

## Fig. S3 (supplement) ----
# n obs + St.dev
png(paste0("./outputs/figures/MS/harvest_maps_nObs_StDev_", fl_date, ".png"),
    height=5, width=10.5, unit="in", res=300)

gg_obs + 
  (gg_harv_obs + theme(axis.text.y = element_blank())) +
  (gg_IntStDev + theme(axis.text.y = element_blank()))

dev.off()

# correlation plots
png(paste0("./outputs/figures/MS/harvest_grid_correlations_", fl_date, ".png"),
    height=3, width=8.5, unit="in", res=300)
cor_gg1 + cor_gg2 + cor_gg3
dev.off()

# write grid to file
grid_out_wgs84 <- data.frame(id = 1:length(grid_annual)) %>%
  mutate(geometry = grid_annual) %>%
  st_as_sf(crs="EPSG:4326") 

grid_out_proj <- grid_harvest %>%
  mutate(id = row_number()) %>%
  select(id, lat, lon, geometry) 

ggplot(grid_out_wgs84) + geom_sf()
ggplot(grid_out_proj) + geom_sf()

st_write(grid_out_wgs84, paste0("./data/processed/harvest_grids/grid_1degree_wgs84_weights_", fl_date, ".gpkg") )
st_write(grid_out_proj, paste0("./data/processed/harvest_grids/grid_1degree_projected_weights_", fl_date, ".gpkg") )
