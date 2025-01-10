################################################################'
##
## Testing the effect of assigned harvest year (mid-interval vs. random)
## Presented in Supplement figures S1 and S2.
##
################################################################'

library(tidyverse)
library(sf)
library(viridis)
library(patchwork)

load("./outputs/basemaps_europe.RData")

# Read files ----

data_annual <- readRDS("./data/processed/data_annual_24-05-24_RFready.rds")
census_stats_final <- readRDS("./data/processed/census_stats_final_24-05-24_RFready.rds")

# census_stats_final_full <- census_stats_final
# 
# census_stats_final <- census_stats_final %>%
#   filter(country == "Finland")

# data_annual_midyear <- data_annual %>%
#   select(tmt.plot.id, census_interval, country, harvest_any, ba0_m2, ba_total0, ba_total.NONHARVEST, ann.id,
#          n_stems_ha0, n_stems_ha.NONHARVEST)

# Harvest mid-interval ----------------------------------------------------

find_harvest_year <- function(x) {
  # find harvest year when harvest in the middle of the census interval
  y_out <- ifelse(x %% 2 == 0, 
                  ifelse(sample(c(TRUE, FALSE)),  # even integers, randomize if going up or down from middle
                         x/2, x/2 + 1),
                  ceiling(x/2) )
  return(y_out)
}

data_annual_midyear2 <- census_stats_final %>%
  rowwise() %>%
  mutate(interval_years = as.integer(round(census_interval)),
         mid_interval = find_harvest_year(interval_years)) %>%
  uncount(weights = interval_years, .id="ann.id", .remove=FALSE) %>%
  mutate(status = case_when(
    !harvest_any ~ "pre-harvest",
    harvest_any & ann.id < mid_interval ~ "pre-harvest",
    harvest_any & ann.id == mid_interval ~ "harvest",
    harvest_any & ann.id > mid_interval ~ "post-harvest"))   %>%  
  # For post-harvest rows, use variables calculated without the harvested trees
  mutate(harvest_any = ifelse(status == "harvest",
                              harvest_any,
                              FALSE),
         ba0_m2 = ifelse(status != "post-harvest",
                         ba_total0,
                         ifelse(is.na(ba_total.NONHARVEST), 0, ba_total.NONHARVEST)),
         n_stems_ha0 = ifelse(status != "post-harvest", n_stems_ha0, 
                              ifelse(is.na(n_stems_ha.NONHARVEST), 0, n_stems_ha.NONHARVEST)) )

idx_include <- which(data_annual_midyear2$n_stems_ha0 > 0)
data_annual_midyear2 <- data_annual_midyear2[idx_include,]

# BA vs harvest for mid-year data -----------------------------------------

data_annual_midyear <- data_annual_midyear2

# calculate esimate for data where harvest set to mid-year
glm_midYear <-  glm(harvest_any ~ ba0_m2, data = data_annual_midyear, family = "binomial")

ann_midYear <- data_annual_midyear %>%
  group_by(country) %>%
  nest() %>%
  mutate(glm = map(data, ~glm(harvest_any ~ ba0_m2, data = .x, family = "binomial")),
         intercept = map_dbl(glm, ~ summary(.x)$coefficients["(Intercept)","Estimate"]),
         estimate = map_dbl(glm, ~ summary(.x)$coefficients["ba0_m2","Estimate"]),
         stError = map_dbl(glm, ~ summary(.x)$coefficients["ba0_m2","Std. Error"]),
         mean_censusint = map_dbl(data, ~ mean(.x$census_interval)),
         n = map_dbl(data, ~nrow(.x)),
         i = NA) %>%
  select(country, intercept, estimate, stError, mean_censusint, n, i) %>%
  ungroup() %>%
  rbind(data.frame(country = "All",
                   intercept= summary(glm_midYear)$coefficients["(Intercept)","Estimate"],
                   estimate = summary(glm_midYear)$coefficients["ba0_m2","Estimate"],
                   stError= summary(glm_midYear)$coefficients["ba0_m2","Std. Error"],
                   mean_censusint = mean(data_annual_midyear$census_interval),
                   n = nrow(data_annual_midyear),
                   i = NA))




# Assign harvest year randomly ----

# Generate one row for each year
# If harvest_any = TRUE, assign harvest randomly

annualise_random <- function(census_stats_final) {
  data_annual_random <- census_stats_final %>%
    select(tmt.plot.id, census_interval, country, latitude, longitude,
           harvest_any, ba_total0, ba_total.HARVEST,  ba_total.NONHARVEST, 
           n_stems_ha0, n_stems_ha.NONHARVEST, forest_area_rep) %>%
    rowwise() %>%
    mutate(interval_years = as.integer(round(census_interval)),
           harvest_year = sample(1:interval_years, 1, replace = TRUE)) %>%
    uncount(weights = interval_years, .id="ann.id", .remove=FALSE) %>%
    mutate(status = case_when(
      !harvest_any ~ "pre-harvest",
      harvest_any & ann.id < harvest_year ~ "pre-harvest",
      harvest_any & ann.id == harvest_year ~ "harvest",
      harvest_any & ann.id > harvest_year ~ "post-harvest")) %>%  
    # For post-harvest rows, use variables calculated without the harvested trees
    mutate(harvest_any = ifelse(status == "harvest",
                                harvest_any,
                                FALSE),
           ba0_m2 = ifelse(status != "post-harvest",
                           ba_total0,
                           ifelse(is.na(ba_total.NONHARVEST), 0, ba_total.NONHARVEST)),
           n_stems_ha0 = ifelse(status != "post-harvest", n_stems_ha0, 
                                ifelse(is.na(n_stems_ha.NONHARVEST), 0, n_stems_ha.NONHARVEST))
    ) # %>%
    # filter(n_stems_ha0 > 0)
  
  idx_include <- which(data_annual_random$n_stems_ha0 > 0)
  data_annual_random <- data_annual_random[idx_include,]
  
  return(data_annual_random)
}


set.seed(97)
ann_list <- list()
ann_data_list = list()
for(i in 1:100) {
  print(paste("start: ", i))
  ann_i <- annualise_random(census_stats_final)
  glm_all_i <- glm(harvest_any ~ ba0_m2, data = ann_i, family = "binomial")
  glm_country_i <- ann_i %>%
    group_by(country) %>%
    nest() %>%
    mutate(glm = map(data, ~glm(harvest_any ~ ba0_m2, data = .x, family = "binomial")),
           intercept = map_dbl(glm, ~ summary(.x)$coefficients["(Intercept)","Estimate"]),
           estimate = map_dbl(glm, ~ summary(.x)$coefficients["ba0_m2","Estimate"]),
           stError = map_dbl(glm, ~ summary(.x)$coefficients["ba0_m2","Std. Error"]),
           mean_censusint = map_dbl(data, ~ mean(.x$census_interval)),
           n = map_dbl(data, ~nrow(.x)),
           i = i) %>%
    select(country, intercept, estimate, stError, mean_censusint, n, i) %>%
    ungroup() %>%
    rbind(data.frame(country = "All",
                     intercept = summary(glm_all_i)$coefficients["(Intercept)","Estimate"],
                     estimate = summary(glm_all_i)$coefficients["ba0_m2","Estimate"],
                     stError= summary(glm_all_i)$coefficients["ba0_m2","Std. Error"],
                     mean_censusint = mean(ann_i$census_interval),
                     n = nrow(ann_i),
                     i = i))
  
  ann_list[[i]] <- glm_country_i
  ann_data_list[[i]] <- ann_i
}

ann_df <- do.call(rbind, ann_list)

write.csv(ann_df, file = "./outputs/random_harvest_year_BAglm_test100.csv",
          row.names = FALSE)

# BA vs. harvest probability ----

ggplot(ann_df,
       aes(country, estimate)) + 
         geom_point(pch = 1, col = "orange") +
  geom_point(data = ann_midYear, col = 1, size = 3, pch = 4) +
  ylab("Coefficient estimate") + xlab("Country") +
  theme_bw() +
  theme(axis.text = element_text(angle = 45, hjust = 1))
ggsave("./outputs/figures/MS/RandomHarvestYear_test_BAeffect_100.png",
       width = 6, height = 3.5)

# ## "Effects" figures - this is slow and memory consuming with a lot of iterations!!
# 
# # make predictions and compare
# 
# inv.logit <- function(x) {
#   exp(x)/(1+exp(x)) }
# 
# ba_newdata <- data.frame(ba0_m2 = seq(quantile(census_stats_final$ba0_m2, probs = 0.05), 
#                   quantile(census_stats_final$ba0_m2, probs = 0.95),
#                   length = 20))
# 
# pred_df <- ann_df %>%
#   cross_join(ba_newdata) %>%
#   dplyr::mutate(pred = intercept + estimate * ba0_m2,
#                 pred_response = inv.logit(pred))
# 
# 
# pred_midYear <- ann_midYear %>%
#   cross_join(ba_newdata) %>%
#   mutate(pred = intercept + estimate * ba0_m2,
#          pred_response = inv.logit(pred),
#          i = 999)
# 
# 
# # slope figures
# # --> the differences are so small, that they are not visible in this figure,
# #     better to just include the previous fig about the slope value diff
# 
# 
# ggplot(pred_df, #%>% filter(country == "All"), 
#        aes(ba0_m2, pred_response, col = i)) + 
#   geom_line() +
#   geom_line(data = pred_midYear, #  %>% filter(country == "All"),
#             col = 1, lty = 2) +
#   facet_wrap(country ~ .) 


# Harvest frequency grid ?? -----------------------------------------------

gridded_harvest_freq <- function(data_annual, thold_nplot = 20) {
  map_data_annual <- data_annual %>%
    mutate(lat = round(latitude),
           lon = round(longitude)) %>%
    group_by(lat, lon) %>%
    summarize(n_plots_annual = n(),
              n_obs = n_distinct(tmt.plot.id),
              mean_censu_interval = Hmisc::wtd.mean(census_interval, weights = forest_area_rep),
              harvest_pros = sum(harvest_any * forest_area_rep)/sum(forest_area_rep) * 100
    ) %>%
    filter(n_obs >= thold_nplot)
  
  return(map_data_annual)
}

grid_midYear <- gridded_harvest_freq(data_annual_midyear2) %>%
  rename(harvest_pros_midYear = harvest_pros)


set.seed(97)
lst_grid_random <- list()
for(i in 1:100) {
  print(paste("start: ", i))
  ann_i <- annualise_random(census_stats_final)
  lst_grid_random[[i]] <- gridded_harvest_freq(ann_i) 
  }

grid_random <- do.call(rbind, lst_grid_random)

write.csv(grid_random, file = "./outputs/randomHarvYear_test_grid_100.csv",
          row.names = FALSE)

# grid_random <- read.csv("./outputs/randomHarvYear_test_grid_100.csv")

grid_random_summary <- grid_random %>%
  group_by(lat, lon) %>%
  summarise(freq_mean = mean(harvest_pros),
            freq_stdev = sd(harvest_pros))

grid_summary <- grid_random_summary %>%
  left_join(grid_midYear) %>%
  mutate(freq_diff = harvest_pros_midYear - freq_mean)
  

ggplot(grid_summary, aes(lon, lat, col = freq_diff)) +
  geom_point() +
  scale_color_viridis_c()

ggplot(grid_summary, aes(lon, lat, col = freq_stdev)) +
  geom_point() +
  scale_color_viridis_c(direction = -1)





### Put on map ----

proj_lambert_conic <- "+proj=lcc +lat_1=43 +lat_2=62 +lat_0=30 +lon_0=10 +x_0=0 +y_0=0 +ellps=intl +units=m no_defs"


map_data_wgs <- st_as_sf(grid_summary, coords=c("lon", "lat"),
                         crs="EPSG:4326", remove=FALSE)

map_data_sf <-  st_as_sf(grid_summary, coords=c("lon", "lat"),
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

### Draw maps ----
set_theme <- theme_bw() +
  theme(strip.background = element_blank(),
        strip.text = element_text(vjust=0.5))

map_theme <- theme(legend.position = "bottom",
                   strip.text = element_text(vjust=0.5),
                   panel.border = element_rect(fill=NA, colour="gray50"),
                   axis.title = element_blank())

gg_diff <- basemap_proj + geom_sf(data=grid_harvest %>% filter(!is.na(freq_mean)), 
                       aes(fill=freq_diff), 
                       alpha=.7, lwd = 0, na.rm=TRUE) +
  scale_fill_distiller(type = "div", limit = c(-0.025, 0.025), palette = "RdYlBu") +
  # scale_fill_viridis(direction=-1, na.value = "white", name = "") +
  # guides(fill=guide_legend("%", nrow = 2)) +
  coord_sf(ylim = c(map_extent$min_y, map_extent$max_y),
           xlim = c(map_extent$min_x, map_extent$max_x)) +
  # annotate(geom = 'text', label = 'B. Frequency', x = -Inf, y = Inf, hjust = 0, vjust = 1, size=6) +
  map_theme +
  theme(legend.position = "right") +
  ggtitle("A. Difference")

gg_sd <- basemap_proj + geom_sf(data=grid_harvest %>% filter(!is.na(freq_stdev)), 
                                   aes(fill=freq_stdev), 
                                   alpha=.7, lwd = 0, na.rm=TRUE) +
  scale_fill_distiller(palette = "YlOrBr", direction = 1) +
  # scale_fill_viridis(direction=-1, na.value = "white", name = "") +
  # guides(fill=guide_legend("%", nrow = 2)) +
  coord_sf(ylim = c(map_extent$min_y, map_extent$max_y),
           xlim = c(map_extent$min_x, map_extent$max_x)) +
  # annotate(geom = 'text', label = 'B. Frequency', x = -Inf, y = Inf, hjust = 0, vjust = 1, size=6) +
  map_theme +
  theme(legend.position = "right") +
  ggtitle("B. St. deviation")

gg_diff + gg_sd


### Put figures together
gg_interval_diff <- ggplot(grid_summary, aes(mean_censu_interval, freq_diff)) +
  geom_point() +
  geom_smooth() +
  ylab("Difference") + xlab("Mean census interval") +
  ggtitle("C.")
gg_interval_sd <- ggplot(grid_summary, aes(mean_censu_interval, freq_stdev)) +
  geom_point() +
  geom_smooth() +
  ylab("St. deviation") + xlab("Mean census interval") +
  ggtitle("D.")
  
gg_n_diff <- ggplot(grid_summary, aes(n_obs , freq_diff)) +
  geom_point() +
  geom_smooth()
gg_n_sd <- ggplot(grid_summary, aes(n_obs , freq_stdev)) +
  # scale_x_continuous(trans = "log10") +
  geom_point() +
  geom_smooth()


gg_diff + gg_sd +
gg_interval_diff + gg_interval_sd +
  plot_layout(ncol = 2) &
  theme_bw()
ggsave("./outputs/figures/MS/RandomHarvestYear_test_grid_100.png", 
       width = 7, height = 5)
