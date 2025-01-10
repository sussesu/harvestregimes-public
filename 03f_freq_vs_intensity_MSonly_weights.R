################################################################'
##
## Frequency vs Intensity of harvest events
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
library(plotly)

source("./src/read_tmt_data.R")

load("./outputs/basemaps_europe.RData")

min_n_plots <- 20 # how many obs needed to keep grid cell
min_n_harvest <- 0 # how many harvest obs needed to keep grid cell

thold_mm_sizediff <- 50

# Read and set stuff ------------------------------------------------------

# Read files
data_annual <- readRDS("./data/processed/data_annual_24-05-24_RFready.rds")
census_stats_final <- readRDS("./data/processed/census_stats_final_24-05-24_RFready.rds")

#colors
div_colors <- c("#d7191c",  "#fdae61",  "#ffffbf", "#abdda4","#2b83ba")

# Czechia
data_annual <- data_annual %>%
  mutate(country = ifelse(country == "Czech Republic", "Czechia", country))

census_stats_final <- census_stats_final %>%
  mutate(country = ifelse(country == "Czech Republic", "Czechia", country))

# order countries based on latitude
country_order <- census_stats_final %>%
  group_by(country) %>%
  summarize(mean_lat = mean(latitude),
            mean_lon = mean(longitude),
            max_lat = max(latitude),
            max_lon = max(longitude)) %>% 
  mutate(country = as.character(country)) %>%
  arrange(desc(max_lat)) %>%
  pull(country)

census_stats_final <- census_stats_final %>%
  mutate(country = factor(country, levels = country_order))

# check weights
# rep_area_weights
# forest_area_rep

summary(census_stats_final$rep_area_weights)
summary(census_stats_final$forest_area_rep)

# Process data to grid ----------------------------------------------------

# exclude data sets
exclude_db <- c("FUN.Fin", "NFL.Bel")

table(census_stats_final$database.code)
table(data_annual$database.code)

# Definitions

fl_date <-format(Sys.Date(), "%d-%m-%y")

# Themes and definitions
set_theme <- theme_bw() +
  theme(strip.background = element_blank(),
        strip.text = element_text(vjust=0.5))

col_harvest <- viridis(4, direction = -1)[1:3]

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


# calc again with country added as grouping factor
map_data_annual_db <- data_annual %>%
  mutate(lat = round(latitude),
         lon = round(longitude)) %>%
  group_by(lat, lon, country) %>%
  summarize(n_plots_annual = n(),
            n_obs = n_distinct(tmt.plot.id),
            harvest_pros_noWeight = sum(harvest_any)/n_plots_annual,
            harvest_pros = sum(harvest_any * forest_area_rep)/sum(forest_area_rep)
  ) %>%
  filter(n_obs >= min_n_plots)

map_data_org_db <- census_stats_final %>%
  mutate(lat = round(latitude),
         lon = round(longitude),
         harvest_percent_ba_partial = ifelse(harvest_percent_ba == 1, NA, harvest_percent_ba),
         harvest_percent_stems_partial = ifelse(harvest_percent_stems == 1, NA, harvest_percent_stems),
         year0 = floor(census.date0),
         year1 = floor(census.date),
         harvest_intensity_class = cut(harvest_percent_ba, 
                                       breaks=c(0, 0.25, 0.5, 0.75, 1),
                                       labels=c("0_25", "25_50", "50_75", "75_100")),
         harvest_size_target = case_when(
           (d_mean_nha.HARVEST - d_mean_nha.NONHARVEST) < -thold_mm_sizediff ~ "below",
           (d_mean_nha.HARVEST - d_mean_nha.NONHARVEST) > thold_mm_sizediff ~ "above",
           harvest_percent_ba == 1 ~ NA_character_,
           TRUE ~ "uniform"
         ),
         forest_area_weights = rep_area_weights) %>%
  group_by(lat, lon, country) %>%
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
                   ba0_mean_noWeight = mean(ba0_m2),
                   ba0_max_noWeight = max(ba0_m2),
                   ba0_q90 = quantile(ba0_m2, probs = 0.9),
                   ba_harvest_mean_noWeight = mean(ba_total.HARVEST, na.rm=TRUE),
                   d0_mean_noWeight = mean(d0_cm),
                   d_harvest_mean_noWeight = mean(d_mean_ht.HARVEST, na.rm=TRUE),
                   d_diff_mean_noWeight = mean(harvest_size_diff, na.rm=TRUE),
                   stems_perc_HARVEST_noWeight = sum(n_stems_ha.HARVEST/ census_interval, na.rm = TRUE) / sum(n_stems_ha0),
                   ba_perc_HARVEST_noWeight = sum(ba_total.HARVEST / census_interval, na.rm=TRUE) / sum(ba_total0),
                   harvest_above_p_noWeight = sum(harvest_size_target == "above", na.rm=TRUE)/sum(harvest_percent_ba < 1, na.rm=TRUE),
                   harvest_below_p_noWeight = sum(harvest_size_target == "below", na.rm=TRUE)/sum(harvest_percent_ba < 1, na.rm=TRUE),
                   
                   
                   harvest_percent_ba_var = ifelse(n_harvest > 1,
                                                   Hmisc::wtd.var(harvest_percent_ba * 100, weights = forest_area_weights, normwt=TRUE, na.rm=TRUE),
                                                   NA),
                   harvest_percent_ba_sd = sqrt(harvest_percent_ba_var),
                   harvest_percent_ba_SEM = harvest_percent_ba_var/n_harvest, # following https://stats.stackexchange.com/a/615418
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
                   ba0_q90 = Hmisc::wtd.quantile(ba0_m2, probs = 0.9, weights = forest_area_weights, normwt = TRUE),
                   ba_harvest_mean = Hmisc::wtd.mean(ba_total.HARVEST, na.rm=TRUE, weights = forest_area_weights),
                   d0_mean = Hmisc::wtd.mean(d0_cm, weights = forest_area_weights),
                   d_harvest_mean = Hmisc::wtd.mean(d_mean_ht.HARVEST, na.rm=TRUE, weights = forest_area_weights),
                   d_diff_mean = Hmisc::wtd.mean(harvest_size_diff, na.rm=TRUE, weights = forest_area_weights),
                   stems_perc_HARVEST = sum((n_stems_ha.HARVEST*forest_area_weights)/ census_interval, na.rm = TRUE) / sum(n_stems_ha0*forest_area_weights),
                   ba_perc_HARVEST = sum((ba_total.HARVEST*forest_area_weights) / census_interval, na.rm=TRUE) / sum(ba_total0*forest_area_weights),
                   harvest_above_p = sum( (harvest_size_target == "above") * forest_area_weights, na.rm=TRUE) / sum((harvest_percent_ba < 1) * forest_area_weights, na.rm=TRUE),
                   harvest_below_p = sum( (harvest_size_target == "below") * forest_area_weights, na.rm=TRUE) / sum((harvest_percent_ba < 1) * forest_area_weights, na.rm=TRUE),
                   NPP_mean = Hmisc::wtd.mean(npp, na.rm=TRUE, weights = forest_area_weights),
  ) %>%
  filter(n_plots >= min_n_plots)

## Set harvest variables to NA when less than min_n_harvest harvest observations
colnames_harvest <- c("harvest_percent_ba_mean", 
                      # "intensity_ba_partial",  "intensity_stems_partial", "ba_harvest_mean",
                      "d_harvest_mean", "d_diff_mean")

idx_few_harvest <- map_data_org_db$n_harvest < min_n_harvest
# colnames_harvest <- c("harvest_percent_ba_mean", "intensity_ba_partial",
#                       "intensity_stems_partial", "ba_harvest_mean",
#                       "d_harvest_mean", "d_diff_mean")
map_data_org_db[idx_few_harvest, colnames_harvest] <- NA

map_data_all <- map_data_annual_db %>%
  left_join(map_data_org_db)



# Fig. 3 Overall Intensity vs Freq-Int ----
# (the visual details like labels are futher edited in Inkscape)

col_option <- "mako" #"rocket" #"plasma" #"viridis" #"mako"
map_data_all <- map_data_all %>%
  mutate(country = factor(country, levels = country_order))

# all plots
freq_int_totalHarvBA <- map_data_all %>%
  arrange(ba_perc_HARVEST) %>%
  ggplot(aes(harvest_percent_ba_mean, harvest_pros*100, 
             col = ba_perc_HARVEST*100)) + 
  geom_point(size=3) + 
  scale_color_gradientn(colors = viridis_pal(direction=-1, option = col_option)(9), limits=c(0, 6.13)) +
  theme_bw() +
  coord_cartesian(ylim=c(0, 14), xlim=c(11, 100)) +
  ylab("Frequency of harvest events") +
  theme(legend.position = "none",
        axis.title.x = element_blank(),
        axis.title.y = element_text(hjust=0.5, size=16),
        plot.margin = margin(t = 5, r = 10, b = 10, l = 5, unit = "pt"),
        strip.background=element_rect(fill="white"),
        strip.text = element_text(colour="black"),
        panel.grid.minor = element_blank()) +
  labs(color="", shape="")  

# country-subplots on the right
freq_int_totalHarvBA_byCountry1 <- map_data_all %>%
  filter(country %in% c(country_order[c(1:6, 9:length(country_order))])) %>%
  ggplot(aes(harvest_percent_ba_mean, harvest_pros*100, 
             col = ba_perc_HARVEST*100)) + #, size=ba_perc_HARVEST*100)) +
  geom_point(data = map_data_all %>% 
               select(harvest_percent_ba_mean, harvest_pros),
             aes(harvest_percent_ba_mean, harvest_pros*100),
             inherit.aes=FALSE, col="grey85", size=1) +
  geom_point() +
  scale_color_gradientn(colors = viridis_pal(direction=-1, option = col_option)(9), limits=c(0, 6.13)) +
  theme_bw() +
  coord_cartesian(ylim=c(0, 14), xlim=c(11, 100)) +
  xlab("Intensity of harvest events") +
  scale_y_continuous(sec.axis = dup_axis())+  #create secondary axis
  theme(legend.position = "none",
        axis.title.y = element_blank(),
        axis.title.x = element_text(hjust=0, size=16),
        strip.background=element_rect(fill="white"),
        strip.text = element_text(colour="black"),
        panel.grid.minor = element_blank(),
        plot.margin = margin(t = 10, r = 10, b = 10, l = 0, unit = "pt"),
        strip.placement = "outside",
        axis.text.y.left = element_blank(), #remove axis label on left side
        axis.title.y.right = element_blank(), #remove axis title on right side
        axis.ticks.length.y.left = unit(0, units="cm")) + #remove axis ticks on left ) +
  facet_wrap(country ~ ., ncol= 3) 

# country-subplots on the left
freq_int_totalHarvBA_byCountry2 <- map_data_all %>%
  filter(country %in% c(country_order[7:8])) %>%
  ggplot(aes(harvest_percent_ba_mean, harvest_pros*100, 
             col = ba_perc_HARVEST*100)) + #, size=ba_perc_HARVEST*100)) +
  geom_point(data = map_data_all %>% 
               select(harvest_percent_ba_mean, harvest_pros),
             aes(harvest_percent_ba_mean, harvest_pros*100),
             inherit.aes=FALSE, col="grey85", size=1) +
  geom_point() +
  scale_color_gradientn(colors = viridis_pal(direction=-1, option = col_option)(9), limits=c(0, 6.13)) +
  theme_bw() +
  coord_cartesian(ylim=c(0, 14), xlim=c(11, 100)) +
  theme(legend.position = "none",
        axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        strip.background=element_rect(fill="white"),
        strip.text = element_text(colour="black"),
        panel.grid.minor = element_blank()) +
  facet_wrap(country ~ ., ncol= 5) 

layout_design <-  c(
  area(t = 1, l = 1, b = 2, r = 2),
  area(t = 1, l = 3, b = 3, r = 5),
  area(t = 3, l = 1, b = 3, r = 2)
)

freq_int_totalHarvBA + freq_int_totalHarvBA_byCountry1 + freq_int_totalHarvBA_byCountry2 +
  plot_layout(design = layout_design)
  # plot_layout(widths = c(1.5, 2.5))

# print to file - Freg vs Int + Overall Intensity
png(paste0("./outputs/figures/MS/freq_int_overallIntensity_coltest1_MS_", fl_date, ".png"),
    height=6, width=9.35, unit="in", res=300)

# pdf(paste0("./outputs/figures/MS/freq_int_overallIntensity2_MS_", fl_date, ".pdf"),
#     height=5.5*1.05, width=8.5*1.10)


freq_int_totalHarvBA + freq_int_totalHarvBA_byCountry1 + freq_int_totalHarvBA_byCountry2 +
  plot_layout(design = layout_design)
# freq_int_totalHarvBA + freq_int_totalHarvBA_byCountry +  
  # plot_layout(widths = c(1.2, 3))

dev.off()

# legend
# from https://stackoverflow.com/questions/12041042/how-to-plot-just-the-legends-in-ggplot2
gg_4legend <- map_data_all %>%
  ggplot(aes(harvest_percent_ba_mean, harvest_pros*100, 
             col = ba_perc_HARVEST*100)) + #, size=ba_perc_HARVEST*100)) +
  geom_point(data = map_data_all %>% 
               select(harvest_percent_ba_mean, harvest_pros),
             aes(harvest_percent_ba_mean, harvest_pros*100),
             inherit.aes=FALSE, col="grey85", size=1) +
  geom_point() +
  scale_color_gradientn(colors = viridis_pal(direction=-1, option = col_option)(9), limits=c(0, 6.13),
                        name="Total harvest rate\n(%BA harvested)") +
  theme(legend.position = "bottom")



legend <- cowplot::get_legend(gg_4legend)

layout_design2 <-  c(
  area(t = 1, l = 1, b = 2, r = 2),
  area(t = 1, l = 3, b = 3, r = 5),
  area(t = 3, l = 1, b = 3, r = 2),
  area(t=4.5, l=1, b=5,r=4)
)

pdf(paste0("./outputs/figures/MS/freq_int_overallIntensity2_MS_", fl_date, ".pdf"),
    height=6, width=9.35)
# png(paste0("./outputs/figures/MS/freq_int_overallIntensity_coltest1_MS_", fl_date, ".png"),
#     height=5.6*1.4, width=8.5*1.10, unit="in", res=300)
freq_int_totalHarvBA + freq_int_totalHarvBA_byCountry1 + freq_int_totalHarvBA_byCountry2 +
  plot_layout(design = layout_design)
dev.off()

pdf(paste0("./outputs/figures/MS/freq_int_overallIntensity2_MS_LEGEND_", fl_date, ".pdf"),
    height=6, width=9.35)
gg_4legend
dev.off()

# check ranges of overall intensity

map_data_all %>%
  mutate(intensity_range = ba_perc_HARVEST >= 0.01 & ba_perc_HARVEST <= 0.03) %>%
  ggplot(aes(harvest_percent_ba_mean, harvest_pros*100, 
             col = intensity_range)) +
  geom_point(size=3) + 
  scale_colour_viridis(direction = -1, discrete = TRUE, option="viridis", name="Pann") +
  theme_bw() +
  coord_cartesian(ylim=c(0, 14), xlim=c(11, 100)) +
  ylab("Frequency of harvest events") +
  xlab("Intensity of harvest events") +
  theme(legend.position = "none",
        strip.background=element_rect(fill="white"),
        strip.text = element_text(colour="black"),
        panel.grid.minor = element_blank()) 

# write results file out
map_data_out <- map_data_all %>%
  select(lat, lon, n_plots, n_plots_annual, n_harvest,
         country, n_plots, harvest_percent_ba_mean, 
         harvest_percent_ba_sd, harvest_percent_ba_SEM,
         harvest_pros, ba_perc_HARVEST,
         harvest_p_0_25, harvest_p_25_50, harvest_p_50_75, 
         harvest_p_75_100,
         harvest_above_p, harvest_below_p, ba0_q90)

write.csv(map_data_out, file = paste0("./data/processed/harvest_grids/harvest_grid_", fl_date, ".csv"))


# Compare weighted vs original values -------------------------------------

gg_weights_FREQ <- ggplot(map_data_all, aes(harvest_pros_noWeight, 
                                            harvest_pros,
                                            col = country)) +
  geom_point() + geom_abline(intercept = 0, slope = 1) +
  theme_classic() +
  xlab("Frequency, no weights")+
  ylab("Frequency, weighted") +
  coord_fixed()

gg_weights_INT <- ggplot(map_data_all, aes(harvest_percent_ba_mean_noWeight, 
                         harvest_percent_ba_mean,
                         col = country)) +
  geom_point() + geom_abline(intercept = 0, slope = 1) +
  theme_classic() +
  xlab("Intensity, no weights")+
  ylab("Intensity, weighted") +
  coord_fixed()



gg_weights_FREQ + gg_weights_INT +
  plot_layout(guides = "collect")

ggsave(paste0("./outputs/figures/MS/weights_comparison_", fl_date, ".pdf"),
       width = 7, height = 3.2)


# Fig. S10: NPP VS Total harvest rate  --------------------------------------

gg_NPPall <- map_data_all %>%
  ggplot(aes(NPP_mean, ba_perc_HARVEST*100)) +
  geom_point() +
  geom_smooth(method = "lm") +
  ylab("Total harvest rate") + xlab("NPP") +
  theme_bw() +
  ggtitle("A.")

gg_NPPall

gg_NPPcountries <- map_data_all %>%
  mutate(country = factor(country, levels = country_order)) %>%
  ggplot(aes(NPP_mean, ba_perc_HARVEST*100)) +
  geom_point(size=0.6) +
  geom_smooth(method = "lm") +
  facet_wrap(country ~ ., ncol = 4) +
  ylab("Total harvest rate") + xlab("NPP") +
  theme_bw() +
  ggtitle("B.")

gg_NPPall +
  gg_NPPcountries +
  plot_layout(widths = c(0.4, 0.6))

ggsave("./outputs/figures/MS/S3_NPP_vs_harvestRate.png",
       width = 9, height = 4)

