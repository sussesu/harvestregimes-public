################################################################'
##
## Frequency vs Intensity of harvest events
##
################################################################'

rm(list=ls())

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

# Read and set stuff ------------------------------------------------------

# Read files
data_annual <- readRDS("./data/processed/data_annual_22-08-23_RFready.rds")
census_stats_final <- readRDS("./data/processed/census_stats_final_22-08-23_RFready.rds")

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
            harvest_pros = sum(harvest_any)/n_plots_annual #,
            # partial_pros = sum(harvest_3class == "PARTIAL_CUT")/n_plots_annual,
            # allcut_pros = sum(harvest_3class == "ALL_CUT")/n_plots_annual
  ) %>%
  filter(n_obs >= min_n_plots)

map_data_org_db <- census_stats_final %>%
  mutate(lat = round(latitude),
         lon = round(longitude),
         # harvest_percent_ba_partial = ifelse(harvest_percent_ba == 1, NA, harvest_percent_ba),
         # harvest_percent_stems_partial = ifelse(harvest_percent_stems == 1, NA, harvest_percent_stems),
         year0 = floor(census.date0),
         year1 = floor(census.date),
         harvest_intensity_class = cut(harvest_percent_ba, 
                                       breaks=c(0, 0.25, 0.5, 0.75, 1),
                                       labels=c("0_25", "25_50", "50_75", "75_100"))) %>%
  group_by(lat, lon, country) %>%
  summarize(n_plots = n(),
            n_harvest = sum(harvest_any),
            # harvest_pros_allcut =  sum(harvest_3class == "ALL_CUT") / sum(harvest_any),
            harvest_percent_ba_av = mean(harvest_percent_ba * 100, na.rm=TRUE),
            harvest_percent_ba_sd = sd(harvest_percent_ba * 100, na.rm=TRUE),
            harvest_percent_ba_SEM = harvest_percent_ba_sd/sqrt(n_plots),
            # intensity_ba_partial = mean(harvest_percent_ba_partial * 100, na.rm=TRUE),
            # intensity_stems_partial = mean(harvest_percent_stems_partial, na.rm=TRUE),
            ba0_mean = mean(ba0_m2),
            ba0_max = max(ba0_m2),
            # ba_harvest_mean = mean(ba_total.HARVEST, na.rm=TRUE),
            d0_mean = mean(d0_cm),
            d_harvest_mean = mean(d_mean_ht.HARVEST, na.rm=TRUE),
            d_diff_mean = mean(harvest_size_diff, na.rm=TRUE),
            sp_conifer_pros = (sum(conifer)/n())*100,
            sp_exotic_pros = (sum(non_native == "exotic")/n())*100,
            sp_outsiderange_pros = (sum(non_native == "outside-range")/n())*100,
            # stems_perc_HARVEST = sum(n_stems_ha.HARVEST/ census_interval, na.rm = TRUE) / sum(n_stems_ha0),
            ba_perc_HARVEST = sum(ba_total.HARVEST / census_interval, na.rm=TRUE) / sum(ba_total0),
            # sum_stems_annual_ALLCUT = sum(ifelse(harvest_3class == "ALL_CUT", n_stems_ha.HARVEST / census_interval, 0)),
            # sum_stems_annual_PARTIALCUT = sum(ifelse(harvest_3class == "PARTIAL_CUT", n_stems_ha.HARVEST / census_interval, 0)),
            # stems_perc_ALLCUT = sum_stems_annual_ALLCUT / sum(n_stems_ha0),
            # stems_perc_PARTIALCUT = sum_stems_annual_PARTIALCUT / sum(n_stems_ha0),
            # sum_ba_annual_ALLCUT = sum(ifelse(harvest_3class == "ALL_CUT", ba_total.HARVEST / census_interval, 0)),
            # sum_ba_annual_PARTIALCUT = sum(ifelse(harvest_3class == "PARTIAL_CUT", ba_total.HARVEST / census_interval, 0)),
            # ba_perc_ALLCUT = sum_ba_annual_ALLCUT / sum(ba_total0),
            # ba_perc_PARTIALCUT = sum_ba_annual_PARTIALCUT / sum(ba_total0),
            year0 = as.numeric(names(sort(table(year0), decreasing = TRUE))[1]),
            year1 = as.numeric(names(sort(table(year1), decreasing = TRUE))[1]),
            harvest_p_0_25 = sum(harvest_intensity_class == "0_25", na.rm=TRUE)/n_harvest,
            harvest_p_25_50 = sum(harvest_intensity_class == "25_50", na.rm=TRUE)/n_harvest,
            harvest_p_50_75 = sum(harvest_intensity_class == "50_75", na.rm=TRUE)/n_harvest,
            harvest_p_75_100 = sum(harvest_intensity_class == "75_100", na.rm=TRUE)/n_harvest,
            harvest_p_100 = sum(harvest_percent_ba == 1, na.rm=TRUE)/n_harvest) %>%
  filter(n_plots >= min_n_plots)

## Set harvest variables to NA when less than min_n_harvest harvest observations
colnames_harvest <- c("harvest_percent_ba_av", 
                      # "intensity_ba_partial",  "intensity_stems_partial", "ba_harvest_mean",
                      "d_harvest_mean", "d_diff_mean")

idx_few_harvest <- map_data_org_db$n_harvest < min_n_harvest
# colnames_harvest <- c("harvest_percent_ba_av", "intensity_ba_partial",
#                       "intensity_stems_partial", "ba_harvest_mean",
#                       "d_harvest_mean", "d_diff_mean")
map_data_org_db[idx_few_harvest, colnames_harvest] <- NA

map_data_all <- map_data_annual_db %>%
  left_join(map_data_org_db)



# Overall Intensity vs Freq-Int ----

col_option <- "mako" #"rocket" #"plasma" #"viridis" #"mako"
map_data_all <- map_data_all %>%
  mutate(country = factor(country, levels = country_order))

# all plots
freq_int_totalHarvBA <- map_data_all %>%
  arrange(ba_perc_HARVEST) %>%
  ggplot(aes(harvest_percent_ba_av, harvest_pros*100, 
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
  ggplot(aes(harvest_percent_ba_av, harvest_pros*100, 
             col = ba_perc_HARVEST*100)) + #, size=ba_perc_HARVEST*100)) +
  geom_point(data = map_data_all %>% 
               select(harvest_percent_ba_av, harvest_pros),
             aes(harvest_percent_ba_av, harvest_pros*100),
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
  ggplot(aes(harvest_percent_ba_av, harvest_pros*100, 
             col = ba_perc_HARVEST*100)) + #, size=ba_perc_HARVEST*100)) +
  geom_point(data = map_data_all %>% 
               select(harvest_percent_ba_av, harvest_pros),
             aes(harvest_percent_ba_av, harvest_pros*100),
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
  ggplot(aes(harvest_percent_ba_av, harvest_pros*100, 
             col = ba_perc_HARVEST*100)) + #, size=ba_perc_HARVEST*100)) +
  geom_point(data = map_data_all %>% 
               select(harvest_percent_ba_av, harvest_pros),
             aes(harvest_percent_ba_av, harvest_pros*100),
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
  ggplot(aes(harvest_percent_ba_av, harvest_pros*100, 
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
         country, n_plots, harvest_percent_ba_av, 
         harvest_percent_ba_sd, harvest_percent_ba_SEM,
         harvest_pros, ba_perc_HARVEST,
         harvest_p_0_25, harvest_p_25_50, harvest_p_50_75, 
         harvest_p_75_100)

write.csv(map_data_out, file = paste0("./data/processed/harvest_grids/harvest_grid_", fl_date, ".csv"))
