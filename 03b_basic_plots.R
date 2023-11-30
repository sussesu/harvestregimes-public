###################################################'
##
## Explore and plot predictors vs responses
##
###################################################'

rm(list=ls())
closeAllConnections()

library(tidyverse)
library(patchwork)
library(ggcorrplot)
library(sf)
library(viridis)

out_pdf <- TRUE

fl_date <- format(Sys.Date(), "%d-%m-%y") #"19-04-22" #
pdf_name <- paste0("./outputs/basic_plots_", fl_date, ".pdf")
  
if(out_pdf) { pdf(file=pdf_name, width=10, height=10) }

data_annual <- readRDS(paste0("./data/processed/data_annual_22-08-23_RFready.rds"))
census_stats_final<- readRDS(paste0("./data/processed/census_stats_final_22-08-23_RFready.rds"))

dim(data_annual)
dim(census_stats_final)

setdiff(colnames(data_annual), colnames(census_stats_final))
setdiff(colnames(census_stats_final), colnames(data_annual))

# remove NPP, originating from stand_dynamics table, to not get mixed with npp from Neumann map
data_annual <- data_annual %>% select(!any_of("NPP"))
census_stats_final <- census_stats_final %>% select(!any_of("NPP"))

# predictors

vars_pred <- c("Dq_mean0",
               "ba0_m2",
               "gini_d_nplot0",
               
               "ba_percent_dom0", 
               # "non_native",
               "species_classes2",
               "dist_barkbeetle_windthrow",
               "dist_fire",
               
               "access_1M",
               "access_50k",
               
               "POPDENS_2015_mean10km",
               "elevation_1KMmd_GMTEDmd",
               "roughness_1KMmd_GMTEDmd",

               "npp",

               "owner_share_public",
               "country_or_region"
)


# Neat names for variables ------------------------------------------------

neat_names <- as.data.frame(rbind(c("Dq_mean0", "DBH", "Forest"),
                                  c("ba0_m2", "BA", "Forest"),
                                  c("gini_d_nplot0", "Gini", "Forest"),
                                  c("ba_percent_dom0", "BA_DomSp", "Forest"),
                                  c("non_native", "Sp_native", "Forest"),
                                  c("species_classes2", "Sp_class", "Forest"),
                                  c("dist_barkbeetle_windthrow", "StormBeetle", "Disturbance"),
                                  c("dist_fire", "Fire", "Disturbance"),
                                  c("elevation_1KMmd_GMTEDmd", "Elevation", "Environment"),
                                  c("roughness_1KMmd_GMTEDmd", "Roughness", "Environment"),
                                  c("wc2.1_2.5m_bio_1", "Temperature", "Environment"),
                                  c("wc2.1_2.5m_bio_12", "Precipitation", "Environment"),
                                  c("POPDENS_2015_mean10km", "PopDens", "Human"),
                                  c("travel_time_to_cities_2", "TravelTime1", "Human"),
                                  c("travel_time_to_cities_4", "TravelTime2", "Human"),
                                  c("travel_time_to_cities_9", "TravelTime3", "Human"),
                                  c("country_or_region", "CountryRegion", "Human"),
                                  c("db_country", "Country", "Human"),
                                  c("owner_share_public", "PublicOwnership", "Human"),
                                  c("npp", "NPP", "Environment"),
                                  c("access_1M", "Access1M", "Human"),
                                  c("access_50k", "Access50k", "Human")))
colnames(neat_names) <- c("variable", "new.name", "type")

new_names <- neat_names$new.name
names(new_names) <- neat_names$variable

new_colnames <- recode(colnames(data_annual), !!!new_names)

any(duplicated(new_colnames))

colnames(data_annual) <- new_colnames

# census level
new_colnames2 <- recode(colnames(census_stats_final), !!!new_names)
any(duplicated(new_colnames2))

colnames(census_stats_final) <- new_colnames2

# new names for the predictor variable vector
vars_pred <- neat_names %>% 
  filter(variable %in% vars_pred) %>%
  mutate(variable = factor(variable, levels = vars_pred)) %>%
  arrange(variable) %>%
  pull(new.name)

# Checks and definitions --------------------------------------------------
# 
# # combine with raster extracted variables
# extract_covs <- readRDS("./data/processed/plots_w_extracted_values_19-04-22.rds")
# 
# data_annual <- data_annual %>%
#   left_join(extract_covs)
# 
# census_stats_final <- census_stats_final %>%
#   left_join(extract_covs)

# Overview per db_country -------------------------------------------------
# check that values seem ok across countries

if(out_pdf) {plot.new(); text(x=.5, y=.5, "Overview per country", cex=3)}

plot_vars <- c("harvest01", "harvest_percent_ba", vars_pred) #c("harvest01",  "harvest_3class", "harvest_percent_ba", "harvest_size_diff",
           # vars_pred[2:length(vars_pred)])



summary(census_stats_final[,plot_vars])

gglst <- list()
for(y_var in plot_vars){
  x_var <- "Country"
  if(is.numeric(census_stats_final[[y_var]])) {
    print(y_var)
    gglst[[y_var]] <- ggplot(census_stats_final, aes_string(x_var, y_var)) +
      geom_boxplot() +
      ggtitle(y_var) +
      theme(legend.position="none",
            axis.text.x = element_text(angle=45, hjust=1),
            axis.title.x = element_blank(),
            axis.title.y = element_blank()) #+
  } else {
    print(y_var)
    gglst[[y_var]] <- ggplot(census_stats_final, aes_string(x_var, fill=y_var)) + 
      geom_bar(position="fill") +
      ggtitle(y_var) +
      theme(legend.position="none",
            axis.text.x = element_text(angle=45, hjust=1),
            axis.title.x = element_blank(),
            axis.title.y = element_blank())#,
    }
  }
print(wrap_plots(gglst[1:12], ncol=4, nrow=3))
print(wrap_plots(gglst[13:length(gglst)], ncol=4, nrow=3))

# Harvest classification ----------------------------------

if(out_pdf) {plot.new(); text(x=.5, y=.5, "Harvest classification ", cex=3)}

# data: check, select & filter

var_res <- "harvest01"
vars <- c(var_res, vars_pred)

all(vars%in%colnames(data_annual))
summary(data_annual[, vars]) 
dim(data_annual)

data_annual <- data_annual %>%
  dplyr::select(all_of(vars)) %>%
  na.omit()

#response var, exclude NAs
summary(data_annual[,1])

# correlation matrix
p_mat <- data_annual %>%
  ungroup() %>%
  mutate(harvest01 = ifelse(harvest01 == "HARVEST", 1, 0)) %>%
  rename(Harvest01 = harvest01) %>%
  select_if(is.numeric) %>%
  cor_pmat()

gg_corall <- data_annual %>%
  ungroup() %>%  
  mutate(harvest01 = ifelse(harvest01 == "HARVEST", 1, 0)) %>%
  rename(Harvest01 = harvest01) %>%
  select_if(is.numeric) %>%
  cor(use="complete.obs", method="spearman") %>%
  ggcorrplot(type = "lower",
             show.legend = TRUE, lab_size = 3,
             lab = TRUE, p.mat = p_mat, insig = "blank")

print(gg_corall)

gg_lst <- list()
for(i in 2:ncol(data_annual)) {
  y_var <- colnames(data_annual)[1]
  x_var <- colnames(data_annual)[i]
  if(is.numeric(data_annual[[x_var]])) {
    print(x_var)
    gg_lst[[i-1]] <- ggplot(data_annual, aes_string(x_var, fill=y_var, col=y_var)) +
      geom_density(alpha=.3) +
      theme(legend.position="none")
  } else {
    print(x_var)
    gg_lst[[i-1]] <- ggplot(data_annual, aes_string(x_var, fill=y_var)) + 
      geom_bar(position="fill") +
      theme(legend.position="none")#,
  }
}
print(wrap_plots(gg_lst[1:9], ncol=3, nrow=3))
print(wrap_plots(gg_lst[10:length(gg_lst)], ncol=3, nrow=3))

# gg_lst[[i-1]] + theme(legend.position="bottom")

# Harvested %BA --------------------------------------------------------
if(out_pdf) { plot.new(); text(x=.5, y=.5, "Harvested %BA", cex=3) }

# data

var_res <- "harvest_percent_ba"
vars <- c(var_res, vars_pred)

all(vars%in%colnames(census_stats_final))
summary(census_stats_final[, vars]) 
dim(census_stats_final)

data_harvested <- census_stats_final %>%
  filter(harvest_any) %>%
  dplyr::select(all_of(vars))

summary(data_harvested)
dim(data_harvested)

summary(data_harvested[,1])

# correlation matrix
p_mat <- data_harvested %>%
  ungroup() %>%
  rename(Harvest_int = harvest_percent_ba) %>%
  select_if(is.numeric) %>%
  cor_pmat()

gg_corharv <- data_harvested %>%
  ungroup() %>%
  rename(Harvest_int = harvest_percent_ba) %>%
  select_if(is.numeric) %>%
  cor(use="complete.obs", method="spearman") %>%
  ggcorrplot(type = "lower",
             show.legend = TRUE, lab_size = 3,
             lab = TRUE, p.mat = p_mat, insig = "blank")

print(gg_corharv)

gg_lst <- list()
for(i in 2:ncol(data_harvested)) {
  y_var <- colnames(data_harvested)[1]
  x_var <- colnames(data_harvested)[i]
  if(is.numeric(data_harvested[[x_var]])) {
    print(x_var)
    gg_lst[[i-1]] <- ggplot(data_harvested, aes_string(x_var, y_var)) +
      geom_point(alpha=.1, pch=20) +
      geom_smooth()
  } else {
    print(x_var)
    gg_lst[[i-1]] <- ggplot(data_harvested, aes_string(x_var, y_var)) + 
      geom_boxplot()
  }
}

print(wrap_plots(gg_lst[1:9], ncol=3, nrow=3))
print(wrap_plots(gg_lst[10:length(gg_lst)], ncol=3, nrow=3))

# # Size of harvested stems --------------------------------------------
# 
# if(out_pdf) {plot.new(); text(x=.5, y=.5, "Size of harvested stems", cex=3)}
# 
# # data
# 
# var_res <- "harvest_size_diff"
# vars <- c(var_res, vars_pred)
# 
# data_harvested <- census_stats_final %>%
#   filter(harvest_any) %>%
#   dplyr::select(all_of(vars))
# 
# all(vars%in%colnames(data_harvested))
# summary(data_harvested) 
# dim(data_harvested)
# 
# summary(data_harvested[,1])
# 
# # correlation matrix
# p_mat <- data_harvested %>%
#   ungroup() %>%
#   select_if(is.numeric) %>%
#   cor_pmat()
# 
# gg_corsize <- data_harvested %>%
#   ungroup() %>%
#   select_if(is.numeric) %>%
#   cor(use="complete.obs", method="spearman") %>%
#   ggcorrplot(type = "lower",
#              show.legend = TRUE, lab_size = 3,
#              lab = TRUE, p.mat = p_mat, insig = "blank")
# 
# print(gg_corsize)
# 
# gg_lst <- list()
# for(i in 2:ncol(data_harvested)) {
#   y_var <- colnames(data_harvested)[1]
#   x_var <- colnames(data_harvested)[i]
#   if(is.numeric(data_harvested[[x_var]])) {
#     print(x_var)
#     gg_lst[[i-1]] <- ggplot(data_harvested, aes_string(x_var, y_var)) +
#       geom_point(alpha=.1, pch=20) +
#       geom_smooth()
#   } else {
#     print(x_var)
#     gg_lst[[i-1]] <- ggplot(data_harvested, aes_string(x_var, y_var)) + 
#       geom_boxplot()
#   }
# }
# print(wrap_plots(gg_lst[1:9], ncol=3, nrow=3))
# print(wrap_plots(gg_lst[10:length(gg_lst)], ncol=3, nrow=3))


if(out_pdf) { dev.off() }


# save correlation plots separately
pdf("./outputs/figures/MS/Suppl_corrplots.pdf", width=12)
# print(gg_corall)
# print(gg_corharv)
gg_corall + ggtitle(expression("A. RF"[Probability])) + theme(title = element_text(size = 16)) +
  gg_corharv + ggtitle(expression("B. RF"[Intensity])) + theme(title = element_text(size = 16)) +
  plot_layout(ncol=2, guides="collect")
dev.off()


# Predictor maps ----------------------------------------------------------

proj_lambert_conic <- "+proj=lcc +lat_1=43 +lat_2=62 +lat_0=30 +lon_0=10 +x_0=0 +y_0=0 +ellps=intl +units=m no_defs"

predictor_data <- census_stats_final %>%
  select(any_of(c("latitude", "longitude", "tmt.plot.id", vars_pred))) %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326) %>%
  st_transform(proj_lambert_conic)

# basemap, themes and map definitions
load("./outputs/basemaps_europe.RData")
set_theme <- theme_bw() +
  theme(strip.background = element_blank(),
        strip.text = element_text(vjust=0.5))

map_extent <- as.data.frame(st_coordinates(predictor_data)) %>%
  summarize(min_x = min(X) - 5e4,
            max_x = max(X) + 1e4,
            min_y = min(Y) - 2e4,
            max_y = max(Y) + 2e4)

map_theme <- theme(legend.position = "bottom",
                   strip.text = element_text(vjust=0.5),
                   panel.border = element_rect(fill=NA, colour="gray50"),
                   axis.title = element_blank())

basemap_proj <- basemap_proj +
  map_theme

map_guide <- guide_colourbar(
  direction = "horizontal",
  title.position = "top",
  label.hjust = 0,
  label.position = "bottom",
  label.theme = element_text(angle = 0, size=8),
  barwidth = 10, barheight = 0.5)

##
range(predictor_data$Access50k)
access50_breaks = c(0, 10, 100, 800)

range(predictor_data$Access1M)
access1M_breaks = c(0, 10, 100, 1800)

map_data <- predictor_data %>% #slice_sample(n=1e3) %>%
  mutate(Access1M = Access1M + 1,
         Access50k = Access50k + 1,
         BA = ifelse(BA > quantile(BA, prob=.995), quantile(BA, prob=.995), BA))
gg_lst <- list()
for(i in 1:length(vars_pred)) {
  var_i <- vars_pred[i]
  print(var_i)
  is_discrete <- !is.numeric(map_data[[var_i]])
  is_access <- grepl(pattern="Access", x = var_i)
  if(var_i == "Access50k") {
    my_breaks <- access50_breaks 
    } else if(var_i == "Access1M")  {
      my_breaks <- access1M_breaks 
      } else  my_breaks <- waiver() 
  # draw plots
  if(is_discrete){
    legend_position <- ifelse(var_i == "CountryRegion", "none", "bottom")
    gg_lst[[var_i]] <- basemap_proj +
      geom_sf(data = map_data, aes_string(col = var_i), size=0.05, alpha=.5) +
      scale_color_viridis(discrete = is_discrete, direction = -1, option="turbo",
                          guide = guide_legend(ncol = 2)) +
      coord_sf(ylim = c(map_extent$min_y, map_extent$max_y),
               xlim = c(map_extent$min_x, map_extent$max_x)) +
      ggtitle(var_i) +
      theme(legend.position = legend_position,
            legend.title = element_blank(),
            legend.spacing = unit(1, 'mm'),
            legend.key.size = unit(5, 'mm'))
  } else {
    gg_lst[[var_i]] <- basemap_proj +
      geom_sf(data = map_data, aes_string(col = var_i), size=0.05) +
      scale_color_viridis(discrete = is_discrete, direction = -1, trans = ifelse(is_access, "log", "identity"),
                          breaks = my_breaks, labels = my_breaks) + #breaks = access1M_breaks, labels = access1M_breaks) +
      coord_sf(ylim = c(map_extent$min_y, map_extent$max_y),
               xlim = c(map_extent$min_x, map_extent$max_x)) +
      ggtitle(var_i) +
      theme(legend.position = "bottom",
            legend.title = element_blank())
  }
}

gg_lst <- gg_lst[names(gg_lst)[c(1:4, 6:15, 5)]] # rearrange to have the categorical variables in the end

png("./outputs/figures/MS/Suppl_predictor_maps.png", width=12, height=16, unit="in", res=600)
wrap_plots(gg_lst, ncol = 5)
dev.off()

# separate maps for each species

map_data <- predictor_data# %>% slice_sample(n=1e3)

png("./outputs/figures/MS/Suppl_species_maps.png", width=8, height=6, unit="in", res=600)
basemap_proj +
  geom_sf(data = map_data, aes(col = Sp_class), size=0.5) +
  facet_wrap(Sp_class ~ ., ncol=4) +
  scale_color_viridis(discrete = is_discrete, direction = -1, option="turbo",
                      guide = guide_legend(ncol = 2)) +
  coord_sf(ylim = c(map_extent$min_y, map_extent$max_y),
           xlim = c(map_extent$min_x, map_extent$max_x)) +
  theme(legend.position = legend_position,
        legend.title = element_blank(),
        legend.spacing = unit(1, 'mm'),
        legend.key.size = unit(5, 'mm'))

dev.off()
