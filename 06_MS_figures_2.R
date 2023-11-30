################################################################################'
##
## Local plotting and tuning of final MS figures
## - CV
## - variable importance
## - PDP plots
##
################################################################################'

rm(list=ls())

library(tidyverse)
library(ranger)
library(sf)
library(patchwork)
library(pROC)
library(colorspace)
library(RColorBrewer)
library(wesanderson)
library(viridis)

source("./src/calc_iml.R")
source("./src/custom_iml_plots.R")

sessionInfo()

varimp_to_df <- function(x, id) {
  x_df <- data.frame(variable = names(x),
                     importance= x,
                     imp_type = id,
                     row.names=NULL)
}

# Set things & prepare --------------------------------------------------------------

fl_date <- format(Sys.Date(), "%d-%m-%y")

outdir_name <- "update_EF"
# outdir_name <- "update_08_23"
# outdir_name <- "noCountry_checks"
# outdir_name <- "noHuman_checks"

run_details_1 <- "-no_tuning-full" 
run_details_ID <- ""
run_details_date <- "-24-08-23"
run_details_full <- paste0(run_details_1, 
                           run_details_ID,
                           run_details_date)
## Input files ----

# RF1
target_variable <- "harvest01"
run_details <- run_details_full
input_dir <- paste0("./outputs/bluebear/", target_variable, "/")

input_fullmodel1 <-  paste0(input_dir, "RF-LOCALIMP-", target_variable, run_details, ".rds")
input_CV_benchmark_scores1 <- paste0(input_dir, "RF-CV-benchmark-scores-harvest01-no_tuning-full-noBalancingCV_23-11-23.csv")#"RF-CV-benchmark-scores-", target_variable, run_details, ".csv")
# input_eff_1 <- paste0(input_dir, "ml_interpretation/effects_results_19-06-23.RData")
input_eff_1 <- list.files(paste0(input_dir, "ml_interpretation/"), pattern="effects_results_24-08-23*",
                          full.names = TRUE)
input_effD_1 <- paste0(input_dir, "ml_interpretation/effects_subsetsD_24-08-23_patch1.RData")

taskdata_fl_1 <- paste0(input_dir, "/TASKDATA-", target_variable, run_details, ".RData")

# RF2
target_variable <- "harvest_percent_ba"
run_details <- paste0(run_details_1, 
                      run_details_ID,
                      "-24-08-23")
input_dir <- paste0("./outputs/bluebear/", target_variable, "/")

input_fullmodel2 <-  paste0(input_dir, "RF-LOCALIMP-", target_variable, run_details, ".rds")
input_CV_benchmark_scores2 <- paste0(input_dir, "RF-CV-benchmark-scores-", target_variable, run_details, ".csv")
input_eff_2 <- c(paste0(input_dir, "ml_interpretation/effects_results_24-08-23.RData"),
                 paste0(input_dir, "ml_interpretation/effects_results_26-08-23.RData"))
input_effD_2 <- paste0(input_dir, "ml_interpretation/effects_subsetsD_24-08-23.RData")

taskdata_fl_2 <- paste0(input_dir, "/TASKDATA-", target_variable, run_details, ".RData")

## Output files -----------

out_dir <- paste0("./outputs/figures/MS/", outdir_name, "/")
cv_pdf <- paste0(out_dir, "CV_results_", fl_date, ".pdf")
cv_png <- paste0(out_dir, "CV_results_", fl_date, ".png")

varimp_pdf <- paste0(out_dir, "varimp_results_", fl_date, ".pdf")

effects_local_pdf <- paste0(out_dir, "effects_local_results_", fl_date, ".pdf")
effects_other_pdf <- paste0(out_dir, "effects_other_results_", fl_date, ".pdf")


# Read input files  --------------------------------------------------

# -- full model --
rf_localimp1 <- readRDS(input_fullmodel1)
rf_localimp2 <- readRDS(input_fullmodel2)

cv_bmscores1 <- read.csv(input_CV_benchmark_scores1)
cv_bmscores2 <- read.csv(input_CV_benchmark_scores2)

# combine from separate files if effects for RF1 ran in several patches
if(length(input_eff_1 > 1)) {
  eff_lst <- list()
  for(i in 1:length(input_eff_1)) {
    load(input_eff_1[i], verbose = TRUE)
    eff_lst[[i]] <- out_lst$effects
    rm(out_lst)
  }
  eff_others_1 <- do.call(c, eff_lst)
} else {
  load(input_eff_1)
  eff_others_1 <- out_lst$effects
}

load(input_effD_1)
eff_D_1 <- eff_subsets

if(length(input_eff_2) > 1) {
  eff_lst <- list()
  for(i in 1:length(input_eff_2)) {
    load(input_eff_2[i], verbose = TRUE)
    eff_lst[[i]] <- out_lst$effects
    rm(out_lst)
  }
  eff_others_2 <- do.call(c, eff_lst)
} else {
  load(input_eff_2)
  eff_others_2 <- out_lst$effects
}

load(input_effD_2)
eff_D_2 <- eff_subsets

load(taskdata_fl_1)
taskdata_1 <- task_data_sp

load(taskdata_fl_2)
taskdata_2 <- task_data_sp

# Cross-validation --------------------------------------------------------

## RF1 ----

cv_bmscores1 <- cv_bmscores1 %>%
  mutate(CV_type = case_when(
    resampling_id == "cv" ~ "Random",
    resampling_id == "spcv_block" ~ "Spatial",
    resampling_id == "custom_cv" ~ "Country"),
    CV_type = factor(CV_type, levels = c("Random", "Spatial", "Country")),
    Model = factor(ifelse(learner_id == "classif.featureless", "Null", "Full"),
                   levels=c("Null", "Full")),
    rf = "RF1 - Probability of harvest") %>%
  filter(CV_type != "Random")

cv_bmscores1 %>% 
  group_by(resampling_id, Model) %>% 
  summarise(median_rocAuc = median(.vars1),
            median_prAuc = median(.vars2),
            mean_rocAuc = mean(.vars1),
            mean_prAuc = mean(.vars2))

## RF2 ----

cv_bmscores2 <- cv_bmscores2 %>%
  mutate(CV_type = case_when(
    resampling_id == "cv" ~ "Random",
    resampling_id == "spcv_block" ~ "Spatial",
    resampling_id == "custom_cv" ~ "Country"),
    CV_type = factor(CV_type, levels = c("Random", "Spatial", "Country")),
    Model = factor(ifelse(learner_id == "regr.featureless", "Null", "Full"),
                   levels=c("Null", "Full")),
    rf = "RF2 - Intensity of harvest") %>%
  filter(CV_type != "Random")

cv_bmscores2 %>% 
  group_by(resampling_id, Model) %>% 
  summarise(median_Rsq = median(.vars1),
            median_RMSE = median(.vars2),
            mean_Rsq = mean(.vars1),
            mean_RMSE = mean(.vars2))


## plot MS figure ----

gg_cv_roc1 <- ggplot(cv_bmscores1, aes(CV_type, .vars1, col=Model)) +
  geom_boxplot() +
  ylab("ROC AUC") + xlab("Cross-validation type") +
  theme_classic() +
  ggtitle(expression("A. RF"[Probability])) #("RF1 - Probability of harvest") 

gg_cv_2 <- ggplot(cv_bmscores2, aes(CV_type, .vars2, col=Model)) +
  geom_boxplot() +
  ylab("RMSE") + xlab("Cross-validation type") +
  theme_classic() +
  ggtitle(expression("B. RF"[Intensity])) #("RF2 - Intensity of harvest")

png(file= cv_png, width=9, height=3.5, unit="in", res=600)
gg_cv_roc1 + scale_color_brewer(palette="Paired") +
  gg_cv_2 + scale_color_brewer(palette="Paired") +
  plot_layout(ncol=2, 
              guides = "collect") & 
  theme(legend.position = 'right',
        # legend.title = element_blank(),
        axis.text = element_text(size=12),
        axis.title.y = element_text(size=12),
        axis.title.x = element_blank()) 
dev.off()


# Variable importance -----------------------------------------------------

cols_varimp <-  brewer.pal(6, "Set2")[c(1,2,6,3)] #brewer.pal(7, "Set3")[c(7,6,5,3)]
# cols_varimp <-  brewer.pal(6, "Dark2")[c(1, 2, 6, 3)] #viridis_pal(option="turbo")(6)[2:5]
# cols_varimp <- wes_palette("Darjeeling1", 5, type ="discrete")[c(2, 4, 3, 5)]
# cols_varimp <-  brewer.pal(4, "YlGnBu")[4:1]
# cols_varimp <- viridis_pal(option="mako")(6)[5:2]

plot(1:7, 1:7, col = cols_varimp, pch =20, cex=10)

### neat names ----


neat_names <- as.data.frame(rbind(c("Dq_mean0", "QMeanDiameter", "Forest"),
                                  c("ba0_m2", "BasalArea", "Forest"),
                                  c("gini_d_nplot0", "SizeStructure", "Forest"),
                                  c("ba_percent_dom0", "SpeciesDominance", "Forest"),
                                  c("non_native", "SpeciesNative", "Forest"),
                                  c("species_classes2", "SpeciesClass", "Forest"),
                                  c("dist_breakage", "Storm", "Disturbance"),
                                  c("dist_barkbeetle_windthrow", "StormBeetle", "Disturbance"),
                                  c("dist_fire", "Fire", "Disturbance"),
                                  c("elevation_1KMmd_GMTEDmd", "Elevation", "Environment"),
                                  c("roughness_1KMmd_GMTEDmd", "TopoRoughness", "Environment"),
                                  c("wc2.1_2.5m_bio_1", "Temperature", "Environment"),
                                  c("wc2.1_2.5m_bio_12", "Precipitation", "Environment"),
                                  c("POPDENS_2015_mean10km", "PopulationDensity", "Human"),
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
neat_names$type <- factor(neat_names$type , levels = c("Forest", "Disturbance", "Environment", "Human"))
  
## RF1 ----

imp_df1 <- varimp_to_df(importance(rf_localimp1), "all") %>%
  left_join(neat_names)

gg_varimp1 <- ggplot(imp_df1, aes(importance, reorder(new.name, importance), fill=type)) +
  geom_bar(stat="identity", width=.8, alpha=1) +
  xlab("") + ylab("") +
  # ggtitle(expression(RF[Probability])) +
  ggtitle("Variable importance") +
  scale_fill_manual(values = cols_varimp) +
  # scale_fill_brewer(palette = "Paired") +
  theme_classic() +
  theme(legend.position = "bottom",
        legend.title=element_blank())

gg_varimp1

## RF2 ----

imp_df2 <- varimp_to_df(importance(rf_localimp2), "all") %>%
  left_join(neat_names)

gg_varimp2 <- ggplot(imp_df2, aes(importance, reorder(new.name, importance), fill=type)) +
  geom_bar(stat="identity", width=.8, alpha=1) +
  xlab("") + ylab("") +
  # ggtitle(expression(RF[Intensity])) +
  ggtitle("Variable importance") +
  scale_fill_manual(values = cols_varimp) +
  # scale_fill_brewer(palette = "Accent") +
  theme_classic() +
  theme(legend.position = "bottom",
        legend.title=element_blank())

gg_varimp2

gg_varimp1 + gg_varimp2 +  
  plot_layout(guides = "collect") & theme(legend.position = 'bottom')

# pdf(file = varimp_pdf, width=8, height=4.5)
gg_varimp1 + gg_varimp2 +  
  plot_layout(guides = "collect") & theme(legend.position = 'bottom')
# dev.off()

png(file = gsub(pattern="pdf", replacement = "png", x = varimp_pdf),
    width=8, height=4.5, unit="in", res=600)
gg_varimp1 + gg_varimp2 +  
  plot_layout(guides = "collect") & theme(legend.position = 'bottom')
dev.off()



# Effects plots -----------------------------------------------------------

# undersampling correction
correct_undersampling <- function(p_s, beta = 1/20) {
  p_corr <- (beta * p_s) / (beta * p_s - p_s +1)
  return(p_corr)
}

cols_eff <-  cols_varimp[4:1] #brewer.pal(name="Dark2", n=6)[c(1:3, 6)] #viridis_pal(option="D")(5)[1:4] # c("gray40", brewer.pal(name="Set2", n=3))
plot(1:4, col=cols_eff, pch=20, cex=10)

size_range <- c(0.6, 0.6)
subset_levels <- c("Europe, all species",
                   "Finland, pines",
                   "Poland, pines",
                   "Spain, pines")

# short country/region names
data_census <- readRDS("./data/processed/census_stats_final_13-06-23_RFready.rds")
colnames(data_census)

idx <- grepl(pattern="country", x = colnames(data_census))
colnames(data_census)[idx]

country_codes <- data_census %>% 
  select(country_or_region, country_or_region_old) %>% 
  distinct() %>%
  rename(country_or_region_code = country_or_region_old)

eff_datasets_1 <- rbind(taskdata_1 %>%
                        mutate(data = "Europe, all species"),
                        taskdata_1 %>%
                        mutate(data = "Poland, pines") %>%
                        filter(db_country == "NPO1.Pol" & species_classes2 == "Pinus"),
                        taskdata_1 %>%
                        mutate(data = "Finland, pines") %>%
                        filter(db_country == "FIN.Fin" &
                                 species_classes2 == "Pinus" &
                                 latitude < 65),
                        taskdata_1 %>%
                        mutate(data = "Spain, pines")  %>%
                        filter(db_country == "FUN.Spa" &
                                 species_classes2 == "Pinus")) %>%
  select(!harvest01) %>% distinct() %>%
  mutate(line_width = ifelse(data == "Europe, all species", 2, 1),
         line_type = ifelse(data == "Europe, all species", 1, 2),
         data = factor(data, levels = subset_levels)) %>%
  left_join(country_codes) %>%
  rename(country_or_region_long = country_or_region,
         country_or_region = country_or_region_code)


eff_datasets_2 <- rbind(taskdata_2 %>%
                          mutate(data = "Europe, all species"),
                        taskdata_2 %>%
                          mutate(data = "Poland, pines") %>%
                          filter(db_country == "NPO1.Pol" & species_classes2 == "Pinus"),
                        taskdata_2 %>%
                          mutate(data = "Finland, pines") %>%
                          filter(db_country == "FIN.Fin" &
                                   species_classes2 == "Pinus" &
                                   latitude < 65),
                        taskdata_2 %>%
                          mutate(data = "Spain, pines")  %>%
                          filter(db_country == "FUN.Spa" &
                                   species_classes2 == "Pinus"))  %>%
  mutate(line_width = ifelse(data == "Europe, all species", 2, 1),
         data = factor(data, levels = subset_levels)) %>%
  left_join(country_codes) %>%
  rename(country_or_region_long = country_or_region,
         country_or_region = country_or_region_code) %>%
  mutate(species_classes2 = fct_recode(species_classes2, 
                                       "Eucalyptus"= "Eucalyptus",
                                       "PinusPin" = "PinusPinaster",
                                       "Picea" = "Picea",
                                       "Pinus" = "Pinus",
                                       "FagusQuercus" = "FagusQuercus",
                                       "OtherConifer" = "OtherConifer",
                                       "OtherBL" = "OtherBroadleaved"))


## D - RF1 ----
eff_D <- eff_D_1 %>%
  mutate(data = ifelse(data == "Europe, all sp.", "Europe, all species", data),
    data = factor(data, levels = subset_levels),
    p_original = .value) %>% #    .value = correct_undersampling(.value)) %>%
  filter(Dq_mean0 >= 100)
eff_D$line_width <- ifelse(eff_D$data == "Europe, all species", 2, 1)


ylim_range <- range(eff_D$.value)
xlim_range <- c(100, quantile((taskdata_1$Dq_mean0), prob=.99))

gg_pines_d_1 <- eff_D %>%
  ggplot(aes(Dq_mean0, .value,  col=data, lty=data)) + 
  geom_line() +
  # scale_color_manual(values = cols_eff) +
  guides(size = "none") +
  coord_cartesian(xlim=xlim_range,
                  ylim = ylim_range) +
  ylab("P (harvest)") +
  theme_bw() + #labs(color='Data', lty = 'Data') +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        plot.margin = unit(c(5.5, 5.5, 0, 5.5), unit = "pt")) +
  labs(title="A.")

gg_ddens_pines_1 <- eff_datasets_1 %>% 
  ggplot(aes(Dq_mean0, fill=data, col=data, lty=data)) + 
  geom_density(alpha=.1, show.legend=FALSE) +
  geom_density(alpha=0, show.legend=FALSE) +
  # scale_color_manual(values = cols_eff) +
  # scale_fill_manual(values = cols_eff) +
  guides(size = "none", fill = "none", color = "none") +
  coord_cartesian(xlim=xlim_range) +
  theme_bw() +
  theme(legend.position = "none",        
        plot.margin = unit(c(0, 5.5, 5.5, 5.5), unit = "pt")) +
  ylab("Density") + xlab("Quadratic mean diameter (mm)") 

gg_pines_d_1 + gg_ddens_pines_1   +
  plot_layout(ncol=1, heights = c(5, 1), guides = "collect")

## D - RF2 ----
eff_D <- eff_D_2 %>%
  mutate(data = factor(data, levels = subset_levels))
eff_D$line_width <- ifelse(eff_D$data == "Europe, all species", 2, 1)

ylim_range <- range(eff_D$.value)
xlim_range <- c(99, quantile((taskdata_2$Dq_mean0), prob=.99))

gg_pines_d_2 <- eff_D %>%
  # filter(data == "Spain, pines") %>%
  ggplot(aes(Dq_mean0, .value,  col=data, lty=data)) +
  geom_line() +
  # scale_color_manual(values = cols_eff) +
  guides(size = "none") +
  coord_cartesian(xlim=xlim_range,
                  ylim = ylim_range) +
  ylab("%BA removed") + 
  theme_bw() + #labs(color='Data') +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        plot.margin = unit(c(5.5, 5.5, 0, 5.5), unit = "pt")) +
  labs(title="C.")

gg_ddens_pines_2 <- eff_datasets_2 %>% 
  ggplot(aes(Dq_mean0, fill=data, 
             col=data, lty=data)) + # != "Europe, all species")) +
  geom_density(alpha=.1, show.legend=FALSE) +
  geom_density(alpha=0, show.legend=FALSE) +
  # scale_color_manual(values = cols_eff) +
  # scale_fill_manual(values = cols_eff) +
  guides(size = "none", fill = "none", color = "none") +
  coord_cartesian(xlim=xlim_range) +
  theme_bw() +
  theme(legend.position = "none",      
        plot.margin = unit(c(0, 5.5, 5.5, 5.5), unit = "pt")) +
  ylab("Density") + xlab("Quadratic mean diameter (mm)") 


# DBH only figure

pdf(gsub(pattern=".pdf", replacement = "_DBH.pdf", x = effects_local_pdf), 
    width=9, height=3.5)

# png(gsub(pattern=".pdf", replacement = "_DBH.png", x = effects_local_pdf), 
#     width=9, height=3.5, unit="in", res=600)

gg_pines_d_1 +  ggtitle(expression("A. RF"[Probability]))+ theme_classic() +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        plot.margin = unit(c(5.5, 5.5, 0, 5.5), unit = "pt")) +
  gg_pines_d_2  +  ggtitle(expression("B. RF"[Intensity])) + theme_classic() + xlab("") +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        plot.margin = unit(c(5.5, 5.5, 0, 5.5), unit = "pt")) +
  gg_ddens_pines_1 + theme_classic() + theme(plot.margin = unit(c(0, 5.5, 5.5, 5.5), unit = "pt"),
                                             axis.title.y = element_blank(),
                                             axis.text.y = element_blank(),
                                             axis.ticks.y = element_blank(),
                                             axis.line.y.left = element_blank() ) +

  gg_ddens_pines_2 + theme_classic() + theme(plot.margin = unit(c(0, 5.5, 5.5, 5.5), unit = "pt"),
                                             axis.title.y = element_blank(),
                                            axis.text.y = element_blank(),
                                            axis.ticks.y = element_blank(),
                                            axis.line.y.left = element_blank() ) + 
  plot_layout(ncol=2, heights = c(5, 1), guides = "collect")

dev.off()


# All others ----

cols_varimp <- cols_eff #viridis_pal(option="mako")(6)[5:2]

size_title <- 10

### RF1 ----

eff_others <- eff_others_1

eff_others[["country_or_region"]] <- eff_others[["country_or_region"]] %>% 
  left_join(country_codes) %>%
  select(!country_or_region) %>%
  rename(country_or_region = country_or_region_code)

eff_others[["ba0_m2"]] <- eff_BA_1 %>% 
  filter(data == "Europe, all species") %>% 
  select(ba0_m2, .class, .value, .type)
eff_others[["Dq_mean0"]] <- eff_D_1 %>%
  filter(data == "Europe, all sp.") %>% 
  select(Dq_mean0, .class, .value, .type)
eff_others <- eff_others[imp_df1 %>% arrange(desc(importance)) %>% pull(variable)]
var_names <- names(eff_others)
sort_names <- data.frame(variable = names(eff_others)) %>% left_join(neat_names)
names(eff_others) <- sort_names$new.name

names(cols_varimp) <- levels(neat_names$type)[4:1]

ylims = c(min = min(sapply(eff_others, function(x) min(x$.value))), #min(correct_undersampling(x$.value)))),
          max = 0.16)
eff_plot_lst <- list()
for(i in 1:length(eff_others)) {
  var_i <- var_names[i]
  type_i <- neat_names %>% filter(variable == var_i) %>% pull(type)
  col_i <- cols_varimp[as.character(type_i)] #cols_varimp[3] 
  neat_name_i <- names(eff_others)[i]
  print(neat_name_i)
  eff_others[[i]] <- eff_others[[i]] %>% 
    mutate(p_original = .value) #.value = correct_undersampling(.value))
  if(!is.numeric(eff_others[[i]][,var_i])) {
    gg_eff_i <- eff_others[[i]] %>%
      filter(.class == "HARVEST") %>%
      ggplot(aes_string(var_i, ".value")) + 
      geom_point(col = col_i) + ggtitle(neat_name_i) +
      ylab("P (harvest)") +
      theme_classic() +
      theme(axis.title.y = element_text(size = size_title),
            axis.title.x = element_blank(),
            axis.text.x = element_blank(),
            axis.ticks.x = element_blank(),
            plot.margin = unit(c(5.5, 5.5, 0, 5.5), unit = "pt"),
            plot.title = element_text(size = size_title)) +
      coord_cartesian(ylim=ylims)
    gg_dens_i <- eff_datasets_1 %>% 
      ggplot(aes_string(var_i, y = )) +
      geom_bar(alpha=.2, col= col_i, fill=col_i) +
      theme_classic()  +
      theme(plot.margin = unit(c(0, 5.5, 10.5, 5.5), unit = "pt"),
            axis.text.x = element_text(angle=45, hjust=1, size=9),
            axis.title.x = element_blank(),
            axis.title.y = element_blank(),
            axis.text.y = element_blank(),
            axis.ticks.y = element_blank(),
            axis.line.y.left = element_blank(),
            plot.title = element_text(size = size_title)) 
    eff_plot_lst[[neat_name_i]] <- gg_eff_i + gg_dens_i + plot_layout(ncol=1, heights=c(4,1))
  } else {
    xlims <- c(min(eff_datasets_1[[var_i]]), quantile(eff_datasets_1[[var_i]], prob=0.99) )
    gg_eff_i <- eff_others[[i]] %>%
      filter(.class == "HARVEST") %>%
      ggplot(aes_string(var_i, ".value")) + 
      geom_line(col = col_i, size=1) + ggtitle(neat_name_i) +
      ylab("P (harvest)") +
      coord_cartesian(xlim = xlims, ylim=ylims) +
      theme_classic()  +
      theme(axis.title.y = element_text(size = size_title),
            axis.title.x = element_blank(),
            axis.text.x = element_blank(),
            axis.ticks.x = element_blank(),
            plot.margin = unit(c(5.5, 5.5, 0, 5.5), unit = "pt"),
            plot.title = element_text(size = size_title)) 
    gg_dens_i <- eff_datasets_1 %>% 
      ggplot(aes_string(var_i)) +
      geom_density(alpha=.2, fill=col_i, col = col_i) +
      coord_cartesian(xlim = xlims) +
      theme_classic()  +
      theme(plot.margin = unit(c(0, 5.5, 10.5, 5.5), unit = "pt"),
            axis.text.x = element_text(angle=45, hjust=1, size=9),
            axis.title.x = element_blank(),
            axis.title.y = element_blank(),
            axis.text.y = element_blank(),
            axis.ticks.y = element_blank(),
            axis.line.y.left = element_blank(),
            plot.title = element_text(size = size_title)) 
    eff_plot_lst[[neat_name_i]] <- gg_eff_i + gg_dens_i + plot_layout(ncol=1, heights=c(4,1))
  }
}


names(eff_plot_lst) <- LETTERS[1:length(eff_plot_lst)]
# design <- c("AAA
#             BCD
#             EFG
#             HIJ
#             KLM
#             NO#")
design <- c("AABC
            DEFG
            HIJK
            LMNO")

# wrap_plots(eff_plot_lst, ncol=4, design = design )


# remove y-axis title from mid-plots
eff_plot_lst_6 <- eff_plot_lst[1:6] # take 6 first here in original form 

for(i in 1:length(eff_plot_lst)) {
  if(!(names(eff_plot_lst)[i] %in% c("A", "D", "H", "L"))) {
    eff_plot_lst[[i]] <- eff_plot_lst[[i]] & theme(axis.title.y = element_blank(),
                                                   axis.text.y = element_blank())
  }
}

png(file = gsub(pattern=".pdf", replacement="_1.png", x = effects_other_pdf),
    height=9, width=9, res=600, unit="in")
# pdf(file = effects_other_pdf, height=11, width=11)
wrap_plots(eff_plot_lst, ncol=4, design = design ) 
dev.off()

## Only 6 with highest var-imp

design6_PROB <- c("AAAB
            CDEF")

for(i in 1:length(eff_plot_lst_6)) {
  if(!(names(eff_plot_lst_6)[i] %in% c("A", "C"))) {
    eff_plot_lst_6[[i]] <- eff_plot_lst_6[[i]] & theme(axis.title.y = element_blank(),
                                                   axis.text.y = element_blank())
  }
}

# png(file = gsub(pattern=".pdf", replacement="_6plots_1.png", x = effects_other_pdf),
    # height=5, width=9.5, res=600, unit="in")
wrap_plots(eff_plot_lst_6, ncol=4, design = design6_PROB)  + 
  plot_annotation(title = expression("A. RF"[Probability]), 
                  theme=theme(plot.title = element_text(size=20)))
# dev.off()

wrap_plots(eff_plot_lst_6, ncol=4, design = design6_PROB)

eff_plot_lst_6_PROB <- eff_plot_lst_6

### RF2 ----

eff_others <- eff_others_2

eff_others[["Dq_mean0"]] <- eff_D_2 %>%
  filter(data == "Europe, all species") %>% 
  select(Dq_mean0, .value, .type)

# order by varimp OR the same order as in RF1?
# eff_others <- eff_others[imp_df1 %>% arrange(desc(importance)) %>% pull(variable)] # plot order from RF1 varimp for easier comparison!
eff_others <- eff_others[imp_df2 %>% arrange(desc(importance)) %>% pull(variable)] # plot order from RF1 varimp for easier comparison!

var_names <- names(eff_others)
sort_names <- data.frame(variable = names(eff_others)) %>% left_join(neat_names)
names(eff_others) <- sort_names$new.name

ylims = c(min = min(sapply(eff_others, function(x) min(x$.value))),
          max = 0.65)
eff_plot_lst <- list()
for(i in 1:length(eff_others)) {
  var_i <- var_names[i]
  type_i <- neat_names %>% filter(variable == var_i) %>% pull(type)
  col_i <- cols_varimp[as.character(type_i)]
  neat_name_i <- names(eff_others)[i]
  if(!is.numeric(eff_others[[i]][,var_i])) {
    gg_eff_i <- ggplot(eff_others[[i]], aes_string(var_i, ".value")) + 
      geom_point(col = col_i) + ggtitle(neat_name_i) +
      ylab("%BA removed") +
      theme_classic() +
      theme(axis.title.y = element_text(size = size_title),
            axis.title.x = element_blank(),
            axis.text.x = element_blank(),
            axis.ticks.x = element_blank(),
            plot.margin = unit(c(5.5, 5.5, 0, 5.5), unit = "pt"),
            plot.title = element_text(size = size_title)) +
      coord_cartesian(ylim = ylims)
    gg_dens_i <- eff_datasets_2 %>% 
      ggplot(aes_string(var_i, y = )) +
      geom_bar(alpha=.2, col=col_i, fill=col_i) +
      theme_classic()  +
      theme(plot.margin = unit(c(0, 5.5, 10.5, 5.5), unit = "pt"),
            axis.text.x = element_text(angle=45, hjust=1, size=9),
            axis.title.x = element_blank(),
            axis.title.y = element_blank(),
            axis.text.y = element_blank(),
            axis.ticks.y = element_blank(),
            axis.line.y.left = element_blank(),
            plot.title = element_text(size = size_title)) 
    eff_plot_lst[[neat_name_i]] <- gg_eff_i + gg_dens_i + plot_layout(ncol=1, heights=c(4,1))
  } else {
    xlims <- c(min(eff_datasets_2[[var_i]]), quantile(eff_datasets_2[[var_i]], prob=0.99) )
    gg_eff_i <- ggplot(eff_others[[i]], aes_string(var_i, ".value")) + 
      geom_line(col = col_i, size=1) + ggtitle(neat_name_i) +
      ylab("%BA removed") +
      coord_cartesian(xlim = xlims, ylim = ylims) +
      theme_classic()  +
      theme(axis.title.y = element_text(size = size_title),
            axis.title.x = element_blank(),
            axis.text.x = element_blank(),
            axis.ticks.x = element_blank(),
            plot.margin = unit(c(5.5, 5.5, 0, 5.5), unit = "pt"),
            plot.title = element_text(size = size_title))
    gg_dens_i <- eff_datasets_2 %>% 
      ggplot(aes_string(var_i)) +
      geom_density(alpha=.2, col = col_i, fill=col_i) +
      coord_cartesian(xlim = xlims) +
      theme_classic()  +
      theme(plot.margin = unit(c(0, 5.5, 10.5, 5.5), unit = "pt"),
            axis.text.x = element_text(angle=45, hjust=1, size=9),
            axis.title.x = element_blank(),
            axis.title.y = element_blank(),
            axis.text.y = element_blank(),
            axis.ticks.y = element_blank(),
            axis.line.y.left = element_blank(),
            plot.title = element_text(size=size_title)) 
    eff_plot_lst[[neat_name_i]] <- gg_eff_i + gg_dens_i + plot_layout(ncol=1, heights=c(4,1))
  }
}

names(eff_plot_lst) <- LETTERS[1:length(eff_plot_lst)]

design <- c("ABBC
            DEFG
            HIJK
            LMNO")

# remove y-axis title from mid-plots
eff_plot_lst_6 <- eff_plot_lst[1:6] # take 6 first here in original form 

for(i in 1:length(eff_plot_lst)) {
  if(!(names(eff_plot_lst)[i] %in% c("A", "D", "H", "L"))) {
    eff_plot_lst[[i]] <- eff_plot_lst[[i]] & theme(axis.title.y = element_blank(),
                                                   axis.text.y = element_blank())
  }
}


png(file = gsub(pattern=".pdf", replacement="_2.png", x = effects_other_pdf),
height=9, width=9, res=600, unit="in")
# pdf(file = effects_other_pdf, height=15, width=10)
wrap_plots(eff_plot_lst, ncol=3, design = design)
dev.off()

## Only 6 with highest var-imps

design6_INT <- c("ABBB
            CDEF")

for(i in 1:length(eff_plot_lst_6)) {
  if(!(names(eff_plot_lst_6)[i] %in% c("A", "C"))) {
    eff_plot_lst_6[[i]] <- eff_plot_lst_6[[i]] & theme(axis.title.y = element_blank(),
                                                       axis.text.y = element_blank())
  }
}

# png(file = gsub(pattern=".pdf", replacement="_6plots_2.png", x = effects_other_pdf),
    # height=5.6, width=9.5, res=600, unit="in")
wrap_plots(eff_plot_lst_6[1:6], ncol=4, design = design6_INT)  + 
  plot_annotation(title = expression("B. RF"[Intensity]), 
                  theme=theme(plot.title = element_text(size=20)))
# dev.off()

eff_plot_lst_6_INT <- eff_plot_lst_6

# VarImp + PDP ------------------------------------------------------------

design_varimpPdp <- c("AABBBB
                       AABBBB")

# png(file = gsub(pattern=".pdf", replacement="_PROB_varImpPDP.png", x = effects_other_pdf),
#     height=5.1, width=10, res=300, unit="in")
pdf(file = gsub(pattern=".pdf", replacement="_PROB_varImpPDP.pdf", x = effects_other_pdf),
    height=5.1, width=10.5)
(gg_varimp1 &
    theme(legend.position = "none") ) + 
  plot_annotation(title = expression("A. RF"[Probability]), 
                  theme=theme(plot.title = element_text(size=14))) + 
  wrap_plots(eff_plot_lst_6_PROB, ncol=4, design = design6_PROB)  +
  plot_layout(design = design_varimpPdp) 
dev.off()

# png(file = gsub(pattern=".pdf", replacement="_INT_varImpPDP.png", x = effects_other_pdf),
#     height=5.1, width=10, res=300, unit="in")
pdf(file = gsub(pattern=".pdf", replacement="_INT_varImpPDP.pdf", x = effects_other_pdf),
    height=5.1, width=10.5)
(gg_varimp2 &
    theme(legend.position = "none") ) + 
  plot_annotation(title = expression("B. RF"[Intensity]), 
                  theme=theme(plot.title = element_text(size=14))) + 
  wrap_plots(eff_plot_lst_6_INT, ncol=4, design = design6_INT)  +
  plot_layout(design = design_varimpPdp)
dev.off()


