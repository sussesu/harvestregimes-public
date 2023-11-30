############################################################'
##
## Checking and visualizing results 
## from the harvest probability RF
##
############################################################'

# update 23-8-23

rm(list=ls())

# Run if working on BlueBear
bb_wrkdir <- "/rds/projects/p/pughtam-formmi/ForMMI_management2"
if(getwd() == bb_wrkdir) {
  library(Rcpp, lib="/rds/homes/s/suvantss/R/x86_64-pc-linux-gnu-library/4.1")
}

library(tidyverse)
library(sf)
library(mlr3verse)
library(mlr3learners)
library(mlr3extralearners)
library(mlr3spatiotempcv)
library(ranger)
library(iml)
library(patchwork)
library(pROC)
# library(pdp)

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

out_id <- "_patch1"
run_subsets <- FALSE

#these exclusion list just to calculate them in parallel runs 
# (need to submit separate sbatch files, didn't write this into an array job...)
exclude_cols <- c(
  # "Dq_mean0",
  # "ba0_m2",
  # "gini_d_nplot0",
  # "ba_percent_dom0",
  # "species_classes2"
  
  # ,
  "dist_barkbeetle_windthrow",
  "dist_fire",
  "access_1M",
  "access_50k",
  "POPDENS_2015_mean10km"
  
  ,
  "elevation_1KMmd_GMTEDmd",
  "roughness_1KMmd_GMTEDmd",
  "npp",
  "owner_share_public",
  "country_or_region"
)

# 

target_variable <- "harvest01"

fl_date <- format(Sys.Date(), "%d-%m-%y")

msr_cv <- msr("classif.auc")

out_dir <- paste0("./outputs/",
                  target_variable,
                  "/ml_interpretation/")

run_cv <- FALSE
run_effects <- TRUE
run_interactions <- FALSE



subset_iml_effects <- 5e4 # NULL if run with full data, otherwise numeric giving the size of subsample
grid_size_pdp <- 15

# effects method
eff_method <- "pdp"

# folder for input files
input_dir <- paste0("./outputs/", target_variable, "/")
run_details <- "-no_tuning-full-24-08-23"

input_fullmodel <-  paste0(input_dir, "RF-LOCALIMP-", target_variable, run_details, ".rds")
input_taskdata <- paste0(input_dir, "TASKDATA-", target_variable, run_details, ".RData")    #"./outputs/harvest_3class/TASKDATA-harvest_3class-no_tuning-full-onlyTraits-10-02-22.RData"

input_CV_benchmark_scores <- paste0(input_dir, "RF-CV-benchmark-scores-", target_variable, run_details, ".csv")

### Output filenames --------------------------------------------------------

if(!dir.exists(out_dir)) dir.create(out_dir)

cv_pdf <- paste0(out_dir, "CV_results_", fl_date, out_id,".pdf")
varimp_pdf <- paste0(out_dir, "varimp_results_", fl_date, out_id,".pdf")
eff_pdf <- paste0(out_dir, "effplot_results_", fl_date, out_id,".pdf")
interaction_pdf <- paste0(out_dir, "interaction_results_", fl_date, out_id,".pdf")
cv_benchmark_pdf <- paste0(out_dir, "CV_benchmark_results_", fl_date, out_id,".pdf")


localEff_pdf <- paste0(out_dir, "local_eff_", fl_date, out_id,".pdf")

cv_df_file <- paste0(out_dir, "CV_results_", fl_date, out_id,".RData")
varimp_df_file <- paste0(out_dir, "varimp_results_", fl_date, out_id,".RData")
effects_list_file <- paste0(out_dir, "effects_results_", fl_date, out_id, ".RData")
effects_subsetsD_file <- paste0(out_dir, "effects_subsetsD_", fl_date, out_id, ".RData")
effects_subsetsBA_file <- paste0(out_dir, "effects_subsetsBA_", fl_date, out_id, ".RData")
interactions_list_file <- paste0(out_dir, "interactions_results_", fl_date, out_id, ".RData")


### Read result files  --------------------------------------------------

# -- full model, local importance --
rf_localimp <- readRDS(input_fullmodel)

# -- data --
load(input_taskdata)

# -- CV results --
if(run_cv) {
  if(file.exists(input_CV)) load(input_CV) 
  if(file.exists(input_CV_spatial)) load(input_CV_spatial)
  if(file.exists(input_CV_country)) load(input_CV_country)
}

cv_bmscores <- read.csv(input_CV_benchmark_scores)

# Feature importance  ----------------------------------------------------

##### All, ranger ----

imp_df <- varimp_to_df(importance(rf_localimp), "all")

gg_varimp <- ggplot(imp_df, aes(importance, reorder(variable, importance))) +
  geom_point() +
  geom_line(data = rbind(imp_df, imp_df %>% mutate(importance = 0)), 
            aes(importance, variable, group=variable)) +
  xlab("") + ylab("") +
  ggtitle(paste0("Variable importance\n(", target_variable, ")")) +
  theme_bw()

pdf(file=localEff_pdf, height = 5, width = 6)
gg_varimp


# iml plots ---------------------------------------------------------------------

predictor_names <- imp_df %>% arrange(desc(importance)) %>% pull(variable)

if(is.null(subset_iml_effects)) {
  data_iml <- task_data_sp 
} else {
  data_iml <- task_data_sp %>%  slice_sample(n = subset_iml_effects, weight_by = rep_area_weights)
  # data_iml <- data_annual %>% slice_sample(n = subset_iml_effects, weight_by = forest_area_rep_new)
}

dim(data_iml)

## Manual subsets ----
pfun <- function(object, newdata) predict(object, data = newdata)$predictions  # setting up custom predictor function needed, see https://github.com/christophM/iml/issues/103

if(eff_method == "pdp") {
  eff_data <- data_iml
} else {
  eff_data <- task_data_sp
}

d_p99 <- quantile(task_data_sp$Dq_mean0, probs = .995)
ba_p99 <- quantile(task_data_sp$ba0_m2, probs = .995)

predictor_iml <- Predictor$new(model = rf_localimp, #rf_localimp,
                               data = eff_data,
                               y = target_variable,
                               type="response",
                               predict.fun = pfun)

### D in different subsets ----

if(run_subsets) {
  # all
  predictor_iml_allD <- Predictor$new(model = rf_localimp,
                                      data = eff_data %>%
                                        filter(Dq_mean0 <= d_p99),
                                      y = target_variable,
                                      type="response",
                                      predict.fun = pfun)
  
  # tic("without future")
  future::plan(future::multisession, workers = parallelly::availableCores())
  eff_d_all <- FeatureEffect$new(predictor_iml_allD, feature=c("Dq_mean0"), method = eff_method)
  # toc()
  
  # Poland, pines
  predictor_iml_PolPines <- Predictor$new(model = rf_localimp, #rf_localimp,
                                          data = task_data_sp %>%
                                            filter(db_country == "NPO1.Pol" & species_classes2 == "Pinus"),
                                          y = target_variable,
                                          type="response",
                                          predict.fun = pfun)
  
  future::plan(future::multisession, workers = parallelly::availableCores())
  eff_d_PolPines <- FeatureEffect$new(predictor_iml_PolPines, feature=c("Dq_mean0"), method = eff_method,
                                      grid.size = grid_size_pdp)
  
  
  
  # Finland, south, pines
  predictor_iml_FinPines <- Predictor$new(model = rf_localimp, #rf_localimp,
                                          data = task_data_sp %>%
                                            filter(db_country == "FIN.Fin" &
                                                     species_classes2 == "Pinus" &
                                                     latitude < 65),
                                          y = target_variable,
                                          type="response",
                                          predict.fun = pfun)
  
  future::plan(future::multisession, workers = parallelly::availableCores())
  eff_d_FinPines <- FeatureEffect$new(predictor_iml_FinPines, feature=c("Dq_mean0"), method = eff_method)
  
  # Spain, pines
  predictor_iml_SpaPines <- Predictor$new(model = rf_localimp, #rf_localimp,
                                          data = task_data_sp %>%
                                            filter(
                                              db_country == "FUN.Spa" &
                                                species_classes2 == "Pinus" &
                                                Dq_mean0 <= d_p99),
                                          y = target_variable,
                                          type="response",
                                          predict.fun = pfun)
  
  future::plan(future::multisession, workers = parallelly::availableCores())
  eff_d_SpaPines <- FeatureEffect$new(predictor_iml_SpaPines, feature=c("Dq_mean0"), method = eff_method)
  
  
  eff_subsets <- rbind(eff_d_all$results %>%
                         mutate(data = "Europe, all sp."),
                       eff_d_PolPines$results %>%
                         mutate(data = "Poland, pines") ,
                       eff_d_FinPines$results %>%
                         mutate(data = "Finland, pines"),
                       eff_d_SpaPines$results %>%
                         mutate(data = "Spain, pines")) %>%
    filter(.class == "HARVEST")
  
  save(eff_subsets, file=effects_subsetsD_file)
  
  eff_datasets <- rbind(task_data_sp %>%
                          mutate(data = "Europe, all sp."),
                        task_data_sp %>%
                          mutate(data = "Poland, pines") %>%
                          filter(db_country == "NPO1.Pol" & species_classes2 == "Pinus"),
                        task_data_sp %>%
                          mutate(data = "Finland, pines") %>%
                          filter(db_country == "FIN.Fin" &
                                   species_classes2 == "Pinus" &
                                   latitude < 65),
                        task_data_sp %>%
                          mutate(data = "Spain, pines")  %>%
                          filter(db_country == "FUN.Spa" &
                                   species_classes2 == "Pinus")) %>%
    select(!harvest01) %>% distinct()
  
  
  ylim_range <- range(eff_subsets$.value)
  xlim_range <- c(min(eff_datasets$Dq_mean0), quantile((eff_datasets$Dq_mean0), prob=.95))
  
  
  gg_d_pines <- eff_subsets %>%
    ggplot(aes(Dq_mean0, .value, col=data)) +
    geom_line() +
    scale_color_brewer(palette = "Set2") +
    coord_cartesian(xlim=xlim_range,
                    ylim = ylim_range) +
    ylab(eff_method) + xlab("") +
    theme_minimal() + labs(color='Data') #+
  # ggtitle(paste("Target variable:", target_variable))
  
  gg_ddens_pines <- eff_datasets %>%
    ggplot(aes(Dq_mean0, fill=data, col=data)) +
    geom_density(alpha=.3) +
    scale_color_brewer(palette = "Set2") +
    scale_fill_brewer(palette = "Set2") +
    coord_cartesian(xlim=xlim_range) +
    theme_minimal() +
    theme(legend.position = "none") +
    ylab("Density") + xlab("Quadratic Mean DBH (mm)")
  
  
  gg_d_pines / gg_ddens_pines +
    plot_layout(heights = c(6, 1))
  
}


## Full data ----
# other variables


pred_names <- setdiff(predictor_names, c("Dq_mean0", exclude_cols))

iml_list_effects <- calculate_iml(model_iml = rf_localimp,
                          data_iml = data_iml,
                          target_variable = target_variable,
                          predictor_names = pred_names,
                          run_effects = run_effects,
                          run_interactions = FALSE,
                          save_to_file = effects_list_file)

# plot effects

eff_plot_lst <- list()
for(i in 1:length(iml_list_effects$effects)) {
  var_i <- names(iml_list_effects$effects)[i]
  if(!is.numeric(iml_list_effects$effects[[i]][,var_i])) {
    eff_plot_lst[[var_i]] <- ggplot(iml_list_effects$effects[[i]], aes_string(var_i, ".value")) +
      geom_point() + ggtitle(var_i)
  } else {
    eff_plot_lst[[var_i]] <- ggplot(iml_list_effects$effects[[i]], aes_string(var_i, ".value")) +
      geom_line() + ggtitle(var_i)
  }
}

wrap_plots(eff_plot_lst, ncol=3) &
  theme(axis.text.x = element_text(angle=45, hjust=1),
        axis.title.x = element_blank())
dev.off()


