############################################################'
##
## Checking and visualizing results 
## from the harvest intensity RF
##
############################################################'

# update 24-8-23

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

source("./src/calc_iml.R")
source("./src/custom_iml_plots.R")

set.seed(798)

sessionInfo()

varimp_to_df <- function(x, id) {
  x_df <- data.frame(variable = names(x),
                     importance= x,
                     imp_type = id,
                     row.names=NULL)
}

# Set things & prepare --------------------------------------------------------------

target_variable <- "harvest_percent_ba"

fl_date <- format(Sys.Date(), "%d-%m-%y")

msr_cv <-  msr("regr.rmse")

out_dir <- paste0("./outputs/",
                  target_variable,
                  "/ml_interpretation/")

run_cv <- FALSE
run_effects <- TRUE
run_interactions <- FALSE
run_subsets <- FALSE

subset_iml_effects <- 5e4 # NULL if run with full data, otherwise numeric giving the size of subsample

# effects method
eff_method <- "pdp"

# folder for input files
input_dir <- paste0("./outputs/", target_variable, "/")
run_details <- "-no_tuning-full-24-08-23"

# input file names
input_fullmodel <-  paste0(input_dir, "RF-LOCALIMP-", target_variable, run_details, ".rds")
input_taskdata <- paste0(input_dir, "TASKDATA-", target_variable, run_details, ".RData")  

input_CV <- paste0(input_dir, "RF-CV-", target_variable,  run_details, ".RData")
input_CV_spatial <- paste0(input_dir, "RF-CV-spatial-", target_variable, run_details, ".RData")
input_CV_country <- paste0(input_dir, "RF-CV-country-", target_variable,  run_details, ".RData")

input_CV_benchmark_scores <- paste0(input_dir, "RF-CV-benchmark-scores-", target_variable, run_details, ".csv")


### Output filenames --------------------------------------------------------

if(!dir.exists(out_dir)) dir.create(out_dir)

cv_pdf <- paste0(out_dir, "CV_results_", fl_date, ".pdf")
varimp_pdf <- paste0(out_dir, "varimp_results_", fl_date, ".pdf")
eff_pdf <- paste0(out_dir, "effplot_results_", fl_date, ".pdf")
interaction_pdf <- paste0(out_dir, "interaction_results_", fl_date, ".pdf")
pred_vs_obs_pdf <- paste0(out_dir, "pred_vs_obs_", fl_date, ".pdf")
cv_benchmark_pdf <- paste0(out_dir, "CV_benchmark_results_", fl_date, ".pdf")


localEff_pdf <- paste0(out_dir, "local_eff_", fl_date, ".pdf")

cv_df_file <- paste0(out_dir, "CV_results_", fl_date, ".RData")
varimp_df_file <- paste0(out_dir, "varimp_results_", fl_date, ".RData")
effects_list_file <- paste0(out_dir, "effects_results_", fl_date, ".RData")
effects_subsetsD_file <- paste0(out_dir, "effects_subsetsD_", fl_date, ".RData")
effects_subsetsBA_file <- paste0(out_dir, "effects_subsetsBA_", fl_date, ".RData")
interactions_list_file <- paste0(out_dir, "interactions_results_", fl_date, ".RData")

### Read result files  --------------------------------------------------

# -- full model, local importance --
rf_localimp <- readRDS(input_fullmodel)

# -- data --
load(input_taskdata)

# -- CV results --
if(run_cv) {
  if(file.exists(input_CV)) {
    print("loading CV non-spatial")
    load(input_CV) 
    }
  if(file.exists(input_CV_spatial)) {
    print("loading CV spatial")
    load(input_CV_spatial) 
  }
  if(file.exists(input_CV_country)) {
    print("loading CV country")
    load(input_CV_country) 
  }
}

cv_bmscores <- read.csv(input_CV_benchmark_scores)

# Cross-validation --------------------------------------------------------


## RF2 ----

cv_bmscores <- cv_bmscores %>%
  mutate(CV_type = case_when(
    resampling_id == "cv" ~ "Random",
    resampling_id == "spcv_block" ~ "Spatial",
    resampling_id == "custom_cv" ~ "Country"),
    CV_type = factor(CV_type, levels = c("Random", "Spatial", "Country")),
    Model = ifelse(learner_id == "regr.featureless", "Featureless", "Full"),
    rf = "RF2 - Intensity of harvest")

gg_cv_1 <- ggplot(cv_bmscores, aes(CV_type, .vars1, col=Model)) +
  geom_boxplot() +
  ylab("R-squared") + xlab("Cross-validation type") +
  theme_bw() +
  coord_cartesian(ylim=c(-0.4, 0.5)) +
  ggtitle("C")

gg_cv_2 <- ggplot(cv_bmscores, aes(CV_type, .vars2, col=Model)) +
  geom_boxplot() +
  ylab("RMSE") + xlab("Cross-validation type") +
  theme_bw() +
  ggtitle("D")

cv_bmscores %>% 
  group_by(resampling_id, Model) %>% 
  summarise(median_Rsq = median(.vars1),
            median_RMSE = median(.vars2),
            mean_Rsq = mean(.vars1),
            mean_RMSE = mean(.vars2))

gg_cv_1 + gg_cv_2

# Feature importance  ----------------------------------------------------

##### All, ranger ----

# RFmodel <- at$learner$model
imp_df <- varimp_to_df(importance(rf_localimp), "all")

gg_varimp <- ggplot(imp_df, aes(importance, reorder(variable, importance))) +
  geom_point() +
  geom_line(data = rbind(imp_df, imp_df %>% mutate(importance = 0)), 
            aes(importance, variable, group=variable)) +
  xlab("") + ylab("") +
  ggtitle(paste0("Variable importance\n(", target_variable, ")")) +
  theme_bw()

# #save data frame
# save(imp_df, file=varimp_df_file)
# 
# save figure
# pdf(file = varimp_pdf, width=6, height=5)
print(gg_varimp)
# dev.off()


# iml plots ---------------------------------------------------------------------

predictor_names <- imp_df %>% arrange(desc(importance)) %>% pull(variable)

if(is.null(subset_iml_effects)) {
  data_iml <- task_data_sp 
} else {
  data_iml <- task_data_sp %>%  slice_sample(n = subset_iml_effects, weight_by = rep_area_weights)
  # data_iml <- task_data_sp %>%  slice_sample(n = subset_iml_effects)
}

dim(data_iml)

## Manual subsets ----

if(run_subsets) {
  pfun <- function(object, newdata) predict(object, data = newdata)$predictions  # setting up custom predictor function needed, see https://github.com/christophM/iml/issues/103
  
  if(eff_method == "pdp") {
    eff_data <- data_iml
  } else {
    eff_data <- task_data_sp
  }
  
  dim(eff_data)
  
  d_p99 <- quantile(task_data_sp$Dq_mean0, probs = .995)
  ba_p99 <- quantile(task_data_sp$ba0_m2, probs = .995)
  
  predictor_iml <- Predictor$new(model = rf_localimp, 
                                 data = eff_data,
                                 y = target_variable,
                                 type="response",
                                 predict.fun = pfun)
  
  ### D in different subsets ----
  
  # all
  predictor_iml_allD <- Predictor$new(model = rf_localimp, 
                                      data = eff_data %>%
                                        filter(Dq_mean0 <= d_p99),
                                      y = target_variable,
                                      type="response",
                                      predict.fun = pfun)
  
  future::plan(future::multisession, workers = parallelly::availableCores())
  eff_d_all <- FeatureEffect$new(predictor_iml_allD, feature=c("Dq_mean0"), method = eff_method)
  
  # Poland, pines
  predictor_iml_PolPines <- Predictor$new(model = rf_localimp, #rf_localimp, 
                                          data = task_data_sp %>%
                                            filter(db_country == "NPO1.Pol" & species_classes2 == "Pinus"),
                                          y = target_variable,
                                          type="response",
                                          predict.fun = pfun)
  
  future::plan(future::multisession, workers = parallelly::availableCores())
  eff_d_PolPines <- FeatureEffect$new(predictor_iml_PolPines, feature=c("Dq_mean0"), method = eff_method)
  
  
  
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
  
  # Plot figures
  
  eff_subsets <- rbind(eff_d_all$results %>%
                         mutate(data = "Europe, all species"),
                       eff_d_PolPines$results %>%
                         mutate(data = "Poland, pines") ,
                       eff_d_FinPines$results %>%
                         mutate(data = "Finland, pines"),
                       eff_d_SpaPines$results %>%
                         mutate(data = "Spain, pines")) 
  
  eff_datasets <- rbind(task_data_sp %>%
                          mutate(data = "Europe, all species"),
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
                                   species_classes2 == "Pinus"))
  
  
  save(eff_subsets, file=effects_subsetsD_file)
  
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
  
  pdf(file=localEff_pdf, height = 5, width = 6)
  gg_d_pines / gg_ddens_pines +  
    plot_layout(heights = c(6, 1))
  dev.off()
}

## Full data ----

# run only ba here, not run in the prev round...
pred_names <- "ba0_m2" # setdiff(predictor_names, c("Dq_mean0"))

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

pdf(file=eff_pdf)
wrap_plots(eff_plot_lst, ncol=3) & 
  theme(axis.text.x = element_text(angle=45, hjust=1),
        axis.title.x = element_blank())
dev.off()

# OLD STUFF ?? ####### ---------------------------------
# 
# #effects
# iml_list_effects <- calculate_iml(model_iml = rf_localimp,
#                           data_iml = data_iml,
#                           target_variable = target_variable,
#                           predictor_names = predictor_names,
#                           run_effects = run_effects,
#                           run_interactions = FALSE,
#                           save_to_file = effects_list_file)
# 
# # save effects-list to file
# save(iml_list_effects, file=effects_list_file)
# 
# # plot effects
# df_values <- do.call(rbind, lapply(map(iml_list_effects$effects, "results"), select, .value))
# y_limits <- df_values %>% 
#   summarize(minVal = min(.value),
#             maxVal = max(.value))
# eff_plot_lst <- lapply(iml_list_effects$effects, function(x) plot(x) + 
#                          ylim(c(0, y_limits$maxVal)) +
#                          theme(axis.text.x = element_text(angle=25, hjust=1, size=6))  )
# 
# pdf(file=eff_pdf)
# wrap_plots(eff_plot_lst, ncol=3)
# 
# wrap_plots(lapply(iml_list_interactions$effects_2feature, plot_effect2), ncol=2)
# 
# dev.off()
# 
# # interactions
# iml_list_interactions <- calculate_iml(model_iml = rf_localimp,
#                           data_iml = data_iml,
#                           target_variable = target_variable,
#                           predictor_names = predictor_names,
#                           run_effects = FALSE,
#                           run_interactions = run_interactions,
#                           save_to_file = interactions_list_file,
#                           interaction_vars = c("SDI0", "Dq_mean0"))
# 
# 
# if(run_interactions) {
#   # save effects-list to file
#   save(iml_list_interactions, file=interactions_list_file)
#   
#   # plot interactions
#   pdf(file=interaction_pdf, width=7, height=6)
#   plot_interaction(iml_list_interactions$interactions$all_features) +
#     ggtitle("All features, strength of interactions")
#   
#   wrap_plots(lapply(iml_list_interactions$interactions$individual_features, plot), ncol=1)
#   
#   dev.off()
# } 

# Predictions vs observations ---------------------------------------------
# 
# preds <- predict(rf_localimp, data=task_data_sp)
# 
# df_preds <- data.frame(observed_intensity = task_data_sp$harvest_percent_ba,
#                        predicted_intensity = preds$predictions)
# 
# gg1 <- ggplot(df_preds, aes(observed_intensity, predicted_intensity)) +
#   geom_abline(intercept = 0, slope=1, col=2, lwd=1) +
#   geom_point(size=1) +
#   geom_smooth(method = "lm", lty=2, se=FALSE) 
# 
# gg2 <- ggplot(df_preds %>% filter(observed_intensity == 1),
#        aes(predicted_intensity)) +
#   geom_density(fill="gray50", alpha=.5) +
#   xlab("Predicted intensity, when observed = 1")
# 
# pdf(file = pred_vs_obs_pdf)
# gg1 + gg2
# dev.off()

