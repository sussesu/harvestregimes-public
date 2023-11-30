############################################################'
##
## Regression RF: Harvest intensity (%BA)
##
############################################################'

rm(list=ls())

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
library(mlr3benchmark)
library(ranger)
library(data.table)

# sessionInfo()
set.seed(456)

# Set stuff ---------------------------------------------------------------

# input file for data?
input_fl <- "./data/processed/census_stats_final_22-08-23_RFready.rds"

# additional identifier for output files?
fileID_add <- ""

# Include only partial harvests?
only_partial <- FALSE

# tune hyperparameters or run with defaults?
tuneHP <- FALSE

# run model with only a subset of data?
run_subsample <- FALSE
n_subsample <- 1e4

run_fullmodel <- TRUE # fit model with full data (i.e., not only CV)?
run_CV <- TRUE # run cross-validation with spatial + nonspatial folds

num_threads_ranger <- 16

# model specifics
target_var <- "harvest_percent_ba" # name of target/response variable
msr_tuning <- msr("regr.rmse") # performance/error measure used for tuning

# output directory
out_dir <- paste0("./outputs/", target_var, "/")

# Prepare output dir & names ----------------------------------------------

# create output directory if it doesn't exist
if(!dir.exists(out_dir)) dir.create(out_dir)

# create output file identifier
fl_date <- format(Sys.Date(), "%d-%m-%y")
outfile_id <- paste0(target_var, "-",
                     ifelse(tuneHP, "tuned", "no_tuning"), "-",
                     ifelse(run_subsample, 
                            paste0("subset", n_subsample/1000,"k_"),  
                            "full-"), fileID_add)

# full model / hyperparameter tuning object filename
fl_name_tuning <- paste0(out_dir, "RF-TUNED-", outfile_id, fl_date, ".rds")

# full model / local importance object fit directly with ranger
fl_name_localimp <- paste0(out_dir, "RF-LOCALIMP-", outfile_id, fl_date, ".rds")

# CV results filename
fl_name_CV <- paste0(out_dir, "RF-CV-", outfile_id, fl_date, ".RData")
fl_name_CV_spatial <- paste0(out_dir, "RF-CV-spatial-", outfile_id, fl_date, ".RData")
fl_name_CV_country <- paste0(out_dir, "RF-CV-country-", outfile_id, fl_date, ".RData")

fl_name_CV_benchmark <- paste0(out_dir, "RF-CV-benchmark-", outfile_id, fl_date, ".RData")
fl_name_CV_benchmark_scores <- paste0(out_dir, "RF-CV-benchmark-scores-", outfile_id, fl_date, ".csv")


# task data filename
fl_name_taskdata <- paste0(out_dir, "TASKDATA-", outfile_id, fl_date, ".RData")


# Read, define, prepare data  --------------------------------------------------

data_census <- readRDS(input_fl)

# include only harvested data points
data_census <- data_census %>%
  filter(harvest_any == TRUE)

##### Variables  ----

preds_rf <- c("Dq_mean0",
              "ba0_m2",
              "gini_d_nplot0",
              
              "ba_percent_dom0", 
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

vars_rf <- c(target_var, # target
             "db_country", # added to be used in country-wise CV
             "rep_area_weights", # add for weights
             preds_rf)



all(vars_rf%in%colnames(data_census))
summary(data_census[, vars_rf]) # 
dim(data_census)

#include only partial harvests?
if(only_partial) {
  data_census <- data_census %>%
    filter(harvest_percent_ba < 1)
}

# RF definitions -----------------------------------------------------------------
# following mlr3 book, https://mlr3book.mlr-org.com/

## Defining task & learner -------------------------------------------------

# prepare task data and save to file
task_data_nsp <- data_census %>%
  dplyr::select(all_of(vars_rf)) %>%
  na.omit()
dim(task_data_nsp)

task_data_sp <- data_census %>%
  dplyr::select(all_of(c(vars_rf, "longitude", "latitude"))) %>%
  na.omit()

if(run_subsample) {
  idx_subsample <- sample(1:nrow(task_data_sp), size = n_subsample)
  task_data_nsp <- task_data_nsp[idx_subsample,]
  task_data_sp <- task_data_sp[idx_subsample,]
}

dim(task_data_nsp)
dim(task_data_sp)

save(task_data_nsp, task_data_sp, file=fl_name_taskdata)

# create a task - non-spatial
task_nsp <- as_task_regr(task_data_nsp, target = target_var)

task_nsp$col_roles$feature <- preds_rf
task_nsp$col_roles$weights <- "rep_area_weights"

# create a task - spatial
task_sp <- TaskRegrST$new("harvest_sp",
                          backend = task_data_sp, 
                          target = target_var,
                          extra_args = list(coordinate_names = c("longitude", "latitude"), 
                                            crs = "EPSG:4326",
                                            coords_as_features = FALSE) )

task_sp$col_roles$feature <- preds_rf
task_sp$col_roles$weights <- "rep_area_weights"

# create a learner
# full list of possible learners: 
# https://mlr3extralearners.mlr-org.com/articles/learners/list_learners.html

rf_learner <- lrn("regr.ranger", 
                  respect.unordered.factors = "order", #"partition",
                  predict_type = "response",
                  importance = "permutation",
                  num.threads = num_threads_ranger,
                  num.trees = 300)
print(rf_learner)

## Define resampling ----
search_space <- ps(
  mtry = p_int(lower = 2, upper = round((ncol(task_data_sp)-3)*0.8)),
  replace = p_lgl(),
  sample.fraction = p_dbl(lower=.25, upper=1))

print(search_space)

# Automated tuning
# - inner resampling
at <- AutoTuner$new(
  learner = rf_learner,
  resampling = rsmp("cv", folds = 5),
  measure = msr_tuning,
  search_space = search_space,
  terminator = trm("evals", n_evals = 25),
  tuner = tnr("random_search") 
  )

at_spatial <- AutoTuner$new(
  learner = rf_learner,
  resampling = rsmp("spcv_block", cols=10, rows=10, folds = 5, selection="systematic"), #rsmp("holdout"),
  measure = msr_tuning,
  search_space = search_space,
  terminator = trm("evals", n_evals = 25),
  tuner = tnr("random_search") 
)

print(at)

## Model accuracy ----
# - outer resampling, spatial + non-spatial + country
outer_resampling_nsp <- rsmp("cv", folds = 10)
outer_resampling_nsp$instantiate(task_nsp)

outer_resampling_sp <- rsmp("spcv_block", cols=10, rows=10, folds = 10, selection="systematic")
data_sf <- st_as_sf(data_census, coords = c("longitude", "latitude"), crs = "EPSG:4326")
outer_resampling_sp$instantiate(task_sp)

outer_resampling_country <- rsmp("custom_cv")
outer_resampling_country$instantiate(task_nsp, col = "db_country")

task_country <- task_nsp

# if(run_CV) {
#   pdf(paste0(out_dir, "Spatial_blocks_", fl_date, ".pdf"))
#   blockCV::spatialBlock(data_sf, rows = 10, cols = 10, k=10, showBlocks = TRUE, selection="systematic")
#   dev.off()
# }

## Define benchmarking full model + featureless model ----

# bm_design <- benchmark_grid(
#   tasks = list(task_nsp, task_sp),
#   learners = list(rf_learner, lrn("regr.featureless")),   # predict all to mean and st.error to sd
#   resamplings = list(outer_resampling_nsp, outer_resampling_sp, outer_resampling_country)
# )

bm_tasks <- list(task_nsp, task_sp, task_country, task_nsp, task_sp, task_country)
bm_learners <- list(rf_learner, rf_learner, rf_learner, 
                    lrn("regr.featureless"), lrn("regr.featureless"), lrn("regr.featureless"))   # predict all to mean and st.error to sd
bm_resamplings <- list(outer_resampling_nsp, outer_resampling_sp, outer_resampling_country,
                       outer_resampling_nsp, outer_resampling_sp, outer_resampling_country)

bm_design <- data.table(task = bm_tasks, learner = bm_learners,
                      resampling = bm_resamplings)

# Fit full model --------------------------------------------------------------

# We can use the AutoTuner to tune the hyperparameters of our learner and
# fit the final model on the full data set.

harvest_formula <- as.formula(paste(target_var, "~ ."))

if(run_fullmodel) {
  set.seed(645)
  
  if(tuneHP) {
  # fit and tune full model
  at_spatial$train(task_sp)
  
  param_values <- at_spatial$learner$param_set$values
  
  # refit with tuned hyperparameters to get local importance
  rf_localimp <- ranger(harvest_formula,
                        data = task_data_nsp %>% select(any_of(c(target_var, preds_rf))), #select(!any_of(c("db_country"))),
                        num.trees = param_values$num.trees,
                        mtry = param_values$mtry,
                        importance = param_values$importance,
                        replace = param_values$replace,
                        sample.fraction = param_values$sample.fraction,
                        respect.unordered.factors = "order", #"partition",
                        num.threads = 4, 
                        case.weights = "rep_area_weights")
  
  saveRDS(at_spatial, file=fl_name_tuning)
  saveRDS(rf_localimp, file=fl_name_localimp)
  }
  if(!tuneHP) {
    rf_localimp <- ranger(harvest_formula,
                          data = task_data_nsp %>% select(any_of(c(target_var, preds_rf))),# %>% select(!any_of(c("db_country"))),
                          respect.unordered.factors = "order", #"partition",
                          importance = "permutation",
                          num.threads = 4, 
                          case.weights = "rep_area_weights")
    saveRDS(rf_localimp, file=fl_name_localimp)
  }
}

# Cross-validation --------------------------------------------------------

if(tuneHP) {
  cv_learner <- at
} else {
  cv_learner <- rf_learner
}


if(run_CV) {
  set.seed(645)
  if(!tuneHP) {
    bmr <- benchmark(bm_design)
    
    bmr_measures <- c("regr.rsq", "regr.rmse", "regr.srho", "regr.ktau")
    agg_measures <- bmr$aggregate(measures = msrs(bmr_measures))
    print(agg_measures)
    
    bmr_scores <- bmr$score(measures = msrs(bmr_measures))
    
    # bmr$resample_result(uhash="7b35d306-1c43-40f7-9309-3f29ac389efb")$prediction() # check predictions for featureless
    
    
    # ggplot(bmr_scores, aes(resampling_id, classif.auc, col=learner_id)) +
    #   geom_boxplot()
    
    write.csv(bmr_scores %>% 
                select(task_id, learner_id, resampling_id, .vars=bmr_measures),
              file=fl_name_CV_benchmark_scores)
    
    save(bmr, file = fl_name_CV_benchmark)
  }
  if(tuneHP) {
    rr_nsp <- mlr3::resample(task_nsp,
                             cv_learner,
                             outer_resampling_nsp,
                             store_models = TRUE)
    save(rr_nsp, file=fl_name_CV)
    print("\n non-spatial CV ready \n")
    
    rr_sp <- mlr3::resample(task_sp,
                            cv_learner,
                            outer_resampling_sp,
                            store_models = TRUE)
    save(rr_sp, file=fl_name_CV_spatial)
    print("\n spatial CV ready \n")
    
    rr_country <- mlr3::resample(task_nsp,
                                 cv_learner,
                                 outer_resampling_country,
                                 store_models = FALSE)
    save(rr_country, file=fl_name_CV_country)
    print("\n country CV ready \n")
  
  
    extract_inner_tuning_results(rr_nsp)
    extract_inner_tuning_results(rr_sp)
    extract_inner_tuning_results(rr_country)
  
    # The aggregated performance of all outer resampling iterations is
    # essentially the unbiased performance of the model with optimal 
    # hyperparameters found by random search.
    
    print(rr_nsp$score(msr_tuning))
    print(rr_sp$score(msr_tuning))
    print(rr_country$score(msr_tuning))
    
    print(rr_nsp$aggregate(msr_tuning))
    print(rr_sp$aggregate(msr_tuning))
    print(rr_country$aggregate(msr_tuning))
    
    print(object.size(rr_sp))
  }
}

