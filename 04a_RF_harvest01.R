############################################################'
##
## Classification of plots to:
##   (0) NO-HARVEST
##   (1) HARVEST
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
input_fl <- "./data/processed/data_annual_06-08-24_RFready.rds"

# additional identifier for output files? (end with "-" for neat filenames...)
fileID_add <- "newWeights_"

# tune hyperparameters or run with defaults?
tuneHP <- FALSE

# run model with only a subset of data?
run_subsample <- FALSE
n_subsample <- 1e4

run_fullmodel <- TRUE # fit model with full data (i.e., not only CV)?
run_CV <- TRUE # run cross-validation with spatial (+ nonspatial folds - these now commented out)

# balance harvest and non-harvest classes?
class_balancing <- FALSE 
balancing_ratio <- 1/20

num_threads_ranger <- 16

# output directory
out_dir <- "./outputs/harvest01/"

# model specifics
target_var <- "harvest01" # name of target/response variable
msr_tuning <- msr("classif.auc") #classif.auc" # performance/error measure used for tuning


# Prepare output dir & names ----------------------------------------------

# create output directory if it doesn't exist
if(!dir.exists(out_dir)) dir.create(out_dir)

# create output file identifier
fl_date <- format(Sys.Date(), "%d-%m-%y")
outfile_id <- paste0(target_var, "-",
                     ifelse(tuneHP, "tuned", "no_tuning"), "-",
                     ifelse(run_subsample, 
                            paste0("subset", n_subsample/1000,"k-"),  
                            "full-"), 
                     ifelse(class_balancing,
                            "balanced-",
                            ""),
                     fileID_add)

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
fl_name_rangerdata <- paste0(out_dir, "rangerDATA-", outfile_id, fl_date, ".csv")


# Read, define, prepare data  --------------------------------------------------

data_annual <- readRDS(input_fl)

dim(data_annual)
prop.table(table(data_annual[,target_var]))

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

vars_rf <- c("harvest01", # target
             "tmt.census.id",    # included as a grouping variable (to group annualized rows from same obs)
             "db_country", #included to be used in the country-level CV
             "rep_area_weights", #included for weights
             preds_rf)



all(vars_rf%in%colnames(data_annual))
summary(data_annual[, vars_rf]) # 
dim(data_annual)



# RF definitions -----------------------------------------------------------------
# following mlr3 book, https://mlr3book.mlr-org.com/

## Defining task & learner -------------------------------------------------

# prepare task data and save to file
task_data_nsp <- data_annual %>%
  dplyr::select(all_of(vars_rf)) %>%
  na.omit()
dim(task_data_nsp)

task_data_sp <- data_annual %>%
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

# create a task
task_nsp <- as_task_classif(task_data_nsp, 
                            target = target_var, 
                            positive="HARVEST")

task_nsp$col_roles$feature <- preds_rf
task_nsp$col_roles$group <- "tmt.census.id"
task_nsp$col_roles$weights <- "rep_area_weights"

task_sp <- TaskClassifST$new("harvest_sp",
                             backend = task_data_sp, 
                             target = target_var,
                             extra_args = list(coordinate_names = c("longitude", "latitude"), 
                                                                  crs = "EPSG:4326",
                                               coords_as_features = FALSE) )

task_sp$col_roles$feature <- preds_rf
task_nsp$col_roles$weights <- "rep_area_weights"

# create a learner
# full list of possible learners: 
# https://mlr3extralearners.mlr-org.com/articles/learners/list_learners.html

rf_learner <- lrn("classif.ranger", 
                  respect.unordered.factors = "order", #"partition",
                  num.trees = 300,
                  predict_type = "prob",
                  importance = "permutation",
                  num.threads = num_threads_ranger)
print(rf_learner)

## Undersample majority class (HARVEST) ----
if(class_balancing) {
  po_under = po("classbalancing",
                id = "undersample", adjust = "major",
                reference = "major", shuffle = FALSE, ratio = balancing_ratio)
  # reduce majority class by factor '1/ratio'
  table(po_under$train(list(task_nsp))$output$truth())
  
  # combine learner with pipeline graph
  learner_under = as_learner(po_under %>>% rf_learner)
  learner_under$id = "undersample.ranger"
  
  rf_learner <- learner_under
}


## Define resampling ----
search_space <- ps(
  classif.ranger.mtry = p_int(lower = 2, upper = round((ncol(task_data_sp)-3)*0.8)),
  classif.ranger.replace = p_lgl(),
  classif.ranger.sample.fraction = p_dbl(lower=.25, upper=1))

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
data_sf <- st_as_sf(data_annual, coords = c("longitude", "latitude"), crs = "EPSG:4326")
outer_resampling_sp$instantiate(task_sp)

task_country <- task_nsp

outer_resampling_country <- rsmp("custom_cv")
outer_resampling_country$instantiate(task_country, col = "db_country")

# if(run_CV) {
#   pdf(paste0(out_dir, "Spatial_blocks_", fl_date, ".pdf"))
#   blockCV::spatialBlock(data_sf, rows = 10, cols = 10, k=10, showBlocks = TRUE, selection="systematic")
#   dev.off()
# }

## Define benchmarking full model + featureless model ----


featless_learner <- lrn("classif.featureless", method="weighted.sample", predict_type = "prob")

bm_tasks <- list(task_nsp, task_sp, task_country, task_nsp, task_sp, task_country)
bm_learners <- list(rf_learner, rf_learner, rf_learner, 
                    featless_learner, featless_learner, featless_learner)   # predict all to mean and st.error to sd
bm_resamplings <- list(outer_resampling_nsp, outer_resampling_sp, outer_resampling_country,
                       outer_resampling_nsp, outer_resampling_sp, outer_resampling_country)

bm_design <- data.table(task = bm_tasks, learner = bm_learners,
                        resampling = bm_resamplings)

# Fit full model --------------------------------------------------------------

# We can use the AutoTuner to tune the hyperparameters of our learner and
# fit the final model on the full data set.

harvest_formula <- as.formula(paste(target_var, "~ ."))

# create manually balanced data set for RF runs directly from rangers
if(class_balancing) {
  data_ranger1 <- task_data_nsp %>%
    filter(harvest01 == "HARVEST")
  
  data_ranger0 <- task_data_nsp %>%
    filter(harvest01 == "NO-HARVEST") %>%
    slice_sample(n = round(nrow(task_data_nsp)*(balancing_ratio)))
  
  data_ranger <- data_ranger0 %>%
    rbind(data_ranger1) %>% 
    select(any_of(c(target_var, preds_rf)))
  
  table(data_ranger$harvest01)
} else {
  data_ranger <- task_data_nsp %>% select(any_of(c(target_var, preds_rf)))
}

if(run_fullmodel) {
  set.seed(645)
  
  if(tuneHP) {
    # fit and tune full model
    at_spatial$train(task_sp)
    
    param_values <- at_spatial$learner$param_set$values
    
    # refit with tuned hyperparameters to get local importance --  no need for local importance no with just 2 classes??
    rf_localimp <- ranger(harvest_formula,
                          data = data_ranger, #task_data_nsp %>% select(any_of(c(target_var, preds_rf))), #select(!any_of(c("tmt.census.id", "db_country"))),
                          num.trees = param_values$classif.ranger.num.trees,
                          mtry = param_values$classif.ranger.mtry,
                          importance = param_values$classif.ranger.importance,
                          local.importance = FALSE,
                          replace = param_values$classif.ranger.replace,
                          sample.fraction = param_values$classif.ranger.sample.fraction,
                          respect.unordered.factors = "order", #"partition",
                          num.threads = 4,
                          probability=TRUE, 
                          case.weights = "rep_area_weights")

    saveRDS(at_spatial, file=fl_name_tuning)
    saveRDS(rf_localimp, file=fl_name_localimp)
    if(class_balancing) write.csv(data_ranger, file = fl_name_rangerdata, row.names=FALSE)
  }
  if(!tuneHP) {
    rf_localimp <- ranger(harvest_formula,
                          data = data_ranger, #task_data_nsp %>% select(any_of(c(target_var, preds_rf))), # select(!any_of(c("tmt.census.id", "db_country"))),
                          local.importance = TRUE,
                          respect.unordered.factors = "order", #"partition",
                          importance = "permutation",
                          num.threads = 4,
                          probability=TRUE, 
                          case.weights = "rep_area_weights")
    saveRDS(rf_localimp, file=fl_name_localimp)
    if(class_balancing) write.csv(data_ranger, file = fl_name_rangerdata, row.names=FALSE)
  }
}

# Cross-validation --------------------------------------------------------

if(tuneHP) {
  cv_learner <- at
  cv_learner_sp <- at_spatial
} else {
  cv_learner <- rf_learner
  cv_learner_sp <- rf_learner
}

if(run_CV) {
  set.seed(645)
  
  if(!tuneHP) {
  bmr <- benchmark(bm_design, store_models = FALSE)
  
  bmr_measures <- c("classif.auc", "classif.prauc")
  
  agg_measures <- bmr$aggregate(measures = msrs(bmr_measures))
  print(agg_measures)
  
  bmr_scores <- bmr$score(measures = msrs(bmr_measures))
  
  # ggplot(bmr_scores, aes(resampling_id, classif.auc, col=learner_id)) +
  #   geom_boxplot()
  
  # bmr$resample_result(uhash="9737145c-1e37-42b0-9b42-7b79ad1e9f2d")$prediction() # check predictions for featureless
  write.csv(bmr_scores %>% 
              select(task_id, learner_id, resampling_id, .vars=bmr_measures),
            file=fl_name_CV_benchmark_scores)
  
  save(bmr, file = fl_name_CV_benchmark)
  }

  if(tuneHP) {
    rr_nsp <- mlr3::resample(task_nsp,
                             cv_learner,
                             outer_resampling_nsp,
                             store_models = FALSE)

    rr_sp <- mlr3::resample(task_sp,
                            cv_learner_sp,
                            outer_resampling_sp,
                            store_models = FALSE)

    rr_country <- mlr3::resample(task_nsp,
                            cv_learner,
                            outer_resampling_country,
                            store_models = FALSE)
    
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
    
    save(rr_nsp, file=fl_name_CV)
    save(rr_sp, file=fl_name_CV_spatial)
    save(rr_country, file=fl_name_CV_country)
  }
  
}
