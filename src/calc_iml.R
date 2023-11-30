#' Calculate effects and interactions with iml package
#'
#' @param model_iml model object to be used (e.g. ranger object)
#' @param data_iml the data used for calculating effects and interactions (data frame)
#' @param target_variable target variable of the model (character vector length 1)
#' @param predictor_names predictors for which to calculate the effects and interactions 
#' (character vector)
#' @param interaction_vars predictors for which the individual interaction strengths are calculated
#' @param run_effects calculate effects (will skip effects calc if this FALSE)
#' @param run_interactions calculate interactions (will skip interactions calc if this FALSE)
#' @param save_to_file character vector with filename for output, will be saved after each major calculation. If NULL, no files saved.
#' @param ... additional arguments passed to FeatureEffect$new and Interaction$new
#'
#' @return
#' @export
#'
#' @examples
calculate_iml <- function(model_iml, data_iml, target_variable, 
                     predictor_names, interaction_vars =  NULL,
                     run_effects = TRUE, run_interactions = TRUE,
                     save_to_file = NULL, ...) {
  
  ## Set-up
  pfun <- function(object, newdata) predict(object, data = newdata)$predictions  # setting up custom predictor function needed, see https://github.com/christophM/iml/issues/103
  
  predictor_iml <- Predictor$new(model = model_iml, #rf_localimp, 
                                 data = data_iml,
                                 y = target_variable,
                                 type="response",
                                 predict.fun = pfun)
  out_lst <- list()
  
  # print(paste("Number of available cores (used as number of workers in future::plan(...) ): ",parallelly::availableCores()))
  
  ## Effects
  if (run_effects) {
    eff_lst <- list()
    for(i in 1:length(predictor_names)) {
      pred_i <- predictor_names[i]
      print(pred_i)
      # future::plan(future::multisession, workers = parallelly::availableCores())
      eff_lst[[pred_i]] <- FeatureEffect$new(predictor_iml, feature=pred_i, method = "pdp", ...)$results
      out_lst[["effects"]] <- eff_lst
      if(!is.null(save_to_file)) {
        save(out_lst, file=save_to_file)
      }
    }
  }

  ## Interactions
  if (run_interactions) {
    future::plan(future::multisession, workers = parallelly::availableCores())
    
    print("Start: interactions all")
    # Overall strength of interaction for all predictors
    interact_all <- Interaction$new(predictor_iml, ...)
    
    out_lst[["interactions"]] <- list(all_features = interact_all)
    if(!is.null(save_to_file)) {
      save(out_lst, file=save_to_file)
    }
    
    # Individual interaction strengths for chosen predictors
    if(is.null(interaction_vars) ) {
      message("No interaction_vars given, only general strength of all variable interaction calculated")
    } else {
      interaction_lst <- list()
      for(i in 1:length(interaction_vars)) {
        pred_i <- interaction_vars[i]
        print(paste("Start: interactions for", pred_i))
        future::plan(future::multisession, workers = parallelly::availableCores())
        interaction_lst[[pred_i]] <- Interaction$new(predictor_iml, 
                                                     feature=pred_i, 
                                                     ...)
        out_lst[["interactions"]] <- list(all_features = interact_all, 
                                          individual_features = interaction_lst)
        if(!is.null(save_to_file)) {
          save(out_lst, file=save_to_file)
        }
      }

    }
    
  }
  if(!is.null(save_to_file)) {
    save(out_lst, file=save_to_file)
  }
  return(out_lst)
}
