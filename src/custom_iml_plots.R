#' Custom effect and interactions plots from iml objects
#'
#' @param effect_iml 
#'
#' @return
#' @export
#'
#' @examples

plot_effect <- function(effect_iml) {
  variable_name <- effect_iml$feature.name
  type <- effect_iml$feature.type
  if(type == "numerical") {
    rug_data <- task_data_nsp %>% 
      slice_sample(n=1e4)
    eff_data <- effect_iml$results %>%
      filter(.class == "HARVEST")
    gg_out <- ggplot(data = eff_data, aes_string(variable_name, ".value")) + 
      geom_line()  +
      geom_rug(data = rug_data, aes_string(variable_name), inherit.aes=FALSE) +
      ylab("Effect")
  }
  if(type == "categorical") {
    eff_data <- effect_iml$results %>%
      filter(.class == "HARVEST")
    gg_out <- ggplot(data = eff_data, aes_string(variable_name, ".value")) + 
      geom_bar(stat="identity") +
      ylab("Effect")
  }
  return(gg_out)
}

plot_interaction <- function(interaction_iml){
  int_data <- interaction_iml$results %>%
    filter(.class == "HARVEST") %>%
    mutate(datapoint = TRUE)  #an actual data point
  
  int_data0 <- int_data %>% # create rows with 0 values to draw the lines
    mutate(.interaction = 0) %>%
    mutate(datapoint = FALSE) # not an actual data point, just to draw the line
  int_data <- rbind(int_data, int_data0)
  
  #exclude features not used as predictors
  int_data <- int_data %>%
    mutate(interaction_features =  word(.feature, 1, sep=":")) %>%
    filter(!interaction_features %in% c("latitude", "longitude", "tmt.census.id"))
    
  # draw plot
  gg_out <- ggplot(int_data %>% filter(datapoint), 
                   aes(.interaction, reorder(.feature, .interaction))) + 
    geom_point() +
    geom_line(data=int_data, aes(.interaction, group = .feature))
  
  return(gg_out)
}

plot_effect2 <- function(effect2_iml) {
  effect2_iml$results <- effect2_iml$results %>% filter(.class == "HARVEST")
  gg_2feat <- plot(effect2_iml) + viridis::scale_fill_viridis(option="D") 
  return(gg_2feat)
}
