# Not ready!!!

plot_freq_int <- function(map_data_all, color_var) {
  freq_int_totalHarvBA <- map_data_all %>%
    arrange(pann_mean) %>%
    ggplot(aes(harvest_percent_ba_av, harvest_pros*100, 
               col = ba_perc_HARVEST*100)) + 
    geom_point(size=3) + 
    scale_colour_viridis(direction = -1, discrete = FALSE, option="viridis", name="Pann") +
    theme_bw() +
    coord_cartesian(ylim=c(0, 14), xlim=c(11, 100)) +
    ylab("Frequency of harvest events") +
    theme(legend.position = "none",
          axis.title.x = element_blank(),
          axis.title.y = element_text(hjust=0.5),
          plot.margin = margin(t = 5, r = 10, b = 10, l = 5, unit = "pt"),
          strip.background=element_rect(fill="white"),
          strip.text = element_text(colour="black"),
          panel.grid.minor = element_blank()) +
    labs(color="", shape="")  
  
  freq_int_totalHarvBA_byCountry1 <- map_data_all %>%
    filter(country %in% c(country_order[c(1:6, 9:length(country_order))])) %>%
    arrange(pann_mean) %>%
    ggplot(aes(harvest_percent_ba_av, harvest_pros*100, 
               col = ba_perc_HARVEST*100)) + #, size=ba_perc_HARVEST*100)) +
    geom_point(data = map_data_all %>% 
                 select(harvest_percent_ba_av, harvest_pros),
               aes(harvest_percent_ba_av, harvest_pros*100),
               inherit.aes=FALSE, col="grey85", size=1) +
    # geom_line(data=totBA_lines, aes(intensity, frequency, group = level),
    #           col="gray70", inherit.aes=FALSE, lty=2) +
    geom_point() +
    # scale_shape_manual(values=shapes) +
    scale_colour_viridis(direction = -1, discrete = FALSE,  breaks=c(1, 2, 3, 4, 5, 10, 15),
                         option="viridis", name="%BA") +
    # scale_size_continuous(breaks=c(1, 2, 3, 4, 5, 10, 15), name="%BA", range = c(.5, 2)) +
    theme_bw() +
    coord_cartesian(ylim=c(0, 14), xlim=c(11, 100)) +
    xlab("Intensity of harvest events") +
    scale_y_continuous(sec.axis = dup_axis())+  #create secondary axis
    theme(legend.position = "none",
          axis.title.y = element_blank(),
          axis.title.x = element_text(hjust=0),
          strip.background=element_rect(fill="white"),
          strip.text = element_text(colour="black"),
          panel.grid.minor = element_blank(),
          plot.margin = margin(t = 10, r = 10, b = 10, l = 0, unit = "pt"),
          strip.placement = "outside",
          axis.text.y.left = element_blank(), #remove axis label on left side
          axis.title.y.right = element_blank(), #remove axis title on right side
          axis.ticks.length.y.left = unit(0, units="cm")) + #remove axis ticks on left ) +
    facet_wrap(country ~ ., ncol= 3) 
  
  freq_int_totalHarvBA_byCountry2 <- map_data_all %>%
    filter(country %in% c(country_order[7:8])) %>%
    arrange(pann_mean) %>%
    ggplot(aes(harvest_percent_ba_av, harvest_pros*100, 
               col = ba_perc_HARVEST*100)) + #, size=ba_perc_HARVEST*100)) +
    geom_point(data = map_data_all %>% 
                 select(harvest_percent_ba_av, harvest_pros),
               aes(harvest_percent_ba_av, harvest_pros*100),
               inherit.aes=FALSE, col="grey85", size=1) +
    # geom_line(data=totBA_lines, aes(intensity, frequency, group = level),
    #           col="gray70", inherit.aes=FALSE, lty=2) +
    geom_point() +
    # scale_shape_manual(values=shapes) +
    scale_colour_viridis(direction = -1, discrete = FALSE,  breaks=c(1, 2, 3, 4, 5, 10, 15),
                         option="viridis", name="%BA") +
    # scale_size_continuous(breaks=c(1, 2, 3, 4, 5, 10, 15), name="%BA", range = c(.5, 2)) +
    theme_bw() +
    coord_cartesian(ylim=c(0, 14), xlim=c(11, 100)) +
    # xlab("Intensity of harvest events") +
    # ylab("Frequency of harvest events") +
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
}

