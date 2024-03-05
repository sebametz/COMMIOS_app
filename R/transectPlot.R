# return a ggplot object with the transect plot
transectPlot <- function(df, selection = character(0), map_selected = character(0)){
  # Plot data
  plot <- ggplot(df, mapping = aes(DateMeanInBP, 
                                   Balkan_N_weight, 
                                   fill = Country)) +
    geom_pointrange(aes(ymin = Balkan_N_weight - Balkan_N_se,
                        ymax = Balkan_N_weight + Balkan_N_se),
                    position = position_dodge(0.3),
                    colour = "black", shape = 21, 
                    alpha = 0.5, size = 0.5)
  
  # Adding selected References  
  if(length(selection)){
    plot <- plot +
      geom_pointrange(filter(df, .data[["GeneticID"]] %in% selection),
                      mapping = aes(
                        ymin = Balkan_N_weight - Balkan_N_se,
                        ymax = Balkan_N_weight + Balkan_N_se,
                        fill = Country),
                      position = position_dodge(0.3),
                      colour = "black", shape = 21, 
                      alpha = 1, size = 0.5)
    
  }
  # Adding selected map  
  if(length(map_selected)){
    plot <- plot +
      geom_pointrange(filter(df, .data[["GeneticID"]] %in% map_selected),
                      mapping = aes(
                        ymin = .data[["Balkan_N_weight"]] - .data[["Balkan_N_se"]],
                        ymax = .data[["Balkan_N_weight"]] + .data[["Balkan_N_se"]]),
                      position = position_dodge(0.3),
                      colour = "black", shape = 23, 
                      alpha = 1, size = 0.7, fill = "#F9C80E")
    
  }
  plot +
    labs(fill = "") +
    scale_y_continuous(labels = scales::percent) +
    xlab("Date in BP") +
    ylab("% of EEF") +
    scale_x_reverse() +
    theme_bw() +
    theme(legend.position = "bottom",
          text = element_text(size = 14),
          axis.text = element_text(size = 11, colour = "black"))
}