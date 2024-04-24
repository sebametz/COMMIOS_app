# return a ggplot object with the transect plot
transectPlot <- function(df, selection = character(0), map_selected = character(0)){
  # Plot data
  plot <- ggplot(df, mapping = aes(DateMeanInBP, 
                                   Balkan_N_weight, 
                                   fill = Country)) +
    scale_fill_manual(values = c("#ffff99",  "#ff9900", "#99ccff")) +
    geom_pointrange(aes(ymin = Balkan_N_weight - Balkan_N_se,
                        ymax = Balkan_N_weight + Balkan_N_se),
                    position = position_dodge(0.3),
                    colour = "black", shape = 21, 
                    alpha = 0.5, size = 0.5, stroke = 0.5)
  
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
                      alpha = 1, size = 0.5, stroke = 0.5)
    
  }
  # Adding selected map  
  if(length(map_selected$GeneticID)){
    plot <- plot +
      geom_pointrange(filter(df, .data[["GeneticID"]] == map_selected$GeneticID),
                      mapping = aes(
                        ymin = .data[["Balkan_N_weight"]] - .data[["Balkan_N_se"]],
                        ymax = .data[["Balkan_N_weight"]] + .data[["Balkan_N_se"]]),
                      position = position_dodge(0.3),
                      colour = "black", shape = 23, 
                      alpha = 1, size = 1.2, fill = "#F9C80E", stroke = 0.5)
    
  }
  plot +
    labs(fill = "") +
    scale_y_continuous(labels = scales::percent) +
    xlab("Date mean in BP in years before 1950 CE") +
    ylab("% of EEF (Early European Farmers)") +
    scale_x_reverse(limits = c(6000,450)) +
    geom_vline(xintercept = c(1907,2700,3500,4400,5900)) +
    annotate("text", x=5150, y=0.9, label= "Neolithic") +
    annotate("text", x=3950, y=0.9, label= "C/EBA") +
    annotate("text", x=3100, y=0.9, label= "BA") +
    annotate("text", x=2303, y=0.9, label= "IA") +
    annotate("text", x=1396, y=0.9, label= "Post IA period") +
    theme_bw() +
    theme(legend.position = "bottom",
          axis.title = element_text(size = 8),
          text = element_text(size = 12),
          axis.text = element_text(size = 8, colour = "black"))
}