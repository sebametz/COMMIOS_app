# Author: S. Metz
# Mail: sebastian.metz [at] york.ac.uk
# Created: 07/02/2024
# Updated: --/02/2024

plotComp <- function(selected = character(0), df){
  if(length(selected)){
  if(selected %in% df$GeneticID){
    df <- filter(df, GeneticID == selected) 
    colnames(df) <- c("GeneticID","GroupID","Steppe", "WHG","EEF","Steppe_se","WHG_se","EEF_se","OldSteppe_z","WHGA_z","Balkan_N_z")
    weight <- df |>
      pivot_longer(cols = c("Steppe", "WHG", "EEF"), names_to = "left", values_to = "weight") |>
      select(GeneticID, left, weight)
    
    sd <- df |>
      pivot_longer(cols = c("Steppe_se", "WHG_se", "EEF_se"), values_to = "se") |>
      select(GeneticID, se)
    
    weight$se <- sd$se
    
    results_qpadm <- weight |>
      mutate(Label = GeneticID) |>
      mutate(Left = factor(left, levels = c("EEF", "WHG", "Steppe"))) |>
      group_by(Label) |>
      mutate(SDpos = cumsum(weight)) |>
      ungroup()
    
    # bar plot
    colours <- c("#EE811C", "#4E2EEE", "#4EA446")
    names(colours) <- c("EEF", "WHG", "Steppe")
    
    p3 <- ggplot(results_qpadm, aes(x = Label, y = weight, fill = Left)) +
      geom_bar(stat="identity", color = "black", position = "fill") +
      # geom_errorbar(aes(ymin=(SDpos-se), ymax=(SDpos+se)), width=.2, 
      #               position = "identity") +
      geom_text(aes(label = str_c(round(weight*100, 1), "%")), 
                color = "white", position = position_stack(vjust = 0.5),size = 8) +
      scale_fill_manual(values = colours) +
      scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
      scale_x_discrete(limits=rev) +
      guides(fill = guide_legend(title = "")) +
      coord_flip() +
      ylab(label = "") +
      xlab(label = "") +
      theme_minimal() +
      theme(axis.text = element_text(colour = "black", size = 8), 
            legend.position = "top",
            legend.text  = element_text(colour = "black", size = 8))
    p3
  }}
}

