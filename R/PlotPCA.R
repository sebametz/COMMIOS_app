# Author: S. Metz
# Mail: sebastian.metz [at] york.ac.uk
# Created: 07/02/2024
# Updated: --/02/2024

plotPCA <- function(data,
                    tables, # Table with current_map from the function from before
                    selected = character(0),
                    references = character(0),
                    colourBy = "Group",
                    zoom = F){
  
  colourBy <<- colourBy
  colnames(data)[colnames(data) == "GroupID"] <- "Group"
  # Plot modern data
  plot <- ggplot(tables[["modern"]](), mapping = aes(PCA1, PCA2)) +
    geom_point(colour = "black", fill = "gray20",  shape = 21, 
               alpha = 0.5, size = 2)
  
  # Adding selected References  
  if(length(tables[["references"]]())){
    plot <- plot +
        geom_point(tables[["references"]](),
                   mapping = aes(PCA1, PCA2, fill = Group),
                   colour = "black",  shape = 24,
                   alpha = 0.7, size = 4)
  }
  # Plot mapped individuals
  plot <- plot + 
    geom_point(data,
                mapping = aes(PCA1, PCA2, fill = .data[[colourBy]]), 
                colour = "black",  shape = 22, 
                alpha = 0.5, size = 4)


  # Adding selected individual

  if(length(selected$GeneticID)){
    plot <- plot +
      geom_point(selected$info,
                 mapping = aes(PCA1, PCA2),
                 colour = "black",  shape = 23,
                 fill = "#F9C80E",
                 alpha = 1, size = 6)
  }
  
  if(zoom){
    plot <- plot +
      xlim(min(data$PCA1)-0.005, max(data$PCA1) + 0.005) +
      ylim(min(data$PCA2)-0.005, max(data$PCA2) + 0.005)
  }
  
  # Getting shapes and colour for references
  labels <- list()
  
  legends <- plot + 
    theme(legend.background = element_blank(), 
          legend.text = element_text(size = 8, colour = "black"), 
          legend.box = element_blank())
  
  labels$colour <- get_legend(legends + guides(shape = FALSE))
  labels$shape <- get_legend(legends + guides(colour = FALSE))
  
  plot <- plot +
    theme_bw() +
    theme(legend.position = "none",
          axis.text = element_text(size = 10))
  
  list(plot = plot, labels = labels)
}
