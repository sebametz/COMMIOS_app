# Author: S. Metz
# Mail: sebastian.metz [at] york.ac.uk
# Created: 07/02/2024
# Updated: --/02/2024

plotPCA <- function(data,
                    tables, # Table with current_map from the function from before
                    selected = character(0),
                    references = character(0),
                    colourBy = "Group"){
  
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
                colour = "black",  shape = 25, 
                alpha = 0.5, size = 4)


  # Adding selected individual

  if(length(selected)){
    plot <- plot +
      geom_point(filter(data, GeneticID %in% selected),
                 mapping = aes(PCA1, PCA2),
                 colour = "black",  shape = 23,
                 fill = "#F9C80E",
                 alpha = 1, size = 6)
  }
    
  plot <- plot +
    theme_bw() +
    theme(legend.position = "none",
          text = element_text(size = 14))
  
  plot
}
