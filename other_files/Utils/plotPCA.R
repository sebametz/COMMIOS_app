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
  
  data <- data |> left_join(select(reference_pca, GeneticID, Group))
  # Plot modern data
  plot <- ggplot(tables[["modern"]](), mapping = aes(PCA1, PCA2)) +
    geom_point(colour = "black", fill = "gray20",  shape = 21, 
               alpha = 0.5, size = 2)
  
 # Adding selected References  
  if(length(tables[["references"]]())){
    # Plot modern
    modernPCA <- filter(tables[["references"]](), Period == "Modern")
    ancientPCA <- filter(tables[["references"]](), Period != "Modern" & !GeneticID %in% data$GeneticID)
    
    if(length(modernPCA)){
      
      palletModern <- getColoursModern()
      palletModern <- palletModern[names(palletModern) %in% modernPCA$Group]
      
      plot <- plot +
        geom_point(modernPCA,
                   mapping = aes(PCA1, PCA2, fill = Group),
                   colour = "black",  shape = 24,
                   alpha = 0.7, size = 4) +
        scale_fill_manual(values = palletModern, name = "Modern References") +
        new_scale_fill()
    }
    # Plot ancient
    if(length(ancientPCA)){
      
      palletAncient <- getColoursAncient()
      palletAncient <- palletAncient[names(palletAncient) %in% ancientPCA$Group]
      
      plot <- plot +
        geom_point(ancientPCA,
                   mapping = aes(PCA1, PCA2, fill = Group),
                   colour = "black",  shape = 22,
                   alpha = 0.7, size = 4)+
        scale_fill_manual(values = palletAncient, name = "Ancient References") +
        new_scale_fill()
    }

  }
  # Plot mapped individuals
  plot <- plot + 
    geom_point(data,
                mapping = aes(PCA1, PCA2, fill = .data[[colourBy]]), 
                colour = "black",  shape = 25, 
                alpha = 0.5, size = 4)
 
  if(colourBy == "Group"){
    palletGroups <- getColoursUKGroups()
    palletGroups <- palletGroups[names(palletGroups) %in% unique(data$Group)]
  }else if(colourBy == "Period"){
    palletGroups <- getColoursUKPeriods()
    palletGroups <- palletGroups[names(palletGroups) %in% unique(data$Period)]
  } else if(colourBy == "Locality"){
    palletGroups <- getColoursUKLoclities()
    palletGroups <- palletGroups[names(palletGroups) %in% unique(data$Locality)]
    
  } else if(colourBy == "Country"){
    palletGroups <- getColoursUKCountries()
    palletGroups <- palletGroups[names(palletGroups) %in% unique(data$Country)]
    
  }
  plot <- plot + 
    scale_fill_manual(values = palletGroups, name = "UK References") +
    new_scale_fill()

  # Adding selected individual
  if(length(selected$GeneticID)){
    plot <- plot +
      geom_point(selected$info,
                 mapping = aes(PCA1, PCA2, fill = GeneticID),
                 colour = "black",  shape = 23,
                 alpha = 1, size = 8) + 
      scale_fill_manual(values = "#F9C80E", name = "Selected individual") +
      new_scale_fill()
  }
  if(zoom){
    plot <- plot +
      xlim(min(data$PCA1)-0.008, max(data$PCA1) + 0.008) +
      ylim(min(data$PCA2)-0.008, max(data$PCA2) + 0.008)
  }
  
  # Getting shapes and colour for references
  labels <- list()
  
  legends <- plot + 
    theme(legend.background = element_blank(), 
          legend.text = element_text(size = 6, colour = "black"),
          legend.key.size = unit(0.3,"cm"),
          legend.key = element_blank(),
          legend.box.background = element_blank(),
          legend.spacing = unit(0.2, "cm"),
          legend.box = element_blank())
  
  labels$colour <- get_legend(legends + guides(shape = FALSE))
  labels$shape <- get_legend(legends + guides(colour = FALSE))
  
  plot <- plot +
    theme_bw() +
    theme(legend.position = "none",
          axis.text = element_text(size = 10))
  
  list(plot = plot, labels = labels)
}



plotPCABrush <- function(data,
                    tables, # Table with current_map from the function from before
                    selected = character(0),
                    references = character(0),
                    colourBy = "Group",
                    zoom = F){
  dataF <- context |> filter(GeneticID %in% data$GeneticID)
  plotPCA(dataF,
          tables,
          selected = selected,
          colourBy = colourBy,
          zoom = T)
  
}
