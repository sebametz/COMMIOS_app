# Author: S. Metz
# Mail: sebastian.metz [at] york.ac.uk

# Ternary plot for composition with current context and selected
ternaryPlot <- function(df, selected = character(0)){
  df <- if(nrow(df) == 0) ancestry else df
  
  if(length(selected)){
    
    df$Selection <- if_else(df$GeneticID == selected, "Selected", "Reference")
    df$GroupID[df$GeneticID == selected] <- selected
    
    df <- df[order(df$Selection), ] # order to plot selected at the end
    
    pallet <- pallet <- c(brewer.pal(n = 12, "Paired"), brewer.pal(n = 12, "Set3"))[1:length(unique(df$GroupID))]
    names(pallet) <- c(unique(df$GroupID))
    pallet[names(pallet) == selected] <- "#F9C80E"
    
    plot <- ggtern(data = df, aes(x = OldSteppe_weight, y = Balkan_N_weight, z = WHGA_weight)) +
      geom_point(aes(fill = GroupID, shape = Selection, size = Selection), colour = "black") +
      scale_shape_manual(values = c(21,23)) +
      scale_size_manual(values = c(5,8)) +
      scale_fill_manual(values = pallet)
  } else {
    plot <- ggtern(data = df, aes(x = OldSteppe_weight, y = Balkan_N_weight, z = WHGA_weight)) +
      geom_point(aes(fill = GroupID), size = 5, shape = 21, colour = "black")
  }

  plot <- plot + theme_rgbw() 
  
  plot +
    xlab("% Steppe") +
    ylab("% EEF") +
    zlab("% WHG") +
    theme(legend.position = "none",
          text = element_text(size = 18),
          axis.text = element_text(size = 15))
}

# ternaryPlot(df = ancestry, selected = "I13754")
# 


