
get_colours <- function(){
  safe_colorblind_palette <- c("#88CCEE", "#CC6677", "#F0E442", "#117733", "#E69F00", "#332288", "#AA4499", 
                               "#44AA99", "#999999", "#882255", "#661100", "#6699CC",  "#999933", 
                               "#E69F00", "#56B4E9", "#009E73", "#DDCC77", "#0072B2", 
                               "#D55E00", "#CC79A7")
  safe_colorblind_palette
}

# function to update reactive context table according to the periods selected on the map
update_context_by_period <- function(df, periods){
  
  if(length(periods)) filter(df, Period %in% periods) else df
  
}

# return a leaflet object based on current context database ----
plotMap <- function(df = tibble()) {
  
  # Condition map is null
  if(!length(df)){
    leaflet() %>%
      addProviderTiles(providers$Esri.WorldTopoMap) %>%
      setMaxBounds(lng1 = -15, lat1 = 35, lng2 = 15, lat2 = 65)
    
  } else {
    
    df.f <- df 
    
    # Map object
    map <-  leaflet(df.f) %>%
      addTiles() %>%
      addMarkers(
        lng = ~Longitude, lat = ~Latitude,
        clusterOptions = markerClusterOptions(), #disableClusteringAtZoom = 15
        layerId = ~`Genetic ID`, 
        # Popup information
        popup = ~ paste0(
          "Genetic ID: ", `Genetic ID`,
          "<br> Master ID: ", `Master ID`,
          "<br> Molecular Sex: ", `Molecular Sex`,
          "<br> Locality: ", `Correct Locality Name`,
          "<br> Usage Note: ", if_else(Type == "Reference", "Good quality", "Check quality"),
          "<br> Warnings: ", Warnings,
          "<br> SNPs Check: ", SNPs),
        
        # Label information
        label = ~paste0("Individual ", `Genetic ID`, " (", `Molecular Sex`, ")") 
        )
  } 
  prov <- c("Esri.WorldTopoMap", "CartoDB.Positron", "OpenTopoMap")
  
  for (i in 1:length(prov)) {
    map <- map %>% 
      addProviderTiles(providers[[prov[i]]], group = prov[i])
  }
  
  map %>%
    addLayersControl(baseGroups = prov)
}

# return a ggplot object with the transect plot
# Functionality: 
# - When nothing happend, ie. map_selected == NULL then plot all the points
# - When there is a selection, if it is byInd then individual is highlighted, if it is by site, then all individuals in the site are highlighted. Rest of the samples in gray
# - Select colour depending on the Country, use unfilled circles and filled one for selected
# - if context changes, extract those individuals from aadr_db, and highlight. The rest go in gray

plotTransect <- function(data, data_ref, selected = NULL, plotBy = "EEF") {
  
  df.f <- data |>
    mutate(plot = if_else(`Genetic ID` %in% data_ref$`Genetic ID`, "plot", "other")) 
 
  # Check variables and change plot characteristics
  if(length(selected)) {
    if(nrow(selected) != 0) {
      df.f <- df.f |>
        mutate(plot = if_else(`Genetic ID` == selected$`Genetic ID`, "highlight", plot)) 
    }
  }

  df.p <-  df.f |> 
    mutate(colour = if_else(plot == "plot", Country, NA)) |>
    mutate(colour = if_else(is.na(colour), "In dataset", colour)) |>
    mutate(colour = if_else(plot == "highlight", "Selected", colour)) 
  
  # Define colours, shapes and sizes
  
  # colours <- get_colours()[1:length(unique(df.p$colour))]
  # names(colours) <- unique(df.p$colour)
  # print(colours)
  
  # Plot data
  plot <- ggplot(df.p, mapping = aes(`Date Mean in BP`, .data[[plotBy]]), shape = 21) +
    # gray points
    geom_point(data = filter(df.p, colour == "In dataset"), 
               fill = "#d1d1d1", alpha = 0.7, size = 0.75) +
    
    # Country points
    geom_point(data = filter(df.p, plot == "plot"),
               aes(fill = Country), colour = "black", alpha = 1, size = 4, shape = 21) +
    
    # Color values
    # scale_fill_manual(values = colours)  +
    
    # References
    guides(fill = guide_legend("Country", override.aes = list(shape = 21, size = 4, alpha = 1))) +
    
    # Highlighted points
    geom_point(data = filter(df.p, plot == "highlight"), 
               colour= "black", fill = "#ef21f5",
               size = 6, alpha = 1, shape = 23) +
    # y scale
    scale_y_continuous(labels = scales::percent, limits = c(0,1)) +
    
    # Labels 
    xlab("Date mean in BP in years before 1950 CE") +
    ylab(str_c("%", plotBy)) +
    
    # Reverse x-axis
    scale_x_reverse(limits = c(5900, 600)) +
    
    # Add annotation
    geom_vline(xintercept = c(1540, 1907,2700,3500,4400)) +
    # annotate("text", x=6600, y = 0.9, label = "Mesolithic") +
    annotate("text", x=5150, y=0.9, label= "Neolithic", size = 4) +
    annotate("text", x=3950, y=0.9, label= "C/EBA", size = 4) +
    annotate("text", x=3100, y=0.9, label= "BA", size = 4) +
    annotate("text", x=2303, y=0.9, label= "IA", size = 4) +
    annotate("text", x=1723, y=0.98, label= "Romans period", size = 4) +
    annotate("text", x=985, y=0.9, label= "Early Medieval/Vikings", size = 4) +
    
    # Theme configuration
    theme_bw() +
    theme(legend.position = "top",
          legend.title = element_text(size = 16),
          legend.text = element_text(size = 16),
          axis.title = element_text(size = 14),
          axis.text = element_text(size = 14, colour = "black")
          )
  plot
}

# return a ggplot object with a PCA plot
# data = df,
# data_ref = context,
# selected = map / transect selected individual
# references = modern or ancient references to plot
get_pca <- function(data, data_ref = NULL, selected = NULL, references = NULL){
  
  pca <- ggplot(filter(data, DataRef == "AADR_Modern"), 
                aes(x = PCA1, y = PCA2)) + 
    geom_point(fill = "#d1d1d1", 
               shape = 21, size = 1) 
  
  if(length(references)) {
    
    data_ref_filtered <- data |>
      filter(Group %in% references)
    
    pca <- pca + geom_point(data_ref_filtered, 
                      mapping = aes(x = PCA1, y = PCA2, fill = Group), 
                      shape = 22, size = 2, colour = "black")
  }
  if(length(selected)) {
    if(nrow(selected)>0){
      pca <- pca +
        geom_point(filter(data, `Genetic ID` %in% selected), 
                   mapping = aes(x = PCA1, y = PCA2), 
                   shape = 23, size = 4, fill = "#ef21f5", 
                   colour = "black")
    }
  }
  # legend <- g_legend(pca +
  #                      theme_bw() +
  #                      theme(legend.position = "bottom",
  #                            legend.title = element_text(size = 16, colour = "black"),
  #                            legend.text = element_text(size = 16, colour = "black")))
  pca <- pca + 
    # Theme configuration
    theme_bw() +
    theme(legend.position = "bottom",
          legend.text = element_text(size = 14, colour = "black"),
          legend.title = element_text(size = 14, colour = "black"),
          axis.title = element_text(size = 14),
          text = element_text(size = 14),
          axis.text = element_text(size = 14, colour = "black"))
  pca
  # list(pca = pca, legend = legend)
}

# return a ggplot object with a Ternary plot
# data = df,
# data_ref = context,
# selected = map / transect selected individual
# references = modern or ancient references to plot
get_ternary <- function(data, data_ref = NULL, selected = NULL, references = NULL){
  
  df <- data |>
    mutate(Selection = NA)
  
  if(length(data_ref)){
    if(nrow(data_ref)>0){
      df <- df |>
        mutate(Selection = if_else(`Genetic ID` %in% data_ref$`Genetic ID`, "onRef", Selection))
    }
    if(length(references)){
      df <- df |>
        mutate(Selection = if_else(Group %in% references, "isRef", Selection))
    }
    if(length(selected)){
      df <- df |>
        mutate(Selection = if_else(`Genetic ID` %in% selected$`Genetic ID`, "isSelected", Selection))
    }
  }
  
  # df <- df |>
  #   mutate(siteName = NA) |>
  #   separate(Locality, sep = ";", into = c("l1", "l2", "l3", "siteName"), remove = F, extra = "drop")
  
  df$label <- unlist(lapply(1:nrow(df), function(i) as.character(
                                                      HTML(
                                                        paste0(
                                                          "<br> Genetic ID: ", df$`Genetic ID`[i],
                                                          "<br> Master ID: ", df$`Master ID`[i],
                                                          "<br> Molecular Sex: ", df$`Molecular Sex`[i],
                                                          "<br> Locality: ", df$`Correct Locality Name`[i],
                                                          "<br> Contributions:",
                                                          "<br>  - EEF: ", round(df$EEF[i], digits = 2), " ± ", round(df$`EEF SE`[i], digits = 2),
                                                          "<br>  - WHG: ", round(df$WHG[i], digits = 2), " ± ", round(df$`WHG SE`[i], digits = 2),
                                                          "<br>  - Steppe: ", round(df$Steppe[i], digits = 2), " ± ", round(df$`Steppe SE`[i], digits = 2)
                                                        )
                                                      )
                                                    )
                            )
                     )  
  
  # out of range
  df <- df |>
    mutate(EEF = if_else(EEF < 0, 0, if_else(EEF > 1, 1, EEF))) |>
    mutate(Steppe = if_else(Steppe < 0, 0, if_else(Steppe > 1, 1, Steppe))) |>
    mutate(WHG = if_else(WHG < 0, 0, if_else(WHG > 1, 1, WHG)))
  
  
  # colour <- c('#d1d1d1', "#DB7365", "#ef21f5")
  # colour <- setNames(colour, c("onRef", "isRef", "isSelected"))

  axis <- function(title) {
    list(
      title = title, 
      titlefont = list(size = 14),
      tickfont = list(size = 12),
      tickcolor = "black",
      ticklen = 5
    )
  }
  
  fig <- plot_ly(type = 'scatterternary', 
                 mode = 'markers', 
                 hoverinfo = "text")
  
  fig <- fig %>%
    add_trace(
      data = filter(df, Selection == "onRef"), 
      a = ~EEF,
      b = ~WHG,
      c = ~Steppe,
      text = ~label,
      marker = list(
        symbol = 100,
        color = '#d1d1d1',
        size = 2,
        line = list('width' = 2)
      ),
      showlegend = F
    )
  fig <- fig %>%
    add_trace(data = filter(df, Selection == "isRef"),
              name = "References",
              a = ~EEF,
              b = ~WHG,
              c = ~Steppe,
              text = ~label,
              marker = list(
                symbol = 101, 
                size = 3,
                color = "#DB7365",
                line = list('width' = 2)
                )
              )
  fig <- fig %>%
    add_trace(data = filter(df, Selection == "isSelected"),
              name = ~`Genetic ID`,
              a = ~EEF,
              b = ~WHG,
              c = ~Steppe,
              text = ~label,
              marker = list(
                symbol = 202, 
                size = 10,
                color = "#ef21f5",
                line = list('width' = 2)
              )
              )
  
  fig <- fig %>% layout(
    legend = list(orientation = "h",   
                  xanchor = "center",  
                  x = 0.5),
    template = "ggplot2",
    ternary = list(
      sum = 100, 
      aaxis = axis("EEF"),
      baxis = axis("WHG"),
      caxis = axis("Steppe")
    )
  )
  
}

# return a ggplot object with a composition plot
get_composition <- function(data) {
  if(!length(data)){
    p <- ggplot()
  }else{
    if(nrow(data) == 0){
      p <- ggplot()
    } else {
      weight <- data |>
        pivot_longer(cols = c("Steppe", "WHG", "EEF"), names_to = "left", values_to = "weight") |>
        select(`Genetic ID`, left, weight)
      
      sd <- data |>
        pivot_longer(cols = c("Steppe SE", "WHG SE", "EEF SE"), names_to = "left", values_to = "se") |>
        select(`Genetic ID`, se)
      
      weight$se <- sd$se
      
      results_qpadm <- weight |>
        mutate(Label = `Genetic ID`) |>
        mutate(Left = factor(left, levels = c("EEF", "WHG", "Steppe"))) |>
        group_by(Label) |>
        mutate(SDpos = cumsum(weight)) |>
        ungroup()
      
      colours <- c("#4EA446", "#4E2EEE", "#EE811C")
      names(colours) <- c("Steppe", "WHG", "EEF")
      
      p <- ggplot(results_qpadm, aes(x = Label, y = weight, fill = Left)) +
        geom_bar(stat="identity", color = 1, position = "fill") +
        geom_errorbar(aes(ymin=(SDpos-se), ymax=(SDpos+se)), width=.2, 
                      position = "identity") +
        geom_text(aes(label = str_c(round(weight*100, 1), "%")), 
                  color = "white", position = position_stack(vjust = 0.5), size = 8) +
        scale_fill_manual(values = colours) +
        scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
        scale_x_discrete(limits=rev) +
        guides(fill = guide_legend(title = "")) +
        coord_flip() +
        ylab(label = "") +
        xlab(label = "") +
        theme_minimal() +
        theme(axis.text = element_text(colour = "white", size = 15), 
              legend.position = "bottom",
              legend.text  = element_text(colour = "white", size = 15))
    }
    
  }
  p
}
  

# weight <- df |>
#   pivot_longer(cols = c("Steppe", "WHG", "EEF"), names_to = "left", values_to = "weight") |>
#   select(GeneticID, left, weight)
# 
# sd <- df |>
#   pivot_longer(cols = c("Steppe_se", "WHG_se", "EEF_se"), values_to = "se") |>
#   select(GeneticID, se)
# 
# weight$se <- sd$se
# 
# results_qpadm <- weight |>
#   mutate(Label = GeneticID) |>
#   mutate(Left = factor(left, levels = c("EEF", "WHG", "Steppe"))) |>
#   group_by(Label) |>
#   mutate(SDpos = cumsum(weight)) |>
#   ungroup()
# 
# # bar plot
# colours <- c("#EE811C", "#4E2EEE", "#4EA446")
# names(colours) <- c("EEF", "WHG", "Steppe")
# 
# p3 <- ggplot(results_qpadm, aes(x = Label, y = weight, fill = Left)) +
#   geom_bar(stat="identity", color = "black", position = "fill") +
#   geom_errorbar(aes(ymin=(SDpos-se), ymax=(SDpos+se)), width=.2,
#                 position = "identity") +
#   geom_text(aes(label = str_c(round(weight*100, 1), "%")), 
#             color = "white", position = position_stack(vjust = 0.5)) +
#   scale_fill_manual(values = colours) +
#   scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
#   scale_x_discrete(limits=rev) +
#   guides(fill = guide_legend(title = "")) +
#   coord_flip() +
#   ylab(label = "") +
#   xlab(label = "") +
#   theme_minimal() +
#   theme(axis.text = element_text(colour = "black", size = 10), 
#         legend.position = "top",
#         legend.text  = element_text(colour = "black", size = 11))
# p3
# "Early European Farmers (EEF)", "Western hunter-gatherer (WHG)", "Yamnaya pastoralists (Steppe)"
# get_ternary <- function(data, data_ref = NULL, selected = NULL, references = NULL){
#   ternary <- ggtern::ggtern(filter(data, DataRef == "AADR_Reference"), 
#                     aes(x = EEF, y = Stepp, z = WHG)) +
#     geom_point(fill = "gray", size = 0.5, alpha = 0.5)
#   
#   ternary
# }

# else {
#   # Site information
#   sites <- df |>
#     group_by(LocID) |>
#     mutate(nSamples = n()) |>
#     ungroup() |>
#     distinct(LocID, .keep_all = T)
#   
#   # Condition map plot by sites
#   map <- leaflet(sites) %>%
#     addTiles() %>% addCircleMarkers(
#       lng = ~Longitude, lat = ~Latitude,
#       radius = ~log(nSamples, base = 10) + 1,
#       color = "#994444", 
#       popup = ~paste0("Locality: ", CorrectLocalityName,
#                       "<br>Latitude: ", Latitude, 
#                       "<br>Longitude: ", Longitude,
#                       "<br># of Individuals: ", nSamples), 
#       
#       label = ~paste0(CorrectLocalityName, " (N of Individuals: ", nSamples ,")")
#     )
# }