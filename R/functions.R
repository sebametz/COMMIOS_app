
# function to update reactive context table according to the period, sex and country selected on the map
# context: actual data used in the app
# df: not modified dataset
# values: list(periods, sex, country)
# Return alert when not posible and a previous state of the dataset
update_context <- function(context, df, values){

  # Periods
  context.n <- df |>
    filter(Period %in% values$periods) 
  
  # Country
  context.n <- context.n |>
    filter(Country %in% values$countries)
  
  # Molecular sex
  context.n <- context.n |>
    filter(`Molecular Sex` %in% values$sex)
  
  # Check combination exist if not return previous value
  if(nrow(context.n) == 0) {
    list(context = context, success = F)
  } else {
    list(context = context.n, success = T)
  }
  
}



update_context_by_period <- function(df, periods){
  
  if(length(periods)) filter(df, Period %in% periods) else df
  
}

update_context_by_country <- function(df, country){
  
  if(length(country)){
    
    if("All" %in% country) df else filter(df, Country %in% country)
    
  } else {
    df
  }

}

update_context_by_sex <- function(df, sex){
  
  if(length(sex)){
    if("All" %in% sex) df else filter(df, `Molecular Sex` %in% sex)
  } else {
    df
  }
  
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
          # "<br> Master ID: ", `Master ID`,
          "<br> Molecular Sex: ", `Molecular Sex`,
          "<br> Locality: ", `Correct Locality Name`,
          "<br> Period: ", Period, 
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
  
  # Define colours
  colours <- c("#e5e5e5", "#d90000", "#0474ff", "#018d01", "#ffc800")
  names(colours) <- c("England", "Wales", "Scotland", "Ireland", "Isle of Man")
  
  shapes <- c(24,25,21)
  names(shapes) <- c("F", "M", "U")
  
  # Plot data
  plot <- ggplot(df.p, mapping = aes(`Date Mean in BP`, .data[[plotBy]]), shape = 21) +
    # gray points
    geom_point(data = filter(df.p, colour == "In dataset"),
               fill = "#d1d1d1", alpha = 0.7, size = 1.25) +
    
    # Country points
    geom_point(data = filter(df.p, plot == "plot"),
               aes(fill = Country, shape = `Molecular Sex`), colour = "black",
               alpha = 1, size = 3) +
    
    # Color values
    scale_fill_manual(values = colours)  +
    scale_shape_manual(values = shapes) +
    
    # References
    guides(fill = guide_legend("Country: ", override.aes = list(shape = 21, size = 5, alpha = 1, order = 1, nrow = 1)),
           shape = guide_legend("'Molecular Sex': ", override.aes = list(size = 5, alpha = 1, nrow = 1))) +
    
    # Highlighted points
    geom_point(data = filter(df.p, plot == "highlight"), 
               colour= "black", fill = "#ffe75e",
               size = 10, alpha = 1, shape = 23) +
    # y scale
    scale_y_continuous(labels = scales::percent, limits = c(0,1)) +
    
    # Labels 
    xlab("Date mean in BP in years before 1950 CE") +
    ylab(str_c("%", plotBy)) +
    
    # Reverse x-axis
    scale_x_reverse(limits = c(5900, 600)) +
    
    # Add annotation
    geom_vline(xintercept = c(1540, 1907,2700,3500,4400), colour = "gray20", linetype = 5) +
    # annotate("text", x=6600, y = 0.9, label = "Mesolithic") +
    annotate("text", x=5150, y=0.9, label= "Neolithic", size = 6, colour = "gray20") +
    annotate("text", x=3950, y=0.9, label= "C/EBA", size = 6, colour = "gray20") +
    annotate("text", x=3100, y=0.9, label= "BA", size = 6, colour = "gray20") +
    annotate("text", x=2303, y=0.9, label= "IA", size = 6, colour = "gray20") +
    annotate("text", x=1723, y=0.98, label= "Romans period", size = 6, colour = "gray20") +
    annotate("text", x=985, y=0.9, label= "Early Medieval/Vikings", size = 6, colour = "gray20") +
    #Colours not changes
    # scale_fill_discrete(limits = unique(data$Country)) +
    # Theme configuration
    theme_classic() +
    theme(
          legend.position = "top",
          legend.title = element_text(size = 16, colour = "black"),
          legend.text = element_text(size = 16, colour = "black"),
          axis.title = element_text(size = 14, colour = "black"),
          axis.text = element_text(size = 14, colour = "black")
          )
  plot
}

# return a ggplot object with a PCA plot
# data = df,
# data_ref = context,
# selected = map / transect selected individual
# references = modern or ancient references to plot
get_pca <- function(data, data_ref = NULL, selected = NULL, references = NULL, locality = NULL, limits = NULL, colours = NULL){
  
  shapes <- c(21, rep(23,7))
  names(shapes) <- c("Modern", "Mesolithic", "Neolithic", "C/EBA", "BA", "IA", "Romans", "Early Medieval/Vikings")
  
  # Add Modern
  data_aux <- filter(data, DataRef == "AADR_Modern")
  pca <- ggplot(data_aux, 
                aes(x = PCA1, y = PCA2)) + 
    geom_point(fill = "#d1d1d1", 
               shape = 21, size = 0.75) 
  
  # Add groups references
  if(length(references)) {
    
    data_ref_filtered <- data |>
      filter(Group %in% references) |>
      mutate(Period = factor(Period, levels = c("Mesolithic", "Neolithic", "C/EBA", "BA", "IA", "Romans", "Early Medieval/Vikings", "Modern"))) |>
      arrange(desc(Period), Group)
      
      
    
    data_aux <- data_aux |>
      add_case(data_ref_filtered)
    
    pca <- pca + geom_point(data_ref_filtered, 
                      mapping = aes(x = PCA1, y = PCA2, fill = Group, shape = Period),
                      size = 3, colour = "black") +
      scale_shape_manual(values = shapes) +
      scale_fill_manual(values = colours) +
      guides(fill = guide_legend("Group: ", override.aes = list(shape = 21, size = 5, alpha = 1, order = 1, nrow = 1)),
             shape = "none")
  }
  
  # Add Localities references
  if(length(locality)){
    
    data_ref_filtered <- data |>
      filter(`Genetic ID` %in% locality)
    
    data_aux <- data_aux |>
      add_case(data_ref_filtered)
    
    pca <- pca + geom_point(data_ref_filtered, 
                            mapping = aes(x = PCA1, y = PCA2),
                            size = 6, colour = "black", shape = 24,
                            fill = "#0058ff")
    
  }
  
  if(length(selected)) {
    if(nrow(selected)>0){
      
      data_ref_filtered <- filter(data, `Genetic ID` %in% selected)
        
      data_aux <- data_aux |>
        add_case(data_ref_filtered)
      
      pca <- pca +
        geom_point(data_ref_filtered, 
                   mapping = aes(x = PCA1, y = PCA2), 
                   shape = 23, size = 10, fill = "#ffe75e", 
                   colour = "black")
    }
  }
  
  if(length(limits)){
    pca <- pca +
      xlim(limits[1], limits[2])  +
      ylim(limits[3], limits[4])
  } else {
    min <- if(min(data_aux$PCA1, na.rm = T) < min(data_aux$PCA2, na.rm = T)) round(min(data_aux$PCA1, na.rm = T), digits = 5) else round(min(data_aux$PCA2, na.rm = T), digits = 5)
    max <- if(max(data_aux$PCA1, na.rm = T) > max(data_aux$PCA2, na.rm = T)) round(max(data_aux$PCA1, na.rm = T), digits = 5) else round(max(data_aux$PCA2, na.rm = T), digits = 5)
    # print(min)
    # print(max)
    pca <- pca +
      xlim(min-0.005, max+0.005)  +
      ylim(min-0.005, max+0.005)
  }
  pca <- pca + 
    # scale_fill_discrete(limits = unique(data$Group)) +
    # Theme configuration
    theme_classic() 
  pca
  # list(pca = pca, legend = legend)
}

# return a ggplot object with a Ternary plot
# data = df,
# data_context = context,
# selected = map / transect selected individual
# references = modern or ancient references to plot
get_ternary <- function(data, data_context = NULL, selected = NULL, references = NULL, locality = NULL){
  
  df <- data |>
    mutate(Selection = NA)
  
  if(length(data_context)){
    if(nrow(data_context)>0){
      # df <- df |>
      #   separate(`Unique Locality Name`, sep = ", ", into = c("L1", "L2", "L3"), remove = F, extra = "drop")
      df <- df |>
        mutate(Selection = if_else(`Genetic ID` %in% data_context$`Genetic ID`, "onRef", Selection))
    }
    if(length(references)){
      df <- df |>
        mutate(Selection = if_else(Group %in% references, "isRef", Selection))
    }
    if(length(locality)){
      df <- df |>
        mutate(Selection = if_else(`Genetic ID` %in% locality, "isLocality", Selection))
        # mutate(Selection = if_else(L3 %in% locality, "isLocality", Selection))
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
                                                          # "<br> Master ID: ", df$`Master ID`[i],
                                                          "<br> Molecular Sex: ", df$`Molecular Sex`[i],
                                                          "<br> Locality: ", df$`Correct Locality Name`[i],
                                                          "<br> Period: ", df$Period[i],
                                                          "<br> Contributions:",
                                                          "<br> - EEF: ", round(df$EEF[i], digits = 2), " ± ", round(df$`EEF SE`[i], digits = 2),
                                                          "<br> - WHG: ", round(df$WHG[i], digits = 2), " ± ", round(df$`WHG SE`[i], digits = 2),
                                                          "<br> - Steppe: ", round(df$Steppe[i], digits = 2), " ± ", round(df$`Steppe SE`[i], digits = 2)
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
        symbol = "101",
        color = '#d1d1d1',
        size = 2,
        line = list('width' = 2)
      ),
      showlegend = F
    )
  
  fig <- fig %>%
    add_trace(data = filter(df, Selection == "isRef"),
              name = ~Country,
              a = ~EEF,
              b = ~WHG,
              c = ~Steppe,
              text = ~label,
              marker = list(
                symbol = "201",
                color = ~Country, 
                size = 8,
                line = list('width' = 2)
                )
              )
 
  fig <- fig %>%
    add_trace(data = filter(df, Selection == "isLocality"),
              name = "",
              a = ~EEF,
              b = ~WHG,
              c = ~Steppe,
              text = ~label,
              marker = list(
                symbol = "triangle-up", 
                size = 16,
                color = "#0058ff",
                line = list('width' = 0.75, 'color' = "black")
              ),
              showlegend = F
              )
              
  fig <- fig %>%
    add_trace(data = filter(df, Selection == "isSelected"),
              name = ~`Genetic ID`,
              a = ~EEF,
              b = ~WHG,
              c = ~Steppe,
              text = ~label,
              marker = list(
                symbol = "diamond", 
                size = 16,
                color = "#ffe75e",
                line = list('width' = 0.75, 'color' = "black")
              )
    )
  
  fig <- fig %>% layout(
    legend = list(orientation = "h",   
                  xanchor = "center",  
                  x = 0.5),
    template = "ggplot2",
    ternary = list(
      sum = 100, 
      aaxis = axis("% EEF"),
      baxis = axis("% WHG"),
      caxis = axis("% Steppe")
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
        geom_bar(stat="identity", colour = 1, position = "fill", colour = "black") +
        geom_errorbar(aes(ymin=(SDpos-se), ymax=(SDpos+se)), width=.5, 
                      position = "identity", colour = "#cfcfcf", linewidth = 1.2, show.legend = F) +
        geom_text(aes(label = str_c(round(weight*100, 2), "%")), 
                  colour = "#f6f6f6", position = position_stack(vjust = 0.5), 
                  show.legend = F, size.unit = "pt", size = 14, fontface = "bold") +
        scale_fill_manual(values = colours) +
        scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
        scale_x_discrete(limits=rev) +
        guides(fill = guide_legend(title = "")) +
        coord_flip() +
        ylab(label = "") +
        xlab(label = "") +
        theme_void() +
        theme(text = element_text(colour = "#f6f6f6", face = "bold"),
              axis.text = element_text(size = 14, face = "bold"),
              legend.text = element_text(size = 16),
              legend.position = "bottom", 
              panel.background = element_rect(fill = "#eff3f9"))
    }
    
  }
  p
}


# 
# get_colours <- function(){
  safe_colorblind_palette <- c("#88CCEE", "#CC6677", "#F0E442", "#117733", "#E69F00", "#332288", "#AA4499",
                               "#44AA99", "#999999", "#882255", "#661100", "#6699CC",  "#999933",
                               "#E69F00", "#56B4E9", "#009E73", "#DDCC77", "#0072B2",
                               "#D55E00", "#CC79A7")
#   safe_colorblind_palette
# }