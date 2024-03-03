# Filter current context data table based on period, country, locality and years inputs 
filterContext <- function(df, period, country, locality, years){
  
  df_filtered <- if(!is.null(period)) filter(df, .data[["Period"]] %in% period) else df
  
  # Check if country exist in df
  for (i in country) {
    if(!i %in% df_filtered[["Country"]]){
      country <- NULL
      break
    }
  }
  
  df_filtered <- if(!is.null(country)) filter(df_filtered, .data[["Country"]] %in% country) else df_filtered
  
  # Check if locality exist in df
  for (i in locality) {
    if(!i %in% df_filtered[["Locality"]]){
      locality <- NULL
      break
    }
  }
  
  df_filtered <- if(!is.null(locality)) filter(df_filtered, .data[["Locality"]] %in% locality) else df_filtered
  
  df_filtered <- if(!is.null(years)) filter(df_filtered, between(.data[["DateMeanInBP"]] - 1950, 
                                                                 .env[["years"]][1], .env[["years"]][2])) else df_filtered
  
  df_filtered 
}

# Get the different choices and selection for filtering context
getChoices <- function(df, var, param, currentFilter){

  # get values  
  choices <- sort(unique(filterContext(df, param$periodFilter, param$countryFilter, 
                                       param$localityFilter, param$yearsFilter)[[var]]))

  selection <- intersect(choices, param[[currentFilter]])
  list(
    choices = if(!is.null(choices)) choices else print("Not choices left"),
    selection = if(length(selection)) param[[currentFilter]] else character(0)
      ) 
}

# return a leaflet object based on current context database
interactiveMap <- function(df, flyto = character(0), current_zoom = character(0)) {
  
  if(nrow(df) == 0){
    map <- leaflet() %>%
      addProviderTiles(providers$Esri.WorldTopoMap) %>%
      setMaxBounds(lng1 = -15, lat1 = 35, lng2 = 15, lat2 = 65)
    map
  } else {
    sexIcons <- iconList(
      M = makeIcon("www/Icons/male.png",iconWidth = 15, iconHeight = 37.5, iconAnchorX = 8.82, iconAnchorY = 37.5),
      F = makeIcon("www/Icons/female.png",iconWidth = 15, iconHeight = 37.5, iconAnchorX = 8.82, iconAnchorY = 37.5),
      U = makeIcon("www/Icons/genderless.png",iconWidth = 15, iconHeight = 37.5, iconAnchorX = 8.82, iconAnchorY = 37.5)
    )
    map <- leaflet(df) %>%
      addProviderTiles(providers$Esri.WorldTopoMap) %>%
      setMaxBounds(lng1 = -15, lat1 = 35, lng2 = 15, lat2 = 65) %>%
      addMarkers(lng = ~Longitude, lat = ~Latitude, 
                 icon = ~sexIcons[MolecularSex],
                 clusterOptions = markerClusterOptions(), 
                 label = ~sprintf("Individual GeneticID: %s", GeneticID),
                 popup = ~sprintf("<b>Individual GeneticID: %s</b></br>Skeleton Code: %s", GeneticID, SkeletonCode),
                 layerId = ~GeneticID
      )
  }
  if(length(flyto) > 0 && nrow(flyto) > 0){
    map <- map %>%
      addPopups(flyto$Longitude[1], flyto$Latitude[1],
                ~sprintf("<b>Individual GeneticID: %s</b></br>Skeleton Code: %s",
                         flyto$GeneticID[1], flyto$SkeletonCode[1]),
                options = popupOptions(closeButton = TRUE,
                                       closeOnClick = F)
      ) %>%
      setView(flyto$Longitude[1], flyto$Latitude[1], zoom = 12)
  }
  map
}

# return a ggplot object with the transect plot
transectPlot <- function(df, selection = character(0), map_selected = character(0)){
  # Plot data
  plot <- ggplot(df, mapping = aes(DateMeanInBP, 
                                   Balkan_N_weight, 
                                   fill = .data[["Country"]])) +
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
                        fill = .data[["Country"]]),
                      position = position_dodge(0.3),
                      colour = "black", shape = 21, 
                      alpha = 1, size = 1)
    
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
                      alpha = 1, size = 1.5, fill = "#F9C80E")
    
  }
  plot
}

# Filter References for PCA
filterPCAforReferences <- function(df, modern, ancient) {
  df_modern <- if(!is.null(modern)) filter(df, .data[["Period"]] == "Modern", .data[["Group"]] %in% modern) else NULL
  df_ancient <- if(!is.null(ancient)) filter(df, .data[["Period"]] != "Modern", .data[["Group"]] %in% ancient) else NULL
  
  df_filtered <- if(!is.null(df_modern) & !is.null(df_ancient)) add_row(df_modern, df_ancient) else NULL
  df_filtered <- if(is.null(df_modern) & !is.null(df_ancient)) df_ancient else df_filtered
  df_filtered <- if(!is.null(df_modern) & is.null(df_ancient)) df_modern else df_filtered
  df_filtered <- if(is.null(df_modern) & is.null(df_ancient)) character(0) else df_filtered
  
  df_filtered
} 