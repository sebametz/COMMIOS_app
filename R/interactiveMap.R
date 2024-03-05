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