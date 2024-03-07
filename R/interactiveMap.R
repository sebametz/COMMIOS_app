# return a leaflet object based on current context database
interactiveMap <- function(df, flyto = character(0)) {
  
  sexIcons <- iconList(
    M = makeIcon("www/Icons/male.png",iconWidth = 15, iconHeight = 37.5, iconAnchorX = 8.82, iconAnchorY = 37.5),
    F = makeIcon("www/Icons/female.png",iconWidth = 15, iconHeight = 37.5, iconAnchorX = 8.82, iconAnchorY = 37.5),
    U = makeIcon("www/Icons/genderless.png",iconWidth = 15, iconHeight = 37.5, iconAnchorX = 8.82, iconAnchorY = 37.5)
  )
  
  if(nrow(df) == 0){
    map <- leaflet() %>%
      addProviderTiles(providers$Esri.WorldTopoMap) %>%
      setMaxBounds(lng1 = -15, lat1 = 35, lng2 = 15, lat2 = 65)
    map
  } else {
    map <- leaflet(df) %>%
      addProviderTiles(providers$Esri.WorldTopoMap) %>%
      setMaxBounds(lng1 = -15, lat1 = 35, lng2 = 15, lat2 = 65) %>%
      addMarkers(lng = ~Longitude, lat = ~Latitude, 
                 icon = ~sexIcons[MolecularSex],
                 clusterOptions = markerClusterOptions(disableClusteringAtZoom=13), 
                 label = ~sprintf("Individual GeneticID: %s", GeneticID),
                 popup = ~sprintf("<b>Individual GeneticID: %s</b></br>Skeleton Code: %s", GeneticID, SkeletonCode),
                 layerId = ~GeneticID
      )
  }
  # Fly to section <- Only update if selected_id comes from transect i.e from = 1
  if(length(flyto) && length(flyto$GeneticID)) {
    map <- map %>%
        addPopups(flyto$info$Longitude, flyto$info$Latitude+0.008,
                  ~sprintf("<b>Individual GeneticID: %s</b></br>Skeleton Code: %s",
                           flyto$info$GeneticID[1], flyto$info$SkeletonCode[1]),
                  options = popupOptions(closeButton = TRUE,
                                         closeOnClick = T)) %>%
       setView(flyto$info$Longitude[1], flyto$info$Latitude[1], zoom = 12)
  }
  map
}