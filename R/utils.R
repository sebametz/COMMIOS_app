library(shiny)
library(purrr)
library(leaflet)
library(dplyr)
library(zeallot)

data("context")

filterCurrent <- function(df, period, country, locality, years){
  df_filtered <- if(!is.null(period)) filter(df, .data[["Period"]] %in% period) else df
  df_filtered <- if(!is.null(country)) filter(df_filtered, .data[["Country"]] %in% country) else df_filtered
  df_filtered <- if(!is.null(locality)) filter(df_filtered, .data[["Locality"]] %in% locality) else df_filtered
  df_filtered <- if(!is.null(years)) filter(df_filtered, between(.data[["DateMeanInBP"]] - 1950, .env[["years"]][1], .env[["years"]][2])) else df_filtered
  df_filtered 
}

interactiveMap <- function(df) {
  map <- leaflet(df) %>%
    addProviderTiles(providers$Esri.WorldTopoMap) %>%
    fitBounds(lng1 = -7.64133, lat1 = 50.10319, lng2 = 1.75159, lat2 = 60.15456) %>%
    addMarkers(lng = ~Longitude, lat = ~Latitude,
               clusterOptions = markerClusterOptions(),
               label = ~GroupID,
               popup = ~GeneticID,
               layerId = ~GeneticID)
  map
}

getChoices <- function(df, var, param, currentFilter){
  choices <- sort(unique(filterCurrent(df, param$periodFilter, param$countryFilter, param$localityFilter, param$yearsFilter)[[var]]))
  selection <- intersect(choices, param[[currentFilter]])
  list(
    choices = if(!is.null(choices)) choices else print("Not choices left"),
    selection = if(length(selection)) param[[currentFilter]] else character(0)
      ) 
}
