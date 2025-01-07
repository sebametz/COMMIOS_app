source("R/filterContext.R")


dataMapInput <- function(id) {
  tagList(
    filterContextInputs(NS(id, "contexFiltering"))
  )
}


mapServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    dataToMap <- filterContextServer("contexFiltering")
    dataToMap
    }
  )
}



mapApp <- function() {
  library(shiny)
  library(purrr)
  library(leaflet)
  library(dplyr)
  library(zeallot)
  source("R/utils.R")
  source("R/filterContext.R")
  data("context")
  
  ui <- fluidPage(
    sidebarLayout(
      sidebarPanel(dataMapInput("map")),
      mainPanel(leafletOutput("plotMap"))
    )
  )
  server <- function(input, output, session){
    mapInfo <- mapServer("map")
    output$plotMap <- renderLeaflet(interactiveMap(mapInfo()))
  }
  shinyApp(ui, server)
}


