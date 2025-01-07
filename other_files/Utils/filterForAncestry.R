
filterAncientReferencesInputs <- function(id) {
  tagList(
    selectInput(NS(id, "ancientFilter"), label = "Select References for Ancestry", 
                choices = sort(unique(ancestry[["GroupID"]])), multiple = T)
  )
}

filterAnceintReferencesServer <- function(id) {
  moduleServer(id, function(input, output, session) {
                 referencesAncestry <- reactive(filter(ancestry, .data[["GroupID"]] %in% input$ancientFilter))
                 referencesAncestry
               })
}

filterAncientReferencesApp <- function(){
  library(shiny)
  library(purrr)
  library(leaflet)
  library(dplyr)
  library(zeallot)
  source("R/utils.R")
  data("ancestry")
  
  ui <- fluidPage(
    sidebarLayout(
      sidebarPanel(filterAncientReferencesInputs("filterAncient")),
      mainPanel(dataTableOutput("tablePCA"))
    )
  )
  server <- function(input, output, session){
    current_df <- filterAnceintReferencesServer("filterAncient")
    output$tablePCA <- renderDataTable(current_df())
    
  }
  shinyApp(ui, server)
}