
filterPCAReferencesInputs <- function(id) {
  tagList(
    selectInput(NS(id, "modernFilter"), label = "Select Modern References", choices = sort(unique(filter(reference_pca, .data[["Period"]] == "Modern")$Group)), multiple = T),
    selectInput(NS(id, "ancientFilter"), label = "Select Ancient References", choices = sort(unique(filter(reference_pca, .data[["Period"]] != "Modern")$Group)), multiple = T)
  )
}

filterPCAReferencesServer <- function(id) {
  moduleServer(id, function(input, output, session) {
                 referencesPCA <- reactive(filterPCAforReferences(reference_pca, input$modernFilter, input$ancientFilter))
                 modernPCA <- reactive(filter(reference_pca, .data[["Period"]] == "Modern"))
                 list(
                   references = referencesPCA,
                   modern = modernPCA
                 )
               })
}

filterPCAReferencesApp <- function(){
  library(shiny)
  library(purrr)
  library(leaflet)
  library(dplyr)
  library(zeallot)
  source("R/utils.R")
  data("context")
  data("references_pca")
  
  ui <- fluidPage(
    sidebarLayout(
      sidebarPanel(filterPCAReferencesInputs("filterPCA")),
      mainPanel(dataTableOutput("tablePCA"))
    )
  )
  server <- function(input, output, session){
    current_df <- filterPCAReferencesServer("filterPCA")
    output$tablePCA <- renderDataTable(current_df[["references"]]())
    
  }
  shinyApp(ui, server)
}