library(shiny)
library(tidyverse)
library(purrr)
library(leaflet)
library(zeallot)
source("R/utils.R")
source("R/filterContext.R")
source("R/filterForPCA.R")
source("R/filterForAncestry.R")
data("context")
data("references_pca")
data("ancestry")

# Define UI for application
ui <- fluidPage(
  navbarPage(
    title = "COMMIOS app",
    tabPanel(
      title = "Explore",
      value = "explore",
      fluidPage(
        sidebarLayout(
            # side bar ----
            sidebarPanel(
              helpText("Explore the COMMIOS aDNA dataset from the UK"),
              h5("Individual(s) selection"),
              # To call filterContext
              filterContextInputs("filterContext"),
              h5("PCA parameters"),
              selectInput("colourBy", "Colour by", 
                          choices = c("Period", "Country", "Locality", "Group"),
                          selected = "Group"),
              filterPCAReferencesInputs("filterPCA"),
              h5("Ancestry plot parameters"),
              filterAncientReferencesInputs("filterAncestry")
            ),
            # main panel ----
            mainPanel(
              fluidRow(
                tabsetPanel(
                  id="dataExplring",
                  tabPanel(
                  "Distribution",
                  leafletOutput("plotMap", width = "100%", height = 400),
                  h5("Time transect"),
                  plotOutput("plotTransect", click = "transect_click")
                  ),
                  tabPanel(
                    "PCA", align = "center",
                    plotOutput("plotPCA", width = "60vh", height = "60vh", 
                               dblclick = "pca_dblclick", brush = "pca_brush"),
                    dataTableOutput("PCAdataTable")
                  ),
                  tabPanel(
                    "Ancestry",
                    column(12, align = "center",  
                           plotlyOutput("plotAncestry", width = "60vh", height = "60vh"))
                  ),
                  tabPanel(
                    "Table",
                    dataTableOutput("dataTable")
                  )
                )
              )
            )
        )
      )
     ),
    # To define
    tabPanel(
      title = "About",
      value = "about",
      fluidPage()
    ),
    tabPanel(
      title = "Contact",
      value = "contact",
      fluidPage()
    )
   )
  )

# Server ----
server <- function(input, output) {
  # inputs from servers ----
  dataContext <- filterContextServer("filterContext")
  dataSetsPCA <- filterPCAReferencesServer("filterPCA")
  dataAncestry <- filterAnceintReferencesServer("filterAncestry")
 
  #
  # render components ----
  
  # Panel distribution
  
  transect_df <- ancestry |> left_join(context, by = c("GeneticID" = "GeneticID"))
  selected_ind <- reactiveVal()
  # map_val <- reactiveValues(transect_selected = character(0)) 
  map_selected <- reactive(input$plotMap_marker_click$id)
  
  
  # PROBLEM! See how to update selection without updating the map when select new sample
  observeEvent(input$transect_click,
    # map_val$transect_selected <- nearPoints(transect_df, input$transect_click)
    selected_ind(nearPoints(transect_df, input$transect_click)))
  
  observeEvent(input$plotMap_marker_click,
                 selected_ind(filter(context, GeneticID %in% input$plotMap_marker_click$id)))
  
  ## render map
  output$plotMap <- renderLeaflet(
    interactiveMap(dataContext(),
                   flyto = selected_ind()))
  
  ## render Transect
  output$plotTransect <- renderPlot(transectPlot(transect_df, 
                                                 selection = dataContext()$GeneticID,
                                                 map_selected = selected_ind()))
  
  #
  
  # Panel Similarity
  ## render pca
  output$plotPCA <- renderPlot(plotPCA(dataContext(), dataSetsPCA, 
                                      selected = selected_ind(), 
                                      colourBy = input$colourBy))
  # render ternary - NEEDS IMPROVES
  output$plotAncestry <- renderPlotly(ternaryPlot(dataAncestry()))
  # render table
  output$dataTable <- renderDataTable(dataContext())
}

# Run the application 
shinyApp(ui = ui, server = server)
