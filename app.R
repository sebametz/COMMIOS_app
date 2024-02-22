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
                  "Map",
                  leafletOutput("plotMap")
                  ),
                  tabPanel(
                    "PCA",
                    plotOutput("plotPCA")
                  ),
                  tabPanel(
                    "Ancestry",
                    column(4, align = "center",  plotlyOutput("plotAncestry")),
                   
                    # To define!!!
                    column(8, align = "center",  
                           fluidRow(
                             plotOutput("plotSteppeVsEEF")
                           ),
                           fluidRow(
                             plotOutput("plotOther")
                           )
                           )
                  ),
                  tabPanel(
                    "Table",
                    dataTableOutput("dataTable")
                  )
                )
              ),
              fluidRow(
                column(4, align = "justify",
                       verbatimTextOutput("selectedInfo")),
                column(8, align = "center",
                       plotOutput("plotComposition"))
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
  # render map
  output$plotMap <- renderLeaflet(interactiveMap(dataContext()))
  # render pca
  output$plotPCA <- renderPlot(plotPCA(dataContext(), dataSetsPCA, 
                                      selected = input$plotMap_marker_click$id, 
                                      colourBy = input$colourBy))
  # render ternary - NEEDS IMPROVES
  output$plotAncestry <- renderPlotly(ternaryPlot(dataAncestry()))
  # render table
  output$dataTable <- renderDataTable(dataContext())
}

# Run the application 
shinyApp(ui = ui, server = server)
