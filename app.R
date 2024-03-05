library(shiny)
library(tidyverse)
library(purrr)
library(leaflet)
library(zeallot)
library(ggtern)
library(DT)
source("R/utils.R")
source("R/filterContext.R")
source("R/filterForPCA.R")
source("R/filterForAncestry.R")
source("R/ternaryPlot.R")
source("R/plotPCA.R")
source("R/transectPlot.R")
source("R/interactiveMap.R")
data("context")
data("references_pca")
data("ancestry")

# Define UI for application
ui <- fluidPage(
  theme = bslib::bs_theme(bootswatch = "cosmo"), # https://bootswatch.com/minty/
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
              filterAncientReferencesInputs("filterAncestry"),
              width = 2
            ),
            # main panel ----
            mainPanel(
              fluidRow(
                tabsetPanel(
                  id="dataExplring",
                  tabPanel(
                  "Distribution",
                  align = "center",
                  leafletOutput("plotMap", width = "60%", height = "60vh"),
                  h5("Time transect"),
                  plotOutput("plotTransect", click = "transect_click", width = "100%", height = "30vh")
                  ),
                  tabPanel(
                    "PCA", align = "center",
                    plotOutput("plotPCA", width = "60vh", height = "60vh", 
                               dblclick = "plotPCA_dblclick"),
                    dataTableOutput("PCAdataTable", width = "80vh", height = "40vh")
                  ),
                  tabPanel(
                    "Ancestry",
                    align = "center",  
                           plotOutput("plotAncestry", width = "80vh", height = "60vh")
                  ),
                  tabPanel(
                    "Table",
                    dataTableOutput("dataTable")
                  )
                )
              ), 
              width = 10
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
  thematic::thematic_shiny() # to match plots with theme
  # inputs from servers ----
  dataContext <- filterContextServer("filterContext")
  dataSetsPCA <- filterPCAReferencesServer("filterPCA")
  dataAncestry <- filterAnceintReferencesServer("filterAncestry")
  # For transect plot
  transect_df <- filter(ancestry, GeneticID %in% context$GeneticID) |> left_join(context, by = c("GeneticID" = "GeneticID"))
  selected_ind <- reactiveVal()
  
  #
  # render components ----
  
  # Panel distribution
  
  
  
  
  # PROBLEM! See how to update selection without updating the map when select new sample
  # These observeEvents return a dataframe
  observeEvent(input$transect_click,
    selected_ind(nearPoints(transect_df, input$transect_click))
    )
  
  observeEvent(input$plotMap_marker_click,
               selected_ind(filter(context, GeneticID %in% input$plotMap_marker_click$id))
               )
  
  ## render map
  output$plotMap <- renderLeaflet(
    interactiveMap(dataContext(),
                   flyto = selected_ind()))
  
  ## render Transect
  output$plotTransect <- renderPlot(transectPlot(transect_df, 
                                                 selection = dataContext()$GeneticID,
                                                 map_selected = selected_ind()), res = 120)
  
  #
  
  # Panel Similarity
  ## render pca
  output$plotPCA <- renderPlot(plotPCA(dataContext(), 
                                       dataSetsPCA,
                                       selected = selected_ind()$GeneticID[1],
                                       colourBy = input$colourBy))
  
  ## render table double click PCA
  output$PCAdataTable <- renderDataTable({
    if(length(input$plotPCA_dblclick)){
      auxdf <- context |> select(GeneticID, GroupID, Period, PCA1, PCA2) |> 
        mutate(Group = GroupID) |> select(GeneticID, Group, Period, PCA1, PCA2) |>
        add_case(reference_pca)
      aux <- nearPoints(auxdf, input$plotPCA_dblclick, threshold = 5)
      aux |> group_by(Group, Period) |> count() |> arrange(desc(n))
    }
  })
  
  ## Panel Ancestry
  # render ternary - click points
  output$plotAncestry <- renderPlot(print(ternaryPlot(dataAncestry(), selected = selected_ind()$GeneticID[1])))
  
  # render table
  output$dataTable <- renderDataTable({
    df <- dataContext()
    if(!is.null(selected_ind())){
      if(nrow(selected_ind()) > 0){
        df <- df |> arrange(fct_relevel(GeneticID, selected_ind()$GeneticID[1]))
        df <- datatable(df) |> formatStyle('GeneticID',
                                     target = 'row',
                                     backgroundColor = styleEqual(selected_ind()$GeneticID[1], '#F9C80E'))
      }
    }  
    df
    }
    )

}

# Run the application 
shinyApp(ui = ui, server = server)
