library(shiny)
library(tidyverse)
library(purrr)
library(leaflet)
library(zeallot)
library(RColorBrewer)
library(ggtern)
library(DT)
library(markdown)
library(cowplot)
library(shinyjs)
source("R/utils.R")
source("R/filterContext.R")
source("R/filterForPCA.R")
source("R/filterForAncestry.R")
source("R/ternaryPlot.R")
source("R/plotPCA.R")
source("R/plotComposition.R")
source("R/transectPlot.R")
source("R/interactiveMap.R")
data("context")
data("references_pca")
data("ancestry")

# Define UI for application
ui <- fluidPage(
  useShinyjs(),
  theme = bslib::bs_theme(bootswatch = "cosmo"), # https://bootswatch.com/minty/
  tags$head(
    HTML('<link rel="icon" href="commios-logo2.jpeg" 
                type="image/jpeg" />')),
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
                  leafletOutput("plotMap", width = "100%", height = "60vh"),
                  h5("Time transect"),
                  plotOutput("plotTransect", click = "transect_click", width = "60%", height = "30vh"),
                 
                  
                   absolutePanel(
                    id = "helpPanel",
                    top = 120, right = 5, width = "15%",
                    draggable = TRUE,
                    wellPanel(
                      HTML(markdownToHTML(fragment.only=TRUE, text=c(
                        "In this panel you find a map of distribution with all the COMMIOS samples and already published from the UK.
                        
To see more information about particular individuals `click` on the map. 

The plot at the bottom is a by individual estimates of EEF ancestry and one standard error for all individuals fitting a 
three-way admixture model (EEF + WHG + Yamnaya) at P > 0.01 using qpAdm. Select a point to see localization in the map.

Selected individuals will be ploted in the `PCA` and `Ancestry` panel.

Here we show the EEF, WHG and Yamnaya composition of selected individual. 

For further information, go to Individual panel.
"
                      ))),
                      plotOutput("plotComposition")
                    ),
                    style = "opacity: 0.92"
                  ),
                  
                  actionButton("toggleButton", "Toggle Panel")
                  
                  
                  
                  ),
                  tabPanel(
                    "PCA", align = "center",
                    plotOutput("plotPCA", width = "60vh", height = "60vh", 
                               dblclick = "plotPCA_dblclick", brush = "plotPCA_brush"),
                    dataTableOutput("PCAdataTable", width = "80vh", height = "40vh"),
                    
                    # For colours <- need to solve the shape problem!
                    absolutePanel(
                      top = 120, right = 5, width = 410,
                      draggable = TRUE,
                      wellPanel(
                        HTML(markdownToHTML(fragment.only=TRUE, text=c(
                          "PCA labels, move if it is in the way!"
                        ))),
                        plotOutput("plotPCAlabels", width = 400, height = 400)
                      ),
                      style = "opacity: 0.92"
                    ),
                    
                    # For zoom <- renderUI
                    uiOutput("zoomPCAbrush")
                    
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
                ),
              ), 
              width = 10
            )
        )
      )
     ),
    # To define
    tabPanel(
      title = "About",
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
  
  selected_ind_from_transect <- reactiveValues(
    GeneticID = character(0),
    info = tibble()
  )
  
  selected_ind_from_map <- reactiveValues(
    GeneticID = character(0),
    info = tibble()
  )
  
  selected_ind <- reactiveValues(
    GeneticID = character(0),
    info = tibble()
  )
  
  PCA_labels <- reactiveValues(labels = character(0))
  #
  # render components ----
  
  # Panel distribution
  
  # PROBLEM! See how to update selection without updating the map when select new sample
  # These observeEvents return a dataframe
  observeEvent(input$transect_click,{
    selection <- nearPoints(transect_df, input$transect_click)
    selected_ind_from_transect$GeneticID <- selection[['GeneticID']][1]
    selected_ind_from_transect$info <- filter(context, GeneticID == selection[['GeneticID']][1])
    
    selected_ind$GeneticID <- selection[['GeneticID']][1]
    selected_ind$info <- filter(context, GeneticID == selection[['GeneticID']][1])
      }
    )
  
  observeEvent(input$plotMap_marker_click,{
    selection <- input$plotMap_marker_click$id
    
    selected_ind_from_map$GeneticID <- selection
    selected_ind_from_map$info <- filter(context, GeneticID == selection)
    
    selected_ind$GeneticID <- selection
    selected_ind$info <- filter(context, GeneticID == selection)
    }
  )
  
  ## render map - updated done
  output$plotMap <- renderLeaflet({
    interactiveMap(dataContext(), flyto = selected_ind_from_transect)
    }
    )
  
  ## render Transect
  output$plotTransect <- renderPlot(transectPlot(transect_df, 
                                                 selection = dataContext()$GeneticID,
                                                 map_selected = selected_ind), res = 120)
  
  #
  ## rebder plotComposition
  
  output$plotComposition <- renderPlot(if(length(selected_ind)) plotComp(selected_ind$GeneticID, ancestry) else plotComp(df = ancestry), res = 94)
  
  
  # Define an observer for the button click
  observeEvent(input$toggleButton, {
    # Use shinyjs to hide the absolutePanel
    shinyjs::toggle("helpPanel")
  })
  
  
  # Panel Similarity
  ## render pca
  output$plotPCA <- renderPlot({
    pca <- plotPCA(dataContext(),
            dataSetsPCA,
            selected = selected_ind,
            colourBy = input$colourBy)
    PCA_labels$labels <- pca[["labels"]]
    pca[["plot"]]
    }    
    )
  
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
  
 ## render labels
  output$plotPCAlabels <- renderPlot(
    print(plot_grid(PCA_labels$labels[["colour"]], ncol = 1)), 
    res = 94
  )
  
  ## render UI zoom
  output$zoomPCAbrush <- renderUI({
    tagList(
      absolutePanel(
        id = "zoomPanel",
        bottom = 20, left = 125, width = "40%",
        draggable = TRUE,
        wellPanel(
          HTML(markdownToHTML(fragment.only=TRUE, text=c(
            "Zoom plot of PCA brush"
          ))),
          plotOutput("plotPCAbrush", width = 300, height = 300)
          
        ),
        style = "opacity: 0.75",
        actionButton("closeZoom", "Close Panel")
      )
    )
  })
  # Define an observer for the button click
  observeEvent(input$closeZoom, {
    # Use shinyjs to hide the absolutePanel
    shinyjs::hide("zoomPanel")
  })
  
  observeEvent(input$plotPCA_brush,
               shinyjs::show("zoomPanel")
               )
  
  
  output$plotPCAbrush <- renderPlot({
    if(length(input$plotPCA_brush)){
      res <- brushedPoints(dataContext(), input$plotPCA_brush)
      plotPCA(res,
              dataSetsPCA,
              selected = selected_ind,
              colourBy = input$colourBy,
              zoom = T)
    }
  })
  
  
  ## Panel Ancestry
  # render ternary - click points
  output$plotAncestry <- renderPlot(print(ternaryPlot(dataAncestry(), selected = selected_ind$GeneticID)))
  
  # render table
  output$dataTable <- renderDataTable({
    df <- dataContext()
    if(length(selected_ind$GeneticID) > 0){
       df <- if(selected_ind$GeneticID %in% df$GeneticID) df else df |> add_row(selected_ind$info)
       df <- df |> arrange(fct_relevel(GeneticID, selected_ind$GeneticID))
       df <- datatable(df) |> formatStyle('GeneticID',
                                       target = 'row',
                                       backgroundColor = styleEqual(selected_ind$GeneticID, '#F9C80E'))
    }
     df
    }
  )
}

# Run the application 
shinyApp(ui = ui, server = server)
