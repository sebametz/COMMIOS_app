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
library(ggnewscale)
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
              # Help panel button
              actionButton("toggleButton", "Hide Help"),
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
                  fluidRow(
                  leafletOutput("plotMap", width = "100vw", height = "55vh"),
                  
                  # Help panel, welcome!
                   absolutePanel(
                    id = "helpPanel", align = "justify",
                    top = 120, left = 600, width = "25vw",
                    draggable = TRUE,
                    # Help panel 1
                    wellPanel(
                      HTML(markdownToHTML(fragment.only=TRUE, text=c(helpPanel1)))
                    ),
                    style = "background: #ffbf00;border-color:black;opacity:0.7"
                  )
                  
                  ),
                  fluidRow(
                    column(4,
                           align = "center", 
                           strong(helpComposition),
                           plotOutput("plotComposition", height = 300)),
                    column(8,
                           align = "center", 
                           strong(helpTransect),
                           plotOutput("plotTransect", click = "transect_click", height = 300))
                           # plotOutput("plotTransect", click = "transect_click", width = "60vw", height = "30vh"))
                  ),
                  ),
                  tabPanel(
                    "PCA", 
                    align = "center",
                    plotOutput("plotPCA", width = "60vw", height = "60vh", 
                               dblclick = "plotPCA_dblclick", brush = "plotPCA_brush"),
                    
                    dataTableOutput("PCAdataTable", width = "80vw", height = "40vh"),
                    
                    # Reference panel <- need to improve the size and colours
                    absolutePanel(
                      top = 120, right = 5, width = 410,
                      draggable = TRUE,
                      wellPanel(
                        strong("References"),
                        plotOutput("plotPCAlabels", width = 400, height = 400)
                      ),
                      style = "opacity:0.9"
                    ),
                    
                    # For zoom <- renderUI
                    uiOutput("zoomPCAbrush")
                    
                  ),
                  tabPanel(
                    "Ancestry",
                    align = "center",  
                           plotOutput("plotAncestry", width = "80vw", height = "60vh")
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
     )
    # # To define
    # tabPanel(
    #   title = "About",
    #   fluidPage()
    # ),
    # tabPanel(
    #   title = "Contact",
    #   value = "contact",
    #   fluidPage()
    # )
   )
  )

# Server ----
server <- function(input, output, session) {
  thematic::thematic_shiny() # to match plots with theme
  
  session$onSessionEnded(function() {
    stopApp()
  })
  
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
  
  # HELP PANEL and UPDATES
  ChangeLableHelp <- reactiveValues(HelpButton = FALSE)
  observeEvent(input$toggleButton, {
    # Use shinyjs to hide the absolutePanel
    shinyjs::toggle("helpPanel")
    ChangeLableHelp$HelpButton <- if_else(ChangeLableHelp$HelpButton == 0, TRUE, FALSE)
  })
  
  observeEvent(input$toggleButton, {
    # Use shinyjs to hide the absolutePanel
    updateActionButton(session, "toggleButton", label = if_else(ChangeLableHelp$HelpButton, "Show Help", "Hide Help"))
  })
  
  
  
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
    interactiveMap(dataContext(), flyto = selected_ind_from_transect)}
    )
  
  ## render Transect
  output$plotTransect <- renderPlot(transectPlot(transect_df, 
                                                 selection = dataContext()$GeneticID,
                                                 map_selected = selected_ind), res = 120)

  ## rebder plotComposition
  output$plotComposition <- renderPlot(if(length(selected_ind)) plotComp(selected_ind$GeneticID, ancestry) else plotComp(df = ancestry), 
                                       res = 120)
  
  # Panel PCA
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
      auxdf <- context |> 
        filter(!GeneticID %in% reference_pca) |>
        select(GeneticID, GroupID, Period, PCA1, PCA2) |>
        mutate(Group = GroupID) |>
        add_case(filter(reference_pca, !GeneticID %in% context$GeneticID))
      aux <- nearPoints(auxdf, input$plotPCA_dblclick, threshold = 5)
      # aux |> group_by(Group) |> count() |> arrange(desc(n))
      aux
    }
  })
  
 ## render labels
  output$plotPCAlabels <- renderPlot(
    print(plot_grid(PCA_labels$labels[["colour"]], 
                    ncol = 1)), 
    res = 120
  )
  
  # Define an observer for the button click
  observeEvent(input$closeZoom, {
    # Use shinyjs to hide the absolutePanel
    shinyjs::hide("zoomPanel")
  })
  
  observeEvent(input$plotPCA_brush,{
               # shinyjs::show("zoomPanel")
    # ## render UI zoom
    output$zoomPCAbrush <- renderUI({
      tagList(
        absolutePanel(
          id = "zoomPanel",
          bottom = 20, left = 400, width = 350,
          draggable = TRUE,
          wellPanel(
           strong("Zoom PCA" ),
           plotOutput("plotPCAbrush", width = 300, height = 300, dblclick = "plotPCA_dblclick")
          ),
          style = "opacity: 1",
          actionButton("closeZoom", "Close", class = "btn-success")
        )
      )
    })
  }
 )
  
  ## render Zoom plot
  output$plotPCAbrush <- renderPlot({
    if(length(input$plotPCA_brush)){
     SelectedPointsBrush <- brushedPoints(dataContext(), input$plotPCA_brush)
     pcaBrush <-  plotPCABrush(SelectedPointsBrush,
              dataSetsPCA,
              selected = selected_ind,
              colourBy = input$colourBy,
              zoom = T)
     pcaBrush[["plot"]]
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
