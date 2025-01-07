library(shiny)
library(tidyverse)
library(shinyjs)
library(leaflet)
library(htmlwidgets)
library(shinyWidgets)
library(plotly)
library(gridExtra)
library(lemon)
source("R/initModal.R")
source("R/functions.R")
source("R/transectPlot.R")
data("data_ref")


## ADD:
# - Sex parameter
# - Select different layouts in map
# define parameters ----

periods_param_choices <- c("Neolithic", "C/EBA", "BA", "IA", "Romans", "Early Medieval/Vikings") #"Mesolithic", 
names(periods_param_choices) <- c("Neolithic", "C/EBA", "BA", "IA", "Romans", "Early Medieval/Vikings") #"Mesolithic",

timeseries_param_choices <- c("EEF", "WHG", "Steppe")
names(timeseries_param_choices) <- c("Early European Farmers (EEF)", "Western hunter-gatherer (WHG)", "Yamnaya pastoralists (Steppe)")

pca_param_choices  <- data_ref |>
  distinct(Period, Country, Group)

#addapt ternary plot
ternary_param_choices <- data_ref |>
  distinct(Period, Country, Group)

# App UI ----
ui <- fluidPage(
  useShinyjs(),
  title = "COMMIOS app",
  theme = bslib::bs_theme(preset = "slate"),
  
  # Add scripts for favicon or bootstrap  
  tags$head(tags$style(HTML('
                              .treejs .treejs-switcher::before,
                              .treejs .treejs-switcher:hover::before {
                                  border-top: 4px solid white;
                              }
                              ')
                       )
            ),
  # Header panel
  headerPanel(
    title=tags$a(href='https://commiosarchaeology.wordpress.com/',tags$img(src='commios-lofo-for-website-1-1.png', width = 100*2.85*1.75), target="_blank"),
    # add here the tags for the head 
    tags$head(tags$link(rel = "icon", type = "image/png", href = "commios-logo2.png"), windowTitle="COMMIOS dashboard")
  ),
  
  # Input widgets
  fluidRow(
    column(5,
           conditionalPanel(condition = "input.plot_tabs != 'User guide'",
                            tabsetPanel(id = "ui_tab",
                                        tabPanel("Map",
                                                 column(12,
                                                        checkboxGroupButtons( 
                                                          inputId = "map_group",
                                                          label = "Select periods: ",
                                                          choices = periods_param_choices,
                                                          # selected = names(periods_param_choices),
                                                          size = "sm", 
                                                          status = 'info', 
                                                          justified = T
                                                        ),
                                                        h5("Click on an Individual for more information"),
                                                        shinycssloaders::withSpinner(
                                                          leaflet::leafletOutput("map", height="800px"), size=2, color="#0080b7"
                                                        )
                                                 )
                                        )
                            )
           ),
           conditionalPanel(condition = "input.plot_tabs != 'User guide'", column(12)),
    ),
    column(7,
           tabsetPanel(id="plot_tabs",
                       tabPanel("Explore",
                                # parameters and plot type
                                fluidRow(
                                  column(9,
                                         radioButtons("plot_type","Plot type:", choices=c("Time series", 
                                                                                          "Principal Components Analysis (PCA)", 
                                                                                          "Ancient contribution"), inline=T)
                                  ),
                                  column(3,
                                         conditionalPanel(condition = "input.plot_type =='Time series'",
                                                          downloadBttn("downladSeries", "Save Plot", color = "royal", size = "sm")
                                         ),
                                         conditionalPanel(condition = "input.plot_type =='Principal Components Analysis (PCA)'",
                                                          downloadBttn("downladPCA", "Save Plot", color = "royal", size = "sm")
                                         ),
                                         conditionalPanel(condition = "input.plot_type =='Ancient contribution'",
                                                          downloadBttn("downladTernary", "Save Plot", color = "royal", size = "sm")
                                         )
                                  )
                                ),
                                conditionalPanel(condition = "input.plot_type =='Time series'",
                                                 fluidRow(
                                                   selectInput("timeseries_param", label = "Select parameter:", 
                                                               choices = timeseries_param_choices))
                                ),
                                conditionalPanel(condition = "input.plot_type =='Principal Components Analysis (PCA)'",
                                                 fluidRow(
                                                   column(3,
                                                          treeInput("pca_param", label = "Select references: ",
                                                                    choices = create_tree(pca_param_choices),
                                                                    returnValue = "text",
                                                                    closeDepth = 0,
                                                                    selected = c("Scottish","English", "Spanish", "French")
                                                          )
                                                   ),
                                                   column(9,
                                                          plotOutput("pca_plot", hover = "pca_hover", height = "850px",
                                                                     width = "850px"),
                                                          # plotOutput("pca_legend", width = "850px"),
                                                          column(6,
                                                                 tags$pre(class = "shiny-text-output noplaceholder",
                                                                          uiOutput("pca_hover_text", inline = TRUE)
                                                                 )
                                                          )
                                                   )
                                                 )
                                ),
                                conditionalPanel(condition = "input.plot_type =='Ancient contribution'",
                                                 fluidRow(
                                                   column(3,
                                                          treeInput("ternary_param", label = "Select references: ",
                                                                    choices = create_tree(ternary_param_choices),
                                                                    returnValue = "text",
                                                                    closeDepth = 0
                                                          )
                                                   ),
                                                   column(9,
                                                          plotlyOutput("ternary_plot", height = "600px")
                                                   )
                                                 )
                                ),
                                # define plots
                                conditionalPanel(condition = "input.plot_type =='Time series'", 
                                                 column(12,
                                                        plotOutput("timersies_plot", click = "transect_click", height = "600px"),
                                                        tags$p("Select individual for more information: "),
                                                        fluidRow(
                                                          column(6,
                                                                 tags$pre(class = "shiny-text-output noplaceholder",
                                                                          uiOutput("transect_clicked", inline = TRUE)
                                                                 )),
                                                          column(6,
                                                                 uiOutput("transect_ancesty", inline = TRUE)
                                                          )
                                                        )
                                                 )
                                )
                       ),
                       # tabPanel("Individual profile",
                       #          
                       #          ),
                       tabPanel("Table",
                                column(12, div(DT::dataTableOutput("table_input"), style = "font-size:70%"))
                       ),
                       tabPanel("User guide",
                                fluidRow(
                                  column(8, includeMarkdown('./other/help.md')
                                  )
                                )
                       )
           )
    )
  )
  
)

# App server ----
server <- function(input, output, session){
  thematic::thematic_shiny() # to match plots with theme
  
  # exit app when session stop
  session$onSessionEnded(function() {
    stopApp()
  })
  
  # prepare data to use 
  data_ref_mod <- data_ref |>
    filter(Period != "Mesolithic") |>
    separate(`Usage Note`, sep = ";", into = c("Type", "Assessment", "Warnings", "isRef", "SNPs"), extra = "drop", remove = F)
  # |>
  #   # done
  #   separate(Locality, sep = ";", into = c("LocID", "Latitude", "Longitude", "CorrectLocalityName", "UniqueLocalityName"), extra = "drop", remove = F) |>
  #   # Not needed
  #   mutate(Latitude = as.numeric(Latitude), Longitude = as.numeric(Longitude)) |>
  #   mutate(siteName = CorrectLocalityName) |>
  #   separate(CorrectLocalityName, sep = ",", into = "Country", extra = "drop", remove = F) |>
  #   mutate(Country = if_else(Country == "Roscommon", "Ireland", Country)) |>
  #   mutate(Period = if_else(`Genetic ID` %in% c("I3044", "I3045"), "Early Medieval/Vikings", Period)) |>
  #   separate(Reference, sep = ";", into = c("political_entity", "Group"), extra = "drop", remove = F) |>
  #   separate(`Usage Note`, sep = ";", into = c("Type", "Assessment", "Warnings", "isRef", "SNPs"), extra = "drop", remove = F) |>
  #   rename(DateMeanInBP = `Date mean in BP in years before 1950 CE [OxCal mu for a direct radiocarbon date, and average of range for a contextual date]`)
  
  df <- data_ref_mod |>
    filter(DataRef == "AADR_UK")
  
  
  # reactive values
  reactive_objects <- reactiveValues()
  reactive_objects$map_periods <- unique(df$Period)
  reactive_objects$context <- df
  
  # Show help when app start
  # helpModalServer("helpModal") # NEED TO WORK ON IT
  
  # Select map set up
  map = leaflet::createLeafletMap(session, 'map')
  
  # render map and transect before Shiny flushes the reactive system
  session$onFlushed(once = T, function() {
    
    # # Rendering the Map
    output$map <- leaflet::renderLeaflet({
      plotMap(reactive_objects$context)
    })
    
    # # Rendering timseries plot
    output$timersies_plot <- renderPlot(.seriesPlot())
    
  })
  # timeseries plot
  .seriesPlot <- reactive({
    plotTransect(data = df, 
                 data_ref = reactive_objects$context, 
                 selected = reactive_objects$selection, # SELECTION!!!
                 plotBy = reactive_objects$transect_par)
  })
  
  plotInputSeries = function(){.seriesPlot()}
  
  # download timeseries plot
  output$downladSeries <- downloadHandler(
    filename = function() {str_c("TimeSeries_", format(Sys.time(),'%Y-%m-%d_%H:%M:%S'), ".svg", sep = "")},
    content = function(file) {
      ggsave(file, plot = plotInputSeries(), units = "cm", device = "svg", dpi = 300, width = 25, height = 18)
    }
  )
  
  
  # # Table interface
  output$table_input=DT::renderDataTable({
    DT::datatable(reactive_objects$context, selection = "single", rownames = FALSE, filter = "top",
                  options = list(scrollY = '600px', paging = FALSE, scrollX = TRUE, dom = "ltipr")
    )
  })
  
  # Map group selected 
  observe({
    req(input$map_group)
    reactive_objects$context <- update_context_by_period(df, input$map_group)
  })
  # 
  
  # if any click occurred in transect
  observeEvent(input$transect_click,{
    req(reactive_objects$transect_par)
    selection <- nearPoints(df, input$transect_click, yvar = reactive_objects$transect_par, maxpoints = 1)
    reactive_objects$selection <- selection
  })
  
  # if any click occurred in map 
  observeEvent(input$map_marker_click,{
    selection <- input$map_marker_click
    reactive_objects$selection <- filter(df, `Genetic ID` == selection$id)
  })
  
  # check for timeseries parameters
  observe({
    transect_par <- input$timeseries_param
    reactive_objects$transect_par <- transect_par
  })
  
  # change map if there is a change in selected individuals
  map_proxy=leaflet::leafletProxy("map")
  observeEvent(input$transect_click,{
    req(reactive_objects$selection)
    if(nrow(reactive_objects$selection) > 0){
      point <- reactive_objects$selection
      map_proxy %>% leaflet::setView(lng=point$Longitude, lat=point$Latitude, zoom=16)
    }
  })
  
  # Info of selected individual
  output$transect_clicked <- renderUI({
    req(reactive_objects$selection)
    point <- reactive_objects$selection 
    HTML(
      paste0(
        "<br> Genetic ID: ", point$`Genetic ID`,
        "<br> Master ID: ", point$`Master ID`,
        "<br> Molecular Sex: ", point$`Molecular Sex`,
        "<br> Locality: ", point$`Correct Locality Name`,
        "<br> % EEF: ", round(point$EEF, digits = 2), " ± ", round(point$`EEF SE`, digits = 2),
        "<br> % Steppe: ", round(point$Steppe, digits = 2), " ± ", round(point$`Steppe SE`, digits = 2),
        "<br> Status: ", if_else(point$`qpAdm Pvalue` > 0.01,
                                 paste0('<span style="color: #77b300;">', paste0(round(point$`qpAdm Pvalue`, digits = 4),'</span>')),
                                 paste0('<span style="color: #c00;">', paste0(round(point$`qpAdm Pvalue`, digits = 4), ' (Warning) </span>'))
        ),
        "<br> Sample Quality: ", if_else(point$`qpAdm Pvalue` > 0.01,
                                         paste0('<span style="color: #77b300;">', paste0(point$SNPs,'</span>')),
                                         paste0('<span style="color: #c00;">', paste0(point$SNPs, '</span>'))
        )
      )
    )
  })
  
  # Ancestry plot of selected indiviudal
  output$transect_ancesty <- renderUI({
    req(reactive_objects$selection)
    point <- reactive_objects$selection
    
    output$plot_ancestry <- renderPlot({
      get_composition(point)
    })
    
    plotOutput("plot_ancestry")
  })
  
  # Selected references parameters
  observe({
    req(input$pca_param)
    reactive_objects$pca_references <- input$pca_param
  })
  
  # plot PCA
  output$pca_plot <- renderPlot(.pcaPlot())
  # output$pca_legend <- renderPlot(grid.arrange(.pcaPlot()[[2]]))
  
  # PCA plot
  .pcaPlot <- reactive({
    get_pca(data_ref_mod, reactive_objects$context, 
            references = reactive_objects$pca_references, 
            selected = reactive_objects$selection)
  })
  
  plotInputPCA = function(){.pcaPlot()}
  
  # download PCA plot
  output$downladPCA <- downloadHandler(
    filename = function() {str_c("PCA", format(Sys.time(),'%Y-%m-%d_%H:%M:%S'), ".svg", sep = "")},
    content = function(file) {
      ggsave(file, plot = plotInputPCA(), units = "px", device = "svg", dpi = 300, width = 850, height = 850)
    }
  )
  
  
  # PCA hover individual show info
  observe({
    req(input$pca_hover)
    selection <- nearPoints(data_ref_mod, input$pca_hover, maxpoints = 5)
    aux <- filter(selection, Period != "Present")
    selection <- if(nrow(aux) > 0) aux[1,] else selection[1,]
    reactive_objects$pca_selection <- selection
  })
  
  output$pca_hover_text <- renderUI({
    req(reactive_objects$pca_selection)
    point <- reactive_objects$pca_selection 
    HTML(
      paste0("<br> Hover: ",
             "<br> Genetic ID: ", point$`Genetic ID`,
             "<br> Master ID: ", point$`Master ID`,
             "<br> Molecular Sex: ", point$`Molecular Sex`,
             "<br> Locality: ", point$`Correct Locality Name`,
             "<br> Period ", point$Period,
             "<br> Date: ", point$`Full date`,
             "<br> Sample Quality: ", if_else(point$SNPs == "SNPs 1240k > 100k",
                                              paste0('<span style="color: #77b300;">', paste0(point$SNPs,'</span>')),
                                              paste0('<span style="color: #f80;">', paste0(point$SNPs, '</span>'))
             )
      )
    )
  })
  
  # # Ternary plot
  # Observe references
  observe({
    req(input$ternary_param)
    reactive_objects$ternary_ref <- input$ternary_param
  })
  # get plot
  output$ternary_plot <- renderPlotly(.ternaryPlot())
  
  # PCA plot
  .ternaryPlot <- reactive({
    get_ternary(data_ref_mod, reactive_objects$context,
                references = reactive_objects$ternary_ref,
                selected = reactive_objects$selection
    )
  })
  
  # download PCA plot
  output$downladTernary <- downloadHandler(
    filename = function() {str_c("Ternary", format(Sys.time(),'%Y-%m-%d_%H:%M:%S'), ".html", sep = "")},
    content = function(file) {
      htmlwidgets::saveWidget(as_widget(.ternaryPlot()), file)
    }
  )
  
}


# Run the application 
shinyApp(ui = ui, server = server)


#########
# Use data_slider for filter sites from an specif range of dates
# h4("Date range in mean BP:"),
# uiOutput("date_slider"),
# materialSwitch(inputId = "SwitchByInd", label = h4("Map by Individual"), status = "info"),

# # date slider widget
# output$date_slider <- renderUI({
#   noUiSliderInput(
#     inputId = "date_range", label = NULL, #"Date range in mean BP:",
#     min = min(df$DateMeanInBP), max = max(df$DateMeanInBP), # needs to be corrected AD and BC
#     value = c(min(df$DateMeanInBP), max(df$DateMeanInBP)),
#     margin = 20, tooltips = TRUE, step = 1, 
#     format = wNumbFormat(decimals = 0, thousand = ".")
#   )    
# })
###############

# data("modernRef")
# data("ancientRef")

# library(purrr)
# library(leaflet)
# library(zeallot)
# library(RColorBrewer)
# 
# library(DT)
# library(markdown)
# library(cowplot)
# 
# library(ggnewscale)
# source("R/utils.R")
# source("R/filterContext.R")
# source("R/filterForPCA.R")
# source("R/filterForAncestry.R")
# source("R/ternaryPlot.R")
# source("R/plotPCA.R")
# source("R/plotComposition.R")
# source("R/transectPlot.R")
# source("R/interactiveMap.R")
# data("context")
# data("references_pca")
# data("ancestry")

# 
# # Define UI for application
# ui <- fluidPage(
#   useShinyjs(),
#   theme = bslib::bs_theme(bootswatch = "cosmo"), # https://bootswatch.com/minty/
#   tags$head(
#     HTML('<link rel="icon" href="commios-logo2.jpeg" 
#                 type="image/jpeg" />')),
#   navbarPage(
#     title = "COMMIOS app",
#     tabPanel(
#       title = "Explore",
#       value = "explore",
#       fluidPage(
#         sidebarLayout(
#             # side bar ----
#             sidebarPanel(
#               helpText("Explore the COMMIOS aDNA dataset from the UK"),
#               h5("Individual(s) selection"),
#               # To call filterContext
#               filterContextInputs("filterContext"),
#               h5("PCA parameters"),
#               selectInput("colourBy", "Colour by", 
#                           choices = c("Period", "Country", "Locality", "Group"),
#                           selected = "Group"),
#               filterPCAReferencesInputs("filterPCA"),
#               h5("Ancestry plot parameters"),
#               filterAncientReferencesInputs("filterAncestry"),
#               # Help panel button
#               actionButton("toggleButton", "Hide Help"),
#               width = 2
#             ),
#             # main panel ----
#             mainPanel(
#               fluidRow(
#                 tabsetPanel(
#                   id="dataExplring",
#                   tabPanel(
#                   "Distribution",
#                   align = "center",
#                   fluidRow(
#                   leafletOutput("plotMap", width = "100vw", height = "55vh"),
#                   
#                   # Help panel, welcome!
#                    absolutePanel(
#                     id = "helpPanel", align = "justify",
#                     top = 120, left = 600, width = "25vw",
#                     draggable = TRUE,
#                     # Help panel 1
#                     wellPanel(
#                       HTML(markdownToHTML(fragment.only=TRUE, text=c(helpPanel1)))
#                     ),
#                     style = "background: #ffbf00;border-color:black;opacity:0.7"
#                   )
#                   
#                   ),
#                   fluidRow(
#                     column(4,
#                            align = "center", 
#                            strong(helpComposition),
#                            plotOutput("plotComposition", height = 300)),
#                     column(8,
#                            align = "center", 
#                            strong(helpTransect),
#                            plotOutput("plotTransect", click = "transect_click", height = 300))
#                            # plotOutput("plotTransect", click = "transect_click", width = "60vw", height = "30vh"))
#                   ),
#                   ),
#                   tabPanel(
#                     "PCA", 
#                     align = "center",
#                     plotOutput("plotPCA", width = "60vw", height = "60vh", 
#                                dblclick = "plotPCA_dblclick", brush = "plotPCA_brush"),
#                     
#                     dataTableOutput("PCAdataTable", width = "80vw", height = "40vh"),
#                     
#                     # Reference panel <- need to improve the size and colours
#                     absolutePanel(
#                       top = 120, right = 5, width = 410,
#                       draggable = TRUE,
#                       wellPanel(
#                         strong("References"),
#                         plotOutput("plotPCAlabels", width = 400, height = 400)
#                       ),
#                       style = "opacity:0.9"
#                     ),
#                     
#                     # For zoom <- renderUI
#                     uiOutput("zoomPCAbrush")
#                     
#                   ),
#                   tabPanel(
#                     "Ancestry",
#                     align = "center",  
#                            plotOutput("plotAncestry", width = "80vw", height = "60vh")
#                   ),
#                   tabPanel(
#                     "Table",
#                     dataTableOutput("dataTable")
#                   )
#                 ),
#               ),        
#               width = 10
#             )
#         )
#       )
#      )
#     # # To define
#     # tabPanel(
#     #   title = "About",
#     #   fluidPage()
#     # ),
#     # tabPanel(
#     #   title = "Contact",
#     #   value = "contact",
#     #   fluidPage()
#     # )
#    )
#   )
# 
# # Server ----
# server <- function(input, output, session) {
#   thematic::thematic_shiny() # to match plots with theme
#   
#   session$onSessionEnded(function() {
#     stopApp()
#   })
#   
#   # inputs from servers ----
#   dataContext <- filterContextServer("filterContext")
#   dataSetsPCA <- filterPCAReferencesServer("filterPCA")
#   dataAncestry <- filterAnceintReferencesServer("filterAncestry")
#   # For transect plot
#   transect_df <- filter(ancestry, GeneticID %in% context$GeneticID) |> left_join(context, by = c("GeneticID" = "GeneticID"))
#   
#   selected_ind_from_transect <- reactiveValues(
#     GeneticID = character(0),
#     info = tibble()
#   )
#   
#   selected_ind_from_map <- reactiveValues(
#     GeneticID = character(0),
#     info = tibble()
#   )
#   
#   selected_ind <- reactiveValues(
#     GeneticID = character(0),
#     info = tibble()
#   )
#   
#   PCA_labels <- reactiveValues(labels = character(0))
#   #
#   # render components ----
#   
#   # HELP PANEL and UPDATES
#   ChangeLableHelp <- reactiveValues(HelpButton = FALSE)
#   observeEvent(input$toggleButton, {
#     # Use shinyjs to hide the absolutePanel
#     shinyjs::toggle("helpPanel")
#     ChangeLableHelp$HelpButton <- if_else(ChangeLableHelp$HelpButton == 0, TRUE, FALSE)
#   })
#   
#   observeEvent(input$toggleButton, {
#     # Use shinyjs to hide the absolutePanel
#     updateActionButton(session, "toggleButton", label = if_else(ChangeLableHelp$HelpButton, "Show Help", "Hide Help"))
#   })
#   
#   
#   
#   # Panel distribution
#   
#   # PROBLEM! See how to update selection without updating the map when select new sample
#   # These observeEvents return a dataframe
#   observeEvent(input$transect_click,{
#     selection <- nearPoints(transect_df, input$transect_click)
#     selected_ind_from_transect$GeneticID <- selection[['GeneticID']][1]
#     selected_ind_from_transect$info <- filter(context, GeneticID == selection[['GeneticID']][1])
#     
#     selected_ind$GeneticID <- selection[['GeneticID']][1]
#     selected_ind$info <- filter(context, GeneticID == selection[['GeneticID']][1])
#       }
#     )
#   
#   observeEvent(input$plotMap_marker_click,{
#     selection <- input$plotMap_marker_click$id
#     
#     selected_ind_from_map$GeneticID <- selection
#     selected_ind_from_map$info <- filter(context, GeneticID == selection)
#     
#     selected_ind$GeneticID <- selection
#     selected_ind$info <- filter(context, GeneticID == selection)
#     }
#   )
#   
#   ## render map - updated done
#   output$plotMap <- renderLeaflet({
#     interactiveMap(dataContext(), flyto = selected_ind_from_transect)}
#     )
#   
#   ## render Transect
#   output$plotTransect <- renderPlot(transectPlot(transect_df, 
#                                                  selection = dataContext()$GeneticID,
#                                                  map_selected = selected_ind), res = 120)
# 
#   ## rebder plotComposition
#   output$plotComposition <- renderPlot(if(length(selected_ind)) plotComp(selected_ind$GeneticID, ancestry) else plotComp(df = ancestry), 
#                                        res = 120)
#   
#   # Panel PCA
#   ## render pca
#   output$plotPCA <- renderPlot({
#     pca <- plotPCA(dataContext(),
#             dataSetsPCA,
#             selected = selected_ind,
#             colourBy = input$colourBy)
#     PCA_labels$labels <- pca[["labels"]]
#     pca[["plot"]]
#     }    
#     )
#   
#   ## render table double click PCA
#   output$PCAdataTable <- renderDataTable({
#     if(length(input$plotPCA_dblclick)){ 
#       auxdf <- context |> 
#         filter(!GeneticID %in% reference_pca) |>
#         select(GeneticID, GroupID, Period, PCA1, PCA2) |>
#         mutate(Group = GroupID) |>
#         add_case(filter(reference_pca, !GeneticID %in% context$GeneticID))
#       aux <- nearPoints(auxdf, input$plotPCA_dblclick, threshold = 5)
#       # aux |> group_by(Group) |> count() |> arrange(desc(n))
#       aux
#     }
#   })
#   
#  ## render labels
#   output$plotPCAlabels <- renderPlot(
#     print(plot_grid(PCA_labels$labels[["colour"]], 
#                     ncol = 1)), 
#     res = 120
#   )
#   
#   # Define an observer for the button click
#   observeEvent(input$closeZoom, {
#     # Use shinyjs to hide the absolutePanel
#     shinyjs::hide("zoomPanel")
#   })
#   
#   observeEvent(input$plotPCA_brush,{
#                # shinyjs::show("zoomPanel")
#     # ## render UI zoom
#     output$zoomPCAbrush <- renderUI({
#       tagList(
#         absolutePanel(
#           id = "zoomPanel",
#           bottom = 20, left = 400, width = 350,
#           draggable = TRUE,
#           wellPanel(
#            strong("Zoom PCA" ),
#            plotOutput("plotPCAbrush", width = 300, height = 300, dblclick = "plotPCA_dblclick")
#           ),
#           style = "opacity: 1",
#           actionButton("closeZoom", "Close", class = "btn-success")
#         )
#       )
#     })
#   }
#  )
#   
#   ## render Zoom plot
#   output$plotPCAbrush <- renderPlot({
#     if(length(input$plotPCA_brush)){
#      SelectedPointsBrush <- brushedPoints(dataContext(), input$plotPCA_brush)
#      pcaBrush <-  plotPCABrush(SelectedPointsBrush,
#               dataSetsPCA,
#               selected = selected_ind,
#               colourBy = input$colourBy,
#               zoom = T)
#      pcaBrush[["plot"]]
#     }
#   })
#   
#   
#   ## Panel Ancestry
#   # render ternary - click points
#   output$plotAncestry <- renderPlot(print(ternaryPlot(dataAncestry(), selected = selected_ind$GeneticID)))
#   
#   # render table
#   output$dataTable <- renderDataTable({
#     df <- dataContext()
#     if(length(selected_ind$GeneticID) > 0){
#        df <- if(selected_ind$GeneticID %in% df$GeneticID) df else df |> add_row(selected_ind$info)
#        df <- df |> arrange(fct_relevel(GeneticID, selected_ind$GeneticID))
#        df <- datatable(df) |> formatStyle('GeneticID',
#                                        target = 'row',
#                                        backgroundColor = styleEqual(selected_ind$GeneticID, '#F9C80E'))
#     }
#      df
#     }
#   )
# }

# Run the application 
# shinyApp(ui = ui, server = server)
