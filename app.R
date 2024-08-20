library(shiny)
library(tidyverse)
library(shinyjs)
library(leaflet)
library(htmlwidgets)
library(shinyWidgets)
library(markdown)
library(plotly)
library(svglite)
source("R/functions.R")
data("data_ref")


## ADD:
# - Sex parameter
# - Colours PCA and References size

# define parameters ----

periods_param_choices <- c("Neolithic", "C/EBA", "BA", "IA", "Romans", "Early Medieval/Vikings") #"Mesolithic", 
names(periods_param_choices) <- c("Neolithic", "C/EBA", "BA", "IA", "Romans", "Early Medieval/Vikings") #"Mesolithic",

timeseries_param_choices <- c("EEF", "WHG", "Steppe")
names(timeseries_param_choices) <- c("Early European Farmers (EEF)", "Western hunter-gatherer (WHG)", "Yamnaya pastoralists (Steppe)")

pca_param_choices  <- data_ref |>
  distinct(Period, Country, Group)

#addapt ternary plot
ternary_param_choices <- data_ref |>
  filter(DataRef != "AADR_Modern") |>
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
    columns <- c(colnames(data_ref)[1:12], colnames(data_ref)[14:33], colnames(data_ref)[59], colnames(data_ref)[60], colnames(data_ref)[34], colnames(data_ref)[38:44])
    # print(columns)
    table <- data_ref |>
      filter(DataRef == "AADR_UK") |>
      select(all_of(columns))
    # Need to round values!!
    DT::datatable(table, selection = "single", rownames = FALSE, filter = "top",
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

