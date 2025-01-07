library(shiny)
library(tidyverse)
library(shinyjs)
library(leaflet)
library(htmlwidgets)
library(shinyWidgets)
library(markdown)
library(plotly)
library(svglite)
library(prompter)
library(gridExtra)

source("R/functions.R")

data("data_ref")
data("locality_params")
data("locality_tree")

data("locality_params2")
data("locality_tree2")

data("custom_palette")

options(warn=-1)

# define parameters ----

periods_param_choices <- c("Neolithic", "C/EBA", "BA", "IA", "Romans", "Early Medieval/Vikings") #"Mesolithic", 
names(periods_param_choices) <- c("Neolithic", "C/EBA", "BA", "IA", "Romans", "Early Medieval/Vikings") #"Mesolithic",

timeseries_param_choices <- c("EEF", "WHG", "Steppe")
names(timeseries_param_choices) <- c("Early European Farmers (EEF)", "Western hunter-gatherer (WHG)", "Yamnaya pastoralists (Steppe)")

molecular_sex_choices <- c("F", "M", "U")
names(molecular_sex_choices) <- c("Female", "Male", "Undetermined")

pca_param_choices  <- data_ref |>
  mutate(Period = factor(Period, levels = c("Mesolithic", "Neolithic", "C/EBA", "BA", "IA", "Romans", "Early Medieval/Vikings", "Modern"))) |>
  arrange(desc(`Date Mean in BP`)) |>
  distinct(Period, Country, Group) |>
  mutate(order = str_c(str_c(Period, Country, sep = ";"), Group, sep = ";")) |>
  arrange(order) |>
  arrange(Period) |>
  select(Period, Country, Group) 

country_param_choices <- unique(filter(data_ref, DataRef == "AADR_UK")$Country)


#addapt ternary plot
ternary_param_choices <- data_ref |>
  filter(DataRef != "AADR_Modern") |>
  filter(`qpAdm Pvalue` > 0.01) |>
  mutate(Period = factor(Period, levels = c("Mesolithic", "Neolithic", "C/EBA", "BA", "IA", "Romans", "Early Medieval/Vikings", "Modern"))) |>
  arrange(desc(`Date Mean in BP`)) |>
  distinct(Period, Country, Group) |>
  mutate(order = str_c(str_c(Period, Country, sep = ";"), Group, sep = ";")) |>
  arrange(order) |>
  arrange(Period) |>
  select(Period, Country, Group)


names(custom_palette) <- unique(pca_param_choices$Group)

# App UI ----
ui <- fluidPage(
  useShinyjs(),
  use_prompt(),
  title = "COMMIOS app",
  theme = bslib::bs_theme(preset = "slate"),
  
  # Header panel
  headerPanel(
    title = tags$a(href='https://commiosarchaeology.wordpress.com/',tags$img(src='commios-lofo-for-website-1-1.png', width = 100*2.85*1.75), target="_blank"),
    # add here the tags for the head 
    tags$head(tags$link(rel = "icon", type = "image/png", href = "commios-logo2.png"), windowTitle="COMMIOS dashboard",
              tags$link(rel = "stylesheet", type = "text/css", href = "custom_css.css"),
              tags$style(".shiny-text-output {
                  border: none;
                  background-color: var(--bs-body-bg)
                  }")
              )
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
                                                          choiceNames = tagList(
                                                            # set padding in the element inside button instead of button itself
                                                            add_prompt(tags$div(periods_param_choices[1], style = "padding: 6px 12px"), message = names(periods_param_choices[1])),
                                                            add_prompt(tags$div(periods_param_choices[2], style = "padding: 6px 12px"), message = "Chalcolithic / Early Bronze Age"),
                                                            add_prompt(tags$div(periods_param_choices[3], style = "padding: 6px 12px"), message = "Bronze Age"),
                                                            add_prompt(tags$div(periods_param_choices[4], style = "padding: 6px 12px"), message = "Iron Age"),
                                                            add_prompt(tags$div(periods_param_choices[5], style = "padding: 6px 12px"), message = names(periods_param_choices[5])),
                                                            add_prompt(tags$div(periods_param_choices[6], style = "padding: 6px 12px"), message = names(periods_param_choices[6]))
                                                          ),
                                                          choiceValues = names(periods_param_choices),
                                                          # choices = periods_param_choices,
                                                          selected = names(periods_param_choices),
                                                          size = "sm",
                                                          status = 'info',
                                                          justified = T
                                                        ),
                                                        tags$div(),
                                                        tags$ul(tags$li(align = "left", "Click on an individual for more information")),
                                                        shinycssloaders::withSpinner(
                                                          leaflet::leafletOutput("map", height="800px"), size=2, color="#0080b7"
                                                        )
                                                 )
                                        ),
                                        tabPanel("Table",
                                                 column(12, div(DT::dataTableOutput("table_input"), style = "font-size:60%"))
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
                                                                                          "Ancient populations contribution"), inline=T),
                                  ),
                                  column(3,
                                         conditionalPanel(condition = "input.plot_type =='Time series'",
                                                          downloadBttn("downladSeries", "Save Plot", color = "royal", size = "sm")
                                         ),
                                         conditionalPanel(condition = "input.plot_type =='Principal Components Analysis (PCA)'",
                                                          downloadBttn("downladPCA", "Save Plot", color = "royal", size = "sm")
                                         ),
                                         conditionalPanel(condition = "input.plot_type =='Ancient populations contribution'",
                                                          downloadBttn("downladTernary", "Save Plot", color = "royal", size = "sm")
                                         )
                                  )
                                ),
                                conditionalPanel(condition = "input.plot_type =='Time series'",
                                                 fluidRow(
                                                   column(4,
                                                          pickerInput(
                                                            inputId = "timeseries_param",
                                                            label = "Select an Ancient Population:",
                                                            choices = timeseries_param_choices,
                                                            options = pickerOptions(
                                                              actionsBox = FALSE,
                                                              size = 10
                                                            ),
                                                            multiple = FALSE,
                                                            selected = "EEF"
                                                          )
                                                   ),
                                                   column(4,
                                                          pickerInput(
                                                            inputId = "timeseries_param_country",
                                                            label = "Select country:",
                                                            choices = country_param_choices,
                                                            options = pickerOptions(
                                                              actionsBox = TRUE,
                                                              size = 10,
                                                              selectedTextFormat = "count > 3"
                                                            ),
                                                            multiple = TRUE, 
                                                            selected = country_param_choices
                                                          )),
                                                   column(4,
                                                          pickerInput(
                                                            inputId = "timeseries_param_sex",
                                                            label = "Select molecular sex:",
                                                            choices = molecular_sex_choices,
                                                            options = pickerOptions(
                                                              actionsBox = TRUE,
                                                              size = 10,
                                                              selectedTextFormat = "count > 1"
                                                            ),
                                                            multiple = TRUE,
                                                            selected = molecular_sex_choices
                                                          ))
                                                   
                                                 )
                                ),
                                conditionalPanel(condition = "input.plot_type =='Principal Components Analysis (PCA)'",
                                                 fluidRow(
                                                   column(3,
                                                          treeInput("pca_param", label = "Select references: ",
                                                                    choices = create_tree(pca_param_choices),
                                                                    returnValue = "text",
                                                                    closeDepth = 0,
                                                                    selected = c("Scottish","English", "British", "Spanish", "French")
                                                          ),
                                                          treeInput("pca_LocParam", label = "Select Locality: ",
                                                                    choices = locality_tree,
                                                                    returnValue = "text",
                                                                    closeDepth = 0
                                                          ),
                                                          tags$div(align = "center",
                                                                   actionButton("clear_pca", label = "Clear",
                                                                                class = "btn-warning")
                                                          )
                                                          
                                                   ),
                                                   column(9,
                                                          align = "center",
                                                          column(12, 
                                                                 align = "left",
                                                                 fluidRow(column(9,
                                                                        tags$ul(tags$li(align = "left", "Select a region for Zoom In"))
                                                                        ),
                                                                 column(3,
                                                                        materialSwitch(inputId = "show_legends", label = "Show Legends", status = "danger", value = FALSE)
                                                                        )
                                                                 )
                                                                 
                                                          ),
                                                          tags$div(
                                                            style = "position:relative;",
                                                            plotOutput("pca_plot",
                                                                       click = "pca_click", 
                                                                       brush = brushOpts(id = "pca_brush", clip = TRUE, resetOnNew = TRUE),  
                                                                       height = "650px",
                                                                       width = "75%"),
                                                            conditionalPanel(condition = "input.show_legends",
                                                                             plotOutput("pca_legends")
                                                                             ),
                                                            uiOutput("pca_zoom", inline = TRUE)
                                                            ),
                                                          
                                                          column(9, align = "left",
                                                                 tags$p(),
                                                                 tags$ul(tags$li(align = "left", "Click on an individual for more information ")),
                                                                 tags$pre(class = "shiny-text-output noplaceholder",
                                                                          uiOutput("pca_click_text", inline = TRUE)
                                                                 )
                                                          )
                                                   )
                                                 )
                                ),
                                conditionalPanel(condition = "input.plot_type =='Ancient populations contribution'",
                                                 fluidRow(
                                                   column(3,
                                                          treeInput("ternary_param", label = "Select references: ",
                                                                    choices = create_tree(ternary_param_choices),
                                                                    returnValue = "text",
                                                                    closeDepth = 0
                                                          ),
                                                          
                                                          uiOutput("ternary_locality", inline = TRUE),
                                                          
                                                          tags$div(align = "center",
                                                                   actionButton("clear_ternary", label = "Clear",
                                                                                class = "btn-warning")
                                                          )
                                                   ),
                                                   column(9,
                                                          plotlyOutput("ternary_plot", 
                                                                       height = "650px",
                                                                       width = "75%")
                                                   )
                                                 )
                                ),
                                # define plots
                                conditionalPanel(condition = "input.plot_type =='Time series'", 
                                                 column(12,
                                                        align = "center",
                                                        plotOutput("timersies_plot", click = "transect_click", height = "40vh", width = "99%"),
                                                        tags$div(),
                                                        tags$ul(tags$li(align = "left", "Click on an individual for more information")),
                                                        fluidRow(
                                                          column(6,
                                                                 tags$pre(align = "left", class = "shiny-text-output noplaceholder",
                                                                          uiOutput("transect_clicked", inline = TRUE)
                                                                 )),
                                                          column(6,
                                                                 uiOutput("transect_ancesty", inline = TRUE)
                                                          )
                                                        )
                                                 )
                                )
                       ),
                       tabPanel("User guide",
                                fluidRow(
                                  column(8, includeMarkdown('./other/help.md')
                                  )
                                )
                       )
           )
    )
  ),
  # Need some works
  # tags$footer(
  #   fluidRow(
  #     column(8, align = "left",
  #            tags$p("Developed by Sebastian Metz (sebastian.metz[at]york.ac.uk)")),
  #     column(4, align = "right",
  #            tags$p( a(href = "https://commiosarchaeology.wordpress.com/", "More information"))
  #            )
  #     )
  #   )
  
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
  reactive_objects$parameters <- list(periods = unique(df$Period), sex = unique(df$`Molecular Sex`), countries = unique(df$Country))
  
  # Show help when app start
  aboutModalServer("aboutModal")
  
  # Select map set up
  map = leaflet::createLeafletMap(session, 'map')
  
  # On the FLUSH [MAP and Transect] ----
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
                 selected = reactive_objects$selection,
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
  
  
  # # Table interface ----
  output$table_input=DT::renderDataTable({
    columns <- c(colnames(data_ref)[1:12], colnames(data_ref)[14:33], colnames(data_ref)[59], colnames(data_ref)[60], colnames(data_ref)[34], colnames(data_ref)[38:44])
    # print(columns)
    table <- reactive_objects$context |>
      select(all_of(columns))
    # Need to round values!!
    DT::datatable(table, selection = "single", rownames = FALSE, filter = "top",
                  options = list(scrollY = '600px', paging = FALSE, scrollX = TRUE, dom = "ltipr")
    )
  })
  
  observe({
    reactive_objects$parameters$periods <- input$map_group
    aux <- update_context(reactive_objects$context, df, reactive_objects$parameters)
    reactive_objects$context <- if(aux$success) aux$context else reactive_objects$context
  })
  # 
  
  observe({
    req(input$timeseries_param_country)
    reactive_objects$parameters$countries <- input$timeseries_param_country
    aux <- update_context(reactive_objects$context, df, reactive_objects$parameters)
    reactive_objects$context <- if(aux$success) aux$context else reactive_objects$context
  })
  
  observe({
    req(input$timeseries_param_sex)
    reactive_objects$parameters$sex <- input$timeseries_param_sex
    aux <- update_context(reactive_objects$context, df, reactive_objects$parameters)
    reactive_objects$context <- if(aux$success) aux$context else reactive_objects$context
  })
  
  
  
  # selected from the table
  observe({
    req(input$table_input_rows_selected)
    reactive_objects$selection <- df[input$table_input_rows_selected,]
  })
  
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
  map_proxy=leaflet::leafletProxy("map")
  observeEvent(input$table_input_rows_selected, {
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
      paste0("<br>Information: ",
             "<br> Genetic ID: ", point$`Genetic ID`,
             "<br> Molecular Sex: ", point$`Molecular Sex`,
             "<br> Locality: ", point$`Correct Locality Name`,
             "<br> Period: ", point$Period,
             "<br> % EEF: ", round(point$EEF, digits = 2), " ± ", round(point$`EEF SE`, digits = 2),
             "<br> % Steppe: ", round(point$Steppe, digits = 2), " ± ", round(point$`Steppe SE`, digits = 2),
             "<br> Status (apAdm P-value): ", if_else(point$`qpAdm Pvalue` > 0.01,
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
  
  # Ancestry plot of selected indiviudal ----
  output$transect_ancesty <- renderUI({
    req(reactive_objects$selection)
    point <- reactive_objects$selection
    
    output$plot_ancestry <- renderPlot({
      get_composition(point)
    })
    plotOutput("plot_ancestry", width = "80%", height = "20vh")
  })
  
  # Selected references parameters
  observe({
    reactive_objects$pca_references <- input$pca_param
  })
  
  # Selected locality parameters
  observe({
    reactive_objects$pca_locality <- input$pca_LocParam
  })
  
  # plot PCA ----
  output$pca_plot <- renderPlot(.pcaPlot())
  
  # PCA plot
  .pcaPlot <- reactive({
    pca <- get_pca(data_ref_mod, reactive_objects$context, 
            references = reactive_objects$pca_references, 
            selected = reactive_objects$selection,
            locality = reactive_objects$pca_locality, colours = custom_palette)
    reactive_objects$pca <- pca
    pca +
      theme(legend.position = "none",
            text = element_text(colour = "black", size = 16),
            axis.text = element_text(colour = "black", size = 14))
  })
  
  plotInputPCA = function(){.pcaPlot()}
  
  
  #
  output$pca_legends <- renderPlot({
    req(reactive_objects$pca)
    # print("active")
    legend <- lemon::g_legend(reactive_objects$pca +
                         theme_bw() +
                         theme(legend.position = "bottom",
                               legend.title = element_text(size = 18, colour = "black"),
                               legend.text = element_text(size = 14, colour = "black", face = 'bold')))
    grid::grid.draw(legend)
  })
  
  
  # download PCA plot
  output$downladPCA <- downloadHandler(
    filename = function() {str_c("PCA", format(Sys.time(),'%Y-%m-%d_%H:%M:%S'), ".svg", sep = "")},
    content = function(file) {
      ggsave(file, plot = plotInputPCA(), units = "cm", device = "svg", dpi = 300, width = 25, height = 25)
    }
  )
  
  
  # PCA clicked individual info ----
  observe({
    req(input$pca_click)
    selection <- nearPoints(data_ref_mod, input$pca_click, maxpoints = 5)
    aux <- filter(selection, Period != "Present")
    selection <- if(nrow(aux) > 0) aux[1,] else selection[1,]
    reactive_objects$pca_selection <- selection
  })
  
  output$pca_click_text <- renderUI({
    req(reactive_objects$pca_selection)
    point <- reactive_objects$pca_selection 
    HTML(
      paste0("<br> Genetic ID: ", point$`Genetic ID`,
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
  
  # PCA Brush ----
  observe({
    req(input$pca_brush)
    limits <- c(input$pca_brush$xmin, input$pca_brush$xmax, input$pca_brush$ymin, input$pca_brush$ymax)
    # limits <- brushedPoints(data_ref_mod, input$pca_brush, allRows = TRUE)$selected_
    reactive_objects$limits <- limits
  })
  
  output$pca_zoom <- renderUI({
    req(reactive_objects$limits)
    absolutePanel(id = "zoom_info", class = "panel panel-default", fixed = FALSE,
                  draggable = TRUE, top = 5, left = "auto", right = "12%", bottom = "auto",
                  width = "400px",
                  height = "400px",
                  style = "background-color: white;
                           opacity: 0.85;
                           padding: 20px 20px 20px 20px;
                           margin: auto;
                           border-radius: 5pt;
                           box-shadow: 0pt 0pt 6pt 0px rgba(61,59,61,0.48);
                           padding-bottom: 2mm;
                           padding-top: 1mm;",
                  
                  fluidRow(
                    column(8,
                           align = "left",
                           h4("PCA Zoom")),
                    column(4,
                           align = "right",
                           actionButton("close_zoom", label = "close", icon = icon("close"),
                                        class = "btn-secondary btn-sm"))
                  ),
                  fluidRow(align = "center",
                           column(12,
                                  plotOutput("plot_zoom", click = "pca_click", width = "350px", height = "350px")
                           )
                  )
                  
    )
  })
  
  output$plot_zoom <- renderPlot(
    get_pca(data_ref_mod, reactive_objects$context, 
            references = reactive_objects$pca_references, 
            selected = reactive_objects$selection, 
            locality = reactive_objects$pca_locality,
            limits = reactive_objects$limits,
            colours = custom_palette) + 
      theme(legend.position = "none",
            text = element_text(colour = "black", size = 16, face = "bold"),
            axis.text = element_text(colour = "black", size = 14))
  )
  
  # Close zoom
  observeEvent(input$close_zoom, {
    toggle(id = "zoom_info")
  })
  
  observeEvent(input$clear_pca, {
    reactive_objects$pca_locality <- NULL
    reactive_objects$pca_references <- c("Scottish","English", "British", "Spanish", "French")
    updateTreeInput("pca_param", selected = c("Scottish","English", "British", "Spanish", "French"))
    updateTreeInput("pca_LocParam", selected = character(0))
  })
  
  # Ternary plot ----
  
  # Render Locality Selection
  output$ternary_locality <- renderUI({
    selected <- if(is.null(reactive_objects$pca_locality)) NULL else filter(locality_params2, `Genetic ID` %in% reactive_objects$pca_locality)$`Genetic ID`
    treeInput("ternary_LocParam", label = "Select Locality: ",
              choices = locality_tree2,
              returnValue = "text",
              closeDepth = 0, 
              selected =  selected
    )
  })
  
  # Observe references
  observe({
    reactive_objects$ternary_ref <- input$ternary_param
  })
  
  # Observe locality
  observe({
    reactive_objects$ternary_locality <- input$ternary_LocParam
  })
  
  
  # get plot
  output$ternary_plot <- renderPlotly(.ternaryPlot())
  
  # PCA plot
  .ternaryPlot <- reactive({
    get_ternary(data_ref_mod, reactive_objects$context,
                references = reactive_objects$ternary_ref,
                selected = reactive_objects$selection,
                locality = reactive_objects$ternary_locality
    )
  })
  
  # download PCA plot
  output$downladTernary <- downloadHandler(
    filename = function() {str_c("Ternary", format(Sys.time(),'%Y-%m-%d_%H:%M:%S'), ".html", sep = "")},
    content = function(file) {
      htmlwidgets::saveWidget(as_widget(.ternaryPlot()), file)
    }
  )
  
  observeEvent(input$clear_ternary, {
    reactive_objects$ternary_locality <- NULL
    reactive_objects$ternary_ref <- c("Scottish","English", "British", "Spanish", "French")
    updateTreeInput("ternary_param", selected = c("Scottish","English", "British", "Spanish", "French"))
    updateTreeInput("ternary_LocParam", selected = character(0))
  })
  
}


# Run the application 
shinyApp(ui = ui, server = server)

