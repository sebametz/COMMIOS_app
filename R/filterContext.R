
filterContextInputs <- function(id) {
  tagList(
    selectInput(NS(id, "periodFilter"), label = "Filter by Period", choices = unique(context$Period), multiple = T),
    selectInput(NS(id, "countryFilter"), label = "Filter by Country", choices = unique(context$Country), multiple = T),
    selectInput(NS(id, "localityFilter"), label = "Filter by Locality", choices = unique(context$Locality), multiple = T),
    sliderInput(NS(id, "yearsFilter"), "Filter by years (MeanDateBP - 1950): ", min = min(context$DateMeanInBP-1950), max = max(context$DateMeanInBP-1950),
                value = range(context$DateMeanInBP-1950))
  )
}

filterContextServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    # Return selection if there is data, otherwise return context
    current_df <- reactive(filterCurrent(context, input$periodFilter, input$countryFilter, input$localityFilter, input$yearsFilter))

    
    # Observers periodFilter
    observeEvent(input$periodFilter,{
      param <- list(periodFilter = input$periodFilter, countryFilter = input$countryFilter, 
                     localityFilter = input$localityFilter, yearsFilter = input$yearsFilter)
      
      c(choices, selection) %<-% getChoices(current_df(), "Country", param, "countryFilter")
      updateSelectInput(session, "countryFilter", 
                        choices = choices,
                        selected = selection)
      
      c(choices, selection) %<-% getChoices(current_df(), "Locality", param, "localityFilter")
      updateSelectInput(session, "localityFilter",
                        choices = choices,
                        selected = selection)
    })
    
    # Observers countryFilter
    observeEvent(input$countryFilter,{
      
      param <- list(periodFilter = input$periodFilter, countryFilter = input$countryFilter, 
                     localityFilter = input$localityFilter, yearsFilter = input$yearsFilter)
      
      
      c(choices, selection) %<-% getChoices(current_df(), "Locality", param, "localityFilter")
      updateSelectInput(session, "localityFilter",
                        choices = choices,
                        selected = selection)
    })
    
    # Observers localityFilter
    observeEvent(input$localityFilter,{
      
      param <- list(periodFilter = input$periodFilter, countryFilter = input$countryFilter, 
                     localityFilter = input$localityFilter, yearsFilter = input$yearsFilter)
      
      
      c(choices, selection) %<-% getChoices(current_df(), "Country", param, "countryFilter")
      updateSelectInput(session, "countryFilter", 
                        choices = choices,
                        selected = selection)
    })
    
    # Observers yearsFilter
    observeEvent(input$yearsFilter,{
      
      param <- list(periodFilter = input$periodFilter, countryFilter = input$countryFilter, 
                    localityFilter = input$localityFilter, yearsFilter = input$yearsFilter)
      
      
      c(choices, selection) %<-% getChoices(current_df(), "Country", param, "countryFilter")
      updateSelectInput(session, "countryFilter", 
                        choices = choices,
                        selected = selection)
      
      c(choices, selection) %<-% getChoices(current_df(), "Locality", param, "localityFilter")
      updateSelectInput(session, "localityFilter",
                        choices = choices,
                        selected = selection)
    })
    #Return current df
    current_df
    }
  )
}

filterContextApp <- function(){
  source("R/utils.R")
  ui <- fluidPage(
    sidebarLayout(
      sidebarPanel(filterContextInputs("filterContext")),
      mainPanel(dataTableOutput("tableContext"))
    )
  )
  server <- function(input, output, session){
    current_df <- filterContextServer("filterContext")
    output$tableContext <- renderDataTable(current_df())
    
  }
  shinyApp(ui, server)
}
