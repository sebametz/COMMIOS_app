
filterContextInputs <- function(id) {
  tagList(
    selectInput(NS(id, "periodFilter"), label = "Period", choices = unique(context$Period), multiple = T),
    selectInput(NS(id, "countryFilter"), label = "Country", choices = unique(context$Country), multiple = T),
    selectInput(NS(id, "localityFilter"), label = "Locality", choices = unique(context$Locality), multiple = T),
    sliderInput(NS(id, "yearsFilter"), "Date mean in BP in years before 1950 CE: ", min = min(context$DateMeanInBP), max = max(context$DateMeanInBP),
                value = range(context$DateMeanInBP), ticks=TRUE)
  )
}

filterContextServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    # Return selection if there is data, otherwise return context
    current_df <- reactive(filterContext(context, input$periodFilter, input$countryFilter, input$localityFilter, input$yearsFilter))
    
    # Observers periodFilter 
    observeEvent(input$periodFilter,{
      print("updating country because of period")
      param <- list(periodFilter = input$periodFilter, countryFilter = input$countryFilter, 
                     localityFilter = input$localityFilter, yearsFilter = input$yearsFilter)
      
      c(choices, selection) %<-% getChoices(current_df(), "Country", param, "countryFilter")
      updateSelectInput(session, "countryFilter", 
                        choices = choices,
                        selected = selection)
    }, ignoreNULL = FALSE)
    
    observeEvent(input$periodFilter,{
      print("updating locality because of period")
      param <- list(periodFilter = input$periodFilter, countryFilter = input$countryFilter, 
                    localityFilter = input$localityFilter, yearsFilter = input$yearsFilter)
      
      c(choices, selection) %<-% getChoices(current_df(), "Locality", param, "localityFilter")
      updateSelectInput(session, "localityFilter",
                        choices = choices,
                        selected = selection)
    }, ignoreNULL = FALSE)
    
    # Observers countryFilter
    observeEvent(input$countryFilter,{
      print("updating locality because of country")
      param <- list(periodFilter = input$periodFilter, countryFilter = input$countryFilter, 
                     localityFilter = input$localityFilter, yearsFilter = input$yearsFilter)
      
      
      c(choices, selection) %<-% getChoices(current_df(), "Locality", param, "localityFilter")
      updateSelectInput(session, "localityFilter",
                        choices = choices,
                        selected = selection)
    }, ignoreNULL = FALSE)
    
    # Observers localityFilter
    observeEvent(input$localityFilter,{
      print("updating country because of locality")
      param <- list(periodFilter = input$periodFilter, countryFilter = input$countryFilter, 
                     localityFilter = input$localityFilter, yearsFilter = input$yearsFilter)
      
      
      c(choices, selection) %<-% getChoices(current_df(), "Country", param, "countryFilter")
      updateSelectInput(session, "countryFilter", 
                        choices = choices,
                        selected = selection)
    }, ignoreNULL = FALSE)
    
    # Observers yearsFilter
    observeEvent(input$yearsFilter,{
      print("updating country because of years")
      param <- list(periodFilter = input$periodFilter, countryFilter = input$countryFilter, 
                    localityFilter = input$localityFilter, yearsFilter = input$yearsFilter)
      
      
      c(choices, selection) %<-% getChoices(current_df(), "Country", param, "countryFilter")
      updateSelectInput(session, "countryFilter", 
                        choices = choices,
                        selected = selection)

    })

    # Observers yearsFilter
    observeEvent(input$yearsFilter,{
      print("updating locality because of years")
      param <- list(periodFilter = input$periodFilter, countryFilter = input$countryFilter, 
                    localityFilter = input$localityFilter, yearsFilter = input$yearsFilter)

      
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
  library(shiny)
  library(purrr)
  library(leaflet)
  library(dplyr)
  library(zeallot)
  source("R/utils.R")
  data("context")
  
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
