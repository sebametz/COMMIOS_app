# Initial Modal used to welcoming user while the map is charging. 
# Version 0.1 Jul 2024

helpModalServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    # Activation at the initiation of the app
    showModal(modalDialog(
      title="Welcome to COMMIOS app!",
      includeMarkdown("other/help.md"),
      easyClose = TRUE,
      footer = NULL,
      size = "l"
    ))}
  )
}


helpModalApp <- function(){
  library(shiny)
  library(markdown)
  
  ui <- fluidPage(
    title = "My app",
    theme = bslib::bs_theme(preset = "slate"),
  )
  server <- function(input, output, session){
    helpModalServer("helpModal")
  }
  shinyApp(ui, server)
}

# helpModalApp()
