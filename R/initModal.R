# Initial Modal used to welcoming user while the map is charging. 
# Version 0.1 Jul 2024

aboutModalServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    # Activation at the initiation of the app
    showModal(modalDialog(
      title="",
      includeMarkdown("other/about.md"),
      easyClose = TRUE,
      footer = NULL,
      size = "l"
    ))}
  )
}


aboutModalApp <- function(){
  library(shiny)
  library(markdown)
  
  ui <- fluidPage(
    title = "My app",
    theme = bslib::bs_theme(preset = "slate"),
  )
  server <- function(input, output, session){
    aboutModalServer("aboutModal")
  }
  shinyApp(ui, server)
}

# aboutModalApp()
