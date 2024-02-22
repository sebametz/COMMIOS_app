# Author: S. Metz
# Mail: sebastian.metz [at] york.ac.uk

library(plotly)
# Ternary plot for composition with current context and selected

lable <- function(txt) {
  list(
    text = txt, 
    x = 0.1, y = 1,
    ax = 0, ay = 0,
    xref = "paper", yref = "paper", 
    align = "center",
    font = list(family = "serif", size = 15, color = "white"),
    bgcolor = "#b3b3b3", bordercolor = "black", borderwidth = 2
  )
}


# reusable function for axis formatting
axis <- function(txt) {
  list(
    title = txt, tickformat = ".0%", tickfont = list(size = 10)
  )
}

ternaryAxes <- list(
  aaxis = axis("Steppe"), 
  baxis = axis("WHG"), 
  caxis = axis("EEF")
)

ternaryPlot <- function(df = ancestry, selected = character(0)){
  plot_ly(
    df,
    a = ~OldSteppe_weight*100,
    b = ~WHGA_weight*100,
    c = ~Balkan_N_weight*100,
    color = ~GroupID,
    mode = "markers",
    type = "scatterternary"
  ) %>%
    layout(
      annotations = lable("Ancestry compositions"),
      ternary = ternaryAxes
    )
} 

# eef vs steppe current context and selected

# eef, whg and steppe plot for selected
