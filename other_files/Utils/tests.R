library(scales)

# Generate a discrete palette with 417 distinct colors
set.seed(42)  # Ensure reproducibility
# Combine several base palettes
base_palette <- c(
  brewer_pal(palette = "Set1")(9),    # 9 colors
  brewer_pal(palette = "Set3")(12),   # 12 colors
  brewer_pal(palette = "Dark2")(8),   # 8 colors
  brewer_pal(palette = "Paired")(12), # 12 colors
  viridis_pal()(50),                  # 50 colors from viridis
  rainbow(200),                       # 100 colors from rainbow
  terrain.colors(50),                 # 50 colors from terrain
  heat.colors(50),                    # 50 colors from heat
  topo.colors(50),                    # 50 colors from topo
  cm.colors(50)                       # 50 colors from cm
)

custom_palette <- sample(base_palette, 417)

# Display the palette in a plot
barplot(rep(1, 417), col = custom_palette, border = NA, space = 0,
        main = "Discrete Custom Palette with 417 Colors")

save(custom_palette, file = "data/custom_palette.RData")
