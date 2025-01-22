library(tidyverse)
library(shinyWidgets)
data("aadr_ref_v62")


# For PCA plot only significant individuals
locality_param_choices <- aadr_ref_v62 |>
  filter(str_detect(`Usage Note`, "Reference")) |>
  filter(!str_detect(`Usage Note`, "Quality Alert")) |>
  filter(!Period %in% c("Mesolithic", "-"))|>
  mutate(Period = factor(Period, levels = c("Neolithic", "C/EBA", "BA", "IA", "Romans", "Early Medieval/Vikings", "Medieval", "Modern"))) |>
  arrange(desc(`Date Mean in BP`)) |>
  select(Country, `Unique Locality Name`, `Genetic ID`, Period) |>
  # distinct(Country, `Unique Locality Name`, `Genetic ID`) |>
  arrange(`Genetic ID`) |>
  arrange(Period) |>
  arrange(`Unique Locality Name`) |>
  mutate(locality = str_remove(`Unique Locality Name`, str_c(Country, ", "))) |>
  select(Country, locality, Period, `Genetic ID`)

locality_params <- locality_param_choices


save(locality_params, file = "data/locality_params.rda")


locality_tree <- create_tree(locality_params)
save(locality_tree, file = "data/locality_tree.rda")


# Locality ternary plot
locality_param_choices2 <- aadr_ref_v62 |>
  filter(DataRef == "AADR_UK") |>
  filter(Period != "Mesolithic")|>
  filter(`qpAdm Pvalue` > 0.01) |>
  filter(str_detect(`Usage Note`, "Reference;")) |>
  mutate(Period = factor(Period, levels = c("Neolithic", "C/EBA", "BA", "IA", "Romans", "Early Medieval/Vikings", "Medieval", "Modern"))) |>
  arrange(desc(`Date Mean in BP`)) |>
  select(Country, `Unique Locality Name`, `Genetic ID`, Period) |>
  # distinct(Country, `Unique Locality Name`, `Genetic ID`) |>
  arrange(`Genetic ID`) |>
  arrange(Period) |>
  arrange(`Unique Locality Name`) |>
  mutate(locality = str_remove(`Unique Locality Name`, str_c(Country, ", "))) |>
  select(Country, locality, Period, `Genetic ID`)

locality_params2 <- locality_param_choices
save(locality_params2, file = "data/locality_params2.rda")


locality_tree2 <- create_tree(locality_params2)
save(locality_tree2, file = "data/locality_tree2.rda")




