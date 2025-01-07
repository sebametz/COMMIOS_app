library(googlesheets4)
library(dplyr)
library(readr)
library(tidyr)
library(tibble)

# Data Sheet is "Dataset"
df <- read_sheet("https://docs.google.com/spreadsheets/d/1VuKkBv5StlB-E0WTyD65q9gBU0LO3srSkuj3qaeXEcg/edit#gid=0", 
                 sheet = "Ancestry_N")

# transform ancestry datatable for better acces
ancestry <- df |>
  pivot_wider(names_from = Group, values_from = c(weight, se, z), names_glue = "{Group}_{.value}")

if(file.exists("data-raw/ancestry.tsv")){
  current <- read_tsv("data-raw/ancestry.tsv")
  current <- filter(current, !GeneticID %in% ancestry$GeneticID)
  ancestry <- ancestry |> add_case(current)
}

# write_tsv(ancestry, "data-raw/ancestry.tsv")
save(ancestry, file = "data/ancestry.rda")
