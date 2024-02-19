# Context dataset from google drive <- update current data if it exists
library(googlesheets4)
library(dplyr)
library(readr)
library(tibble)

# Data Sheet is "Dataset"
context <- read_sheet("https://docs.google.com/spreadsheets/d/1VuKkBv5StlB-E0WTyD65q9gBU0LO3srSkuj3qaeXEcg/edit#gid=0", 
                 sheet = "Context")

if(file.exists("data-raw/context.tsv")){
  current <- read_tsv("data-raw/context.tsv")
  current <- current |>
    filter(!GeneticID %in% context$GeneticID)
  
  context <- context |> add_case(current)
}

write_tsv(context, "data-raw/context.tsv")
save(context, file = "data/context.rda")
