library(googlesheets4)
library(dplyr)
library(readr)

# Data Sheet is "Dataset"
df <- read_sheet("https://docs.google.com/spreadsheets/d/1VuKkBv5StlB-E0WTyD65q9gBU0LO3srSkuj3qaeXEcg/edit#gid=0", 
                 sheet = "Sequencing")

if(file.exists("data-raw/sequencing.tsv")){
  current <- read_tsv("data-raw/sequencing.tsv")
  df <- df |> add_case(current)
}

write_tsv(df, "data-raw/sequencing.tsv")
save(df, file = "data/sequencing.rda")