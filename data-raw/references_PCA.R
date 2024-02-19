library(googlesheets4)
library(dplyr)
library(readr)
library(tibble)

# Data Sheet is "Dataset"
reference_pca <- read_sheet("https://docs.google.com/spreadsheets/d/1VuKkBv5StlB-E0WTyD65q9gBU0LO3srSkuj3qaeXEcg/edit#gid=0", 
                 sheet = "References_PCA")

if(file.exists("data-raw/references_pca.tsv")){
  current <- read_tsv("data-raw/references_pca.tsv")
  
  current <- current |>
    filter(!GeneticID %in% reference_pca$GeneticID)
  
  reference_pca <- reference_pca |> add_case(current)
}

write_tsv(reference_pca, "data-raw/references_pca.tsv")
save(reference_pca, file = "data/references_pca.rda")
