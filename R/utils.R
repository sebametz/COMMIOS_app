# Filter current context data table based on period, country, locality and years inputs 
filterContext <- function(df, period, country, locality, years){
  
  df_filtered <- if(!is.null(period)) filter(df, .data[["Period"]] %in% period) else df
  
  # Check if country exist in df
  for (i in country) {
    if(!i %in% df_filtered[["Country"]]){
      country <- NULL
      break
    }
  }
  
  df_filtered <- if(!is.null(country)) filter(df_filtered, .data[["Country"]] %in% country) else df_filtered
  
  # Check if locality exist in df
  for (i in locality) {
    if(!i %in% df_filtered[["Locality"]]){
      locality <- NULL
      break
    }
  }
  
  df_filtered <- if(!is.null(locality)) filter(df_filtered, .data[["Locality"]] %in% locality) else df_filtered
  
  df_filtered <- if(!is.null(years)) filter(df_filtered, between(.data[["DateMeanInBP"]], 
                                                                 .env[["years"]][1], .env[["years"]][2])) else df_filtered
  
  df_filtered 
}

# Get the different choices and selection for filtering context
getChoices <- function(df, var, param, currentFilter){

  # get values  
  choices <- sort(unique(filterContext(df, param$periodFilter, param$countryFilter, 
                                       param$localityFilter, param$yearsFilter)[[var]]))

  selection <- intersect(choices, param[[currentFilter]])
  list(
    choices = if(!is.null(choices)) choices else print("Not choices left"),
    selection = if(length(selection)) param[[currentFilter]] else character(0)
      ) 
}

# Filter References for PCA
filterPCAforReferences <- function(df, modern, ancient) {
  df_modern <- if(!is.null(modern)) filter(df, .data[["Period"]] == "Modern", .data[["Group"]] %in% modern) else NULL
  df_ancient <- if(!is.null(ancient)) filter(df, .data[["Period"]] != "Modern", .data[["Group"]] %in% ancient) else NULL
  
  df_filtered <- if(!is.null(df_modern) & !is.null(df_ancient)) add_row(df_modern, df_ancient) else NULL
  df_filtered <- if(is.null(df_modern) & !is.null(df_ancient)) df_ancient else df_filtered
  df_filtered <- if(!is.null(df_modern) & is.null(df_ancient)) df_modern else df_filtered
  df_filtered <- if(is.null(df_modern) & is.null(df_ancient)) character(0) else df_filtered
  
  df_filtered
} 

# colourPallets
getColours <- function(length = 10){
 colours <- c(brewer.pal(n = 12, "Paired"), 
              brewer.pal(n = 12, "Set3"), 
              brewer.pal(n = 8, "Set2"), 
              brewer.pal(n = 8, "Pastel2"),
              brewer.pal(n = 9, "Set1"), 
              brewer.pal(n = 8, "Dark2"), 
              brewer.pal(n = 9, "Pastel1"))
 colours[1:length]
}

getColoursModern <- function(){
  names <- unique(filter(reference_pca, Period == "Modern")$Group)
  colours <- getColours(length = length(names))
  names(colours) <- names
  colours
}

getColoursAncient <- function(){
  names <- unique(filter(reference_pca, Period != "Modern")$Group)
  colours <- rep(getColours(66),4)[1:length(names)]
  names(colours) <- names
  colours
}

getColoursUKGroups <- function(){
    names <- unique(filter(reference_pca, GeneticID %in% context$GeneticID)$Group)
    colours <- getColours(length = length(names))
    names(colours) <- names
    colours
}

getColoursUKPeriods <- function(){
  names <- unique(filter(reference_pca, GeneticID %in% context$GeneticID)$Period)
  colours <- getColours(length = length(names))
  names(colours) <- names
  colours
}

getColoursUKLoclities <- function(){
  names <- unique(context$Locality)
  colours <- rep(getColours(66),4)[1:length(names)]
  names(colours) <- names
  colours
}

getColoursUKCountries <- function(){
  names <- unique(context$Country)
  colours <- c("#ffff99",  "#ff9900", "#99ccff")
  names(colours) <- names
  colours
}

getColoursAncestry <- function(){
  names <- unique(ancestry$GroupID)
  colours <- getColours(length = length(names))
  names(colours) <- names
  colours
}

# Information absolut panels
helpPanel1 <- "
### Welcome to COMMIOS app!
How it works? 
- __Explore__ the distribution of the individuals in the map
- Use the __filters__ to localise interested groups
- `Click` on the individual of interest and explore the results
- `Click` on data point in the transect to localise the individual in the map

If I am in the way - `Move me` or `Hide me` with the __Hide Help__ butom in the panel control.
"

helpComposition <- "
Individual Ancient Composition: Estimation of ancestry EEF, WHG and Steppe composition of the selected individual following Patterson et al. (2020)."

helpTransect <- "
Britain time transect: Estimates of EEF ancestry for all individuals in the dataset following Patterson et al. (2020)."