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
  
  df_filtered <- if(!is.null(years)) filter(df_filtered, between(.data[["DateMeanInBP"]] - 1950, 
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