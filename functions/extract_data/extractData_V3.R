extractData <- function(paths, fun){
    
    list.of.series <- lapply(paths, fun)
    series <- unlist(list.of.series, recursive = FALSE)
  
  return(series)
  
}

