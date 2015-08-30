extractInseeData <- function(paths, fun){
  
  # if (length(paths)>1){
    
    list.of.series <- lapply(paths, fun)
    series <- unlist(list.of.series, recursive = FALSE)
    
  # } else {
    
  #   series <- list()
  #   series <- append(series, lapply(paths, FUN = fun))
  #   #names(series) <- names(series$ts)
  # }
  
  return(series)
  
}

