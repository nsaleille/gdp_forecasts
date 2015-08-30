extractInseeData <- function(paths){
  
  if (length(paths)>1){
    
    list.of.series <- lapply(paths, insee_xls_to_zoo)
    series <- list()
    for (i in list.of.series){
      series <- append(series, i)
    }
    
  } else {
    
    series <- list()
    series <- append(series, insee_xls_to_zoo(paths))
    #names(series) <- names(series$ts)
  }
  
  return(series)
  
}