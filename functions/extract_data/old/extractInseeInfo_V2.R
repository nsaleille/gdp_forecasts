extractInseeInfo <- function(paths){
  
  series.info <- data.frame()
  series.desc <- c()
  ids <- c()
  
  for (i in paths){
    
    series <- insee_xls_to_zoo(i)
    
    if (length(series) == 1){
      
      series <- unlist(series, recursive = FALSE)
      desc <- series$name
      info <- data.frame(start = start(series$zoo), end = end(series$zoo), 
        freq = series$freq, nobs = length(series$zoo), path = i, stringsAsFactors = FALSE)
      ids <- c(ids, series$id)
      
    } else {
      
      desc <- sapply(series, function (x) x$name) 
      info <- sapply(series, function(x) data.frame(start = start(x$zoo), end = end(x$zoo), 
        freq = x$freq, nobs = length(x$zoo), path = i, stringsAsFactors = FALSE))
      info <- t(as.data.frame(info))
      ids <- c(ids, sapply(series, function(x) x$id))
      
    }
    
    series.info <- rbind(series.info, info)
    series.desc <- c(series.desc, desc)
    print(i)
  }
  
  #colnames(series.info) <- c('first obs', 'last obs', 'freq', 'nb obs', 'file path')
  names(series.desc) <- rownames(series.info) <- ids
  return(list(info = series.info, desc = series.desc, ids = ids))
  
}