extractInseeInfo <- function(paths){
  
  series.info <- data.frame()
  series.desc <- c()
  ids <- c()
  
  for (i in paths){
    
    series <- insee_xls_to_zoo(i)
    
    if (length(series$name) == 1){
      
      desc <- series$name
      info <- c(head(index(series$zoo), 1), 
                tail(index(series$zoo), 1), series$freq, length(series$zoo), i)
      ids <- c(ids, series$id)
      
    } else {
      
      desc <- sapply(series, function (x) x$name) 
      info <- sapply(series, function(x) c(head(index(x$zoo), 1), 
                                           tail(index(x$zoo), 1), x$freq, length(x$zoo), i))
      
      ids <- c(ids, sapply(series, function(x) x$id))
      
    }
    
    series.info <- rbind(series.info, t(info))
    series.desc <- c(series.desc, desc)
  }
  
  colnames(series.info) <- c('first obs', 'last obs', 'freq', 'nb obs', 'file path')
  names(series.desc) <- rownames(series.info) <- ids
  return(list(info = series.info, desc = series.desc, ids = ids))
  
}