get_series_info_desc <- function(series, i){

  ids <- c()

  if (length(series) == 1){
  
  series <- unlist(series, recursive = FALSE)
  desc <- series$name
  info <- data.frame(start = start(series$zoo), end = end(series$zoo), 
    freq = series$freq, nobs = length(series$zoo), path = i, stringsAsFactors = FALSE)
  ids <- series$id
  
  } else {
    
    desc <- sapply(series, function (x) x$name) 
    info <- sapply(series, function(x) data.frame(start = start(x$zoo), end = end(x$zoo), 
      freq = x$freq, nobs = length(x$zoo), path = i, stringsAsFactors = FALSE))
    info <- t(as.data.frame(info))
    ids <- sapply(series, function(x) x$id)
    
  }

  return(list(info = info, desc = desc, ids = ids))

}