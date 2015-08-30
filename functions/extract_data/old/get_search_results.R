get_search_results <- function(search, zoo.out = TRUE){
  
  paths <- unique(search$info[,'path'])
  paths <- sapply(paths, as.character)
  data <- extractInseeData(paths)
  series.id <- sapply(data, FUN = function(x) x$id)
  #series.id <- unlist(series.id)[(!(mgrepl('zoo|name|freq', unlist(series.id))))] # clean ids
  index <- mgrepl(paste(rownames(search$info), collapse = '|'), series.id)
  
  if (zoo.out == TRUE){
    return(lapply(data[index], FUN = function(x) x$zoo))
  } else {
    return(data[index])
  }
  
}