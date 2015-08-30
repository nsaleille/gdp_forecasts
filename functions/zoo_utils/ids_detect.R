ids_detect <- function(ind, pattern){
  
  require(stringr)
  
  ids <- ind
  ids <- lapply(pattern, str_detect, string = as.character(ids))
  ids <- as.logical(Reduce('+', ids))
  
  return(ids)
}

