get_series <- function(ids, data = data.full){
  ids.full <- sapply(data.full, FUN = function(x) x$ids)
  return(data.full[[ids == ids]])
}