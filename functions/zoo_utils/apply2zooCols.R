apply2zooCols <- function(zooframe, FUN = function(x) x - mean(x)){
  
  zoolist <- lapply(zooframe, FUN = function(x) x)
  new <- lapply(zoolist, FUN)
  new <- Reduce(function(x,y) merge(x,y), new)
  zoonew <- zoo(new, order.by = index(zooframe))
  colnames(zoonew) <- colnames(zooframe)
  
  return(zoonew)
  
}