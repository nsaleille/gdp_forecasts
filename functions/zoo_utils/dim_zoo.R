dim.zoos <- function(x){
  
  if (is.univariate.zoos(x)){
    return(c(1, length(x)))
  }
  else {
    return(dim(x))
  }
}