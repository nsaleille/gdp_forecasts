zoo_month_to_trim <- function(serie){
  
  # converts a matrix with monthly observations
  # into a matrix with quarterly observations
  # each predictor corresonds to a variable and a month

  m <- yearqtr_to_month((index(serie)))
  vals <- lapply(unique(m), function(x) serie[which(m == x), ])
  vals <- lapply(vals, function(x) {index(x) <- as.yearqtr(index(x)); return(x)})
  vals <- Reduce(merge, vals)
  vars <- paste('V', 1:ncol(serie), sep = '')
  months <- paste('M', 1:3, sep = '')
  colnames(vals) <- paste(sort(rep(months, ncol(serie))), vars)
  
  return(vals)
  
}