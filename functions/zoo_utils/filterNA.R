filterNA <- function(series, max.space = 1){
  
  # find NAs in series
  # suppress observations space by more than max.space
  # keep the rest
  
  require(zoo)
  
  nonna.index <- index(series[!is.na(series)])
  series <- window(series, start = nonna.index[1], end = nonna.index[length(nonna.index)])
  nonna.index <- index(series[!is.na(series)])
  drop <- (abs(nonna.index - c(nonna.index[2:length(nonna.index)], nonna.index[length(nonna.index)])) >= max.space)
  print(paste(sum(drop), 'observations dropped'))
  return(series[!drop])
  
}