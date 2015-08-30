forecasts_to_zoo <- function(forecasts, unlist.data = FALSE){
  data <- sapply(forecasts, function(x) x['zoo',]$zoo)
  if (unlist.data){
    data <- unlist(data)
  }
  data.zoo <- zoo(data, as.yearqtr(sapply(forecasts, function(x) x[4,]$forecast.reference.period)))
  return(data.zoo)
}