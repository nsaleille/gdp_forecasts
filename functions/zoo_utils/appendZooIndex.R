appendZooIndex <- function(y, by = 'month'){
  
  dates <- as.Date(index(y))
  new.index <- seq(from = head(dates, 1), to = tail(dates, 1), by = by)
  index(y) <- as.Date(index(y))
  y <- merge(zoo(, as.Date(new.index)), y, fill = NA)
  index(y) <- as.yearmon(index(y))
  
  return(y)
  
}