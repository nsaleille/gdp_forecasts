insee_to_zoo <- function(file, freq = 'quarterly'){
  library(zoo)
  raw <- read.csv2(file = file, header = TRUE, strip.white = TRUE, sep = ';', skip = 3, colClasses = 'numeric')
  names <- names(read.csv2(file = file, header = TRUE, strip.white = TRUE, sep = ';', fileEncoding="latin1"))
  if (freq %in% c('quarterly', 'monthly')){
    index <- paste(raw[,1], '.', raw[,2], sep='')
  }
  if (freq %in% c('annual')){
    index <- paste(raw[,1], '.1', sep='')
  }
  ts <- zoo(x = raw[,3:ncol(raw)], order.by = index)
  names(ts) <- names[3:length(names)]
  return(ts)
}