insee_to_zoo <- function(file, freq = 4){
  library(zoo)
  raw <- read.csv2(file = file, header = TRUE, strip.white = TRUE, sep = ';', skip = 3, colClasses = 'numeric')
  names <- names(read.csv2(file = file, header = TRUE, strip.white = TRUE, sep = ';', fileEncoding="latin1"))
  ts <- zoo(x = raw[,3:ncol(raw)], order.by = raw[,1], freq = freq)
  names(ts) <- names[3:length(names)]
  return(ts)
}

