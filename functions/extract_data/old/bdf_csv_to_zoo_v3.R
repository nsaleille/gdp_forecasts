setwd("/Users/nsaleille/Dropbox/ofpr/scripts") # chemin perso
files <- list.files("./data/bdf")
paths <- paste('./data/bdf/', files, sep='')
file <- paths[2]

bdf_xls_to_zoo <- function(file){
  
  require(zoo)
  
  raw <- read.csv(file, header = TRUE, skip = 0, sep = ';')
  data <- raw[7:nrow(raw), 2:ncol(raw)]
  names <- names(data)
  index <- strptime(raw[,1], format = "%b %Y")
  
  id <- str_split(names, pattern = '.')
  
  ts <- zoo(x = data, order.by = index)
  
  return(ts = ts, names = name, freq = freq, id = id)
}