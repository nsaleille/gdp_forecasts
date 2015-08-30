eurostat_xls_to_zoo <- function(file, freq = 'mois'){
  
  require(xlsx)
  require(zoo)
  require(stringr)
  
  skip <- 8 # hard coded skip
  raw <- read.xlsx(file, sheetIndex = 1, header = TRUE, startRow = 1)
  data <- fact2num(raw[skip:dim(raw)[1] , 2:dim(raw)[2]])
  data <- na.omit(data)
  names <- paste(names(raw)[1], raw[5,2])
  
  if (freq == 'mois'){
    index <- str_replace(raw[skip:(length(data)+skip), 1], pattern = 'M', 
                         replacement = '-')
    index <- as.yearmon(index)
  }
  
  id <- paste(sapply(strsplit(names, ' '), substr, 1, 1), sep = '', collapse = '')
  ts <- zoo(x = data, order.by = index)
  
  return(list(list(zoo = ts, names = names, freq = freq, id = id, filepath = file, source = 'eurostat', release = 2)))
}