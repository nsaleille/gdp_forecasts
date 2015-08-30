ocde_xls_to_zoo <- function(file){
  
  require(xlsx)
  require(zoo)
  
  raw <- read.xlsx(file, sheetIndex = 1, header = TRUE, startRow = 1)
  data <- as.numeric(as.character(raw[7:dim(raw)[1] , 3:dim(raw)[2]]))
  data <- na.omit(data)
  freq <- as.character(raw[3,3])
  name <- as.character(raw[2,3])
  
  if (freq == 'Monthly'){
    index <- as.character(raw[7:length(data)+6, 1])
    index <- as.yearmon(index, format = '%b-%Y')
  }
  
  ts <- zoo(x = data, order.by = index)
  names <- as.character(raw[1,1])
  id <- constructSeriesId(name)
  
  return(list(list(zoo = ts, names = names, freq = freq, id = id, filepath = file, source = 'ocde', release = 2)))
  
}