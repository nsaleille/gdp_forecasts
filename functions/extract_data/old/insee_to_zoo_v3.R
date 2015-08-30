insee_xls_to_zoo <- function(file){

  require(xlsx)
  require(zoo)
  
  raw <- read.xlsx2(file, sheetIndex = 1, header = TRUE, startRow = 2)
  
  if (prod(raw[1,1:2] == c('Année', 'Mois'))){
    index <- paste(raw[2:nrow(raw),1], '.', raw[2:nrow(raw),2], sep='')
    freq = 'mois'
  }
  
  if (prod(raw[1,1:2] == c('Année', 'Bimestre'))){
    index <- paste(raw[2:nrow(raw),1], '.', raw[2:nrow(raw),2], sep='')
    freq = 'bimestre'
  }
  
  if (prod(raw[1,1:2] == c('Année', 'Semestre'))){
    index <- paste(raw[2:nrow(raw),1], '.', raw[2:nrow(raw),2], sep='')
    freq = 'semestre'
  }
  
  #if (freq %in% c('annual')){
   # index <- paste(raw[,1], '.1', sep='')
  #}
  
  ts <- zoo(x = raw[,3:ncol(raw)], order.by = index)
  names <- names(read.xlsx2(file, sheetIndex = 1, header = TRUE))
  
  return(ts = ts, names = names[3:length(names)], freq = freq)
}