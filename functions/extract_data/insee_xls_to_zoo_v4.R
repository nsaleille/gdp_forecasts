insee_xls_to_zoo <- function(file){

  require(xlsx)
  require(zoo)
  
  raw <- read.xlsx2(file, sheetIndex = 1, header = TRUE, startRow = 2)
  raw[raw == ''] <- NA
  
  if (prod(raw[1,1:2] == c('Année', 'Mois'))){
    index <- paste(raw[2:nrow(raw),1], raw[2:nrow(raw),2], sep='-')
    index <- as.yearmon(index)
    freq = 'mois'
  }
  
  if (prod(raw[1,1:2] == c('Année', 'Bimestre'))){
    index <- paste(raw[2:nrow(raw),1], ' B', raw[2:nrow(raw),2], sep='')
    freq = 'bimestre'
  }
  
  if (prod(raw[1,1:2] == c('Année', 'Semestre'))){
    index <- paste(raw[2:nrow(raw),1], ' S', raw[2:nrow(raw),2], sep='')
    freq = 'semestre'
  }
  
  if (prod(raw[1,1:2] == c('Année', 'Trimestre'))){
    index <- paste(raw[2:nrow(raw),1], ' Q', raw[2:nrow(raw),2], sep='')
    index <- as.yearqtr(index)
    freq = 'Trimestre'
  }
  
  #if (freq %in% c('annual')){
   # index <- paste(raw[,1], '.1', sep='')
  #}
  
  raw[,3:ncol(raw)] <- sapply(raw[,3:ncol(raw)], FUN = fact2num)
  if (ncol(raw)>3){
    
    # convert to numeric zoo ts
    ts <- lapply(raw[,3:ncol(raw)], FUN = zoo, order.by = index)
    #ts <- lapply(ts, FUN = function(x){fact2num(coredata(x))})
    
    names <- names(read.xlsx2(file, sheetIndex = 1, header = TRUE))
    names <- names[3:length(names)]
    series <- mapply(FUN = function(x,y,z,u){list(zoo = x, name = y, freq = z, id = u, filepath = file, source = 'insee', release = 0)}, 
                     ts, names, rep(freq, length(names)), colnames(raw)[3:ncol(raw)], 
                     SIMPLIFY = FALSE, USE.NAMES = FALSE)
    
  } else{
  
    # convert to numeric zoo ts
    ts <- zoo(na.omit(raw[,3]), order.by = index)
    #coredata(ts) <- fact2num(coredata(ts))
    
    name <- names(read.xlsx2(file, sheetIndex = 1, header = TRUE))[3]
    series <- list(zoo = ts, name = name, freq = freq, id = colnames(raw)[3], filepath = file, source = 'insee', release = 0)
    series <- list(series)
    #names(series)[4] <- colnames(raw)[3]
  }
  
  return(series)
  
}