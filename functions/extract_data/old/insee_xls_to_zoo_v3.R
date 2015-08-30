insee_xls_to_zoo <- function(file){

  require(xlsx)
  require(zoo)
  
  raw <- read.xlsx2(file, sheetIndex = 1, header = TRUE, startRow = 2)
  raw[raw == ''] <- NA
  
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
  
  if (prod(raw[1,1:2] == c('Année', 'Trimestre'))){
    index <- paste(raw[2:nrow(raw),1], '.', raw[2:nrow(raw),2], sep='')
    freq = 'Trimestre'
  }
  
  #if (freq %in% c('annual')){
   # index <- paste(raw[,1], '.1', sep='')
  #}
  
  if (ncol(raw)>3){
    
    # convert to numeric zoo ts
    ts <- lapply(raw[,3:ncol(raw)], FUN = zoo, order.by = index)
    ts <- lapply(ts, FUN = function(x){coredata(x) <- as.numeric(as.character(coredata(x))); return(x)})
    
    names <- names(read.xlsx2(file, sheetIndex = 1, header = TRUE))
    names <- names[3:length(names)]
    series <- mapply(FUN = function(x,y,z, u){list(zoo = na.omit(x), name = y, freq = z, id = u)}, 
                     ts, names, rep(freq, length(names)), colnames(raw)[3:ncol(raw)], 
                     SIMPLIFY = FALSE, USE.NAMES = FALSE)
    
  } else{
    
    # convert to numeric zoo ts
    ts <- zoo(raw[,3], order.by = index)
    coredata(ts) <- as.numeric(as.character(coredata(ts)))
    
    name <- names(read.xlsx2(file, sheetIndex = 1, header = TRUE))[3]
    series <- list(zoo = na.omit(ts), name = name, freq = freq, id = colnames(raw)[3])
    series <- list(series)
    #names(series)[4] <- colnames(raw)[3]
  }
  
  return(series)
  
}