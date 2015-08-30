bdf_xls_to_zoo <- function(file, freq = 'mois'){
  
  require(xlsx)
  require(zoo)
  
  raw <- read.csv2(file, header = FALSE, sep =";")
  data <- sapply(raw[7:dim(raw)[1] , 2:dim(raw)[2]], fact2num)
  data <- as.data.frame(data)
  names <- as.character(unlist(raw[1, 2:length(names(raw))]))
  ids <- as.character(unlist(raw[2,2:ncol(raw)]))

  if (freq == 'mois'){
    trans <- list(c('Fév', 'Feb'), c('Mai', 'May'), c('Aoû', 'Aug'), c('Déc', 'Dec'), c('Avr', 'Apr'))
    index <- translate_vals(raw[7:nrow(raw), 1], trans)
    index <- as.yearmon(index)
  }

  if (ncol(raw)>2){
    
    ts <- lapply(data, FUN = zoo, order.by = index)
    series <- mapply(FUN = function(x,y,z,u){list(zoo = na.omit(x), name = y, freq = z, 
      id = u, filepath = file, source = 'bdf', release = 2)}, ts, names, rep(freq, length(names)), ids, 
      SIMPLIFY = FALSE, USE.NAMES = FALSE)
  }
  
  return(series)

}