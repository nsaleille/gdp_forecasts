fred_csv_to_zoo <- function(file){
  
  require(zoo)
  require(stringr)
  
  raw <- read.csv(file, na.string = ".")
  raw <- na.omit(raw)
  vals <- zoo(raw[,'VALUE'], order.by = as.Date(raw[,'DATE']))
  
  if (mean(diff(index(vals))) > 31){
    freq = 'Trimestre'
  } else {
    freq = 'mois'
  }
  
  vals <- aggregate(vals, as.yearmon, mean)
  id <- tail(unlist(str_split(file, '/')), 1)
  serie <- list(zoo = vals, name = 'fred_no_desc', freq = freq, id = id, filepath = file, source = 'fred', release = 0)
    
  return(list(serie))
  
}