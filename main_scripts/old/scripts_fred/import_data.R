library('quantmod')
library('xts')
library('zoo')

rm(list=ls(all=TRUE))

# Utils

merge_xts_using_symbols <- function(symbols){
  # names : vector of names strings for variables in the current envir
  names <- sapply(symbols, FUN = as.name, simplify = TRUE)
  data <- eval(names[[1]])
  for (i in 2:length(names)){
    data <- merge(data, eval(names[[i]]), fill = NA)
  }
  return(data)
}

merge_fred_data <- function(symbols, names){
  getSymbols(symbols, src='FRED', env = globalenv())
   data <- merge_xts_using_symbols(symbols)
  names(data) <- names
  rm(list = symbols, envir = globalenv())
  return(data)
}

# Series for Total Production of Investment Goods for Manufacturing
# missing: Luxembourg, Netherlands, Slovenia

nation_id <- c('DE', 'AT', 'BE', 'ES', 'FI', 'FR', 'IE', 'IT', 'PT', 'GR')
investment_symbols <- paste('PRMNVG01', nation_id, 'Q661N', sep = '')
investment <- merge_fred_data(investment_symbols, nation_id)
plot(as.zoo(na.omit(investment)), main = "Total Production of Investment Goods for Manufacturing")

# Series for Harmonized Unemployment Rate: All Persons
# missing: Luxembourg, Netherlands, Slovenia, Portugal

nation_id2 <- c('DEU', 'AUT', 'BEL', 'ESP', 'FIN', 'FRA', 'IRL', 'ITA', 'GRC')
symbols_unemployement <- paste(nation_id2, 'URHARMQDSMEI', sep = '')
unemployment <- merge_fred_data(symbols_unemployement, nation_id2)
plot(as.zoo(na.omit(unemployment)), main = "Harmonized Unemployment Rate: All Persons")

# Series for Consumer Price Index of All Items

symbols_cpi <- paste(nation_id2, 'CPIALLQINMEI', sep = '')
cpi <- merge_fred_data(symbols_cpi, nation_id2)
plot(as.zoo(diff(na.omit(cpi))), main = "Consumer Price Index of All Items")
