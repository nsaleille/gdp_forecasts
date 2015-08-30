
rm(list=ls(all=TRUE))
library(R.utils)
setwd("/Users/nsaleille/Dropbox/ofpr/scripts") # chemin perso
sourceDirectory('./fonctions/extract_data', recursive = FALSE)

sources <- c('./data/fred')
paths <- sapply(sources, FUN = function(x) {paste(x, list.files(x), sep = '/')})


read.csv(tmp, na.string = ".")

require(quantmod)
library(RCurl)

symbols <- c('SP500', 'EXUSEC', 'DEXUSEU', 'MCOILBRENTEU', 'GOLDAMGBD228NLBM')

for (i in symbols){
  getSymbols(symbols[5],src="FRED")
}


getSymbols.FRED("DEXJPUS")

require(fImport)
temp <- tempfile()
fredImport('EXUSEC', file = temp)

library(utils)
