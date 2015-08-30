# France data loading v1
# source: INSEE
# master thesis Nicolas Saleille

rm(list=ls(all=TRUE))

# setwd("/Users/nicolassaleille/Dropbox/AEQD") # chemin perso
setwd("//paradis/eleves/NSALEILLE/Bureau/sauvegarde_ofpr")
source('insee_to_zoo.R')

file1 <- paste(getwd(), '/data/insee/comptes_nationaux_trimestriels_base_2010/Valeurs.csv', sep = '')
data <- insee_to_zoo(file1)
summary(data)