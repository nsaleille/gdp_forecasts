# France raw loading v1
# source: INSEE
# master thesis Nicolas Saleille

rm(list=ls(all=TRUE))
setwd("/Users/nsaleille/Dropbox/ofpr/scripts") # chemin perso
source('./fonctions/insee_xls_to_zoo_v3.R')
files <- list.files("./data/insee/enquetes_conjoncture")
paths <- paste('./data/insee/enquetes_conjoncture/', files, sep='')

extractInseeInfo <- function(paths){
  
  series.info <- data.frame()
  series.desc <- c()
  for (i in paths){
    series <- insee_xls_to_zoo(i)
    desc <- sapply(series, function (x) x$name) 
    info <- sapply(series, function(x) c(head(index(x$zoo), 1), 
                                         tail(index(x$zoo), 1), x$freq, length(x$zoo)))
    series.info <- rbind(series.info, t(info))
    series.desc <- c(series.desc, desc)
  }
  
  return(list(info = series.info, desc = series.desc))
  
}

raw <- extractInseeInfo(paths)
library(xtable)
print(xtable(data.frame(raw$desc)), file = '../tex/series_desc.tex', size = 'footnotesize')
print(xtable(raw$info), file = '../tex/series_info.tex')




raw <- insee_xls_to_zoo(paths[1])



raw <- lapply(paths, FUN = insee_xls_to_zoo)
names <- sapply(raw, FUN = function(x){x$names})
nseries <- sapply(raw, FUN = function(x){ncol(x$ts)})
freqs <- sapply(raw, FUN = function(x){x$freq})
test <- mapply(FUN = rep, freqs, nseries)


sapply(raw, FUN = function(x){summary(x$ts)})



raw[[1]]


files <- c('comptes_nationaux_trimestriels_base_2010', 'salaires_nets_annuels_moyens', 
           'indices_harmonisés_des_prix_consommation_EUROSTAT', 'nombre_chomeurs_taux_chomage_BIT', 
           'stocks_industrie_commerce_services_base_2000', 'cout_travail_industrie_construction_tertiaire_base_2008'
           ,'indices_salaire_prive', 'nombre_chomeurs_taux_chomage_BIT_series_arretees',
           'indices_prix_consommation_base_1980_series_arretees', 'consommation_mensuelle_menages_base_2010',
           'zone_fr_taux_interet_series_arretees', 'agregats_monetaires_zone_euro_series_arretees')

freqs <- c('quarterly', 'annual', 'monthly', 'quarterly', 'quarterly', 'quarterly', 'quarterly', 'monthly', 'monthly', 'monthly', 'monthly', 'monthly')
paths <- sapply(files, function(x) {return(paste(getwd(), '/data/insee/', x, '/Valeurs.csv', sep=''))}, simplify = 'array')
raw  <- mapply(FUN = insee_to_zoo, file = paths, freq = freqs)
summary(raw)

#names(raw$comptes_nationaux_trimestriels_base_2010)
gdp <- (raw$comptes_nationaux_trimestriels_base_2010)[,35]
#names(raw$salaires_nets_annuels_moyens)
#wages <- (raw$salaires_nets_annuels_moyens)[,2]
names(raw$indices_salaire_prive)
wages <- (raw$indices_salaire_prive)[,6]
#names(raw$indices_prix_consommation_base_1980_series_arretees)
cpi1 <- (raw$indices_prix_consommation_base_1980_series_arretees)[,5] # base 100 1980
#names(raw$indices_harmonisés_des_prix_consommation_EUROSTAT)
cpi2 <- (raw$indices_harmonisés_des_prix_consommation_EUROSTAT)[,3] # base 100 2005
#names(raw$nombre_chomeurs_taux_chomage_BIT)
#unemployment <- (raw$nombre_chomeurs_taux_chomage_BIT)[,13]
#names(raw$nombre_chomeurs_taux_chomage_BIT_series_arretees)
unemployment <- (raw$nombre_chomeurs_taux_chomage_BIT_series_arretees)[,12]
#names(raw$stocks_industrie_commerce_services_base_2000)
stocks <- (raw$stocks_industrie_commerce_services_base_2000)[,1]
#names(raw$cout_travail_industrie_construction_tertiaire_base_2008)
labor_costs <- (raw$cout_travail_industrie_construction_tertiaire_base_2008)[,1]
#names(raw$consommation_mensuelle_menages_base_2010)
pce <- (raw$consommation_mensuelle_menages_base_2010)[,1]
#names(raw$zone_fr_taux_interet_series_arretees)
nbr <- (raw$zone_fr_taux_interet_series_arretees)[,10] # national bond rate
# names(raw$agregats_monetaires_zone_euro_series_arretees)
m2fr <- (raw$agregats_monetaires_zone_euro_series_arretees)[,10]

data <- merge(gdp, unemployment, wages, cpi1, cpi2, labor_costs, stocks, pce, nbr, m2fr)
rm(list=ls()[which(ls()!='data')])