# France raw loading v1
# source: INSEE
# master thesis Nicolas Saleille

rm(list=ls(all=TRUE))
setwd("/Users/nicolassaleille/Dropbox/ofpr") # chemin perso
# setwd("//paradis/eleves/NSALEILLE/Bureau/sauvegarde_ofpr")
source(paste(getwd(), '/scripts/fonctions/insee_to_zoo_v2.R', sep = ''))

files <- c('comptes_nationaux_trimestriels_base_2010', 'salaires_nets_annuels_moyens', 
           'indices_harmonisés_des_prix_consommation_EUROSTAT', 'nombre_chomeurs_taux_chomage_BIT', 
           'stocks_industrie_commerce_services_base_2000', 'cout_travail_industrie_construction_tertiaire_base_2008')

freqs <- c('quarterly', 'annual', 'monthly', 'quarterly', 'quarterly', 'quarterly')
paths <- sapply(files, function(x) {return(paste(getwd(), '/data/insee/', x, '/Valeurs.csv', sep=''))}, simplify = 'array')
raw  <- mapply(FUN = insee_to_zoo, file = paths, freq = freqs)
summary(raw)

#names(raw$comptes_nationaux_trimestriels_base_2010)
gdp <- (raw$comptes_nationaux_trimestriels_base_2010)[,35]
#names(raw$salaires_nets_annuels_moyens)
wages <- (raw$salaires_nets_annuels_moyens)[,2]
#names(raw$indices_harmonisés_des_prix_consommation_EUROSTAT)
pci <- (raw$indices_harmonisés_des_prix_consommation_EUROSTAT)[,3]
#names(raw$nombre_chomeurs_taux_chomage_BIT)
unemployment <- (raw$nombre_chomeurs_taux_chomage_BIT)[,13]
#names(raw$stocks_industrie_commerce_services_base_2000)
stocks <- (raw$stocks_industrie_commerce_services_base_2000)[,1]
#names(raw$cout_travail_industrie_construction_tertiaire_base_2008)
labor_costs <- (raw$cout_travail_industrie_construction_tertiaire_base_2008)[,1]

data <- merge(gdp, unemployment, wages, pci, labor_costs, stocks)
rm(list=ls()[which(ls()!='data')])