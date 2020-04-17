# France raw loading v1
# source: INSEE
# master thesis Nicolas Saleille

rm(list=ls(all=TRUE))

library(R.utils)
setwd("/Users/nsaleille/Dropbox/ofpr/scripts") # chemin perso
sourceDirectory('./functions/extract_data', recursive = FALSE,, modifiedOnly = FALSE)

## Data Loading

sources <- c("../data/data_raw/insee/full", "../data/data_raw/eurostat", '../data/data_raw/ocde', 
	'../data/data_raw/bdf', '../data/data_raw/fred', '../data/data_raw/yahoo')
paths <- sapply(sources, FUN = function(x) {paste(x, list.files(x), sep = '/')})

#raw insee info
raw.full <- extractInfo(unlist(paths))
desc.full <- raw.full$desc
info.full <- raw.full$info

################################
##### selection des series #####
################################

series.id <- c()

# serie du PIB
search1 <- search_print(pattern = c('produit','brut'), desc = desc.full, info = info.full)
plot_search(search1)
series.id <- append(series.id, 'X001688995')

### series d'enquetes industrie

# commandes
search2 <- search_print(pattern = c('enquête', 'industrie'), desc = desc.full, info = info.full)
search3 <- search_print(pattern = c('commandes'), desc = desc.full, info = info.full)
plot_search(search3)
series.id <- append(series.id, c('X001585940', 'X001585978'))

# stocks
search4 <- search_print(pattern = c('enquête', 'industrie', 'stocks'), desc = desc.full, info = info.full)
plot_search(search4)
series.id <- append(series.id, 'X001586015')

# tendances passées et prévues de production
search6 <- search_print(pattern = c('enquête', 'industrie', 'tendance'), desc = desc.full, info = info.full)
plot_search(search6)
series.id <- append(series.id, c('X001586062', 'X001586101'))

### series d'enquetes services

# tendances passées et prévues de l'activité
search7 <- search_print(pattern = c('enquête', 'services', 'activité'), desc = desc.full, info = info.full)
plot_search(search7)
series.id <- append(series.id, c('X001582081', 'X001582085'))

# tendance prévue de la demande
search8 <- search_print(pattern = c('enquête', 'services', 'demande'), desc = desc.full, info = info.full)
plot_search(search8)
get_search_results(search8)
series.id <- append(series.id, 'X001585791')

### series d'enquetes batiment

# tendances passées et prevue, activité, prix et effectifs
search9 <- search_print(pattern = c('enquête', 'bâtiment', 'tendance'), desc = desc.full, info = info.full)
plot_search(search9)
series.id <- append(series.id, rownames(search9$info))

# carnet de commandes
search10 <- search_print(pattern = c('enquête', 'bâtiment', 'carnet'), desc = desc.full, info = info.full)
series.id <- append(series.id, 'X001586918')

## series d'enquetes commerce de détail

# vaffaires passées et futures
search11 <- search_print(pattern = c('enquête', 'commerce', 'affaires', 'CVS'), desc = desc.full, info = info.full)
series.id <- append(series.id, c('X001580008', 'X001580015'))

# commandes
search12 <- search_print(pattern = c('enquête', 'commerce', 'commandes'), desc = desc.full, info = info.full)
series.id <- append(series.id, 'X001580429')

## series d'enquetes ménages

# situation financière
search12 <- search_print(pattern = c('enquête', 'ménages', 'CVS', 'situation'), desc = desc.full, info = info.full)
series.id <- append(series.id, rownames(search12$info))

# niveau de vie
search13 <- search_print(pattern = c('enquête', 'ménages', 'CVS', 'niveau'), desc = desc.full, info = info.full)
series.id <- append(series.id, rownames(search13$info))

# achats importants
search14 <- search_print(pattern = c('enquête', 'ménages', 'CVS', 'achats', 'opinion'), desc = desc.full, info = info.full)
series.id <- append(series.id, rownames(search14$info))


# services, résultats d'exploitation (series trimestrielles)
search15 <- search_print(pattern = c('enquête', 'services', 'résultats'), desc = desc.full, info = info.full)
series.id <- append(series.id, rownames(search15$info))

## variables réelles

# ménages
search16 <- search_print(pattern = c('immatriculations', 'CVS'), desc = desc.full, info = info.full)
series.id <- append(series.id, rownames(search16$info))
search17 <- search_print(pattern = c('consommation', 'mensuelle', 'manuf|durables|logement|auto|textile'), desc = desc.full, info = info.full)
series.id <- append(series.id, rownames(search17$info))

# indice prod industrielle
search18 <- search_print(pattern = c('indice', 'industrielle', 'CVS'), desc = desc.full, info = info.full)
series.id <- append(series.id, rownames(search18$info))

# séries bdf 

# credit - flux APU inclus
search19 <- search_print(pattern = c('Crédits', 'flux'), desc = desc.full, info = info.full)
series.id <- append(series.id, rownames(search19$info)[1])

# credit - flux APU inclus
search20 <- search_print(pattern = c('Agrégats', 'monétaires'), desc = desc.full, info = info.full)
series.id <- append(series.id, rownames(search20$info))

# chômage mensuel France ocde
search21 <- search_print(pattern = c('unemployment'), desc = desc.full, info = info.full)
series.id <- append(series.id, rownames(search21$info))

# series FRED (préselectionnées)
search22 <- search_print(pattern = c('fred_no_desc'), desc = desc.full, info = info.full)
series.id <- append(series.id, rownames(search22$info))

# series yahoo
search23 <- search_print(pattern = c('yahoo'), desc = unlist(info.full[,'path']), info = info.full)
series.id <- append(series.id, rownames(search23$info))

### summary of selected series
series.id <- unique(series.id)
desc.full <- desc.full[series.id]
search.full <- list(desc = desc.full, info = info.full[series.id,])
data.full <- get_search_results(search.full, zoo.out = FALSE)

save(data.full, file = '../data/data_raw/full_set_of_series.RData')
save(search.full, file= '.../data/data_raw/full_description_of_series.RData')

