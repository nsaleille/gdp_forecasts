# France raw loading v1
# source: INSEE
# master thesis Nicolas Saleille

rm(list=ls(all=TRUE))
setwd("/Users/nsaleille/Dropbox/ofpr/scripts") # chemin perso
source('./fonctions/insee_xls_to_zoo_v3.R')
source('./fonctions/extractInseeInfo.R')
source('./fonctions/search_print.R')
source('./fonctions/extractInseeData.R')
source('./fonctions/mgrepl.R')
source('./fonctions/get_search_results.R')

files.conjoncture <- list.files("./data/insee/enquetes_conjoncture")
paths.conjoncture <- paste('./data/insee/enquetes_conjoncture/', files.conjoncture, sep='')
files.autres <- list.files("./data/insee/autres")
paths.autres <- paste('./data/insee/autres/', files.autres, sep='')

#raw insee info
raw.conjoncture <- extractInseeInfo(paths.conjoncture)
raw.autres <- extractInseeInfo(paths.autres)

desc.full <- c(raw.conjoncture$desc, raw.autres$desc)
info.full <- rbind(raw.conjoncture$info, raw.autres$info)

search1 <- search_print(pattern = 'chômage', desc = desc.full, 
                        info = info.full, out.path = '../tex/data_selection/')

search2 <- search_print(pattern = 'industrie', desc = desc.full, 
                        info = info.full, out.path = '../tex/data_selection/')

search3 <- search_print(pattern = c('industrie','prix'), desc = desc.full, 
                        info = info.full, out.path = '../tex/data_selection/')

search4 <- search_print(pattern = c('ménages', 'situation'), desc = desc.full, 
                        info = info.full, out.path = '../tex/data_selection/')

search5 <- search_print(pattern = c('ménages', 'situation'), desc = desc.full, info = info.full)

test <- get_search_results(search5, zoo.out = TRUE)
for (i in test){plot(i)}