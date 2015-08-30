###################################################
################	Descriptive Statistics	#######
###################################################

rm(list=ls(all=TRUE))
library(R.utils)
setwd("/Users/nsaleille/Dropbox/ofpr/scripts") # chemin perso
load('./dataset/full_set_of_series.RData')
load('./dataset/full_description_of_series.RData')
load('./dataset/predictors.RData')
sourceDirectory('./fonctions/zoo_utils', modifiedOnly = FALSE)

# raw series
dataset.ids <- names(predictors)
ids <- sapply(data.full, FUN = function(x) x$id)
dataset.names <- search.full$desc[which(dataset.ids %in% ids)] # missing: 4 series called 'unrate'
write.csv(dataset.names, file = '../tex/tables/data_names_fr.tex')

library(xtable)
save(dataset.names, file = '../tex/tables/data_names_fr.tex')

names <- get_desc(ids)

library(pastecs)
util <- c('nbr.val', 'nbr.na', 'min', 'max', 'median', 'mean', 'std.dev')
table.brut <- t(sapply(get_zoo(data.full), function(x) t(stat.desc(x))[,util]))
starts <- as.character(as.yearmon(sapply(get_zoo(data.full), start)))
ends <- as.character(as.yearmon(sapply(get_zoo(data.full), end)))
freqs <- as.character(t(sapply(data.full, function(x) x$freq)))
rownames(table.brut) <- sapply(data.full, function(x) x$id)
table.brut <- as.data.frame(round(table.brut, digits = 4))
table.brut <- cbind(freqs, starts, ends, table.brut)

load('./predictors.RData')
load('./outcome.RData')

t(stat.desc(predictors))[,util]

library(xtable)
print(xtable(table.brut), file = '../tex/tables/table_brut.tex')
print(xtable(table.trans), file = '../tex/tables/table_trans.tex')