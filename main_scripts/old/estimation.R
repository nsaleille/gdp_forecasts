# France raw loading v1
# source: INSEE
# master thesis Nicolas Saleille

rm(list=ls(all=TRUE))
# setwd("//paradis/eleves/NSALEILLE/Bureau/sauvegarde_ofpr")
setwd("/Users/nicolassaleille/Dropbox/ofpr") # chemin perso
source(paste(getwd(), '/scripts/load_data.R', sep = ''))

plot(log(data))

stats <- rbind(sapply(data, FUN = function(x){return(length(na.omit(x)))}), 
      sapply(data, FUN = function(x){return(head(index(na.omit(x)), n = 1))}),
      sapply(data, FUN = function(x){return(tail(index(na.omit(x)), n = 1))}))
rownames(stats) <- c('obs number', 'first', 'last')
t(stats)
#summary(data)
plot(data$unemployment)

growth <- function(ts){return((ts-lag(ts))/lag(ts))}

plot(na.omit(merge(growth(data$pce), growth(data$nbr), growth(data$cpi2), growth(data$m2fr))))
nrow(na.omit(merge(data$pce, data$nbr, data$cpi2, data$m2fr)))
