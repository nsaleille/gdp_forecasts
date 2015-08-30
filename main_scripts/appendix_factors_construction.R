################################################################################################
##### Plot contribution of variables to the construction of factors #############
################################################################################################

# en résumé
# - on sélectionne 8 facteurs
# - il faut faire attention à la trimestrialisation des facteurs (pas de moyenne)
# - le reste donne des résultats corrects


rm(list=ls(all=TRUE))
setwd("/Users/nsaleille/Dropbox/ofpr/scripts") # chemin perso

########### STEP 0 ############################################
########### load information sets  ############################
###############################################################

library(R.utils)
sourceDirectory('./fonctions/zoo_utils', modifiedOnly = FALSE)
sourceDirectory('./fonctions/extract_data', modifiedOnly = FALSE)
sourceDirectory('./fonctions/dynamic_factor_model', modifiedOnly = FALSE)

load('./dataset/full_description_of_series.RData')
load('./dataset/full_set_of_series.RData')
load('./dataset/factors.RData')
load('./dataset/predictors.RData')
load('./dataset/outcome.RData')

# travail sur la construction des facteurs
# objectif: améliorer l'ajustement
# pas de travail récursif pour le moment

sourceDirectory('./fonctions/dynamic_factor_model', modifiedOnly = FALSE)
estimates <- dynamicFactorsEstimates(predictors, q = 10, s = 12, lag.max = 5)
f.kalman <- estimates$f.kalman; f <- estimates$f; lambda <- estimates$lambda
F <- bindLags(f.kalman, s, na.omit = TRUE, bind.original = TRUE)

# le filtre de Kalman a un intérêt très limité à l'oeuil nu
plotCols(f, f.kalman)

# le modèle à facteur permet bien de reconstituer les prédicteurs
X.hat <- estimates$X.hat
plotCols(X.hat, predictors, 2)

# trimestrialisation (on prend la première valeur; pas de moyenne !)
f.trim <- aggregate(f.kalman, as.yearqtr, head, 1) 
plotCols(f.trim, f.kalman)

# sélection du nombre optimal pour le modèle à facteur
# critère: R^2 ajusté de la regression de prévision pour h = 1
# on sélectionne 8 facteurs
# ajouter des variables pourrait permettre d'accroitre encore un peu le R2
qs <- 3:14
ss <- qs + 2
regs <- mapply(ForecastRegression, qs, ss, 
               MoreArgs = list(outcome = outcome, predictors = predictors, h = 0))
adj.r2 <- sapply(regs[1,], function(x) summary(x)$adj.r.squared)
qs.optim <- qs[which(adj.r2 == max(adj.r2))]
ss.optim <- ss[which(adj.r2 == max(adj.r2))]
reg.optim <- ForecastRegression(outcome, predictors, qs.optim, ss.optim, h = 0)
summary(reg.optim$lm)
plotGrowth(y, reg.optim$y.hat)


#graphe construction des facteurs
desc <- read.csv('../tex/tables/data_description_ordered.csv', header = FALSE)
# attention les noms ne sorrespondent pas aux clés
keep <- !((search.full$info['start'] > 1991) | (search.full$info['end'] < 2014))
names(predictors[,which(!(names(predictors) %in% names(search.full$desc[which(keep)])))])
rownames(desc) <- names(search.full$desc[which(keep)])

contrib <- contrib[order(-desc[,1]), ]
q <- 8
contrib <- lambda[[1]][,1:q]
for (i in 1:ncol(contrib)){
  barplot(contrib[,i], main = paste('factor', i))
}



desc[,1]
