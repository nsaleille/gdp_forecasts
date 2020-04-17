################################################
##### Large Bayesian VAR  ######################
################################################

### Conjugate prior version of Koop
### Plus conjugate SSVS

rm(list=ls(all=TRUE))
library(R.utils)
setwd("/Users/nsaleille/Dropbox/ofpr/scripts") # chemin perso
load('../data/data_clean/predictors.RData')
load('../data/data_clean/outcome.RData')
load('../data/data_clean/full_description_of_series.RData')
sourceDirectory('./fonctions/zoo_utils', modifiedOnly = FALSE)
sourceDirectory('./fonctions/bayesian_vars', modifiedOnly = FALSE)

start(predictors); end(predictors); nrow(predictors); ncol(predictors)
search.full$info

tail(predictors)
merge(outcome, predictors)
library(pastecs)
util <- c('nbr.val', 'nbr.na', 'min', 'max', 'median', 'mean', 'std.dev')
t(stat.desc(predictors))[,util]
t(stat.desc(outcome))[,util]

# stack all variables in vector
Z <- aggregate(predictors, as.yearqtr, mean)
y <- aggregate(outcome, as.yearqtr, na.omit)
Y <- na.omit(merge(y, Z))

# matrix X contains the p lags of Y
# i.e. the predictors in the VAR context
p <- 1
X <- bindLags(Y, p, na.omit = TRUE, bind.original = FALSE)
bvar.model <- conjugateBVAR(Y, X)

# one step ahead predictions
y.hat.mean <- tail(bvar.model$X, 1) %*% bvar.model$A_mean_post
z <- tail(bvar.model$X, 1) %*% bvar.model$V_post %*% t(tail(bvar.model$X, 1))
y.hat.var <- (1 / (bvar.model$nu_post + 2)) * (1 + as.numeric(z)) * bvar.model$S_post

# implementation VAR with conjugate SSVS prior: relou cf Brown et Al.1998

k <- round(0.75 * dim(Y)[1], 0)
recForecasts(Y, X, h = 1, k = k, FUN = conjugateBVARforecast)


