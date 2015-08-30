################################################
##### Dynamic factor model #####################
################################################

rm(list=ls(all=TRUE))
setwd("/Users/nsaleille/Dropbox/ofpr/scripts") # chemin perso

########### STEP 0 ###################################
########### load dataset  ############################
######################################################

library(R.utils)
sourceDirectory('./fonctions/zoo_utils', modifiedOnly = FALSE)
sourceDirectory('./fonctions/extract_data', modifiedOnly = FALSE)
sourceDirectory('./fonctions/dynamic_factor_model', modifiedOnly = FALSE)

load('./predictors.RData')
load('./outcome.RData')
load('./release.RData')

########### STEP 1 ###################################
########### Model Parameters   #######################
######################################################

k <- round(0.5 * nrow(predictors), 0) # size of the initial training set
q <- 8 # number of static factors
s <- 10 # number of dynamic factors

########### STEP 2 ###################################
########### Create Information Sets  #################
######################################################

library(zoo)
Xs <- lapply(k:nrow(predictors), function(x) predictors[1:x, ])
Xs <- lapply(Xs, infoSet, release)
estimation.dates <- as.yearmon(sapply(Xs, end))
names(Xs) <- estimation.dates

## filter known values of the growth rate
# GDP in T is known only at T+2M
y <- appendZooIndex(outcome, by = 'month')
y <- window(y, start = start(predictors), end = end(predictors))
ys <- lapply(k:nrow(predictors), function(x) y[1:x, ])
names(ys) <- estimation.dates

# GDP in T is known only at the end of T+1
# if we are in M3 of T, then T-1 known
# otherwise only T-2
current.quarter <- lapply(ys, function(x) as.yearqtr(index(x)) == tail(as.yearqtr(index(x)), 1))
ys[which(lapply(current.quarter, sum) < 3)] <- lapply(ys[which(lapply(current.quarter, sum) < 3)], 
             function(y) {y[(length(y) - 1):length(y)] <- NA; return(y)})

# information set for quarterly GDP growth serie
# depending on the month we are in we know
# either the GDP in T-1 or in T-2
ys.trim <- lapply(ys, na.locf, fromLast = FALSE)
last.obs <- lapply(ys, function(x) tail(which(!is.na(x)), 1))
ys.trim <- mapply(FUN = function(x, y) {x[which(index(x)>=index(x)[unlist(y)])] <- NA; return(x)}, ys.trim, last.obs)
ys.trim <- lapply(ys.trim, aggregate, as.yearqtr, mean)

########### STEP 3 ###################################
########### Estimate Dynamic Factors  ################
######################################################

f.kalman <- lapply(Xs, dynamicFactorsEstimates, q, s)
f.trim <- lapply(f.kalman, aggregate, by = as.yearqtr, tail, 1)
names(f.trim) <- as.yearmon(estimation.dates)
save(f.kalman, file = './factors.RData')

########### STEP 4 ###################################
########### Forecast Using Factors  ##################
######################################################

forecasts <- mapply(FUN = olsForecast, ys.trim, f.trim)
y.hat <- zoo(forecasts, order.by = as.yearmon(names(forecasts)))
y.test <- lapply(ys, na.locf, fromLast = TRUE, na.rm = FALSE)[[length(ys.trim)]]
table <- na.omit(merge(y.hat, y.test))

print(RMSE(table[,1], table[,2]))
plotGrowth(ys.trim[[length(ys.trim)]], y.hat, file = '../tex/plots/dynamic_factor_forecasts.pdf')