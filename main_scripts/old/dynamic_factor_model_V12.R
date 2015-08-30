################################################
##### Dynamic factor model #####################
################################################

# performs a recursive forecast exercise
# for each information sets
# dynamic factors are estimated
# then the forecast regression is estimated
# and forecasts are computed

rm(list=ls(all=TRUE))
setwd("/Users/nsaleille/Dropbox/ofpr/scripts") # chemin perso

########### STEP 0 ############################################
########### load information sets  ############################
###############################################################

library(R.utils)
sourceDirectory('./fonctions/zoo_utils', modifiedOnly = FALSE)
sourceDirectory('./fonctions/extract_data', modifiedOnly = FALSE)
sourceDirectory('./fonctions/dynamic_factor_model', modifiedOnly = FALSE)

load('./dataset/information_sets.RData')

ys <- information_sets$ys
ys.trim <- information_sets$ys.trim
Xs <- information_sets$Xs

########### STEP 1 ###################################
########### Model Parameters   #######################
######################################################

# we select the number of static factors looking at the adj R^2
# of the forecast regression with h = 1

# test with q = 11 and s = 13
# these are the selected orders with h = 0
# may provide better results than previsouly plotted

q <- 8 # number of static factors
s <- 10 # number of dynamic factors

########### STEP 3 ###################################
########### Estimate Dynamic Factors  ################
######################################################

factors <- lapply(Xs, dynamicFactorsEstimates, q, s)
f.kalman <- lapply(factors, function(x) x$f.kalman)
lambda <- lapply(factors, function(x) x$lambda)
estimation.dates <- as.yearmon(sapply(Xs, end))

f.trim <- lapply(f.kalman, aggregate, by = as.yearqtr, head, 1) # !!!
names(f.trim) <- as.yearmon(estimation.dates)
save(factors, file = './dataset/factors.RData')

########### STEP 4 ###################################
########### Forecast Using Factors  ##################
######################################################

forecast.dates <- as.yearmon(names(f.trim))
forecasts <- mapply(FUN = olsForecast, ys.trim, f.trim, forecast.dates, USE.NAMES = FALSE)
h <- sort(unique(unlist(lapply(forecasts, function(x) x['h',]))))

y.test <- list()
for (i in h){
  obs <- lapply(forecasts, function(x) x[,(x['h',] == i)])
  y.test[[paste('h =', i)]] <- zoo(unlist(sapply(obs, function(x) x$y.hat)), 
             order.by = as.yearqtr(unlist(sapply(obs, function(x) x$horizon))))
}

pdf (file = '../plots/dynamic_factor_forecasts_V2.pdf', width = 9, height = 6)
par(mfrow = c(3, 2), mar = c(3,5,2,2))
for (i in 1:length(y.test)){
  plotGrowth(ys.trim[[length(ys.trim)]], y.test[i], col = rainbow(length(y.test))[i], names(y.test[i]))
}
dev.off ()

rmse <- sapply(X = y.test, FUN = function(x) RMSE(x, ys.trim[[144]]))
print(rmse)
