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

sourceDirectory('./fonctions/zoo_utils', modifiedOnly = FALSE)
forecast.dates <- as.yearmon(names(f.trim))
forecasts <- mapply(FUN = olsForecast, ys.trim, f.trim, forecast.dates, USE.NAMES = FALSE)
h1 <- lapply(forecasts, function(x) x[,(x['h',] == -1)])
y.h1 <- zoo(unlist(sapply(h1, function(x) x$y.hat)), order.by = as.yearqtr(unlist(sapply(h1, function(x) x$horizon))))
plotGrowth(ys.trim[[length(ys.trim)]], y.h1)

h2 <- lapply(forecasts, function(x) x[,(x['h',] == -0.5)])
y.h2 <- zoo(unlist(sapply(h1, function(x) x$y.hat)), order.by = as.yearqtr(unlist(sapply(h1, function(x) x$horizon))))
lines(y.h2, lwd = 3, col = 'darkgreen')

# probleme, il faudrait avoir une seul prevision
# a horizon t+h et tel que h = -1

forecast.horizon <- as.yearqtr(forecasts['index', ])
h1 <- sapply(1:length(forecast.horizon), function(x) (x%%3 == 2))
h2 <- sapply(1:length(forecast.horizon), function(x) (x%%3 == 1))
h3 <- sapply(1:length(forecast.horizon), function(x) (x%%3 == 0))

y.h1 <- zoo(unlist(forecasts['prediction',])[h1], order.by = forecast.horizon[h1])
y.h2 <- zoo(unlist(forecasts['prediction',])[h2], order.by = forecast.horizon[h2])
y.h3 <- zoo(unlist(forecasts['prediction',])[h3], order.by = forecast.horizon[h3])

plotGrowth(ys.trim[[length(ys.trim)]], y.h1)
lines(y.h2, col = 'darkgreen', lwd = 2)
lines(y.h3, col = 'pink', lwd = 2)

t <- as.yearmon(names(ys.trim))
tplush <- as.yearmon(names(forecasts))
y.hat <- zoo(forecasts, order.by = as.yearmon(names(ys.trim)))
y.test <- lapply(ys, na.locf, fromLast = TRUE, na.rm = FALSE)[[length(ys.trim)]]
table <- na.omit(merge(y.hat, y.test))

print(RMSE(table[,1], table[,2]))
plotGrowth(ys.trim[[length(ys.trim)]], y.hat)

par(mfrow = c(1, 1), mar = c(3,5,2,2))
plot(ys.trim[[length(ys.trim)]], col = 'darkred', type = 'l',  xlab = '', ylim = c(-2, 2), lwd = 1.5)
lines(y.hat, col = 'darkblue', type = 'l', lwd = 2)
abline(h = 0, lwd = 0.5)



plotGrowth(ys.trim[[length(ys.trim)]], y.hat, file = '../tex/plots/dynamic_factor_forecasts.pdf')
