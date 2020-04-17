######################################################
###########	Benchmark Random Walk model	##############
######################################################

rm(list=ls(all=TRUE))
setwd("/Users/nsaleille/Dropbox/ofpr/scripts")

## functions

library(R.utils)
sourceDirectory('./functions/ar', modifiedOnly = FALSE)
sourceDirectory('./functions/zoo_utils', modifiedOnly = FALSE)

## dataset
library(zoo)
load('../data/data_clean/predictors.RData')
load('../data/data_clean/outcome.RData')
load('../data/data_clean/release.RData')

########### STEP 2 ###################################
########### Recursive forecasts  #####################
######################################################

# prediction during month M is simply the
# last observed value of the outcome

outcome <- window(outcome, start = '1991 Q1', end = '2014 Q4')

forecasts.rw <- list()
for (h in 1:8){
  sets <- build_training_sets(outcome, predictors, release, prop.train = 0.5, n.steps.ahead = h)
  ys <- sets$outcome
  forecasts.rw[[paste('h =', h)]] <- zoo(sapply(ys, function(x) na.omit(x$zoo)[length(na.omit(x$zoo))]), 
                      order.by = as.yearqtr(sapply(ys, function(x) x$forecast.reference.period)))
}

rmse.rw <- list(rmse = sapply(forecasts.rw, RMSE, outcome))
rmse.rw$type = 'random walk'
save(rmse.rw, file = '../data/rmse/random_walk.RData')
plotGrowth(outcome, forecasts.rw[8], col = rainbow(1))


