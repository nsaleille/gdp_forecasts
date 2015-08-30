######################################################
###########	Benchmark AR Model	######################
######################################################

rm(list=ls(all=TRUE))
setwd("/Users/nsaleille/Dropbox/ofpr/scripts")

## functions

library(R.utils)
sourceDirectory('./functions/ar', modifiedOnly = FALSE)
sourceDirectory('./functions/zoo_utils', modifiedOnly = FALSE)

## dataset

load('../data/data_clean/predictors.RData')
load('../data/data_clean/outcome.RData')
load('../data/data_clean/release.RData')

########### STEP 2 ###################################
########### Recursive forecasts  #####################
######################################################

# what happens if we add regressors instead of a simple AR ?

outcome <- window(outcome, start = '1991 Q1', end = '2014 Q4')

## for each forecasting horizon
## select the order that minimise the RMSE

ar_rmse <- list()
for (h in 1:8){
  rmse <- c()
  sets <- build_training_sets(outcome, predictors, release, prop.train = 0.5, n.steps.ahead = h)
  ys <- sets$outcome
  for (order in 1:10){
    y.hat <- recursive_ar(order, ys, n.ahead = h)
    rmse <- c(rmse, RMSE(outcome, y.hat))
  }
  names(rmse) <- paste('order =', 1:10)
  ar_rmse[[paste('h =', h)]] <- rmse
  print(h)
}

ar_rmse_clean <- Reduce(rbind, ar_rmse)
rownames(ar_rmse_clean) <- paste('h =', 1:8)


orders <- sapply(ar_rmse_clean, function(x) which((x==min(x))))
names(rmse.ar) <- paste('h =', 1:8)
rmse.orders <- sapply(ar_rmse, function(x) which((x==min(x))))
names(rmse.orders) <- paste('h =', 1:8)

## get recursive forecasts of selected models
## in order to plot them

forecasts.ar <- list()
for (h in 1:8){
  sets <- build_training_sets(outcome, predictors, release, prop.train = 0.5, n.steps.ahead = h)
  ys <- sets$outcome
  forecasts.ar[[paste('h =', h)]] <- recursive_ar(rmse.orders[h], ys, n.ahead = h)
}

rmse.ar <- list(rmse = sapply(forecasts.ar, RMSE, outcome), order = rmse.orders, type = 'ar')
save(rmse.ar, file = '../data/rmse/ar_model.RData')

plotGrowth(outcome, forecasts.ar, col = rainbow(8), file = '../plots/recursive_ar.pdf')
