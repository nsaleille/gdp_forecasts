######################################################
########### Dynamic factor estimates #################
######################################################

rm(list=ls(all=TRUE))
setwd("/Users/nsaleille/Dropbox/ofpr/scripts")

## functions

library(R.utils)
sourceDirectory('./functions/zoo_utils', modifiedOnly = FALSE)
sourceDirectory('./functions/dynamic_factor_model', modifiedOnly = FALSE) # for the estimation of dynamic factors
sourceDirectory('./functions/dynamic_model_averaging', modifiedOnly = FALSE, recursive = FALSE)

## dataset

load('../data/data_clean/predictors.RData')
load('../data/data_clean/outcome.RData')
load('../data/data_clean/release.RData')

########### STEP 1 ###################################
########### Estimates  ###############################
######################################################

q <- 8
s <- q + 2

fs.trim <- list()
for (h in 1:8){
  sets <- build_training_sets(outcome, predictors, release, prop.train = 0.5, n.steps.ahead = h)
  Xs <- sets$predictors; ys <- sets$outcome
  print(paste('Dynamic factors estimates for h =', h,': Started'))
  fs.trim[[paste('h =', h)]] <- dynamicFactorsEstimatesVec(Xs, q, s = s, aggregate = TRUE)
  print(paste('Dynamic factors estimates for h =', h,': Done'))
}

save(fs.trim, file = '../data/factors_estimates/factors.RData')