######################################################
###########DMA: Dynamic model averaging###############
######################################################

rm(list=ls(all=TRUE))
setwd("/Users/nsaleille/Dropbox/ofpr/scripts")

## functions

library(R.utils)
sourceDirectory('./functions/zoo_utils', modifiedOnly = FALSE)
sourceDirectory('./functions/dynamic_factor_model', modifiedOnly = FALSE) # for the estimation of dynamic factors
sourceDirectory('./functions/dynamic_model_averaging', modifiedOnly = FALSE, recursive = FALSE)

## dataset

load('../data_clean/predictors.RData')
load('../data_clean/outcome.RData')

########### STEP 1 ###################################
########### Model Parameters  ########################
######################################################

## DFM parameters

# select only q factors as predictors
# increasing the number of factors slightly changes the selected models
# probabilities have a comparable shape

q <- 8 # number of factors
h <- 1 # forecast horizon

## DMA parameters 

lambda <- 0.99 # forgetting factor for the covariance
alpha <- 0.99 # forgetting factor for the model prior
kappa <- 0.99 # forgetting factor for model probabilities

########### STEP 2 ###################################
########### Recursive forecasts  #####################
######################################################

## estimate factors

sets <- build_training_sets(outcome, predictors, release, prop.train = 0.5, n.steps.ahead = h)
Xs <- sets$predictors; ys <- sets$outcome
f.trim <- dynamicFactorsEstimatesVec(Xs, q, s = q + 2)
save(f.trim, file = '../data_clean/factors.RData')

## add an intercept

load('../data_clean/factors.RData')
intercept <- zoo(1, order.by = index(f.trim[[1]]))
fs.trim <- lapply(f.trim, function(x) cbind(intercept = zoo(1, order.by = index(x$zoo)), x$zoo))
fs.trim <- mapply(function(x, y){x$zoo <- y; return(x)}, Xs, fs.trim, SIMPLIFY = FALSE)

## for each training sets: run estimates and predict

forecast.dma <- list()
forecast.dms <- list()

for (i in 1:length(fs.trim)){
  
  # select the training set
  outcome <- ys[[i]]$zoo
  predictors <- fs.trim[[i]]
  
  # run dma parameter estimates
  dma <- dmaEstimates(outcome, predictors$zoo, lambda, alpha, kappa)
  
  # forecast, i.e. predict the outcome
  forecasts.raw <- lapply(dma$models, dma_model_prediction, h = h, predictors = predictors, theta = dma$parameters, 
         sigma = dma$sigma, lambda = lambda)
  forecasts <- zoo(sapply(forecasts.raw, function(x) x$y_pred), order.by = sapply(dma$models, function(x) x$name))
  
  # predict probabilities
  prob_last <- dma$probs[forecasts.raw[[1]]$last_observed ,]
  prob_pred <- prob_last^alpha / sum(prob_last) # predicted probability for model k

  # DMA and DMS forecasts
  forecast.dma[[i]] <- sapply(forecasts %*% t(prob_pred), function(x) {predictors$zoo <- x; return(predictors)})
  forecast.dms[[i]] <- sapply(forecasts[which(prob_pred == max(prob_pred))], function(x) {predictors$zoo <- x; return(predictors)})

  print(paste('DMA forecasts for', predictors$today, 'training set: Done (', i, '/', length(fs.trim), ')'))
  
}

## save results

save(forecast.dma, file = '../data_clean/forecast_dma.RData')
save(forecast.dms, file = '../data_clean/forecast_dms.RData')

load('../data_clean/forecast_dma.RData')
load('../data_clean/forecast_dms.RData')

########### STEP 2 ###################################
########### Interpretation of results  ###############
######################################################

y.hat.dma <- zoo(sapply(forecast.dma, function(x) x['zoo',]$zoo), 
    as.yearqtr(sapply(forecast.dma, function(x) x[4,]$forecast.reference.period)))

y.hat.dms <- zoo(unlist(sapply(forecast.dms, function(x) x['zoo',]$zoo)), 
                 as.yearqtr(sapply(forecast.dms, function(x) x[4,]$forecast.reference.period)))

y.hat <- list(dma = y.hat.dma, dms = y.hat.dms)

plotGrowth(outcome, y.hat, rainbow(2), file = '../plots/dma_in_sample_fit.pdf')
lapply(y.hat, RMSE, outcome)
dma_driving_models(dma$probs, 0.1, file = '../plots/dma_posterior_probabilities.pdf')

# models with high probabilities at least at one point in time
# many models means many probabilities pattrens very close
# seems that some groups of models can be isolated
# how can we use clustering ?
# the constant is always there

#posterior.theta <- results$parameters[driving.names]
#par(mfrow = c(ncol(driving.models), 1))
#lapply(posterior.theta, plot, plot.type = 'single', lwd = 2)