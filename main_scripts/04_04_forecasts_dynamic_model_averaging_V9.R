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

load('../data/factors_estimates/factors.RData')
load('../data/data_clean/outcome.RData')
load('../data/data_clean/predictors.RData')
load('../data/data_clean/release.RData')

########### STEP 1 ###################################
########### Model Parameters  ########################
######################################################

lambda <- 0.99 # forgetting factor for the covariance
alpha <- 0.99 # forgetting factor for the model prior
kappa <- 0.99 # forgetting factor for model probabilities

########### STEP 2 ###################################
########### Recursive forecasts  #####################
######################################################

dma.forecasts <- list()
dms.forecasts <- list()
for (h in 1:8){
  print(paste('Recursive DMA with h =', h))
  sets <- build_training_sets(outcome, predictors, release, prop.train = 0.5, n.steps.ahead = h)
  ys <- sets$outcome
  forecasts <- recursive_dma(ys, fs.trim[[h]], n.steps.ahead = h, lambda, alpha, kappa)  
  dma.forecasts[[paste('h =', h)]] <- forecasts$dma
  dms.forecasts[[paste('h =', h)]] <- forecasts$dms
}

save(dma.forecasts, file = '../data/forecasts/dma_forecasts.RData')
save(dms.forecasts, file = '../data/forecasts/dms_forecasts.RData')

########### STEP 2 ###################################
########### Interpretation of results  ###############
######################################################

load('../data/forecasts/dma_forecasts.RData')
load('../data/forecasts/dms_forecasts.RData')

y.hat.dma <- lapply(dma.forecasts, forecasts_to_zoo)
rmse.dma <- list(rmse = sapply(y.hat.dma, RMSE, outcome), type = 'dma')
save(rmse.dma, file = '../data/rmse/rmse_dma.RData')

y.hat.dms <- lapply(dms.forecasts, forecasts_to_zoo, unlist.data = TRUE)
rmse.dms <- list(rmse = sapply(y.hat.dms, RMSE, outcome), type = 'dms')
save(rmse.dms, file = '../data/rmse/rmse_dms.RData')

y.hat <- list(DMA = y.hat.dma[[1]], DMS = y.hat.dms[[1]])
outcome.subset <- outcome[which(index(outcome) == '1991 Q2'):length(outcome)]
plotGrowth(outcome.subset, y.hat[1], 'darkblue', file = '../plots/dma_in_sample_fit.pdf')
plotGrowth(outcome.subset, y.hat[2], 'darkblue', file = '../plots/dms_in_sample_fit.pdf')


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