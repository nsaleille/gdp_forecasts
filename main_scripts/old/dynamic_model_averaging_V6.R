######################################################
###########DMA: Dynamic model averaging###############
######################################################

rm(list=ls(all=TRUE))
library(R.utils)
setwd("/Users/nsaleille/Dropbox/ofpr/scripts") # chemin perso

########### STEP 0 ###################################
########### load dataset  ############################
######################################################

library(R.utils)
sourceDirectory('./functions/zoo_utils', modifiedOnly = FALSE)
sourceDirectory('./functions/extract_data', modifiedOnly = FALSE)
sourceDirectory('./functions/dynamic_model_averaging', modifiedOnly = FALSE, recursive = FALSE)

load('../data_clean/factors.RData') # use dynamic factors as predictors
load('../data_clean/outcome.RData')
load('../data_clean/release.RData')
load('../data_clean/information_sets.RData')

########### STEP 1 ###################################
########### Model Parameters  ########################
######################################################

# select only q factors as predictors
# increasing the number of factors slightly changes the selected models
# probabilities have a comparable shape
m <- 8

# parameters
#K <- (2^m)-1 # we do not consider the model with 0 variables
lambda <- 0.99 # forgetting factor for the covariance
alpha <- 0.99 # forgetting factor for the model prior
kappa <- 0.99

########### STEP 2 ###################################
########### Recursive forecasts  #####################
######################################################

library(zoo)
ys <- information_sets$ys
estimation.dates <- as.yearmon(sapply(ys, end))
names(ys) <- estimation.dates
ys.trim <- information_sets$ys.trim

# select m first factors and appropriate dates
f.kalman <- lapply(factors, function(x) x$f.kalman)
f.kalman <- lapply(f.kalman, function(x) x[, 1:m])
f.kalman <- lapply(f.kalman, function(x) window(x, start = start(ys.trim[[1]]), end = end(x)))

# aggregate to quarterly observations and add an intercept
fs.trim <- lapply(f.kalman, aggregate, by = as.yearqtr, tail, 1)
intercept <- zoo(1, order.by = index(ys.trim[[1]]))
fs.trim <- lapply(fs.trim, function(x) cbind(zoo(1, order.by = index(x)), x))
names(fs.trim) <- as.yearmon(estimation.dates)

# compute DMA (vectorialized)

fs.train <- mapply(window, x = fs.trim, end = lapply(fs.trim, end), 
                   MoreArgs = list(start = start(fs.trim[[1]])))

recursive.forecasts <- mapply(compute_dma, ys.trim, fs.train, 
                              MoreArgs = list(lambda = lambda, alpha = alpha, kappa = kappa))
recursive.forecasts['predictions_dma',]

names(recursive.forecasts) <- names(fs.train)
save(recursive.forecasts, file = '../data_clean/recursive_dma_forecasts.RData')

posterior.probs <- zoo(results$prob, order.by = index(y))
y.hat.dma <- zoo(results$pred_dma, order.by = index(y))
y.hat.dms <- zoo(results$pred_dms, order.by = index(y))
plotGrowth(y, list(dma = y.hat.dma, dms = y.hat.dms), rainbow(2), file = '../plots/dma_in_sample_fit.pdf')
RMSE(ys.trim.omit[[1]], y.hat.dma)

# models with high probabilities at least at one point in time
# many models means many probabilities pattrens very close
# seems that some groups of models can be isolated
# how can we use clustering ?

driving.models <- sapply(posterior.probs, function(x) {if (sum(x>0.06)>0) return(x)})
driving.models <- driving.models[!(sapply(driving.models, is.null))]
driving.names <- names(driving.models)
driving.models <- Reduce(function(x,y) merge(x,y), driving.models)
names(driving.models) <- driving.names
par(mfrow = c(1,1))
plot(driving.models, col = rainbow(ncol(driving.models)), plot.type = 'single', lwd = 2)
legend('bottomleft', legend = names(driving.models), col = rainbow(ncol(driving.models)), 
       lty = 1, lwd = 2)

#posterior.theta <- results$parameters[driving.names]
#par(mfrow = c(ncol(driving.models), 1))
#lapply(posterior.theta, plot, plot.type = 'single', lwd = 2)


