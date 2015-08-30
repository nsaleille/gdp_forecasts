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
sourceDirectory('./functions/dynamic_factor_model', modifiedOnly = FALSE)
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
q <- 8

# parameters
lambda <- 0.99 # forgetting factor for the covariance
alpha <- 0.99 # forgetting factor for the model prior
kappa <- 0.99

########### STEP 2 ###################################
########### Recursive forecasts  #####################
######################################################

h <- 1

# estimate factors
load('../data_clean/predictors.RData') # use dynamic factors as predictors
sets <- build_training_sets(outcome, predictors, prop.train = 0.5, n.steps.ahead = h)
Xs <- sets$predictors; ys <- sets$outcome
f.trim <- dynamicFactorsEstimatesVec(Xs, q, s = q + 2)
save(f.trim, file = '../data_clean/factors.RData')

load('../data_clean/factors.RData')

# add an intercept
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

save(forecast.dma, file = '../data_clean/forecast_dma.RData')
save(forecast.dms, file = '../data_clean/forecast_dms.RData')

dma_model_prediction(fs.trim[[1]], model, theta, sigma, lambda)
model <- test$models[[1]]; theta <- test$parameters; predictors <- fs.trim[[1]]$zoo
sigma <- test$sigma
predictors <- fs.trim[[2]]





lapply(f.trim, modelPredictions, model = model, theta = theta, sigma = sigma, lambda = lambda)
modelPredictions(f.trim[[1]], model, theta, sigma, lambda)

recursive.forecasts <- mapply(dmaEstimates, ys, fs.trim, 
                              MoreArgs = list(lambda = lambda, alpha = alpha, kappa = kappa))

#recursive.forecasts <- mapply(compute_dma, ys, fs.trim, 
 #                             MoreArgs = list(lambda = lambda, alpha = alpha, kappa = kappa))


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


