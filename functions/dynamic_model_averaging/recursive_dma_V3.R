recursive_dma <- function(ys, f.trim, n.steps.ahead, lambda, alpha, kappa){

  ## add an intercept to factor matrix
  intercept <- zoo(1, order.by = index(f.trim[[1]]$zoo))
  Xs <- lapply(f.trim, function(x) cbind(intercept = zoo(1, order.by = index(x$zoo)), x$zoo))
  Xs <- mapply(function(x,y) {y$zoo <- x; return(y)}, Xs, ys, SIMPLIFY = FALSE)

  ## for each training sets: run estimates and predict

  forecast.dma <- list()
  forecast.dms <- list()

  for (i in 1:length(Xs)){
    
    # select the training set
    outcome <- ys[[i]]$zoo
    predictors <- Xs[[i]]
    
    # run dma parameter estimates
    dma <- dma_estimates(outcome, predictors$zoo, lambda, alpha, kappa)
    
    # forecast, i.e. predict the outcome
    forecasts.raw <- lapply(dma$models, dma_model_prediction, h = n.steps.ahead, predictors = predictors, theta = dma$parameters, 
           sigma = dma$sigma, lambda = lambda)
    forecasts <- zoo(sapply(forecasts.raw, function(x) x$y_pred), order.by = sapply(dma$models, function(x) x$name))
    
    # predict probabilities
    prob_last <- dma$probs[forecasts.raw[[1]]$last_observed ,]
    prob_pred <- prob_last^alpha / sum(prob_last) # predicted probability for model k

    # DMA and DMS forecasts
    forecast.dma[[i]] <- sapply(forecasts %*% t(prob_pred), function(x) {predictors$zoo <- x; return(predictors)})
    forecast.dms[[i]] <- sapply(forecasts[which(prob_pred == max(prob_pred))], function(x) {predictors$zoo <- x; return(predictors)})

    print(paste('DMA forecasts for', predictors$today, 'training set: Done (', i, '/', length(f.trim), ')'))

  }

  return(list(dma = forecast.dma, dms = forecast.dms))

}