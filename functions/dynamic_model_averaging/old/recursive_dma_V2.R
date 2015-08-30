recursive_dma <- function(outcome, predictors, release, q, prob.train, n.steps.ahead){

  ## estimate factors

  sets <- build_training_sets(outcome, predictors, release, prop.train = 0.5, n.steps.ahead = n.steps.ahead)
  Xs <- sets$predictors; ys <- sets$outcome
  print(paste('Dynamic factors estimates for h =', n.steps.ahead,': Started'))
  f.trim <- dynamicFactorsEstimatesVec(Xs, q, s = q + 2)
  print(paste('Dynamic factors estimates for h =', n.steps.ahead,': Done'))
  # save(f.trim, file = '../data_clean/factors.RData')

  ## add an intercept

  # load('../data_clean/factors.RData')
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
    forecasts.raw <- lapply(dma$models, dma_model_prediction, h = n.steps.ahead, predictors = predictors, theta = dma$parameters, 
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

  return(list(dma = forecasts.dma, dms = forecasts.dms))

}