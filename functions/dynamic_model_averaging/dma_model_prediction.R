dma_model_prediction <- function(h, predictors, model, theta, sigma, lambda){
  
  today <- as.yearqtr(predictors$today)
  frp <- predictors$forecast.reference.period
  frd <- predictors$forecast.release.date
  last_observed <- (frp - 0.25 * h)
  
  # the last available value for the parameter
  # is frp - 1
  theta_last <- theta[[model$name]][last_observed,]
  X_last <- predictors$zoo[last_observed, model$vars]
  sigma_last <- sigma[[model$name]]
  
  # predict for one model / one step ahead
  theta_pred <- theta_last # F: identité
  sigma_pred <- (1/lambda) * sigma_last # predicted variance for (theta | y^t) 
  y_pred <- X_last %*% t(theta_pred) # forecast

  # return(list(forecast = y_pred, today = today, forecast.release.date = frd, forecast.reference.period = frp))
  return(list(y_pred = y_pred, sigma_pred = sigma_pred, model = model, last_observed = last_observed))
  
}