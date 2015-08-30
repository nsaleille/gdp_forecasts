predict_dma <- function(t, model, predictors, theta, sigma, prob_last, lambda, alpha){
  
  # select model
  X_current <- predictors[t, model$vars]
  theta_last <- theta[[model$name]][t-1,]
  sigma_last <- sigma[[model$name]]
  
  # predict for one model / one step ahead
  theta_pred <- theta_last # F: identité
  sigma_pred <- (1/lambda) * sigma_last # predicted variance for (theta | y^t) 
  prob_pred <- prob_last^alpha / sum(prob_last) # predicted probability for model k
  y_pred <- X_current %*% theta_pred # forecast

  return(list(theta_pred = theta_pred, sigma_pred = sigma_pred, y_pred = y_pred))

}