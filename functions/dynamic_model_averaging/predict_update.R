predict_update <- function(t, model, outcome, predictors, theta, sigma, H, prob_last, lambda, alpha, kappa){
  
  # select model
  y_current <- outcome[t]
  X_current <- predictors[t, model$vars]
  theta_last <- theta[[model$name]][t-1,]
  H_last <- H[[model$name]]
  sigma_last <- sigma[[model$name]]
  
  # predict for one model / one step ahead
  theta_pred <- theta_last # F: identity
  S_pred <- (1/lambda) * sigma_last # predicted variance for (theta | y^t) 
  prob_pred <- prob_last^alpha / sum(prob_last) # predicted probability for model k
  y_pred <- X_current %*% theta_pred # forecast
  
  # Update for one model / one step ahead
  error <- y_current - y_pred # forecast error
  xSx <- X_current %*% S_pred %*% t(X_current)
  H_up <- kappa * H_last + (1-kappa) * t(error) %*% error # predicted variance for (y_t | y^t-1)
  F_inv <- solve(H_up + xSx)
  theta_up <- theta_pred +  S_pred %*% t(X_current) %*% F_inv %*%  (y_current - X_current %*% theta_pred)
  sigma_up <- S_pred - S_pred %*% t(X_current) %*% F_inv %*% X_current %*% S_pred
  weight <- pnorm(q = y_current, mean = X_current %*% theta_pred, sd = sqrt(H_up + xSx)) * prob_pred[model$name]
  
  return(list(weight = weight, theta_up = theta_up, sigma_up = sigma_up, y_pred = y_pred,
              H = H_up, sigma = sigma_up))
}