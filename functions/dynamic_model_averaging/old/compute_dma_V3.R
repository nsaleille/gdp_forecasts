compute_dma <- function(y, X, lambda, alpha, kappa){

  y <- na.omit(y)
  
## construction of the model space
  
  T <- dim.zoos(X)[1]
  m <- dim.zoos(X)[2] 
  d <- dim.zoos(y)[1]
  Ty <- dim.zoos(y)[2]
  
  y_dma <- array(dim = c(T,d))
  y_dms <- array(dim = c(T,d))
  models <- lapply(1:m, FUN = combn, x = m, simplify = FALSE)
  models <- unlist(models, recursive = FALSE)
  models <- models[sapply(models, function(x) 1 %in% x)] # delete models without intercept
  models <- lapply(models, FUN = function(x){return(list(name = paste('model', paste(x, collapse=''), sep = '_'), vars = x))})
  model_names <- sapply(models, FUN = function(x){return(x$name)})
  vars <- sapply(models, function(x) x$vars)
  K <- length(models)
  
  print(paste('DMA with', m, 'predictors and ', K, 'models'))
  
## priors
  
  # model probabilities : uninformative uniform prior
  prob <- matrix(nrow = T, ncol = K)
  colnames(prob) <- model_names
  prob[1, ] <- rep(1/K, K) # uniform prior on models
  
  # parameter theta : OLS estimator
  regs.ols <- lapply(models, function(model) lm(y ~ -1 + ., data = merge(y, X[,model$vars])))
  theta_init <- lapply(regs.ols, coefficients)
  theta <- lapply(models, FUN = function(x){return(matrix(nrow = T, ncol = length(x$vars)))})
  theta <- mapply(function(x,y) {x[1,] <- y; return(x)}, theta, theta_init)
  names(theta) <- model_names
  
  # measurement equation covariance matrix : homscedastic OLS estimator
  H_init <- lapply(regs.ols, function(x) (1 / (T - m - 1)) * sum(residuals(x)^2))
  names(H_init) <- model_names
  
  # covariance of the parameter: homoscedatic OLS estimator
  sigma_init <- mapply(function(h, model) solve(t(X[,model$vars])%*%X[,model$vars]) * h, H_init, models)
  
## initialisation
  
  sigma <- sigma_init
  H <- H_init

## obtain predictions recursively
  
  for (t in 2:Ty){
    
    # apply predict_update over model space
    updates <- lapply(models, FUN = predict_update, t = t, outcome = y, predictors = X, 
                      theta = theta, sigma = sigma, H = H, prob_last = prob[t-1,], 
                      lambda = lambda, alpha = alpha, kappa = kappa)
    # update theta
    theta_up <- sapply(updates, FUN = function(x){return(x$theta_up)})
    theta <- mapply(FUN = function(x,y){x[t,] <- y; return(x)}, theta, theta_up)
    
    # update probabilities
    weights_up <- sapply(updates, FUN = function(x){return(x$weight)})
    prob[t,] <- weights_up / sum(weights_up)
    
    # update covariance matrices
    H <- lapply(updates, FUN = function(x){return(x$H)}) # prediction covariance matrix
    sigma <- sapply(updates, FUN = function(x){return(x$sigma)}) # parameter covariance matrix
    names(H) <- model_names; names(sigma) <- model_names
    
    # compute dma prediction y_t|t-1
    y_pred <- sapply(updates, FUN = function(x){return(x$y_pred)})
    y_dma[t] <- prob[t,] %*% y_pred
    y_dms[t] <- y_pred[which(prob[t,] == max(prob[t,]))]
    
    #print(paste('t = ', t, ': done'))
  }
  
  for (t in (Ty + 1):T){
    
    predictions <- lapply(models, FUN = predict_dma, t = t, predictors = X, theta = theta, 
                          sigma = sigma, prob_last = prob[Ty,], lambda = lambda, alpha = alpha)
    
    ## predict unobserved y
    y_pred <- sapply(predictions, FUN = function(x){return(x$y_pred)})
    y_dma_pred[t] <- prob[Ty,] %*% y_pred
    y_dms_pred[t] <- y_pred[which(prob[t,] == max(prob[Ty,]))]
    
  }
  
  

# wrap up results nicely
  
  theta <- lapply(theta, zoo, order.by = index(X))
  
  prob <- zoo(prob, order.by = index(y))
  y_dma <- zoo(y_dma, order.by = index(y))
  y_dms <- zoo(y_dms, order.by = index(y))
  
  return(list(pred_dma = y_dma, pred_dms = y_dms, parameters = theta, probs = prob))

}