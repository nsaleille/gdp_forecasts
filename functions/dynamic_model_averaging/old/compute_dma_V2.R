compute_dma <- function(y, X, prob_init, theta_init, sigma_init, H_init, lambda, alpha, kappa){

  #y <- ys.trim[[1]]
  # dimensions
  m <- ncol(X); T <- nrow(X)
  d <- dim.zoos(y.trim)[1]

  # construction of the model space
  y_dma <- array(dim = c(T,d))
  models <- lapply(seq(1:m), FUN = combn, x = m, simplify = FALSE)
  models <- unlist(models, recursive = FALSE)
  models <- lapply(models, FUN = function(x){return(list(name = paste('model', paste(x, collapse=''), sep = '_'), vars = x))})
  model_names <- sapply(models, FUN = function(x){return(x$name)})
  vars <- sapply(models, function(x) x$vars)
  
  print(paste('DMA with', m, 'predictors and ', length(models), 'models'))

  # for each model, keep track of the estimated values in a list of matrix
  # one matrix for each model
  theta <- lapply(models, FUN = function(x){return(matrix(nrow = nrow(X), ncol = length(x$vars)))})
  theta <- mapply(FUN = function(x, y){x[1,] <- theta_init[y]; return(x)}, theta, vars)
  names(theta) <- model_names
  
  prob <- matrix(nrow = T, ncol = length(models))
  colnames(prob) <- model_names
  prob[1, ] <- prob_init
  
  # initialisation of covariance matrices
  
  # estimation de la condition initiale à l'aide d'OLS simple
  # il semble important de mettre une constante dans le modèle (signif >0)
  # le modèle est bien meilleur en terme d'explication de la variance
  # les coefficients bougent peu mais sont moins bien estimés (biais imposé)
  regs.ols <- lapply(models, function(model) lm(y.trim ~ -1 + ., data = merge(y.trim, X[,model$vars])))
  H_init <- lapply(regs.ols, function(x) (1 / (T - m - 1)) * sum(residuals(x)^2))
  names(H_init) <- model_names
  theta_init <- lapply(regs.ols, coefficients) # OLS estimates
  sigma_init <- mapply(function(h, model) solve(t(X[,model$vars])%*%X[,model$vars]) * h, H_init, models)
  
  # initialisation
  sigma <- sigma_init
  H <- H_init

  # obtain predictions recursively
  for (t in 2:T){
    
    # apply predict_update over model space
    updates <- lapply(models, FUN = predict_update, t = t, y_obs = y, predictors = X, 
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

  }

  theta <- lapply(theta, zoo, order.by = index(X))
  prob <- zoo(prob, order.by = index(X))
  y_dma <- zoo(y_dma, order.by = index(X))
  
  return(list(pred = y_dma, parameters = theta, probs = prob))

}