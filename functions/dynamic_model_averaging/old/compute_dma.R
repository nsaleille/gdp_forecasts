compute_dma <- function(y, X, prob_init, theta_init, sigma_init, H_init, lambda, alpha, kappa){

  # dimensions
  m <- ncol(X); d <- ncol(y); T <- nrow(X)

  # construction of the model space
  y_dma <- array(dim = c(T,d))
  models <- lapply(seq(1:m), FUN = combn, x = m, simplify = FALSE)
  models <- unlist(models, recursive = FALSE)
  models <- lapply(models, FUN = function(x){return(list(name = paste('model', paste(x, collapse=''), sep = '_'), vars = x))})
  model_names <- sapply(models, FUN = function(x){return(x$name)})

  # for each model, keep track of the estimated values in a list of matrix
  # one matrix for each model
  theta <- lapply(models, FUN = function(x){return(matrix(nrow = nrow(X), ncol = length(x$vars)))})
  theta <- lapply(theta, FUN = function(x){x[1,] <- theta_init; return(x)})
  names(theta) <- model_names
  prob <- matrix(nrow = T, ncol = length(models))
  colnames(prob) <- model_names
  prob[1, ] <- prob_init
  
  # initialisation of covariance matrices
  sigma <- lapply(models, FUN = function(x){sigma_init*diag(length(x$vars))}) # parameter
  names(sigma) <- model_names
  H <- lapply(models, FUN = function(x){H_init*diag(d)}) # forecast
  names(H) <- model_names

  # obtain predictions recursively
  for (t in 2:T){
    
    # apply predict_update over model space
    updates <- lapply(models, FUN = predict_update, t = t, y_obs = y, vars = X, 
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

  return(list(pred = y_dma, parameters = theta, probs = prob))

}