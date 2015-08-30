recursive_DMA <- function(){

  for (t in (Ty + 1):T){
    
    predictions <- lapply(models, FUN = predict_dma, t = t, predictors = X, theta = theta, 
                          sigma = sigma, prob_last = prob[Ty,], lambda = lambda, alpha = alpha)
    
    # update theta
    theta_pred <- sapply(predictions, FUN = function(x){return(x$theta_pred)})
    theta <- mapply(FUN = function(x,y){x[t,] <- y; return(x)}, theta, theta_pred)
    
    ## predict unobserved y
    y_pred <- sapply(predictions, FUN = function(x){return(x$y_pred)})
    y_dma[t] <- prob[Ty,] %*% y_pred
    y_dms[t] <- y_pred[which(prob[Ty,] == max(prob[Ty,]))]
    
  }

}