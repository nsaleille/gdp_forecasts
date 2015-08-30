olsForecast <- function(y, X, summary = FALSE, na.omit = FALSE){
  
  #y <- ys.trim[[144]]
  #X <- f.trim[[144]]
  
  # dim(y) = (T x 1); dim(X) = (T+h x N)
  # h steps ahead forecasts of y
  # using OLS regression
  
  if (na.omit){
    y <- na.omit(y)
  }

  if (end(y) > end(X)){

    stop('information in predictors shorter than in outcome !')

  }

  # predict.index <- index(X)[which(!(index(X) %in% index(y)))]
  predict.index <- index(X)[which(is.na(y))[1]]
  factor.model <- lm(y ~ -1 + ., data = merge(y, X))
  y.hat <- fitted.values(factor.model)
  # y.hat <- X[predict.index, ] %*% coefficients(factor.model)
  # y.hat <- zoo(y.hat, order.by = predict.index)
  # names(y.hat) <- paste('h =', predict.index - tail(index(y), 1))
  
  if (summary == TRUE){
    return(list(regression = summary(factor.model), predictions = y.hat))
  } else {
    return(y.hat)
    # return(list(prediction = y.hat, index = index(y.hat)))
  }

}