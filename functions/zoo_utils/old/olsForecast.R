olsForecast <- function(y, X, summary = FALSE){
  
  # dim(y) = (T x 1); dim(X) = (T+h x N)
  # h steps ahead forecasts of y
  # using OLS regression
  
  # if (length(y) > dim(X)[1]){
  #   stop('the vector to predict has more observation than predictors')
  # }
  
  if (end(y) > end(X)){
    stop('information in predictors shorter than in outcome !')
  }

  T <- length(y); h <- nrow(X) - T
  data <- na.omit(merge(y, X))
  factor.model <- lm(y ~ -1 + ., data = data)
  y.hat <- X[T:(T+h), ] %*% coefficients(factor.model)
  y.hat <- zoo(y.hat, order.by = index(X[T:(T+h), ]))
  
  if (summary == TRUE){
    return(list(regression = summary(factor.model), predictions = y.hat))
  } else {
    return(y.hat)
  }

}