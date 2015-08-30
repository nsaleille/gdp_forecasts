olsForecast <- function(y, X, forecast.date){
  
  # dim(y) = (T x 1); dim(X) = (T+h x N)
  # h steps ahead forecasts of y
  # using OLS regression

  if (end(y) > end(X)){
    stop('information in predictors shorter than in outcome !')
  }

  forecast.horizons <- index(y)[which(is.na(y))]
  h <- (forecast.horizons - forecast.date) * 4
  models <- lapply(h, function(h) lm(y ~ ., data = na.omit(merge(y, lag(X, -h)))) )
  X$cst <- 1; X <- X[, c(which(names(X) == 'cst'), which(names(X) != 'cst'))]
  y.hat <- mapply(function(x,y) coefficients(x) %*% t(X[forecast.horizons[y],]), 
                  x = models, y = 1:length(models))

  forecast <- mapply(function(x,y,z) list(y.hat = x, h = y, horizon = z, forecast.date = forecast.date), 
                     y.hat, h, forecast.horizons)
  
  return(forecast)

}