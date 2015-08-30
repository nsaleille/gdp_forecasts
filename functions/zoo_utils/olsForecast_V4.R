olsForecast <- function(y, X){
  
  # dim(y) = (T x 1); dim(X) = (T+h x N)
  # h steps ahead forecasts of y
  # using OLS regression

  if (end(y) > end(X)){
    stop('information in predictors shorter than in outcome !')
  }

  h.max <- (end(na.omit(y)) - end(X)) * 4
  forecast.horizons <- index(y)[which(is.na(y))]
  models <- lapply((sign(h.max) * 1):h.max, function(h) lm(y ~ ., data = na.omit(merge(y, lag(X,-h)))))
  X$cst <- 1; X <- X[, c(which(names(X) == 'cst'), which(names(X) != 'cst'))]
  y.hat <- mapply(function(x,y) coefficients(x) %*% t(X[forecast.horizons[y],]), 
                  x = models, y = 1:length(models))
  forecast <- mapply(function(x,y,z) list(y.hat = x, h = y, horizon = z, forecast.date = end(X)), 
                     y.hat, (sign(h.max) * 1):h.max, forecast.horizons)
  
  return(forecast)

}