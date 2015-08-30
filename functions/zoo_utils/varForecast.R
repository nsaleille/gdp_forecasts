varForecast <- function(y, X, lag.max = 5){
  
  #X <- Xs[[25]]
  #y <- ys[[25]]
  
  p <- VARselect(as.data.frame(X), lag.max = lag.max, type="const")$selection['AIC(n)']
  var.model <- VAR(as.data.frame(X), p = p, type = "none")
  A.hat <- t(sapply(var.model$varresult, function(x) coefficients(x)))
  
  # forecast factor values using the Var representation
  X.full <- bindLags(X, p)
  y <- window(y, start = start(X.full), end = end(y))
  T <- length(y); h <- nrow(X.full) - T
  
  if (length(y) > dim(X.full)[1]){
    stop('the vector to predict has more observation than predictors')
  }
  
  f.for <- zoo(X.full[T, ] %*% t(A.hat), order.by = end(X) + 0.25)
  if (h>1){
    for (i in 2:h){
      X.full.for <- bindLags(rbind(X, f.for), p)
      f.for <- rbind(f.for, zoo(X.full.for[i-1,] %*% t(A.hat), order.by = end(f.for) + 0.25))
    }
    
  }

  # regress y on f and predict
  ols.model <- lm(y ~ -1 + ., data = merge(y, X))
  y.hat <- f.for %*% coefficients(ols.model)
  y.hat <- zoo(y.hat, order.by = index(f.for))
  
  return(y.hat)
  
}