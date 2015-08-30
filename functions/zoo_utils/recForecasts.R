recForecasts <- function(y, X, h, k, FUN){
  
  # compute recursive forecasts of y using predictors in X
  # dim(y) = (T x d); dim(X) = (T+h x N)
  # k is the initial size of the training sample
  
  #y <- y.trim
  #X <- predictors
  
  if (is.null(ncol(y))){ # test if univariate zoo or not

    ys <- lapply(k:(length(y)-(h+1)), FUN = function(i) y[1:i])

  } else {

    ys <- lapply(k:(dim(y)[1]-(h+1)), FUN = function(i) y[1:i,])

  }
  
  Xs <- lapply((k+h):nrow(X), FUN = function(i) X[1:i,]) # adapté aux facteurs seulement
  # Xs <- lapply(k:nrow(X), FUN = function(i) X[1:i,]) # adapté au BVAR
  
  forecasts <- mapply(FUN = FUN, ys, Xs)
  forecasts <- zoo(t(forecasts), order.by = index(X)[(k+h):nrow(X)])

  names(forecasts) <- paste('h =', 0:(ncol(forecasts) - 1))
  
  return(forecasts)
}