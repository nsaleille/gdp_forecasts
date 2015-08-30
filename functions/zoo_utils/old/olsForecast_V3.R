olsForecast <- function(y, X, h = 1, summary = FALSE, na.omit = FALSE){
  
  # dim(y) = (T x 1); dim(X) = (T+h x N)
  # h steps ahead forecasts of y
  # using OLS regression
  
  if (na.omit){
    y <- na.omit(y)
  }

  if (end(y) > end(X)){
    stop('information in predictors shorter than in outcome !')
  }

  forecast.horizon <- index(y)[which(is.na(y))]
  forecast.date <- index(X)[nrow(X)]
  horizons <- (forecast.horizon - forecast.date) * 4
  h.select <- which(horizons == h)
  if (is.null(h.select)){
    print(paste('Not possible to predict', h, 'steps aeahd with the information set'))
  }

  data <- na.omit(merge(y, lag(X,-h)))
  factor.model <- lm(y ~ ., data = data)
  X$cst <- 1; X <- X[, c(which(names(X) == 'cst'), which(names(X) != 'cst'))]
  y.hat <- X[forecast.date, ] %*% coefficients(factor.model)
  
  if (summary == TRUE){
    return(list(regression = summary(factor.model), predictions = y.hat))
  } else {
    # return(y.hat)
    return(list(prediction = y.hat, forecast.date = forecast.date, forecast.horizon = forecast.horizon, h = h))
  }

}