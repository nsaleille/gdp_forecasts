build_training_sets <- function(outcome, predictors, realease, prop.train = 0.5, n.steps.ahead = 3){
  
  # creates training sets from outcome and predictors data matrices
  # these training sets are meant to be used for recursive forecasting exercises
  # prop.train is the proportion of observations to be included in the training set
  # n.steps.ahead is the number of periods between the date where we build the forecast and the actual release date
  # 
  # returns :
  #  - an outcome object containing a list of training sets each one associated with three dates, 
  #     today (i.e. time t), the forecast realease date (i.e. t+h) and the reference period (i.e. the quarter correponding to the forecasted value)
  #  - a predictor object with a similar structure.

  require(zoo)
  
  # size of the initial training set
  k <- round(prop.train * nrow(predictors), 0)
  
  # create an array of release dates
  tspan.test <- c(end(predictors[1:k, ]), end(predictors))
  outcome.test <- window(outcome, start = as.yearqtr(tspan.test)[1], end = as.yearqtr(tspan.test)[2])
  reference.dates <- as.yearmon(index(outcome.test))
  release.dates <- reference.dates + 5 / 12 #  release date = reference + 5 month
  
  # choose forecasting horizon to compute forecast dates
  today <- release.dates - h / 12
  
  # create training sets for predictors
  Xs <- lapply(today, function(x) window(predictors, start = start(predictors), end = x))
  Xs <- lapply(Xs, infoSet, release) # remove data not released yet
  Xs <- mapply(function(x, y, z, v) list(zoo = x, today = v, forecast.release.date = y, forecast.reference.period = z, n.steps.ahead = h), 
               Xs, release.dates, index(outcome.test), today, SIMPLIFY = FALSE)
  
  # create corresponding training sets for the outcome
  # for each forecast dates, the last GDP figure released
  # is either 
  # - the one released in T-1 (reference date T-2) if we are in month M1/T or M2/T
  # - the one released in T (reference date T-1) if we are in month M3/T
  
  is.M3 <- function(monthly.dates){(as.yearqtr(monthly.dates + 1/12) - as.yearqtr(monthly.dates) > 0)}
  
  ys <- lapply(today, 
               function(x) window(outcome, start = as.yearqtr(start(predictors)), end = as.yearqtr(x)))
  ys[is.M3(today)] <- lapply(ys[is.M3(today)], 
                                              function(x) {x[length(x)] <- NA; return(x)})
  ys[!is.M3(today)] <- lapply(ys[!is.M3(today)], 
                                               function(x) {x[(length(x)-1):length(x)] <- NA; return(x)})
  ys <- mapply(function(x, y, z, v) list(zoo = x, today = v, forecast.release.date = y, forecast.reference.period = z, n.steps.ahead = h), 
               ys, release.dates, index(outcome.test), today, SIMPLIFY = FALSE)
  
  return(list(predictors = Xs, outcome = ys))
  
}

