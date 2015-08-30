ForecastRegression <- function(outcome, predictors, q, s, h){

	estimates <- dynamicFactorsEstimates(predictors, q, s, lag.max = 5)
	f.trim <- aggregate(estimates$f.kalman, as.yearqtr, head, 1) 
	f.lag <- lag(f.trim, -h)
	y <- window(outcome, start = start(f.lag), end = end(f.lag))
	reg <- lm(y ~ f.lag)
	y.hat <- fitted.values(reg)

	return(list(lm = reg, y.hat = y.hat))

}