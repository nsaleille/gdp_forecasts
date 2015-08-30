recursive_DFM <- function(ys, f.trim){

	## recursive forecasts

	f.trim.lag <- lapply(f.trim, function(x) lag(x$zoo, -x$n.steps.ahead))
	ys.coredata <- lapply(ys, function(x) x$zoo)
	regs <- mapply(function(x, y) lm(y ~ ., data = merge(y = y, x, all = FALSE)), f.trim.lag, ys.coredata, SIMPLIFY = FALSE)
	coefs <- lapply(regs, coefficients)
	forecasts <- mapply(function(x, y) c(1, x$zoo[as.yearqtr(x$today)]) %*% y, f.trim, coefs)
	forecasts <- zoo(unlist(forecasts), order.by = as.yearqtr(sapply(ys, function(x) x$forecast.reference.period)))
	forecasts <- list(zoo = forecasts, n.steps.ahead = ys[[1]]$n.steps.ahead)

	return(forecasts)

}