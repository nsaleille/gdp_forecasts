dynamicFactorsEstimatesVec <- function(Xs, q, s, aggregate = TRUE){

	## estimate dynamic factors

	Xs.coredata <- lapply(Xs, function(x) x$zoo)
	factors <- lapply(Xs.coredata, dynamicFactorsEstimates, q, s)
	f.kalman <- lapply(factors, function(x) x$f.kalman)

	if (aggregate){

		f.trim <- lapply(f.kalman, aggregate, by = as.yearqtr, head, 1) # !!!
		f.trim <- mapply(function(x, y){x$zoo <- y; return(x)}  , Xs, f.trim, SIMPLIFY = FALSE)		
		return(f.trim)

	} else {

		f.kalman <- mapply(function(x, y){x$zoo <- y; return(x)}  , Xs, f.kalman, SIMPLIFY = FALSE)
		return(f.kalman)
	}


}