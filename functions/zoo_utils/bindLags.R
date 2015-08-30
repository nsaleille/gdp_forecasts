bindLags <- function(X, p, na.omit = FALSE, bind.original = TRUE){

	# dim(X) = (T x N)
	# returns a matrix Z such that dim(Z) = (T x (N * p))
	# where the new columns are the lagged variables

	lags <- lapply(1:p, function(x) lag(X, -x))
	lags <- Reduce(function(x,y) merge(x, y), lags)
	# vars <- paste('var.', 1:ncol(X), sep = '')
	vars <- paste('lag.', 1:p, sep = '')
	# names(lags) <- sapply(vars, function(x) paste(x, '.lag.', 1:p, sep = ''))
	names(lags) <- sapply(vars, function(x) paste(x, '.var.', 1:ncol(X), sep = ''))

	if (bind.original){
		X.new <- cbind(X, lags)
		names(X.new) <- c(paste('var.', 1:ncol(X), sep = ''), names(lags))
	} else {
		X.new <- lags
	}

	if (na.omit){
		X.new <- na.omit(X.new)
		X.new <- as.zoo(X.new)
	}
	
	return(X.new)
}

