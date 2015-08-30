full_dma <- function(f.trim, y.trim, lambda, alpha, kappa){

  f.trim <- fs.trim[[1]]
  y.trim <- ys.trim[[1]]
  
  # add an intercept
  intercept <- zoo(1, order.by = index(f.trim))
  X <- cbind(intercept, f.trim)
  
	# dimensions
	m <- ncol(X); T <- nrow(X)
	K <- (2^m)-1 # we do not consider the model with 0 variables

	## priors
	prob_init <- rep(1/K, K) # uniform prior on models
	
	# DMA

	results <- compute_dma(y.trim, X, prob_init, theta_init, sigma_init, H_init, lambda, alpha, kappa)
	return(results)

}