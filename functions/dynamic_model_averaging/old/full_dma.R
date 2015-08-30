full_dma <- function(f.trim, y.trim, lambda, alpha, kappa){

  f.trim <- fs.trim[[1]]
  y.trim <- ys.trim[[1]]
  
	# dimensions
	m <- ncol(f.trim)
	T <- nrow(f.trim)
	K <- (2^m)-1 # we do not consider the model with 0 variables

	## priors
	
	# estimation de la condition initiale à l'aide d'OLS simple
	# il semble important de mettre une constante dans le modèle (signif >0)
	# le modèle est bien meilleur en terme d'explication de la variance
	# les coefficients bougent peu mais sont moins bien estimés (biais imposé)

	intercept <- zoo(1, order.by = index(f.trim))
	X <- cbind(intercept, f.trim)
	reg.ols <- lm(y.trim ~ -1 + ., data = merge(y.trim, X))

	theta_init <- coefficients(reg.ols)
	sigma_init <- solve(t(X)%*%X) * (1 / (T - m - 1)) * sum(residuals(reg.ols)^2)
	#sigma_init <- (1/T) * residuals(reg.ols) %*% t(residuals(reg.ols))
	prob_init <- rep(1/K, K) # uniform prior on models
	sigma_init <- 1 # initial variance of theta
	H_init <- var(residuals(reg.ols)) # initial variance of espsilon; take the OLS estimator

	# DMA

	results <- compute_dma(y.trim, X, prob_init, theta_init, sigma_init, H_init, lambda, alpha, kappa)
	return(results)

}