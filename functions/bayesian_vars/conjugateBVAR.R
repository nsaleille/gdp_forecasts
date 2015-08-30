conjugateBVAR <- function(Y, X){

	### LARGE BVAR estimation as in KOOP 2013
	### Using a natural conjugate prior

	require(fBasics)

	Y <- window(Y, start = start(X), end = end(X))
	T <- dim(X)[1]; K <- dim(X)[2]; M <- dim(Y)[2]

	## OLS estimators
	A_hat <- solve(t(X) %*% X) %*% (t(X) %*% Y)
	alpha <- vec(A_hat)
	sigma <- (1/(T*K)) * t(Y - X %*% A_hat) %*% (Y - X %*% A_hat)

	# Hyperparameters on alpha ~ N(alpha_mean_prior, SIGMA x V_prior)
	A_mean_prior <- matrix(0, K, M)
	alpha_mean_prior <- vec(A_mean_prior) # alpha_bar
	V_prior <- 10 * diag(K) # prior on 

	# Hyperparameters on inv(SIGMA) ~ W(nu_prior,inv(S_prior))
	nu_prior <- M
	S_prior <- diag(M)
	inv_S_prior <- solve(S_prior)

	## Parameters of the Multi-t marginal posterior of alpha

	V_post <- solve( solve(V_prior) + t(X) %*% X )
	A_mean_post <- V_post %*% (inv(V_prior) %*% A_mean_prior + t(X) %*% X %*% A_hat)
	alpha_mean_post <- vec(A_mean_post)

	S_post <- sigma + S_prior + t(A_hat) %*% t(X) %*% X %*% A_hat + 
	  t(A_mean_prior) %*% inv(V_prior) %*% A_mean_prior - 
	  t(A_mean_post) %*% (inv(V_prior) + t(X) %*% X) %*% A_mean_post

	nu_post <- T + nu_prior
	alpha_var_post <- (1 / (nu_post - M - 1)) * kronecker(S_post, V_post)

	return(list(X = X, A_mean_post = A_mean_post, S_post = S_post, nu_post = nu_post, V_post = V_post))
}