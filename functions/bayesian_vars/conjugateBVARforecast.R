conjugateBVARforecast <- function(Y, X, forecast.cov = FALSE){

	bvar.model <- conjugateBVAR(Y, X)
	# one step ahead predictions
	y.hat.mean <- tail(bvar.model$X, 1) %*% bvar.model$A_mean_post
	z <- tail(bvar.model$X, 1) %*% bvar.model$V_post %*% t(tail(bvar.model$X, 1))
	y.hat.var <- (1 / (bvar.model$nu_post + 2)) * (1 + as.numeric(z)) * bvar.model$S_post

	if (forecast.var){
		return(list(mean = y.hat.mean, cov = y.hat.var))
	} else{
		return(y.hat.mean)	
	}
}