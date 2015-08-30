generateAR1 <- function(init, rho, sd, length.out){
	x <- init
	for (i in 1:length.out){
		x <- c(x, rho * tail(x, 1) + rnorm(1, sd = sd))
	}
	return(as.matrix(x, ncols = 1))
}