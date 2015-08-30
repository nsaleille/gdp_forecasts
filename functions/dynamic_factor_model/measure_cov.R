measure.cov <- function(xrow, e.diag){
	
  e.diag[which(is.na(xrow))] <- 10e10
  covmat <- diag(e.diag)
  
  return(covmat)
}
