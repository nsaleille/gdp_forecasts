cubicSplineInterp <- function(series, by = 'month', zoo = TRUE){

	if (zoo == FALSE){
		series <- series$zoo
	}

  series.new <- appendZooIndex(series, by = by)
  series.new <- na.spline(series.new)

	if (zoo == FALSE){
		series$zoo <- series.new
		series$freq <- 'mois'
  		return(series)
	}
	else {
		return(series.new)
	}

}