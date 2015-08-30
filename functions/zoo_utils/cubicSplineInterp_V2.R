cubicSplineInterp <- function(series, by = 'month', maxgap = 3){

	# if (class(series) == 'zoo'){
	# 	series <- list(zoo = series)
	# }

	# if (by == 'mois'){

	# 	series$freq <- 'mois'
	# 	by <- 'month'

	# }

	# series.new <- appendZooIndex(series$zoo, by = by)
	series.appended <- appendZooIndex(series, by = by)
	series.new <- na.spline(series.appended, maxgap = maxgap, na.rm = FALSE)
	series.omit <- na.omit(series.new)

	print(paste(length(series.new) - length(series.omit), 'missing observations dropped while interpolating'))
	print(paste(length(length(series.omit) - series[which(!is.na(series))]), 'observations added through interpolation'))

	# series$zoo <- series.new
	# return(series)
	return(series.omit)

}