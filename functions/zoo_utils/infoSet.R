infoSet <- function(predictors, release){
  
  # suppress from the predictor matrix
  # observations that are not known due to release dates
  
  for (i in which(release != 0)){
    predictors[(nrow(predictors) - release[i] + 1):nrow(predictors), names(release[i])] <- rep(NA, release[i])
  }
  
  return(predictors)
}