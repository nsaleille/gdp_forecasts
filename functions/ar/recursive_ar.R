recursive_ar <- function(order, ys, n.ahead){
  
  forecasts.ar <- list()
  for (i in 1:length(ys)){
    h <- (ys[[i]]$forecast.reference.period - index(na.omit(ys[[i]]$zoo))[length(na.omit(ys[[i]]$zoo))]) * 4
    ar.model <- ar(ys[[i]]$zoo, method = 'mle', na.action = na.omit, aic = FALSE, order.max = order)
    data <- ys[[i]][which(!(names(ys[[i]])%in%'zoo'))]
    preds <- predict(ar.model, newdata = na.omit(ys[[i]]$zoo), n.ahead = n.ahead)$pred
    data$pred <- preds[length(preds)]
    forecasts.ar[[i]] <- data
  }
  
  y.hat <- zoo(sapply(forecasts.ar, function(x) x$pred), 
               order.by = as.yearqtr(sapply(forecasts.ar, function(x) x$forecast.reference.period)))
  
  return(y.hat)
  
}