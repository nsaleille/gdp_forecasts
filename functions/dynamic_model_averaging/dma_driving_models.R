dma_driving_models <- function(posterior.probs, level, plot = TRUE, file = NULL){
  
  driving.models <- sapply(na.omit(posterior.probs), function(x) {if (sum(x>level)>0) return(x)})
  driving.models <- driving.models[!(sapply(driving.models, is.null))]
  
  if (is.null(driving.models)){
    print(paste('No posterior probabilities above level', level))
    break
  }
  
  driving.names <- names(driving.models)
  driving.models <- Reduce(function(x,y) merge(x,y), driving.models)
  names(driving.models) <- driving.names
  
  if (plot){
    
    par(mfrow = c(1,1))
    plot(driving.models, col = rainbow(ncol(driving.models)), plot.type = 'single', lwd = 2)
    legend('topleft', legend = names(driving.models), col = rainbow(ncol(driving.models)), 
           lty = 1, lwd = 2)
    
  }
  
  if (plot & !is.null(file)){
    pdf (file = file, width = 9, height = 6)
    plot(driving.models, col = rainbow(ncol(driving.models)), plot.type = 'single', lwd = 2)
    legend('topleft', legend = names(driving.models), col = rainbow(ncol(driving.models)), 
           lty = 1, lwd = 2)
    dev.off ()
  }
  
}