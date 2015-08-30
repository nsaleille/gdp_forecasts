plotGrowth <- function(y, x, col, ylab = 'GDP growth rate', file = NULL){
  
  par(mar = c(3,5,2,2), mfrow = c(1, 1))
  plot(y, col = 'darkred', type = 'l', ylab = ylab, xlab = '', ylim = c(-2, 2), lwd = 1.5)
  mapply(lines, x, col = col, MoreArgs = list(type = 'l', lwd = 2))
  if (length(x)>1){
  	legend('bottomleft', legend = names(x), col = col, lwd = 2)
  }

  if (!is.null(file)){
	pdf (file = file, width = 9, height = 6)
  plot(y, col = 'darkred', type = 'l', ylab = ylab, xlab = '', ylim = c(-2, 2), lwd = 1.5)
  mapply(lines, x, col = col, MoreArgs = list(type = 'l', lwd = 2))
  if (length(x)>1){
    legend('bottomleft', legend = names(x), col = col, lwd = 2)
  }	
  dev.off ()
  }
  
}