plotCols <- function(zoo1, zoo2, max.plots = 3){
  q <- (max.plots < ncol(zoo1)) * max.plots + (1 - (max.plots < ncol(zoo1))) * ncol(zoo1)
  par(mfrow = c(q, 1), mar = c(3,5,2,2))
  for (i in 1:q){
    plot(merge(zoo1[,i]), col = 'darkblue')
    lines(zoo2[,i], col = 'darkred')
  }
}
