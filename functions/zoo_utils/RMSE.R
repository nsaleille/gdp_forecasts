RMSE <- function(y.hat, y.test){
  
  table <- merge(y.hat, y.test)
  errors <- na.omit(table[,1] - table[,2])
  rmse <- sqrt(1/nrow(table) * sum(errors^2))
  
  return(rmse)
  
}
