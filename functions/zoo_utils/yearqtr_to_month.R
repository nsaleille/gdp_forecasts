yearqtr_to_month <- function(ind){
  
  M1 <- c("Jan", "Apr", "Jul", "Oct")
  M2 <- c("Feb", "May", "Aug", "Nov")
  M3 <- c("Mar", "Jun", "Sep", "Dec")
  
  ids <- lapply(list(M1, M2, M3), ids_detect, ind = ind)
  ind.new <- c()
  for (i in 1:3){
    ind.new[which(ids[[i]])] <- paste('M', i, sep = '')
  }
  
  return(ind.new)
  
}