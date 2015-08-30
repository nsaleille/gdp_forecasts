mgrepl <- function(pattern, x){
  
  if (length(pattern)>1){
    
    index <- Reduce(`&`, lapply(pattern, FUN = grepl, x = x, ignore.case = TRUE))
    
  } else {
    
    index <- grepl(pattern = pattern, x = x, ignore.case = TRUE)
    
  }
  
  return(index)
}