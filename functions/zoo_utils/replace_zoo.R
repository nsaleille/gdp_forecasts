replace_zoo <- function(series, zoo){
  
  series.new <- mapply(function(x,y){x$zoo <- y; return(x)}, series, zoo, SIMPLIFY = FALSE)
  return(series.new)
  
}