search_print <- function(pattern, desc = desc.full, info = info.full, out.path = NULL){
  
  index <- mgrepl(pattern, x = desc)
  matches <- data.frame(desc[index])
  rownames(matches) <- rownames(info[index,])
  
  if (!is.null(out.path)){
    
    require(xtable)
    print(xtable(matches), file = paste(out.path, paste(pattern, collapse = "_&_"), '_desc.tex', sep = ''), size = 'footnotesize')
    print(xtable(info[index,]), file = paste(out.path, paste(pattern, collapse = "_&_"), '_info.tex', sep = ''))
    print(paste('2 tables printed in folder', out.path))
    
  }
  
  print(list(desc = matches, info = info[index,]))
  return(list(desc = matches, info = info[index,]))
}