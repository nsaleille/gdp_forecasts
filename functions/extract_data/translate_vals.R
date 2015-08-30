translate_vals <- function(init, trans){
vals <- init
for (i in trans){
  vals <- str_replace_all(vals, i[1], i[2])
}
return(vals)
}