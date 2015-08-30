plot_search <- function(search){
  series <- get_search_results(search, zoo.out = FALSE)
  for (i in series){
    plot(i$zoo, ylab = i$id)
  }
}