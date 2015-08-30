yahoo_csv_to_zoo <- function(file){
	raw <- read.csv(file)
	name <- tail(unlist(str_split(file, "/")), 1)
	serie <- list(zoo = zoo(raw[,'Close'], order.by = as.yearmon(raw[,1])),
              name = name, id = name, freq = 'mois', filepath = file, source = 'yahoo', release = 0)
	return(list(serie))
}