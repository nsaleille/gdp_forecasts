constructSeriesId <- function(names, sep = ' '){

	paste(sapply(strsplit(names, sep), substr, 1, 1), sep = '', collapse = '')
}