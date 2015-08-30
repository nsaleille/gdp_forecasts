search_apply <- function(pattern, data, desc, info, FUN, ...){

	search <- search_print(pattern = pattern, desc, info)
	ids.full <- sapply(data, function(x) x$id)
	ids.search <- which(ids.full %in% rownames(search$info))
	data[ids.search] <- lapply(ids.search, FUN, ...)

	return(data)

}