get_search_results <- function(search, zoo.out = TRUE){
  
  paths <- unique(search$info[,'path'])
  paths <- sapply(paths, as.character)

  data <- list()
  for (i in paths){

    if (grepl(pattern = 'insee', x = i)){
      series <- extractData(i, insee_xls_to_zoo)
    }

    if (grepl(pattern = 'eurostat', x = i)){
      series <- extractData(i, eurostat_xls_to_zoo)
    }

    if (grepl(pattern = 'bdf', x = i)){
      series <- extractData(i, bdf_xls_to_zoo)
    }

    if (grepl(pattern = 'ocde', x = i)){
      series <- extractData(i, ocde_xls_to_zoo)
    }

    if (grepl(pattern = 'fred', x = i)){
      series <- extractData(i, fred_csv_to_zoo)
    }

    data <- append(data, series)
  }

  series.id <- sapply(data, FUN = function(x) x$id)
  index <- mgrepl(paste(rownames(search$info), collapse = '|'), series.id)
  
  if (zoo.out == TRUE){
    return(lapply(data[index], FUN = function(x) x$zoo))
  } else {
    return(data[index])
  }
  
}