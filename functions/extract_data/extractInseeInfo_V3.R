extractInfo_null <- function(paths){
  
  series.info <- data.frame()
  series.desc <- c()
  ids <- c()

  for (i in paths){

    print(i)
    
    if (grepl(pattern = 'insee', x = i)){
      series <- insee_xls_to_zoo(i)
    }

    if (grepl(pattern = 'eurostat', x = i)){
      series <- eurostat_xls_to_zoo(i)
    }

    if (grepl(pattern = 'bdf', x = i)){
      series <- bdf_xls_to_zoo(i)
    }

    if (grepl(pattern = 'ocde', x = i)){
      series <- ocde_xls_to_zoo(i)
    }

    if (grepl(pattern = 'fred', x = i)){
      series <- fred_csv_to_zoo(i)
    }

    if (grepl(pattern = 'yahoo', x = i)){
      series <- yahoo_csv_to_zoo(i)
    }

    series_info_desc <- get_series_info_desc(series, i)
    series.info <- rbind(series.info, series_info_desc$info)
    series.desc <- c(series.desc, series_info_desc$desc)
    ids <- c(ids, unlist(series_info_desc$ids))
    
  }
  
  #colnames(series.info) <- c('first obs', 'last obs', 'freq', 'nb obs', 'file path')
  names(series.desc) <- rownames(series.info) <- ids
  return(list(info = series.info, desc = series.desc, ids = ids))
  
}