# install FredR Package
library('devtools')
devtools::install_github(repo = "jcizel/FredR")

library(FredR)

library(RCurl)
library(pipeR)
#library(dplyr)
library(XML)

api.key = "c3bc005d1a70992b2b3f3ef97d896c32"
fred <- FredR(api.key)
str(fred,1)
gdp.series <- fred$series.search(text = "france")

text = "gdp"
search_type = "full_text" 
order_by = "popularity" 
limit = 100
query = "fred/series/search"
root = "http://api.stlouisfed.org/"
test  <- sprintf("%s/%s?search_text=%s&search_type=%s&order_by=%s&limit=%s&api_key=%s", root, query, text, search_type, order_by, limit, api.key)
l <- getURL(url = test%>>% URLencode) %>>% xmlTreeParse(useInternalNodes = TRUE)

setwd("/Users/nicolassaleille/Dropbox/ofpr/scripts/scripts_fred") # chemin perso
source("test.R")

xml2dt <- function(xmlObj){
  xmlApply(xmlRoot(xmlObj), xmlAttrs) %>>%
    list.map(. %>>% as.list %>>% as.data.table) %>>%
    rbindlist() ->
    dt        
  return(dt)
}

xml2dt(l)

gdp.series %>>%
  select(
    id,
    title,
    observation_start,
    observation_end,
    popularity
  ) %>>%
  arrange(
    desc(as.numeric(popularity))
  )

gdp <- fred$series.search.tags(series_search_text = 'cpi')  

fred$series.search("DEUURHARMQDSMEI")
