# download data from BdF

library(RCurl)
library(XML)
u = "https://en.wikipedia.org/wiki/List_of_countries_by_population"
xData <- getURL(u)
tables = readHTMLTable(xData, trim = TRUE, as.data.frame = TRUE)
names(tables)
tables[[2]]

v = "http://webstat.banque-france.fr/en/browse.do?node=5384244"
xData <- getURL(v)
tables = readHTMLTable(xData, trim = TRUE, as.data.frame = TRUE)

library(RSelenium)
startServer()
# use default server initialisation values
remDr <- remoteDriver$new()
# send request to server to initialise session
remDr$open()
# navigate to R home page
remDr$navigate("http://www.r-project.org")


setwd('~/Documents/ofpr/')

rm(list = ls())
data <- read.fwf('./scripts/data/bdf/manufacturing_industry_whole_sector.csv',
                 header = TRUE)
names <- 



