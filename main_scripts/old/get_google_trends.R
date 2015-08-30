############################################
##    Query GoogleTrends from R
##
## by Christoph Riedl, Northeastern University
## Additional help and bug-fixing re cookies by
## Philippe Massicotte Université du Québec à Trois-Rivières (UQTR)
############################################


# Load required libraries
library(RCurl)  	# For getURL() and curl handler / cookie / google login
library(stringr)	# For str_trim() to trip whitespace from strings

# Google account settings
username <- "nicolas.saleille@gmail.com"
password <- "CCCPxp1447"

# URLs
loginURL 		<- "https://accounts.google.com/accounts/ServiceLogin"
authenticateURL <- "https://accounts.google.com/accounts/ServiceLoginAuth"
trendsURL 		<- "http://www.google.com/trends/TrendsRepport?"



############################################
## This gets the GALX cookie which we need to pass back with the login form
############################################
getGALX <- function(curl) {
  txt = basicTextGatherer()
  curlPerform( url=loginURL, curl=curl, writefunction=txt$update, header=TRUE, ssl.verifypeer=FALSE )
  
  tmp <- txt$value()
  
  val <- grep("Cookie: GALX", strsplit(tmp, "\n")[[1]], val = TRUE)
  strsplit(val, "[:=;]")[[1]][3]
  
  return( strsplit( val, "[:=;]")[[1]][3]) 
}


############################################
## Function to perform Google login and get cookies ready
############################################
gLogin <- function(username, password) {
  ch <- getCurlHandle()
  
  ans <- (curlSetOpt(curl = ch,
                     ssl.verifypeer = FALSE,
                     useragent = getOption('HTTPUserAgent', "R"),
                     timeout = 60,         
                     followlocation = TRUE,
                     cookiejar = "./cookies",
                     cookiefile = ""))
  
  galx <- getGALX(ch)
  authenticatePage <- postForm(authenticateURL, .params=list(Email=username, Passwd=password, GALX=galx, PersistentCookie="yes", continue="http://www.google.com/trends"), curl=ch)
  
  authenticatePage2 <- getURL("http://www.google.com", curl=ch)
  
  if(getCurlInfo(ch)$response.code == 200) {
    print("Google login successful!")
  } else {
    print("Google login failed!")
  }
  return(ch)
}


############################################
## Read data for a query
############################################
ch <- gLogin( username, password )
authenticatePage2 <- getURL("http://www.google.com", curl=ch)
res1 <- getForm(trendsURL, q="madonna", content=1, export=1, graph="all_csv", curl=ch)
res
# Check if quota limit reached
if( grepl( "You have reached your quota limit", res ) ) {
  stop( "Quota limit reached; You should wait a while and try again lateer" )
}

# Parse resonse and store in CSV
# We skip ther first 5 rows which contain the Google header; we then read 503 rows up to the current date
x <- try( read.table(text=res, sep=",", col.names=c("Week", "TrendsCount"), skip=32, nrows=513) )
test <- read.table(text=res, sep=",", col.names=c("Week", "TrendsCount"), skip=32, nrows=513)

library('GTrendsR')
## Not run: 
usr <- "nicolas.saleille@gmail.com"
psw <- "CCCPxp144"
ch <- gLonnect(usr, psw)  # or use env.var or options 
location <- "CA"
query <- "France"
data <- gtrends(ch, query = query)

plot(data)

## End(Not run)

