#############################################################################
##### Create information sets for recursive forecasting #####################
#############################################################################

rm(list=ls(all=TRUE))
setwd("/Users/nsaleille/Dropbox/ofpr/scripts") # chemin perso

########### STEP 0 ###################################
########### load dataset  ############################
######################################################

library(R.utils)
sourceDirectory('./functions/zoo_utils', modifiedOnly = FALSE)
sourceDirectory('./functions/extract_data', modifiedOnly = FALSE)
sourceDirectory('./functions/dynamic_factor_model', modifiedOnly = FALSE)

load('../data_clean/predictors.RData')
load('../data_clean/outcome.RData')
load('../data_clean/release.RData')

########### STEP 1 ###################################
########### Create Information Sets  #################
######################################################

library(zoo)

# size of the initial training set
k <- round(0.5 * nrow(predictors), 0) 

# filter information in predictors not known due to release dates
Xs <- lapply(k:nrow(predictors), function(x) predictors[1:x, ])
Xs <- lapply(Xs, infoSet, release)
estimation.dates <- as.yearmon(sapply(Xs, end))
names(Xs) <- estimation.dates

## filter known values of the growth rate
# GDP in T is known only at T+2M
y <- appendZooIndex(outcome, by = 'month')
y <- window(y, start = start(predictors), end = end(predictors))
ys <- lapply(k:nrow(predictors), function(x) y[1:x, ])
names(ys) <- estimation.dates

# GDP in T is known only at the end of T+1
# if we are in M3 of T, then T-1 known
# otherwise only T-2
current.quarter <- lapply(ys, function(x) as.yearqtr(index(x)) == tail(as.yearqtr(index(x)), 1))
ys[which(lapply(current.quarter, sum) < 3)] <- lapply(ys[which(lapply(current.quarter, sum) < 3)], 
             function(y) {y[(length(y) - 1):length(y)] <- NA; return(y)})

# information set for quarterly GDP growth serie
# depending on the month we are in we know
# either the GDP in T-1 or in T-2
ys.trim <- lapply(ys, na.locf, fromLast = FALSE)
last.obs <- lapply(ys, function(x) tail(which(!is.na(x)), 1))
ys.trim <- mapply(FUN = function(x, y) {x[which(index(x)>=index(x)[unlist(y)])] <- NA; return(x)}, ys.trim, last.obs)
ys.trim <- lapply(ys.trim, aggregate, as.yearqtr, mean)

information_sets <- list(ys = ys, Xs = Xs, ys.trim = ys.trim)
save(information_sets, file ='./dataset/information_sets.RData')
