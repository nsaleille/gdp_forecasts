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

########### STEP 0 ###################################
########### Create Information Sets  #################
######################################################



## STEP 1: model parameters

# we select the number of static factors looking at the adj R^2
# of the forecast regression with h = 1

# test with q = 11 and s = 13
# these are the selected orders with h = 0
# may provide better results than previsouly plotted

q <- 8 # number of static factors
s <- 10 # number of dynamic factors

## STEP 2 : Recursive forecasts

horizons <- c(1, 3, 6)

results.dfm <- list()
for (h in horizons){
  sets <- build_training_sets(outcome, predictors, prop.train = 0.5, n.steps.ahead = h)
  Xs <- sets$predictors; ys <- sets$outcome
  results.dfm[paste('n.ahead =', h)] <- recursive_DFM(ys, Xs, q, s)
}


  
