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

load('../data/data_clean/predictors.RData')
load('../data/data_clean/outcome.RData')
load('../data/data_clean/release.RData')
load('../data/factors_estimates/factors.RData')

## STEP 1: model parameters

# we select the number of static factors looking at the adj R^2
# of the forecast regression with h = 1

# test with q = 11 and s = 13
# these are the selected orders with h = 0
# may provide better results than previsouly plotted

q <- 8 # number of static factors
s <- 10 # number of dynamic factors

## STEP 2 : Recursive forecasts

dfm.forecasts <- list()
for (h in 1:8){
  print(paste('forecasts', h, 'months before official GDP release date'))
  sets <- build_training_sets(outcome, predictors, release, prop.train = 0.5, n.steps.ahead = h)
  dfm.forecasts[paste('n.ahead =', h)] <- recursive_DFM(sets$outcome, fs.trim[[h]])
}

save(dfm.forecasts, file = '../data/forecasts/dfm_forecasts.RData')

## Step 3 : analyse results

load('../data/forecasts/dfm_forecasts.RData')
dfm.rmse <- lapply(dfm.forecasts, RMSE, y.test = outcome)
dfm.rmse

save(dfm.rmse, file = '../data/rmse/dfm_model.RData')
