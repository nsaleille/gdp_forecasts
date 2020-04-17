#############################################################################
##### Create information sets for recursive forecasting #####################
#############################################################################

rm(list=ls(all=TRUE))
setwd("/Users/nsaleille/Dropbox/ofpr/scripts") # chemin perso

########### STEP 1 ###################################
########### load dataset  ############################
######################################################

library(R.utils)
sourceDirectory('./functions/zoo_utils', modifiedOnly = FALSE)
sourceDirectory('./functions/extract_data', modifiedOnly = FALSE)
sourceDirectory('./functions/dynamic_factor_model', modifiedOnly = FALSE)

load('../data/data_clean/predictors.RData')
load('../data/data_clean/outcome.RData')
load('../data/data_clean/release.RData')
load('../data/factors_estimates/factors_monthly.RData')

########### STEP 2 ###################################
########### Recursive DFM ############################
######################################################

dfm.forecasts <- list()
for (h in 1:8){
  print(paste('forecasts', h, 'months before official GDP release date'))
  sets <- build_training_sets(outcome, predictors, release, prop.train = 0.5, n.steps.ahead = h)
  fs <- lapply(fs.trim[[h]], function(x) {x$zoo <- na.omit(zoo_month_to_trim(x$zoo)); return(x)})
  dfm.forecasts[paste('n.ahead =', h)] <- recursive_DFM(sets$outcome, fs)
}

save(dfm.forecasts, file = '../data/forecasts/dfm_forecasts.RData')

## Step 3 : analyse results

load('../data/forecasts/dfm_forecasts.RData')
rmse.dfm <- sapply(dfm.forecasts, RMSE, y.test = outcome)
names(rmse.dfm) <- paste('h =', 1:8)
rmse.dfm <- list(rmse = rmse.dfm, type = 'dfm forecasts')
save(rmse.dfm, file = '../data/rmse/dfm_model.RData')
