######################################################
########### RMSE results  ############################
######################################################

# Gather RMSE of every models to construct tex table

rm(list=ls(all=TRUE))
setwd(dir = '/Users/nsaleille/Dropbox/ofpr/data')

load('./rmse/ar_model.RData')
load('./rmse/random_walk.RData')
load('./rmse/dfm_model.RData')

results <- list(rmse.rw, rmse.ar, rmse.dfm, rmse.dma, rmse.dms)
rmse <- sapply(results, function(x) x$rmse)
colnames(rmse) <- sapply(results, function(x) x$type)
rmse

library(xtable)

cap <- 'Recursive forecast exercise - RMSE from various models'
label <- 'table:rmse'
align <- rep('c', (ncol(rmse) + 1))
rmse.table <- xtable(rmse, caption = cap, label = label, align = align)
print(rmse.table, file = '../tex/tables/rmse.tex')

