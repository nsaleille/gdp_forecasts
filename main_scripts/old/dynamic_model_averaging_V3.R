######################################################
###########DMA: Dynamic model averaging###############
######################################################

rm(list=ls(all=TRUE))
library(R.utils)
setwd("/Users/nsaleille/Dropbox/ofpr/scripts") # chemin perso

########### STEP 0 ###################################
########### load dataset  ############################
######################################################

library(R.utils)
sourceDirectory('./fonctions/zoo_utils', modifiedOnly = FALSE)
sourceDirectory('./fonctions/extract_data', modifiedOnly = FALSE)
sourceDirectory('./fonctions/dynamic_model_averaging', modifiedOnly = FALSE, recursive = FALSE)

load('./factors.RData') # use dynamic factors as predictors
load('./outcome.RData')
load('./release.RData')
load('./information_sets.RData')

########### STEP 1 ###################################
########### Model Parameters  ########################
######################################################

# select only q factors as predictors
# increasing the number of factors slightly changes the selected models
# probabilities have a comparable shape
m <- 6

# parameters
K <- (2^m)-1 # we do not consider the model with 0 variables
lambda <- 0.99 # forgetting factor for the covariance
alpha <- 0.99 # forgetting factor for the model prior
kappa <- 0.99

########### STEP 2 ###################################
########### Priors  ##################################
######################################################

ys.trim <- information_sets$ys.trim
# f.trim <- lapply(f.kalman, function(x) x[, 1:m])
f.trim <- lapply(f.kalman, aggregate, by = as.yearqtr, tail, 1)
names(f.trim) <- as.yearmon(estimation.dates)

# estimation de la condition initiale à l'aide d'OLS simples
# il semble important de mettre une constante dans le modèle (signif >0)
# le modèle est bien meilleur en terme d'explication de la variance
# les coefficients bougent peu mais sont moins bien estimés (biais imposé)

results <- full_dma(f.trim, y.trim, lambda, alpha, kappa)


f.init <- f.trim[[1]]
intercept <- zoo(1, order.by = index(f.trim))
X <- cbind(intercept, f.trim)
reg.ols <- lm(y ~ -1 + X)
summary(reg.ols)
#reg.2sls <- lm(y ~ -1 + X, weights = sigma_init)

theta_init <- coefficients(reg.ols)
sigma_init <- (1/T) * residuals(reg.ols) %*% t(residuals(reg.ols))
prob_init <- rep(1/K, K) # uniform prior on models
sigma_init <- 1 # initial variance of theta
H_init <- var(residuals(reg.ols)) # initial variance of espsilon; take the OLS estimator

########### STEP 3 ###################################
########### Recursive forecasts  #####################
######################################################


# dma
results <- compute_dma(y, X, prob_init, theta_init, sigma_init, H_init, lambda, alpha, kappa)
posterior.probs <- zoo(results$prob, order.by = index(X))
y.hat <- zoo(results$pred, order.by = index(X))
#plot(posterior.probs)

plotGrowth(y, y.hat)
RMSE(coredata(y), coredata(y.hat))

# models with high probabilities at least at one point in time
# the intercept and 2nd factors never appears !!!!

driving.models <- sapply(posterior.probs, function(x) {if (sum(x>0.4)>0) return(x)})
driving.models <- driving.models[!(sapply(driving.models, is.null))]
driving.names <- names(driving.models)
driving.models <- Reduce(function(x,y) merge(x,y), driving.models)
names(driving.models) <- driving.names
par(mfrow = c(1,1))
plot(driving.models, col = rainbow(ncol(driving.models)), plot.type = 'single', lwd = 2)
legend('bottomleft', legend = names(driving.models), col = rainbow(ncol(driving.models)), 
       lty = 1, lwd = 2)

posterior.theta <- results$parameters[driving.names]
par(mfrow = c(ncol(driving.models), 1))
lapply(posterior.theta, plot, plot.type = 'single', lwd = 2)
