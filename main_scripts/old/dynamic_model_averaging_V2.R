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

# select only 4 factors as predictors
f.kalman <- f.kalman[[144]][,1:4]
f.trim <- aggregate(f.kalman, as.yearqtr, mean)
y <- aggregate(outcome, as.yearqtr, na.omit)
y <- window(y, start = start(f.trim), end = end(f.trim))
plot(f.trim)

########### STEP 1 ###################################
########### Model Parameters  ########################
######################################################

# estimation de la condition initiale à l'aide d'OLS simples
# il semble important de mettre une constante dans le modèle (signif >0)
# le modèle est bien meilleur en terme d'explication de la variance
# les coefficients bougent peu mais sont moins bien estimés (biais imposé)
intercept <- zoo(1, order.by = index(f.trim))
X <- cbind(intercept, f.trim)
reg.ols <- lm(y ~ -1 + X)
summary(reg.ols)
theta_init <- coefficients(reg.ols)
sigma_init <- (1/T) * residuals(reg.ols) %*% t(residuals(reg.ols))
#reg.2sls <- lm(y ~ -1 + X, weights = sigma_init)

dim(y) <- c(length(y), 1)
m <- ncol(X); T <- nrow(X); d <- ncol(y)

# parameters
K <- (2^m)-1 # we do not consider the model with 0 variables
lambda <- 0.99 # forgetting factor for the covariance
alpha <- 0.99 # forgetting factor for the model prior
kappa <- 0.99

# priors
prob_init <- rep(1/K, K) # uniform prior on models

#theta_init <- 0 # only supports a scalar value for now
sigma_init <- 1 # initial variance of theta
H_init <- var(residuals(reg.ols)) # initial variance of espsilon; take the OLS estimator

########### STEP 2 ###################################
########### Recursive Forecasts  #####################
######################################################

# dma
results <- compute_dma(y, X, prob_init, theta_init, sigma_init, H_init, lambda, alpha, kappa)
#posterior.probs <- zoo(results$prob, order.by = index(X))
#y.hat <- zoo(results$pred, order.by = index(X))
#plot(posterior.probs)

plotGrowth(y, y.hat)
RMSE(coredata(y), coredata(y.hat))

# models with high probabilities at least at one point in time
# the 1st and 3rd factors never appears !!!!

driving.models <- sapply(posterior.probs, function(x) {if (sum(x>0.5)>0) return(x)})
driving.models <- driving.models[!(sapply(driving.models, is.null))]
driving.names <- names(driving.models)
driving.models <- Reduce(function(x,y) merge(x,y), driving.models)
names(driving.models) <- driving.names
plot(driving.models, col = rainbow(ncol(driving.models)), plot.type = 'single', lwd = 2)
names(driving.models)

posterior.theta <- results$parameters[driving.names]
par(mfrow = c(ncol(driving.models), 1))
lapply(posterior.theta, plot, plot.type = 'single', lwd = 2)





true.beta <- cbind(c(rep(1, length(id1)), rep(0, length(id2))), c(rep(0, length(id1)), rep(1, length(id2)))) %*% cbind(beta1, beta2)

#plot(zoo(results$parameters$model_12), main = '', ylab = c('\\beta_1','\\beta_2'), col = 'darkred', lwd = 2)

par(mfrow = c(2, 2), mar = c(5,5,2,2))
for (i in 1:2){
  plot(true.beta[,i], main = '', ylab = paste('\\beta_', i, sep = ''), 
       col = 'skyblue4', lwd = 2, type = 'l')
  lines(results$parameters$model_12[,i], col = 'darkred', lwd = 2)
}

plot(zoo(results$parameters$model_1), main = '', ylab = c('\\beta_1'), col = 'darkred', lwd = 2)
lines(true.beta[,1], col = 'skyblue4', lwd = 2)
plot(zoo(results$parameters$model_2), main = '', ylab = c('\\beta_2'), col = 'darkred', lwd = 2, ylim = c(0, 10))
lines(true.beta[,2], col = 'skyblue4', lwd = 2)

library('zoo')
plot(zoo(results$prob), ylim = c(0,1))

library(extrafont)
loadfonts()

pdf (file = './dma_toy_ex.pdf', family ="CM Roman", width = 9, height = 6)
par(mfrow = c(1, 1), mar = c(5,5,2,2))
plot(zoo(results$parameters$model_12))
plot(zoo(results$parameters$model_1))
plot(zoo(results$parameters$model_2))
dev.off ()




head(cbind(y, results$pred, round((y-results$pred) / y , digits = 2)))
errors <- (y- results$pred)^2
plot(errors[10:length(errors)], main = 'percentage forecast error')

#results2zoo <- function(results, index){
 # require('zoo')
  
#}