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



# select the factors as predictors
X <- aggregate(f.kalman, as.yearqtr, mean)
y <- aggregate(outcome, as.yearqtr, na.omit)
dim(y) <- c(length(y), 1)
m <- ncol(X); T <- nrow(X); d <- ncol(y)
plot(X)

# parameters
K <- (2^m)-1 # we do not consider the model with 0 variables
lambda <- 0.95 # forgetting factor for the covariance
alpha <- 0.95 # forgetting factor for the model prior
kappa <- 0.95

# priors
prob_init <- rep(1/K, K) # uniform prior on models
theta_init <- 0 # only supports a scalar value for now
sigma_init <- 0.1
H_init <- 0.1

# dma
results <- compute_dma(y, X, prob_init, theta_init, sigma_init, H_init, lambda, alpha, kappa)
attributes(results)

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