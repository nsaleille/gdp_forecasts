# DMA: Dynamic model averaging

rm(list = ls(all = TRUE))
#setwd('//paradis/eleves//NSALEILLE//Bureau//sauvegarde_ofpr//data')
setwd('~/Documents/ofpr')
source('./scripts/dynamic_model_averaging/predict_update.R')
source('./scripts/dynamic_model_averaging/compute_dma.R')
source('./scripts/dynamic_model_averaging/generateAR1.R')

# toy model
m <- 2 # ncol(X)
d <- 1 # ncol(y)
T <- 500 # nrow(X)
beta1 <- matrix(c(1,0), nrow = m, ncol = d)
beta2 <- matrix(c(0,10), nrow = m, ncol = d)

X <- sapply(rep(0,m), FUN = generateAR1, rho = 0.8, sd = 0.1, length.out = T)
colnames(X) <- paste('VAR', seq(1:m))

id1 <- 1:round(nrow(X)/2)
id2 <- (round(nrow(X)/2)+1):nrow(X)

# AR1 parameter
beta <- sapply(c(0,1), FUN = generateAR1, rho = 1, sd = 0.01, length.out = T)
plot(zoo(beta))

y <- c()
for (t in 1:nrow(X)){
  y <- rbind(y, X[t,] %*% beta[t,] + rnorm(d, sd = 0.01))
}
plot(y, type = 'l')

# parameters
K <- (2^m)-1 # we do not consider the model with 0 variables
lambda <- 0.99 # forgetting factor for the covariance
alpha <- 0.5 # forgetting factor for the model prior
kappa <- 0.99

# priors
prob_init <- rep(1/K, K) # uniform prior on models
theta_init <- 0 # only supports a scalar value for now
sigma_init <- 0.1
H_init <- 0.1

# dma
results <- compute_dma(y, X, prob_init, theta_init, sigma_init, H_init, lambda, alpha, kappa)
attributes(results)

#true.beta <- cbind(c(rep(1, length(id1)), rep(0, length(id2))), c(rep(0, length(id1)), rep(1, length(id2)))) %*% cbind(beta1, beta2)

#plot(zoo(results$parameters$model_12), main = '', ylab = c('\\beta_1','\\beta_2'), col = 'darkred', lwd = 2)

par(mfrow = c(2, 2), mar = c(5,5,2,2))
for (i in 1:2){
  plot(beta[,i], main = '', ylab = paste('\\beta_', i, sep = ''), 
       col = 'skyblue4', lwd = 2, type = 'l')
  lines(results$parameters$model_12[,i], col = 'darkred', lwd = 2)
}

plot(zoo(results$parameters$model_1), main = '', ylab = c('\\beta_1'), col = 'darkred', lwd = 2)
lines(beta[,1], col = 'skyblue4', lwd = 2)
plot(zoo(results$parameters$model_2), main = '', ylab = c('\\beta_2'), col = 'darkred', lwd = 2)
lines(beta[,2], col = 'skyblue4', lwd = 2)

library('zoo')
plot(zoo(results$prob), ylim = c(0,1), col = 'darkblue', lwd = 2, main = 'model probabilities')

library(extrafont)
loadfonts()

pdf (file = './dma_toy_ex_mod12.pdf', family ="CM Roman", width = 9, height = 6)
par(mfrow = c(1, 2), mar = c(5,5,2,2))
for (i in 1:2){
  plot(beta[,i], main = '', ylab = paste('\\beta_', i, sep = ''), 
       col = 'skyblue4', lwd = 2, type = 'l')
  lines(results$parameters$model_12[,i], col = 'darkred', lwd = 2)
}
dev.off ()

pdf (file = './dma_toy_ex_mod1.pdf', family ="CM Roman", width = 9, height = 6)
par(mfrow = c(1, 1), mar = c(5,5,2,2))
plot(zoo(results$parameters$model_1), main = '', ylab = c('\\beta_1'), col = 'darkred', lwd = 2)
lines(beta[,1], col = 'skyblue4', lwd = 2)
dev.off ()

pdf (file = './dma_toy_ex_mod2.pdf', family ="CM Roman", width = 9, height = 6)
par(mfrow = c(1, 1), mar = c(5,5,2,2))
plot(zoo(results$parameters$model_2), main = '', ylab = c('\\beta_2'), col = 'darkred', lwd = 2)
lines(beta[,2], col = 'skyblue4', lwd = 2)
dev.off ()

head(cbind(y, results$pred, round((y-results$pred) / y , digits = 2)))
errors <- (y- results$pred)^2
plot(errors[10:length(errors)], main = 'percentage forecast error')

#results2zoo <- function(results, index){
 # require('zoo')
  
#}