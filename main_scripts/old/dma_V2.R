# DMA: Dynamic model averaging

rm(list = ls(all = TRUE))
#setwd('//paradis/eleves//NSALEILLE//Bureau//sauvegarde_ofpr//data')
setwd('/Users/nicolassaleille/Dropbox/ofpr')
source('./scripts/dynamic_model_averaging/predict_update.R')
source('./scripts/dynamic_model_averaging/compute_dma.R')

# toy model
m <- 4
d <- 1
T <- 100
beta <- matrix(2, nrow = m, ncol = d)
X <- matrix(data = rnorm(m*T), nrow = T, ncol = m)
#colnames(X) <- past('VAR', seq(1:m))
y <- X%*%beta + matrix(rnorm(d*T), ncol = d)
K <- (2^m)-1 # we do not consider the model with 0 variables

# parameters
lambda <- 0.99 # forgetting factor
alpha <- 0.99 # forgetting factor
kappa <- 0.98

# priors
prob_init <- rep(1/K, K)
theta_init <- 0 # only supports a scalar value for now
sigma_init <- 0.1
H_init <- 0.1

# dma
results <- compute_dma(y, X, prob_init, theta_init, sigma_init, H_init, lambda, alpha, kappa)
attributes(results)
results$parameters

model <- models[[1]]
prob_last <- prob[t-1,]
vars <- X
y_obs <- y

# select model
y_obs <- y_obs[t,]
X <- vars[t,model$vars]
theta_last <- theta[[model$name]][t-1,]
sigma_last <- diag(length(model$vars)) # !!!!!!

# predict for one model / one step ahead

theta_pred <- theta_last # F: identité
sigma_pred <- (1/lambda) * sigma_last
prob_pred <- prob_last^alpha / sum(prob_last)
y_pred <- X %*% theta_pred

# Update for one model / one step ahead

error <- y_obs - y_pred
S <- sigma_pred
xSx <- t(X) %*% S %*% X
R <- (1/t)*(error^2 - xSx)
F_inv <- solve(R + xSx)
theta_up <- theta_pred +  S %*% X %*% F_inv %*%  (y_obs - X %*% theta_pred)
sigma_up <- S - S %*% X %*% F_inv %*% X %*% S
weight <- pnorm(q = y_obs, mean = X %*% theta_pred, sd = sqrt(R + xSx)) * prob_pred[model$name]