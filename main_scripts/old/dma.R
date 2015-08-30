# DMA: Dynamic model averaging

rm(list = ls(all = TRUE))
#setwd('//paradis/eleves//NSALEILLE//Bureau//sauvegarde_ofpr//data')
setwd('/Users/nicolassaleille/Dropbox/ofpr')
source('./scripts/dynamic_model_averaging/predict_update.R')

# toy model
m <- 4
d <- 2
T <- 100
beta <- matrix(1, nrow = m, ncol = d)
X <- matrix(data = rnorm(m*T), nrow = T, ncol = m)
#colnames(X) <- past('VAR', seq(1:m))
y <- X%*%beta + matrix(rnorm(d*T), ncol = d)
K <- 2^m

# parameters
lambda <- 0.99 # forgetting factor
alpha <- 0.99 # forgetting factor

# construction of the model space
models <- lapply(seq(1:m), FUN = combn, x = m, simplify = FALSE)
models <- unlist(models, recursive = FALSE)
models <- lapply(models, FUN = function(x){return(list(name = paste('model', paste(x, collapse=''), sep = '_'), vars = x))})
model_names <- sapply(models, FUN = function(x){return(x$name)})

# priors
prob0 <- rep(1/2^length(models), length(models))
theta0 <- 0 # only supports a scalar value for now

# for each model, we keep track of the estimated values in a list of matrix
# one matrix for each model
theta <- lapply(models, FUN = function(x){return(matrix(nrow = nrow(X), ncol = length(x$vars)))})
theta <- lapply(theta, FUN = function(x){x[1,] <- theta0; return(x)})
names(theta) <- model_names
sigma <- lapply(models, FUN = function(x){list()})
prob <- matrix(nrow = nrow(X), ncol = length(models))
colnames(prob) <- model_names
prob[1, ] <- prob0

t = 3
results <- lapply(models, FUN = predict_update, t = t, y_obs = y, vars = X, 
               theta = theta, sigma = sigma, prob_last = prob[t-1,], 
               lambda = lambda, alpha = alpha)

theta_up <- sapply(results, FUN = function(x){return(x$theta_up)})
theta <- mapply(FUN = function(x,y){x[t,] <- y; return(x)}, theta, theta_up)

test <- lapply(merge(theta, theta_up), FUN = function(x)
names(test) <- rep(model_names, 2)
attributes(test)
length(test)
lapply(list(theta, theta_up), FUN = function)

unlist(theta_up) # tapply ???
model_dims <- sapply(theta_up, FUN = length)

lapply(theta_up, FUN = function(x){theta[[models]][t,] <- theta_up})

theta[[model_name]][t,] <- theta_up
names(results) <- model_names

model <- models[[14]]
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
R <- (1/t)*(error%*%t(error)) # !!!!
F_inv <- solve(R + xSx)
theta_up <- theta_pred +  S %*% X %*% F_inv %*%  (y_obs - X %*% theta_pred)
sigma_up <- S - S %*% X %*% F_inv %*% X %*% S
weight <- pnorm(q = y_obs, mean = X %*% theta_pred, sd = sqrt(R + xSx)) * prob_pred[model$name]

