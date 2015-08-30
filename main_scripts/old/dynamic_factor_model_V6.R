################################################
##### Dynamic factor model #####################
################################################

rm(list=ls(all=TRUE))
library(R.utils)
setwd("/Users/nsaleille/Dropbox/ofpr/scripts") # chemin perso
load('./predictors.RData')
load('./outcome.RData')
load('./full_description_of_series.RData')
sourceDirectory('./fonctions/zoo_utils', modifiedOnly = FALSE)
sourceDirectory('./fonctions/extract_data', modifiedOnly = FALSE)

start(predictors); end(predictors); nrow(predictors); ncol(predictors)
search.full$info

# centered / reduced variables
round(apply(predictors, 2,mean), 0)
round(apply(predictors, 2,sd), 0)

########### ETAPE 1 ##################################
########### estimation des facteurs par ACP ##########
######################################################

# 1. estimation préliminaire des facteurs par ACP

y <- window(outcome, start = start(predictors), end = end(predictors))
X <- predictors
T <- dim(X)[1]; N <- dim(X)[2]; d <- 1 # dimension de y

########################################################
################  FACTOR ESTIMATES (Step 1)  ###########
########################################################

### PCA to estimate the factors (cf doz 2011)
# à l'aide des vp de la matrice de covariance
# on trouve une rotation différente mais avec certain facteurs
# qui semblent très corrélés
# bizarre les variables transformées ne sont pas centrées.

# prenons les 5 premières composantes principales (74% de la variance expliquée)
# il faudrait dans un second temps implémenter les tests de Bai et Ng (2007)
q <- 5 # nombre de facteurs (ici imposé sans test !)

S <- (1/T) * Reduce("+", lapply(as.data.frame(t(X)), function(x){x%*%t(x)}))
D <- diag(eigen(S)$values[1:q]) # (q x q)
P <- eigen(S)$vectors[, 1:q] # (N x q)
f.hat <- t(solve(D)^{1/2} %*% t(P) %*% t(X))
f.hat <- zoo(f.hat, order.by = index(X))
plot(f.hat)


########################################################
################ MODEL PARAMETER ESTIMATES  ############
########################################################

### Estimation of matrix A
# the parameter of the VAR applied on factors

library(vars)
p <- VARselect(as.data.frame(f.hat), lag.max = 15, type="const")$selection['AIC(n)']
f.hat.var <- VAR(as.data.frame(f.hat), p = p, type = "none")
A.hat <- t(sapply(f.hat.var$varresult, function(x) coefficients(x)))
A.hat <- rbind(A.hat, cbind(diag(q*p - q), matrix(0, q*p - q,  q)))

### Estimation of lambda
# OLS on the outcome using estimated factors
# theorically we have a closed formula when there is only one lag
# lambda.0 <- P %*% D^{1/2}

y.mens <- cubicSplineInterp(y)
measure.equation <- lm(y.mens ~ -1 + ., data = merge(y.mens, f.hat))
summary(measure.equation)
lambda.hat <- coef(measure.equation)
lambda.hat <- matrix(c(lambda.hat, rep(0, p * q - q)), nrow = 1) #????????

# matrice F des facteurs incluant les lags
# require(fBasics)
F <- bindLags(f.hat, p-1)
tail(F)

# covariance de l'équation de transition
sigma.xi.hat <- cov(residuals(f.hat.var))
# la matrice B est donnée par le modèle
B <- rbind(cbind(chol(sigma.xi.hat) * diag(q), matrix(0, q, p*q - q)), matrix(0, p*q-q, p * q))

########################################################
################  FACTOR ESTIMATES (Step 2)  ###########
########################################################

# Using the Kalman filter / smoother
# and the full information set

## covariance of the measure equation
# set to +\infty when the outcome is not observed
phi <- is.na(y) * 10e10
phi[which(phi == 0)] <- sd(residuals(measure.equation))^2

## Kalman filter to get better estimates of factors
# using the previously estimated parameters

library(FKF)
y.kalman <- fkf(
    a0 = as.vector(F[p,]), # ???? NA dans F
    P0 = diag(q * p), 
    dt = matrix(0, q * p, 1), 
    ct = matrix(0),
    Tt = array(A.hat, dim = c(dim(A.hat), 1)),
    Zt = array(lambda.hat, dim = c(dim(lambda.hat), 1)),
    HHt = array(B, dim = c(dim(B), 1)),
    GGt = array(as.matrix(phi), dim = c(d, d, length(phi))),
    yt = t(as.matrix(y.mens)) #???? Kalman fonctionne avec des NA dans y
    )

f.kalman <- zoo(t(y.kalman$att), order.by = index(X))
f.kalman <- f.kalman[,1:q] # remove f_{t-1} from what we call f.kalman

save(f.kalman, file = './factors.RData')

plot(merge(f.hat, f.kalman[,1:5]))
for (i in 1:q){
  plot(f.hat[,2], type = 'l', col = 'blue')
  lines(f.kalman[,1], type = 'l', col = 'red')
}

## Trimestrialisation des facteurs
f.trim <- aggregate(f.kalman, by = as.yearqtr, FUN = mean)

# prevision approche mco
y.trim <- na.omit(y); index(y.trim) <- as.yearqtr(index(y.trim))
#y.trim <- aggregate(y, as.yearqtr, FUN = na.omit)
plot(y.mens, type = 'l') # quarterly extrapolated to monthly
lines(y, type = 'p') # extrapolated index only (NAs)
lines(y.trim, type = 'p', col = 'red') # original ts

sourceDirectory('./fonctions/zoo_utils', modifiedOnly = FALSE)
sourceDirectory('./fonctions/extract_data', modifiedOnly = FALSE)

start(f.trim); start(y.trim)
end(f.trim); end(y.trim)
olsForecast(y.trim, f.trim, summary = TRUE)


k <- round(0.5 * length(y), 0)
h <- 1 # one step ahead forecast
window(y.trim, start = start(y.trim), end = end(y.trim[1:i]))

dh <- index(y.trim)[2:length(index(y.trim))] - index(y.trim)[1:(length(index(y.trim))-1)]
ys <- lapply(k:length(y.trim), FUN = function(i) y.trim[1:i])
ends <- lapply(ys, FUN = function(x) end(x) + 0.25 * h)
Xs <- lapply(ends, FUN = function(x) window(f.trim, start = start(y.trim), end = x))

forecasts <- mapply(FUN = olsForecast, ys, Xs)
unlist(forecasts, recursive = FALSE, use.names = TRUE)

y <- ys[[1]]; X <- Xs[[1]]

predict.index <- index(X)[which(!(index(X) %in% index(y)))]
factor.model <- lm(y ~ -1 + ., data = merge(y, X))
y.hat <- X[predict.index, ] %*% coefficients(factor.model)
y.hat <- zoo(y.hat, order.by = predict.index)

y.hat <- zoo(unlist(forecasts), order.by = as.yearqtr(unlist(ends)))
y.test <- window(y.trim, start = start(y.hat), end = end(y.hat))
RMSE(y.hat, y.test)


names(forecasts) <- paste('h =', 0:(ncol(forecasts) - 1))


y.hat <- recForecasts(y.trim, f.trim, h = 1, k = k, FUN = olsForecast)
y.test <- window(y.trim, start = start(y.hat), end = end(y.hat))
RMSE(y.hat, y.test)

plot(y.trim)
lines(y.hat, col = 'red')

#prevision approche var
#varForecast(head(y.trim, length(y.trim) - 3), f.trim)
#recForecasts(y, X, h = 1, k = 30, varForecast)