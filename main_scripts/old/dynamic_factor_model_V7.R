################################################
##### Dynamic factor model #####################
################################################

rm(list=ls(all=TRUE))
library(R.utils)
setwd("/Users/nsaleille/Dropbox/ofpr/scripts") # chemin perso
load('./predictors.RData')
load('./outcome.RData')
load('./release.RData')
load('./full_description_of_series.RData')
sourceDirectory('./fonctions/zoo_utils', modifiedOnly = FALSE)
sourceDirectory('./fonctions/extract_data', modifiedOnly = FALSE)

start(predictors); end(predictors); nrow(predictors); ncol(predictors)
search.full$info

# centered / reduced variables
round(apply(predictors, 2,mean), 0)
round(apply(predictors, 2,sd), 0)

# create information sets
release
predictors

########### ETAPE 1 ##################################
########### create information sets  #################
######################################################

y <- window(outcome, start = start(predictors), end = end(predictors))
X <- predictors
tail(X)
tail(infoSet(X, release))
release

lapply

infoSet <- function(predictors, release){
  
  # suppress from the predictor matrix
  # observations that are not known due to release dates
  
  for (i in which(release != 0)){
    predictors[(nrow(predictors) - release[i] + 1):nrow(predictors), names(release[i])] <- rep(NA, release[i])
  }
  
  return(predictors)
}



mapply(FUN = function(x,y,z){x[(nrow(x) - y):nrow(x), z] <- rep(NA, y)}, 
       X[,which(release != 0)], release[which(release != 0)], 1:ncol(X[,which(release != 0)]))

T <- dim(X)[1]; N <- dim(X)[2]; d <- 1 # dimension de y

k <- round(0.5 * length(y), 0)
X <- lapply(k:nrow(predictors), function(x) predictors[1:x, ])
X <- lapply(X, infoSet, release)
sapply(X[[34]], function(x) sum(is.na(x)))


X <- mapply(FUN = function(x, y) {x[(nrow(x) - y):nrow(x), ] <- rep(NA, y); return(x)}, X, release)

X[[1]]


lapply(1:ncol(X), function(x) c(X[1:(nrow(X) - release[x]), x], rep(NA, release[x]))
matrix(dim(X[[1]]))
lapply(release, function(x) X[[]] )

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

ys <- lapply(k:length(y.trim), FUN = function(i) y.trim[1:i])
ends <- lapply(ys, FUN = function(x) end(x) + 0.25 * h)
Xs <- lapply(ends, FUN = function(x) window(f.trim, start = start(y.trim), end = x))
y.hat <- zoo(unlist(forecasts['prediction',]), order.by = as.yearqtr(unlist(forecasts['index',])))

y.test <- window(y.trim, start = start(y.hat), end = end(y.hat))
RMSE(y.hat, y.test)
plot(y.trim)
lines(y.hat, col = 'red')

# problème dans les dates
# il faut prendre en compte les délais de publication
# cf calendrier des prévisions 

# les facteurs sont trops informatifs car l'ensemble d'information est trop grand

# 1. sélectionner les observations qui doivent rentrer dans le bloc prédicteur compte tenu
# de la date où on se trouve et du délais de prévision. Les variables qui ne sont pas encore disponibles
# a l'instant T ne peuvent pas entrer dans la construction des facteurs.

# 2. construire les facteurs en incluant seulement les données disponibles dans l'ensemble d'info

# 3. estimer le modèle

# Comment procéder ? Partir de la matrice des prédicteurs a posteriori, i.e. avec toutes les 
# observations associées à la bonne date; puis pour chaque date, remplacer par des NAs tout 
# ce qui est inconnu en date t en se basant sur les délais de publication
# i.e un ensemble d'information pour chaque date