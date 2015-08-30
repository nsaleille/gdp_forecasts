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

########### ETAPE 1 ##################################
########### estimation des facteurs par ACP ##########
######################################################

# 1. estimation préliminaire des facteurs par ACP

y <- outcome; X <- predictors
T <- dim(X)[1]; N <- dim(X)[2]; d <- 1 # dimension de y

# prenons les 5 premières composantes principales (74% de la variance expliquée)
# il faudrait dans un second temps implémenter les tests de Bai et Ng (2007)

q <- 5 # nombre de facteurs (ici imposé sans test !)

# TEST 1 : PCA avec prcomp
# ! l'acp dans prcomp est faite à partir de la décomposition de la matrice des données
# et non à partir des vp de la matrice de covariance.

pca <- prcomp(X)
summary(pca); dim(pca$rotation)
f.hat.prcomp <- pca$x[,1:q]

# TEST 2: pca à la main (cf doz 2011)
# à l'aide des vp de la matrice de covariance
# on trouve une rotation différente mais avec certain facteurs
# qui semblent très corrélés
# bizarre les variables transformées ne sont pas centrées.

S <- (1/T) * Reduce("+", lapply(as.data.frame(t(X)), function(x){x%*%t(x)}))
D <- diag(eigen(S)$values[1:q]) # (q x q)
P <- eigen(S)$vectors[, 1:q] # (N x q)
f.hat <- t(solve(D)^{1/2} %*% t(P) %*% t(X))
f.hat <- zoo(f.hat, order.by = index(X))

# essayer de retrouver les résultats de la fonction PCA
plot(zoo(f.hat), type = 'l')
plot(zoo(f.hat.prcomp))

plot(zoo(cbind(f.hat.prcomp[,1], f.hat[,3])))
plot(zoo(cbind(f.hat.prcomp[,2], f.hat[,5])))
plot(f.hat[,4], type = 'l')

# 2. estimation des paramètres du modèle
# en version espace-etat

### Estimation of matrix A
# the parameter of the VAR applied on factors

library(vars)
p <- VARselect(as.data.frame(f.hat), lag.max = 15, type="const")$selection['AIC(n)']
f.hat.var <- VAR(as.data.frame(f.hat), p = p, type = "none")
A.hat <- t(sapply(f.hat.var$varresult, function(x) coefficients(x)))
A.hat <- rbind(A.hat, cbind(diag(q*p - q), matrix(0, q*p - q, q * p - q)))

### Estimation of lambda
# OLS on the outcome using estimated factors
# theorically we have a closed formula when there is only one lag
# lambda.0 <- P %*% D^{1/2}

measure.equation <- lm(y ~ -1 + ., data = merge(y, f.hat))
summary(measure.equation)
lambda.hat <- coef(measure.equation)
lambda.hat <- matrix(c(lambda.hat, rep(0, p * q - q)), nrow = 1)

# matrice F des facteurs incluant les lags
# require(fBasics)
F <- bindLags(f.hat, p-1)
head(F)

# covariance de l'équation de transition
sigma.xi.hat <- cov(residuals(f.hat.var))
# la matrice B est donnée par le modèle
B <- rbind(cbind(chol(sigma.xi.hat) * diag(q), matrix(0, p*q-q, q)), matrix(0, p*q-q, p * q))

# covariance de l'équation de mesure
phi <- is.na(y) * 10e10
phi[which(phi == 0)] <- sd(residuals(measure.equation))^2

library(FKF)
y.kalman <- fkf(
    a0 = as.vector(F[1,]), 
    P0 = diag(q * p), 
    dt = matrix(0, q * p, 1), 
    ct = matrix(0),
    Tt = array(A.hat, dim = c(dim(A.hat), 1)),
    Zt = array(lambda.hat, dim = c(dim(lambda.hat), 1)),
    HHt = array(B, dim = c(dim(B), 1)),
    GGt = array(as.matrix(phi), dim = c(d, d, length(phi))),
    yt = t(as.matrix(y))
    )

f.kalman <- zoo(t(y.kalman$att), order.by = index(X))[,1:q]
plot(merge(f.hat, f.kalman[,1:5]))
for (i in 1:q){
  plot(f.hat[,i], type = 'l', col = 'blue')
  lines(f.kalman[,i], type = 'l', col = 'red')
}

## Trimestrialisation des facteurs
f.trim <- aggregate(f.kalman, by = as.yearqtr, FUN = mean)

# prevision approche mco
y.hat <- recForecasts(y.trim, f.trim, h = 4, k = 30, olsForecast)
y.test <- window(y.trim, start = start(y.hat), end = end(y.hat))
RMSE(y.hat, y.test)

plot(y.trim)
lines(y.hat[,1], col = 'red')

#prevision approche var
#varForecast(head(y.trim, length(y.trim) - 3), f.trim)
#recForecasts(y, X, h = 1, k = 30, varForecast)