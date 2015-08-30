################################################
##### Dynamic factor model #####################
################################################

rm(list=ls(all=TRUE))
library(R.utils)
setwd("/Users/nsaleille/Dropbox/ofpr/scripts") # chemin perso
load('./full_set_of_series.RData')
load('./full_description_of_series.RData')
sourceDirectory('./fonctions/zoo_utils', modifiedOnly = FALSE)

data.mens <- data.full[sapply(data.full, FUN = function(x) (x$freq == 'mois'))]
data.trim <- data.full[sapply(data.full, FUN = function(x) (x$freq == 'Trimestre'))]
ids <- sapply(data.trim, FUN = function(x) x$id)
get_desc(ids)

# mensualisation des séries
# selection du \delta PIB, la variable expliquée
library(zoo)
data.trim.interp <- lapply(data.trim, cubicSplineInterp)

# données sphéricisées
x <- unlist(list(data.mens, data.trim.interp), recursive = FALSE)
xsp <- Reduce(function(x,y) merge(x,y), get_zoo(x))
colnames(xsp) <- sapply(x, function(x) x$id)
xsp <- na.omit(xsp)

dPIB <- get_series('X001688995')
#y <- cubicSplineInterp(dPIB)$zoo
#y <- na.omit(merge(xsp, y))$y
y.trim <- window(dPIB$zoo, start = as.yearqtr(start(xsp)), end = end(dPIB$zoo))
y <- appendZooIndex(y.trim)
y <- (y - mean(y.trim)) / sd(y.trim)
y.trim <- (y.trim - mean(y.trim)) / sd(y.trim)
y <- merge(xsp, y)$y

# stationnarisation puis centrage/réduction (ordre donné par SW10)
# on stationnarise avant de centrer / réduire (logique)

# test for stationarity
library(tseries)
tests <- lapply(xsp, adf.test, alternative = 'stationary')
tests.bool <- sapply(tests, function(x) (x$p.value > 0.5))
dxsp <- merge(xsp[ , tests.bool == FALSE], diff(xsp[ , tests.bool == TRUE]))
dxsp <- na.omit(dxsp)
tests <- lapply(dxsp, adf.test, alternative = 'stationary')
tests.bool <- sapply(tests, function(x) (x$p.value > 0.5))
# etrange on atteint pas la stationnarité en différenciant
# il faut peut être traiter ces séries autrement...

# variables centree - réduites
cdxsp <- apply2zooCols(dxsp, FUN = function(x) x - mean(x))
cdxsp <- apply2zooCols(dxsp, FUN = function(x) x / sd(x))


########### ETAPE 1 ##################################
########### estimation des facteurs par ACP ##########
######################################################

# 1. estimation préliminaire des facteurs par ACP

X <- cdxsp; T <- dim(X)[1]; N <- dim(X)[2]; d <- 1 # dimension de y

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
f.hat.doz <- t(solve(D)^{1/2} %*% t(P) %*% t(X))
f.hat.doz <- zoo(f.hat.doz, order.by = index(X))

# essayer de retrouver les résultats de la fonction PCA
plot(zoo(f.hat.doz), type = 'l')
plot(zoo(f.hat.prcomp))

plot(zoo(cbind(f.hat.prcomp[,1], f.hat.doz[,3])))
plot(zoo(cbind(f.hat.prcomp[,2], f.hat.doz[,5])))
plot(f.hat.doz[,4], type = 'l')

# 2. estimation des paramètres du modèle
# en version espace-etat

# matrice A : VAR sur les facteurs
library(vars)
p <- VARselect(as.data.frame(f.hat.doz), lag.max = 15, type="const")$selection['AIC(n)']
f.hat.var <- VAR(as.data.frame(f.hat.doz), p = p, type = "none")
A.hat <- t(sapply(f.hat.var$varresult, function(x) coefficients(x)))
A.hat <- rbind(A.hat, cbind(diag(q*p - q), matrix(0, q*p - q, q * p - q)))

measure.equation <- lm(y ~ -1 + ., data = merge(y, f.hat.doz))
summary(measure.equation)
lambda.hat <- coef(measure.equation)
lambda.hat <- matrix(c(lambda.hat, rep(0, p * q - q)), nrow = 1)

# matrice F des facteurs incluant les lags
library(fBasics)
F <- Reduce(cbind, lapply(0:(p-1), function(x) lag(f.hat.doz, x))) # !!!!!!!!!!!!!!!!!!
#F <- f.hat.doz # test sans lag
head(F)

# probleme, comment estimer lambda à partir des formules de l'ACP ?
# le fait d'avoir ajouté des lags pose problème
lambda.0 <- P %*% D^{1/2}
dim(lambda.0)

# covariance de l'équation de transition
sigma.xi.hat <- cov(residuals(f.hat.var))
# la matrice B est donnée par le modèle
#B <- rbind(chol(sigma.xi.hat) * diag(q), matrix(0, p*q-q, q))
B <- rbind(cbind(chol(sigma.xi.hat) * diag(q), matrix(0, p*q-q, q)), matrix(0, p*q-q, p * q))

# covariance de l'équation de mesure
#phi <- sd(residuals(measure.equation))^2
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

attributes(y.kalman)
f.kalman <- zoo(t(y.kalman$att), order.by = index(X))[,1:q]
plot(merge(f.hat.doz, f.kalman[,1:5]))
for (i in 1:q){
  plot(f.hat.doz[,i], type = 'l', col = 'blue')
  lines(f.kalman[,i], type = 'l', col = 'red')
}

## Trimestrialisation des facteurs
f.trim <- aggregate(f.kalman, by = as.yearqtr, FUN = mean)
plot(f.trim)
plot(y.trim)

sourceDirectory('./fonctions/zoo_utils', modifiedOnly = FALSE)

# prevision approche mco
olsForecast(head(y.trim, 30), f.trim, summary = TRUE)
y.hat <- recForecasts(y.trim, f.trim, h = 4, k = 30, olsForecast)
y.test <- window(y.trim, start = start(y.hat), end = end(y.hat))
RMSE(y.hat, y.test)

plot(y.trim)
lines(y.hat[,1], col = 'red')

#prevision approche var
#varForecast(head(y.trim, length(y.trim) - 3), f.trim)
#recForecasts(y, X, h = 1, k = 30, varForecast)