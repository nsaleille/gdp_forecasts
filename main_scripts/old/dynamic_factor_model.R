# Dynamic factor model

rm(list=ls(all=TRUE))
setwd("/Users/nsaleille/Dropbox/ofpr/scripts") # chemin perso
load('./full_set_of_series.RData')
load('./full_description_of_series.RData')
library(R.utils)
sourceDirectory('./fonctions/zoo_utils')

data.mens <- data.full[sapply(data.full, FUN = function(x) (x$freq == 'mois'))]
data.trim <- data.full[sapply(data.full, FUN = function(x) (x$freq == 'Trimestre'))]
ids <- sapply(data.trim, FUN = function(x) x$id)
get_desc(ids)

# mensualisation des séries
# selection du \delta PIB, la variable expliquée
library(zoo)
dPIB <- get_series('X001688995')
y <- cubicSplineInterp(dPIB)
data.trim.interp <- lapply(data.trim, cubicSplineInterp)

# données sphéricisées
get_zoo <- function(x){lapply(x, function(x) x$zoo)}
x <- unlist(list(data.mens, data.trim.interp), recursive = FALSE)
xsp <- Reduce(function(x,y) merge(x,y), get_zoo(x))
colnames(xsp) <- sapply(x, function(x) x$id)
xsp <- na.omit(xsp)
plot(xsp)


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

X <- t(cdxsp); T <- dim(X)[2]; N <- dim(X)[1]

# TEST 1 : PCA avec prcomp
# ! l'acp dans prcomp est faite à partir de la décomposition de la matrice des données
# et non à partir des vp de la matrice de covariance.
# prenons les 5 premières composantes principales (74% de la variance expliquée)
# il faudrait dans un second temps implémenter les tests de Bai et Ng (2007)

pca <- prcomp(t(X))
summary(pca)
dim(pca$rotation)
head(pca$rotation)
f.hat.prcomp <- pca$x[,1:5]

# TEST 2: pca à la main (cf doz 2011)
# à l'aide des vp de la matrice de covariance
# on trouve une rotation différente mais avec certain facteurs
# qui semblent très corrélés
# bizarre les variables transformées ne sont pas centrées.

S <- (1/T) * Reduce("+", lapply(as.data.frame(X), function(x){x%*%t(x)}))
D <- diag(eigen(S)$values)
P <- t(eigen(S)$vectors)
f.hat.doz <- t(solve(D)^{1/2} %*% t(P) %*% X)[,1:5]

# essayer de retrouver les résultats de la fonction PCA
plot(zoo(f.hat.doz), type = 'l')
plot(zoo(f.hat.prcomp))

plot(zoo(cbind(f.hat.prcomp[,1], f.hat.doz[,3])))
plot(zoo(cbind(f.hat.prcomp[,2], f.hat.doz[,5])))
plot(f.hat.doz[,4], type = 'l')

# 2. estimation des paramètres du modèle
# en version espace-etat

lambda.hat.doz <- P %*% D^{1/2}

library(vars)
p <- VARselect(as.data.frame(f.hat.doz), lag.max = 15, type="const")$selection['AIC(n)']
f.hat.var <- VAR(as.data.frame(f.hat.doz), p = p, type = "const")
A.hat <- t(sapply(f.hat.var$varresult, function(x) coefficients(x)))

# estimation de la matrice B
# indispensable pour appliquer kalman

sigma.xi.hat <- cov(residuals(f.hat.var))
B <- prcomp(sigma.xi.hat)
eps.hat <- solve(B$rotation, t(residuals(f.hat.var)))
mean(eps.hat); sd(eps.hat)

plot(f.hat.var)



# pour estimer lambda, OLS equation par equation
# x.ols <- lm(cdxsp ~ f, data = cdxsp)
# lambda.hat <- coef(x.ols)
# faut-il modéliser l'autocorrelation des résidus ?
# pacf(coredata(residuals(x.ols)[,2])) # probablement oui
#cov(pca$rotation[,1], pca$rotation[,3])


biplot(pca)


AR1 <- function(rho, n = 100){
  x <- c(0)
  for (i in 1:n){
    x <- c(x, rho*tail(x, 1) + rnorm(1))
  }
  return(x)
}

x <- AR1(0.8)
