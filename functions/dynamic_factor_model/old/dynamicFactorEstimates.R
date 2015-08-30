dynamicFactorsEstimates <- function(y, X, q, lag.max = 15){

    require(vars)
    require(zoo)
    require(FKF)
  
    data <- na.omit(X)
    print(paste(nrow(X) - nrow(data), 'observations dropped to estimate factors'))

    X <- data
    #y <- data[,1]; X <- data[,2:ncol(data)]
    T <- dim(X)[1]; N <- dim(X)[2]; d <- 1 # dimension de y

    ########################################################
    ################  FACTOR ESTIMATES (Step 1)  ###########
    ########################################################

    S <- (1/T) * Reduce("+", lapply(as.data.frame(t(X)), function(x){x%*%t(x)}))
    D <- diag(eigen(S)$values[1:q]) # (q x q)
    P <- eigen(S)$vectors[, 1:q] # (N x q)
    f.hat <- t(solve(D)^{1/2} %*% t(P) %*% t(X))
    f.hat <- zoo(f.hat, order.by = index(X))
    lambda <- P %*% D^{1/2}
    
    ########################################################
    ################ MODEL PARAMETER ESTIMATES  ############
    ########################################################

    ### Estimation of matrix A
    # the parameter of the VAR applied on factors

    p <- VARselect(as.data.frame(f.hat), lag.max = lag.max, type= "none")$selection['AIC(n)']
    f.hat.var <- VAR(as.data.frame(f.hat), p = p, type = "none")
    A.hat <- t(sapply(f.hat.var$varresult, function(x) coefficients(x)))
    A.hat <- rbind(A.hat, cbind(diag(q*p - q), matrix(0, q*p - q,  q)))

    ### Estimation of lambda
    # OLS on the outcome using estimated factors
    # theorically we have a closed formula when there is only one lag
    lambda.0 <- P %*% D^{1/2}

    # !!!!!!!!!!!!!!!!!!!!!!!!!!!
    # problème ici, quelque chose ne va pas

    y.mens <- cubicSplineInterp(y)
    measure.equation <- lm(y.mens ~ ., data = merge(y.mens, f.hat))
    # summary(measure.equation)
    intercept <- coef(measure.equation)[1]
    lambda.hat <- coef(measure.equation)[2:length(coef(measure.equation))]
    lambda.hat <- matrix(c(lambda.hat, rep(0, p * q - q)), nrow = 1) #????????

    # matrice F des facteurs incluant les lags
    # require(fBasics)
    F <- bindLags(f.hat, p-1)

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

    y.kalman <- fkf(
        a0 = as.vector(F[p,]), # ???? NA dans F
        P0 = diag(q * p), 
        dt = matrix(0, q * p, 1), 
        ct = matrix(intercept),
        Tt = array(A.hat, dim = c(dim(A.hat), 1)),
        Zt = array(lambda.hat, dim = c(dim(lambda.hat), 1)),
        HHt = array(B, dim = c(dim(B), 1)),
        GGt = array(as.matrix(phi), dim = c(d, d, length(phi))),
        yt = t(as.matrix(y.mens))
        )

    f.kalman <- zoo(t(y.kalman$att), order.by = index(X))
    f.kalman <- f.kalman[,1:q] # remove f_{t-1} from what we call f.kalman

    return(list(f.hat = f.hat, f.kalman = f.kalman))

}