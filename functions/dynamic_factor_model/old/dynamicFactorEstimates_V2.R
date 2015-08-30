dynamicFactorsEstimates <- function(y, X, q, lag.max = 15){

    require(vars)
    require(zoo)
    require(FKF)
  
    X.omit <- na.omit(X)
    print(paste(nrow(X) - nrow(X.omit), 'observations dropped to estimate factors'))

    ########################################################
    ################  FACTOR ESTIMATES (Step 1)  ###########
    ########################################################

    T <- dim(X.omit)[1]; N <- dim(X.omit)[2]
    S <- (1/T) * Reduce("+", lapply(as.data.frame(t(X.omit)), function(x){x%*%t(x)}))
    D <- diag(eigen(S)$values[1:q]) # (q x q)
    P <- eigen(S)$vectors[, 1:q] # (N x q)
    f.hat <- t(solve(D)^{1/2} %*% t(P) %*% t(X.omit))
    f.hat <- zoo(f.hat, order.by = index(X.omit))
    lambda.0 <- P %*% D^{1/2}
    
    ########################################################
    ################ MODEL PARAMETER ESTIMATES  ############
    ########################################################

    ### Estimation of matrix A
    # the parameter of the VAR applied on factors

    p <- VARselect(as.data.frame(f.hat), lag.max = lag.max, type= "none")$selection['AIC(n)']
    f.hat.var <- VAR(as.data.frame(f.hat), p = p, type = "none")
    A.hat <- t(sapply(f.hat.var$varresult, function(x) coefficients(x)))
    A.hat <- rbind(A.hat, cbind(diag(q*p - q), matrix(0, q*p - q,  q)))

    F <- bindLags(f.hat, p-1)
    lambda.hat <- cbind(lambda.0, matrix(0, ncol = p * q - q, nrow = N))

    # covariance de l'équation de transition
    sigma.xi.hat <- cov(residuals(f.hat.var))
    # la matrice B est donnée par le modèle
    B <- rbind(cbind(chol(sigma.xi.hat) * diag(q), matrix(0, q, p*q - q)), matrix(0, p*q-q, p * q))

    # covariance de l'équation de mesure
    e <- X.omit - f.hat %*% t(lambda.0)
    sigma.hat.e <- unlist(apply(e, MARGIN = 2, var))
    
    ########################################################
    ################  FACTOR ESTIMATES (Step 2)  ###########
    ########################################################

    # Using the Kalman filter / smoother
    # and the full information set

    ## covariance of the measure equation
    # set to +\infty when the outcome is not observed
    covs <- array(dim = c(N, N, nrow(X)))
    for (t in 1:nrow(X)){
      covs[,,t] <- measure.cov(X[t, ], sigma.hat.e)
    }

    ## Kalman filter to get better estimates of factors
    # using the previously estimated parameters

    X.kalman <- fkf(
        a0 = as.vector(F[p,]), # ???? NA dans F
        P0 = diag(q * p), 
        dt = matrix(0, q * p, 1), 
        ct = matrix(0, N, 1),
        Tt = array(A.hat, dim = c(dim(A.hat), 1)),
        Zt = array(lambda.hat, dim = c(dim(lambda.hat), 1)),
        HHt = array(B, dim = c(dim(B), 1)),
        GGt = covs,
        yt = t(as.matrix(X))
        )

    a0
    dim(P0)
    dim(dt)
    dim(ct)
    dim(Tt)
    dim(Zt)
    dim(HHt)
    dim(GGt)
    dim(yt)
    
    f.kalman <- zoo(t(X.kalman$att), order.by = index(X))
    f.kalman <- f.kalman[,1:q] # remove f_{t-1} from what we call f.kalman

    return(list(f.hat = f.hat, f.kalman = f.kalman))

}