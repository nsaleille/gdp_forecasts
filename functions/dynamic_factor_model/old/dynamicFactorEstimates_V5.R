dynamicFactorsEstimates <- function(X, q, s, lag.max = 5){

    # X_t = lambda * F_t + epsilon_t
    # F_t = A F_{t-1} + xi_t
    #
    # F_t = [f_t', ..., f_{t-s+1}']'
    # f_t is a (q x 1) vector
    # F_t is a (s * q) vector
    #
    # q is the number of static factors
    # s is the number of dynamic factors
    #
    # var(espilon_t) = diag(phi_1, ..., phi_n) = Phi
    # var(xi_t) = Sigma
    # xi_t = P nu_t
    # where var(nu_t) = I_(q x s), var(xi_t) = P %*% t(P) = Sigma

    require(vars)
    require(zoo)
    require(FKF)
  
    X <- Xs[[1]]
  
    X.omit <- na.omit(X)
    print(paste(nrow(X) - nrow(X.omit), 'observations dropped to estimate factors'))

    ########################################################
    ################  FACTOR ESTIMATES (Step 1)  ###########
    ########################################################

    T <- dim(X.omit)[1]; N <- dim(X.omit)[2]
    S <- (1/T) * Reduce("+", lapply(as.data.frame(t(X.omit)), function(x){x%*%t(x)}))
    D <- diag(eigen(S)$values[1:q]) # (q x q)
    P <- eigen(S)$vectors[, 1:q] # (N x q)
    f <- t(solve(D)^{1/2} %*% t(P) %*% t(X.omit))
    f <- zoo(f, order.by = index(X.omit))
    
    ########################################################
    ################ ESTIMATES (MEASUREMENT EQUATION)  #####
    ########################################################

    # matrix lambda
    lambda.0 <- P %*% D^{1/2}
    F <- bindLags(f, s, na.omit = TRUE, bind.original = TRUE)
    regs <- lapply(X.omit, function(x) lm(x ~ -1 + ., data = merge(x, F)))
    lambda <- t(sapply(regs, coefficients))
    
    # covariance of residuals
    epsilon <- X.omit - zoo(F %*% t(lambda), index(F))
    Phi <- unlist(apply(epsilon, MARGIN = 2, var))
    
    ########################################################
    ################ ESTIMATES (STATE EQUATION)  ###########
    ########################################################

    # Matrix A
    p <- VARselect(as.data.frame(f), lag.max = lag.max, type= "none")$selection['AIC(n)']
    f.var <- VAR(as.data.frame(f), p = p, type = "none")
    A <- t(sapply(f.var$varresult, function(x) coefficients(x)))
    A <- cbind(A, matrix(0, q, (s - p + 1) * q))
    A <- rbind(A, cbind(diag(s) %x% diag(q), matrix(0, s * q, q)))
  
    # Matrix B 
    # xi <- residuals(f.var)
    #B <- prcomp(xi)$rotation %*% diag(prcomp(xi)$sdev)
    #B <- rbind(B, matrix(0, s * q, q))
    #B <- cbind(B, matrix(0, (s + 1) * q, s * q))
    
    # Matrix P = chol(Sigma)
    xi <- F - zoo(t(A %*% t(lag(F, -1))), order.by = index(F))
    Sigma <- diag(diag(cov(xi)))
    P <- solve(Sigma)^{1/2}
    #P <- t(chol(Sigma))
    #tail(P %*% t(P) )
    #tail(Sigma)
    
    #D <- diag(eigen(Sigma)$values)
    #U <- eigen(Sigma)$vectors
    #Sigma.inv <- U%*%solve(D)^{1/2}%*%solve(U)
    
    ########################################################
    ################  FACTOR ESTIMATES (Step 2)  ###########
    ########################################################

    ## covariance of the measure equation
    # set to +\infty when the outcome is not observed
    covs <- array(dim = c(N, N, nrow(X)))
    for (t in 1:nrow(X)){
      covs[,,t] <- measure.cov(X[t, ], Phi)
    }

    ## Kalman filter to get better estimates of factors
    # using the previously estimated parameters
    X.kalman <- fkf(
        a0 = as.vector(F[1,]), # ???? NA dans F
        P0 = diag((s+1)*q), 
        dt = matrix(0, (s+1)*q, 1), 
        ct = matrix(0, N, 1),
        Tt = array(A, dim = c(dim(A), 1)),
        Zt = array(lambda, dim = c(dim(lambda), 1)),
        HHt = array(P, dim = c(dim(P), 1)),
        GGt = covs,
        yt = t(as.matrix(X))
        )
    
    F.kalman <- zoo(t(X.kalman$att), order.by = index(X))
    f.kalman <- F.kalman[,1:q] # remove f_{t-1} from what we call f.kalman

    return(f.kalman)

}

# a0 = as.vector(F[1,]) # ???? NA dans F
# P0 = diag((s+1)*q) 
# dt = matrix(0, (s+1)*q, 1) 
# ct = matrix(0, N, 1)
# Tt = array(A, dim = c(dim(A), 1))
# Zt = array(lambda, dim = c(dim(lambda), 1))
# HHt = array(B, dim = c(dim(B), 1))
# GGt = covs
# yt = t(as.matrix(X))

# length(a0) 
# dim(P0) 
# dim(dt) 
# dim(ct) 
# dim(Tt) 
# dim(Zt )
# dim(HHt) 
# dim(GGt) 
# dim(yt) 

# a0
# P0
# dt
# ct
# Tt
# Zt
