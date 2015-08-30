function [alpha_L,beta_L,tau2_L,V_L,lambda2_L,sigma2_L]=lasso_step(y,X_alpha,X_beta,alpha_L,beta_L,sigma2_L,T,p,A,V_L,lambda2_L) %#ok<*INUSL>
% Function to draw from the Lasso

% Prior
% lambda2 ~ Gamma(r,d)
r = .01;
delta = .01;


y_til_beta = y - X_beta*beta_L;
post_mean_alpha = A*X_alpha'*y_til_beta; %#ok<*MINV>
post_var_alpha = sigma2_L*A;
alpha_L = Draw_Normal(post_mean_alpha,post_var_alpha);

% 1. Update beta from Normal
y_til_alpha = y - X_alpha*alpha_L;
B = inv(X_beta'*X_beta + inv(V_L));
post_mean_beta = B*X_beta'*y_til_alpha; %#ok<*MINV>
post_var_beta = sigma2_L*B;
beta_L = Draw_Normal(post_mean_beta,post_var_beta);
        
% 2. Update tau2_j from Inverse Gaussian
for j = 1:p       
    a1 = (lambda2_L*sigma2_L)./(beta_L(j,1).^2);   
    a2 = lambda2_L;
    tau2_inverse = Draw_IG(sqrt(a1),a2);
    tau2_L(j,1) = 1/tau2_inverse + 1e-15; %#ok<AGROW>
end

% Now that we have the new estimate of tau2, update V_L (the prior       
% covariance matrix of beta)
V_L = diag(tau2_L);

% 3. Update lambda2 from Gamma
b1 = p + r;
b2 = 0.5*sum(tau2_L) + delta;      
lambda2_L = Draw_Gamma(b1,b2);

% 4. Update sigma2 from Inverse Gamma
c1 = (T-1+p)/2;
PSI = (y-X_alpha*alpha_L-X_beta*beta_L)'*(y-X_alpha*alpha_L-X_beta*beta_L);
c2 = 0.5*PSI + 0.5*(beta_L'/V_L)*beta_L;
sigma2_L = Draw_iGamma(c1,c2);