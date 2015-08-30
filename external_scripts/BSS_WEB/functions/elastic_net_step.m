function [alpha_EN,beta_EN,tau2_EN,V_EN,lambda2_1_EN,lambda2_2_EN,sigma2_EN]=elastic_net_step(y,X_alpha,X_beta,alpha_EN,beta_EN,sigma2_EN,T,p,A,V_EN,lambda2_1_EN,lambda2_2_EN) %#ok<INUSL>

% Priors
% lambda2_1 ~ Gamma(r1,d1)
r_1 = .01;
delta_1 = .01;

% lambda2_2 ~ Gamma(r2,d2)
r_2 = .01;
delta_2 = .01;


tau2_EN = zeros(p,1);
tau2_inverse = zeros(p,1);

%-----| STEP 1: DRAW UNRESTRICTED PARAMETERS (intercept and lags)
y_til_beta = y - X_beta*beta_EN;
post_mean_alpha = A*X_alpha'*y_til_beta; %#ok<*MINV>
post_var_alpha = sigma2_EN*A;
alpha_EN = Draw_Normal(post_mean_alpha,post_var_alpha);

% 1. Update beta from Normal
y_til_alpha = y - X_alpha*alpha_EN;
B = inv(X_beta'*X_beta + inv(V_EN));
post_mean_beta = B*X_beta'*y_til_alpha; %#ok<*MINV>
post_var_beta = sigma2_EN*B;
beta_EN = Draw_Normal(post_mean_beta,post_var_beta);
    
% 2. Update tau2_j from Inverse Gaussian   
for j = 1:p
    a1 = (lambda2_1_EN*sigma2_EN)./(beta_EN(j,1).^2);
    a2 = lambda2_1_EN;
    tau2_inverse(j,1) = Draw_IG(sqrt(a1),a2);
    tau2_EN(j,1) = 1/tau2_inverse(j,1);
end

% Now that we have the new estimate of tau2, update V_L (the prior 
% covariance matrix of beta)
V_EN = diag(1./(tau2_inverse + sqrt(lambda2_2_EN)) + 1e-15);
    
% 3. Update lambda2_1 from Gamma
ba1 = p + r_1;
ba2 = 0.5*sum(tau2_EN) + delta_1;
lambda2_1_EN = Draw_Gamma(ba1,ba2);
    
% 4. Update lambda2_2 from Gamma
bb1 = p/2 + r_2;
bb2 = 0.5*sum((beta_EN.^2))./sigma2_EN + delta_2;
lambda2_2_EN = Draw_Gamma(bb1,bb2);

% 5. Update sigma2 from Inverse Gamma
c1 = (T-1+p)/2;
PSI = (y-X_alpha*alpha_EN-X_beta*beta_EN)'*(y-X_alpha*alpha_EN-X_beta*beta_EN);
c2 = 0.5*PSI + 0.5*(beta_EN'/V_EN)*beta_EN;
sigma2_EN = Draw_iGamma(c1,c2);