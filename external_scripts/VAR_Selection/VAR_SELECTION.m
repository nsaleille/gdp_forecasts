%--------------------------------------------------------------------------
% Variable selection in a VAR model
%--------------------------------------------------------------------------
% The VAR can be written as
%    
%      Y = Z x GAMMA x BETA + e,     e ~ N(0,I x SIGMA)
%
% where GAMMA is a diagonal matrix of 0-1 restriction indices, BETA are the
% regression coefficients and SIGMA the covariance matrix. This code
% samples recursively BETA - GAMMA - SIGMA from their posteriors. For more
% information, check the manual
%--------------------------------------------------------------------------
% I use artificial data to demonstrate how restriction search can lead to
% efficient inference. I generate a 5 variable VAR with only T=50 time
% series observations, one lag and no constant. While the unrestricted OLS 
% estimates perform poorly, variable selection gives better estimates. This
% is because we restrict irrelevant variables, then there are more degrees 
% of freedom to estimate the parameters on the relevant variables. Run the 
% code and see the final results printed in the command window.
%--------------------------------------------------------------------------

clear all;
clc;
tic;

% Generate artificial data
[Y,PHI] = simvardgp();
y = Y - repmat(mean(Y),size(Y,1),1);
%--------------------------DATA HANDLING-----------------------------------
[Traw m] = size(y);

plag=1; %choose number of lags

% Generate lagged Y matrix. This will be part of the X matrix
ylag = mlag2(y,plag);

n=(plag*m)*m; 
p=(plag*m);
x2 = ylag(plag+1:Traw,:);
x = kron(eye(m),x2);

% Form y matrix accordingly
% Delete first "L" rows to match the dimensions of X matrix
y2 = y(plag+1:Traw,:); % This is the final Y matrix used for the VAR
y = y2(:);
%Traw was the dimesnion of initial data. T is the number of actual 
%time series observations of ymat and xmat (final Y & X)
T=Traw-plag;

% ----------------Gibbs related preliminaries
nsave = 5000;   % Number of draws to save
nburn = 5000;  % Number of draws to discard
ntot = nsave + nburn;  % Number of total draws

beta_draws = zeros(nsave,n);
gamma_draws = zeros(nsave,n);
sigma_draws = zeros(nsave,m,m);

% ----------------Set priors
% beta ~ N(b0,D0)
b0 = 0*ones(n,1);
D0 = 9*eye(n);
D0_inv = inv(D0);

% gamma_j ~ Bernoulli(1,p_j), for j = 1,...,p
p_j = 0.5*ones(n,1);

% sigma ~ iWishart(a,eta)
a = m;
eta = 1*eye(m);
eta_inv = inv(eta);

% Initialize parameters
gamma = ones(n,1);
beta_OLS = inv(x'*x)*(x'*y);
beta_OLS2 = inv(x2'*x2)*(x2'*y2);
sse = (y2 - x2*beta_OLS2)'*(y2 - x2*beta_OLS2);
sigma = sse./(T-(p-1));

%-----------Gibbs iterations
disp('Running variable selection for VARs')
disp('Iterations')
for irep = 1:ntot
    if mod(irep,500)==0
        disp(irep)
        toc;
    end
    
    V=kron(inv(sigma),eye(T));
    x_star = (ones(T*m,1)*gamma').*x;
    
    % ------Step 1:
    % update BETA ~ Normal
    beta_var = inv(D0_inv + x_star'*V*x_star);
    beta_mean = beta_var*(D0*b0 + x_star'*V*y);
    beta = beta_mean + chol(beta_var)'*randn(n,1);
    
    beta_mat = zeros(p,m); % beta_mat is the matrix of regr. coefficients
    for i=1:m
        beta_mat(:,i) = beta((i-1)*p+1:i*p,:);
    end
    
    % ------Step 2:    
    % It is preferable to draw each gamma(j) in random order
    ind_perm = randperm(n)';
    % update GAMMA ~ Bernoulli
    for kk = 1:n
        j = ind_perm(kk);
        theta = beta.*gamma;
        theta_j_star = theta;
        theta_j_star_star = theta;
        theta_j_star(j) = beta(j);
        theta_j_star_star(j) = 0;
        c_j = p_j(j)*exp(-0.5*(y - x*theta_j_star)'*V*(y - x*theta_j_star));
        d_j = (1-p_j(j))*exp(-0.5*(y - x*theta_j_star_star)'*V*(y - x*theta_j_star_star));
        p_j_tilde = c_j / (c_j+d_j);
        gamma(j) = bernoullirnd(p_j_tilde);
    end
        
    gamma_mat = zeros(p,m); % gamma_mat concatenates the restriction indices in matrix form
    for i=1:m
        gamma_mat(:,i) = gamma((i-1)*p+1:i*p,:);
    end
    % ------Step 3:        
    theta = beta_mat.*gamma_mat;
    % update SIGMA ~ iWishart
    R_1 = inv(eta_inv + (y2 - x2*theta)'*(y2 - x2*theta));
    R_2 = (a + T);
    rd = wish(R_1,R_2);
    sigma=inv(rd);
    
    % Save draws
    if irep > nburn
        beta_draws(irep-nburn,:) = beta;
        gamma_draws(irep-nburn,:) = gamma;
        sigma_draws(irep-nburn,:,:) = sigma;
    end 
end
clc;
% The first column is the mean of the restriction indices,
% the second gives the means of the regression coefficients B,
% the third column gives the OLS estimate of B,
% the fourth column gives the true (generated) value of B
disp('restrict*   | Bayes** |   OLS  |  True')
disp([mean(gamma_draws)' mean(beta_draws)' beta_OLS PHI(:)]);
disp('    ')
disp('* This is the average posterior probability of the restriction indexes')
disp('** This is the mean of the posterior density of beta')
disp('    ')



toc;