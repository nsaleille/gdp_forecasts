% This code implements the model selection in a TVP-VAR.

clear all;
clc;
randn('state',sum(100*clock)); %#ok<*RAND>
rand('twister',sum(100*clock));
%-----------------------------LOAD DATA------------------------------------
% Load Korobilis (2008) quarterly data
% load ydata.dat;
% load yearlab.dat;
% 
% Y = ydata - repmat(mean(ydata),size(ydata,1),1);

[Y,b] = tvpvarsim();

if sum(std(Y)>5)>0
    error('Explosive model generated, I have to stop. Rerun code.');
end

% Number of observations and dimension of Y
t=size(Y,1);
M=size(Y,2);
p = M; % p is the dimensionality of Y
plag = 1; % plag is number of lags in the VAR part
numa = p*(p-1)/2;
% ===================================| VAR EQUATION |=========================
% Generate lagged Y matrix. This will be part of the X matrix
ylag = mlag2(Y,plag); % Y is [T x m]. ylag is [T x (nk)]
% Form RHS matrix X_t = [1 y_t-1 y_t-2 ... y_t-k] for t=1:T
ylag = ylag(plag+1:t,:);

m = plag*(p^2); % m is the number of elements in the state vector
% Create X_t matrix as in Primiceri equation (4). Of course I have reserved
% "X" for the data I use to extract factors, hence name this matrix "Z".
Z = zeros((t-plag)*p,m);
for i = 1:t-plag
    ztemp = [];%eye(p);
    for j = 1:plag        
        xtemp = ylag(i,(j-1)*p+1:j*p);
        xtemp = kron(eye(p),xtemp);
        ztemp = [ztemp xtemp];  %#ok<AGROW>
    end
    Z((i-1)*p+1:i*p,:) = ztemp;
end

% Redefine VAR variables y
y = Y(plag+1:t,:)';
%yearlab = yearlab(plag+1:t);

% Time series observations
t=size(y,2);
%----------------------------PRELIMINARIES---------------------------------
% Set some Gibbs - related preliminaries
nrep = 50;  % Number of replications
nburn = 50;   % Number of burn-in-draws
nthin = 1;   % Consider every thin-th draw (thin value)
it_print = 100;  %Print in the screen every "it_print"-th iteration

%========= PRIORS:
% Use uninformative values
B_OLS = zeros(m,1);
VB_OLS = eye(m);

% Set some hyperparameters here
k_Q = 0.01;

% We need the sizes of some matrices as prior hyperparameters
sizeQ = m; % Size of matrix Q

%-------- Now set prior means and variances (_prmean / _prvar)
% B_0 ~ N(B_OLS, 4Var(B_OLS))
B_0_prmean = B_OLS;
B_0_prvar = 4*VB_OLS;

% Note that for IW distribution I keep the _prmean/_prvar notation,
% but these are scale and shape parameters...
% Q ~ IW(k2_Q*size(subsample)*Var(B_OLS),size(subsample))
Q_prmean = ((k_Q).^2)*(1 + sizeQ)*VB_OLS;
Q_prvar = 1 + sizeQ;

% gamma_j ~ Bernoulli(1,p_j), for j = 1,...,m
p_j = 0.5*ones(m,1);

% sigma ~ iWishart(a,eta)
a = 0;
eta = 1*eye(p);
eta_inv = 0*inv(eta);

%========= INITIALIZE MATRICES:
% Specify covariance matrices for measurement and state equations*/
consQ = 0.0001;
consH = 0.0001;
Qdraw = consQ*eye(m);
Qchol = sqrt(consQ)*eye(m);
sigma_sd = [1.0000   -0.5000   -0.2500   -0.1250;
           -0.5000    1.2500   -0.3750   -0.1875;
           -0.2500   -0.3750    1.3125   -0.3437;
           -0.1250   -0.1875   -0.3437    1.3281];
Ht = kron(ones(t,1),sigma_sd);
Htsd = kron(ones(t,1),chol(sigma_sd));
Btdraw = zeros(m,t);
gamma = ones(m,1);

% Storage matrices for posteriors and stuff
Bt_postmean = zeros(m,t);
Qmean = zeros(m,m);
gamma_draws = zeros(nrep,m);
sigma_draws = zeros(nrep,p,p);
%----------------------------- END OF PRELIMINARIES ---------------------------

%====================================== START SAMPLING ========================================
%==============================================================================================
tic; % This is just a timer
disp('Number of iterations');

for irep = 1:nrep + nburn    % GIBBS iterations starts here
    % Print iterations
    if mod(irep,it_print) == 0
        disp(irep);toc;
    end
    % -----------------------------------------------------------------------------------------
    %   STEP I: Sample B from Normal
    % -----------------------------------------------------------------------------------------
    [Btdrawc,log_lik] = carter_kohn(y,Z*diag(gamma),Ht,Qdraw,m,p,t,B_0_prmean,B_0_prvar);
    Btdraw = Btdrawc;

    Btemp = Btdraw(:,2:t)' - Btdraw(:,1:t-1)';
    sse_2 = zeros(m,m);
    for i = 1:t-1
        sse_2 = sse_2 + Btemp(i,:)'*Btemp(i,:);
    end

    Qinv = inv(sse_2 + Q_prmean);
    Qinvdraw = wish(Qinv,t+Q_prvar);
    Qdraw = inv(Qinvdraw);
    Qchol = chol(Qdraw);
    
    % -----------------------------------------------------------------------------------------
    %   STEP II: Sample gamma from Bernoulli
    % -----------------------------------------------------------------------------------------
    for j = 1:m
        theta = diag(gamma)*Btdraw;
        theta_j_star = theta;
        theta_j_star_star = theta;
        theta_j_star(j,:) = Btdraw(j,:);
        theta_j_star_star(j,:) = 0;
        x_star=[];
        x_star_star=[];
        for i = 1:t
            xtemp1 = inv(Htsd((i-1)*p+1:i*p,:))*(y(:,i) - Z((i-1)*p+1:i*p,:)*theta_j_star(:,i));       
            xtemp2 = inv(Htsd((i-1)*p+1:i*p,:))*(y(:,i) - Z((i-1)*p+1:i*p,:)*theta_j_star_star(:,i));
            x_star = [x_star  xtemp1]; %#ok<AGROW>
            x_star_star = [x_star_star  xtemp2]; %#ok<AGROW>
        end   
        c_j = p_j(j)*exp(-1/2*trace(x_star'*x_star));
        d_j = (1-p_j(j))*exp(-1/2*trace(x_star_star'*x_star_star));
        p_j_tilde = c_j / (c_j+d_j);
        gamma(j,1) = bernoullirnd(p_j_tilde);
    end
        
    % -----------------------------------------------------------------------------------------
    %   STEP II: Sample SIGMA from inverse-Wishart
    % -----------------------------------------------------------------------------------------        
    sse_2=zeros(p,p);
    for i = 1:t
        theta = Btdraw(:,1).*gamma;
        sse_2 = sse_2 + (y(:,i) - Z((i-1)*p+1:i*p,:)*theta)*(y(:,i) - Z((i-1)*p+1:i*p,:)*theta)';
    end        
    % Draw SIGMA
    R_1 = inv(eta_inv + sse_2);
    R_2 = (a + t);
    rd = wish(R_1,R_2);
    sigma=inv(rd);
    
    
    Ht = kron(ones(t,1),sigma);
    Htsd = kron(ones(t,1),chol(sigma));
    
    %----------------------------SAVE AFTER-BURN-IN DRAWS AND IMPULSE RESPONSES -----------------
    if irep > nburn;
        Bt_postmean = Bt_postmean + Btdraw;
        gamma_draws(irep-nburn,:) = gamma;
        Qmean = Qmean + Qdraw;
        sigma_draws(irep-nburn,:,:) = sigma;
    end % END saving after burn-in results 
end %END main Gibbs loop (for irep = 1:nrep+nburn)
clc;
toc; % Stop timer and print total time
%=============================GIBBS SAMPLER ENDS HERE==================================
Bt_postmean = Bt_postmean./nrep;   % mean of time-varying parameters B(t)
Qmean = Qmean./nrep;          % mean of the matrix Q, which is the covariance of B(t) 
gamma_mean = mean(gamma_draws)';   % average restriction indices
SIGMA_mean = squeeze(mean(sigma_draws,1));  % mean of VAR covariance matrix



