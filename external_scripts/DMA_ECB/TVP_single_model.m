% Online prediction using DMA
% One model case (and hence no model averaging)
% TVP-AR with forgetting and recurssive moment estimation of the
% measurement variance. 
%==========================================================================
%This is just for demonstration. If you need to implement DMA with many 
%models, use DMA.m and DMA_memory.m
%==========================================================================

% Clear memory and screen
clear all;
clc;
% Reset random number generator to a random value
RandStream.setDefaultStream(RandStream('mt19937ar','seed',sum(100*clock)))

% Set some variables to be used globally as input in functions
global K index yearlab prob_update Xnames %#ok<*NUSED>

% Add path of data files, utility functions etc.
addpath('data_files')
addpath('functions')

% =============================| MODEL SPECIFICATION |========================= 
% Estimate intercept?
intercept = 1;            % 0: no
                          % 1: yes
% Define lags
plag = 0;                 % Lags of dependent variables
hlag = 0;                 % Lags of exogenous variables
% Select stationarity transformations
stationarity = 2;         % 1: Use fully stationary variables
                          % 2: Use approximately stationary variables
                          % 3: Do not transform data
% Select set of exogenous predictors to use
use_x = 4;                % 1: Use all exogenous predictors
                          % 2: Use only "standard measures"
                          % 3: Use only "measures of underlying monetary expansion"
                          % 4: Use no exogenous predictors (estimate AR(p) model)
% Select if you want other exogenous predictors in the model (unemployment, exchange rate etc)
use_other = 0;            % 0: Do not use other exogenous predictors
                          % 1: Use other exogenous predictors
% How to treat missing values (note that these are only in the begining of
% the sample)
miss_treatment = 1;       % 1: Fill in missing values with zeros, let the KF do the rest
                          % 2: Trim quarters which are not observed (that way we lose information 
                                                     % for variables which have no missing values)
% Forgetting factors
lambda = 0.95;            % For the time-varying parameters theta
kappa = 0.95;             % For the error covariance matrix
% ---------FORECASTING
% Define forecast horizon (applied to direct forecasts)
h_fore = 6;

% Define the last observation of the first sample used to start producing forecasts
% recursively
first_sample_ends = cellstr('1990.Q4');
% =============================| end model specification |=========================


% ===================================| Create lagged depended variable|=========================
% Now load data, transform them accordingly and create left-hand side
% variable y_t (inflation), and R.H.S variables Z_t (unrestricted
% variables) and z_t (restricted variables)
apply_dma = 1;
data_in;
%m=1;
x_t = [Z_t z_t];

% From all the R.H.S. variables you loaded (intercept, lags of inflation and exogenous), 
% create a vector of the names of the variables you will actually use to forecast.
% Call this vector of variable names "Xnames".
effective_variable_names;

%----------------------------PRELIMINARIES---------------------------------
% For data-based priors, get the first sample in the recursive forecasting
% exercise
t0 = find(strcmp(yearlab,first_sample_ends)==1);

%========= PRIORS:
%-------- Now seT prior means and variances (_prmean / _prvar)
% theta_0 ~ N(theta_OLS, Var(theta_OLS))
theta_OLS = (y_t(1:T,:)'/x_t(1:T,:)')';
%theta_0_prmean = theta_OLS;
theta_0_prmean = zeros(m,1);
temp = (y_t(1:T,:)'/x_t(1:T,:)')';
s_02 = temp(1,1).^2 + var(y_t(1:T,:));
%theta_0_prvar = 0.1*diag([s_02./100]);% (var(y_t(1:T,:))./var(x_t(1:T,2:end)))']);
theta_0_prvar = 10*eye(m);

V_0 = var(y_t)./4;

%----------------------------- END OF PRELIMINARIES ---------------------------
tic;
for irep = 1:T
    % Predict
    if irep==1
        theta_pred(:,irep) = theta_0_prmean;  % this is eq.(3)
        R_t(:,:,irep) = (1./lambda)*theta_0_prvar;  % this is eq.(5)
    else
        theta_pred(:,irep) = theta_update(:,irep-1);    % this is eq.(3)
        R_t(:,:,irep) = (1./lambda)*squeeze(S_t(:,:,irep-1));     % this is eq.(5)
    end
    y_t_pred(irep,:) = x_t(irep,:)*theta_pred(:,irep);  % this is one step ahead prediction
    
    % Update
    e_t(:,irep) = y_t(irep,:) - x_t(irep,:)*theta_pred(:,irep);  % this is one step ahead prediction error
    % Update V_t - measurement error covariance matrix using rolling   
    % moments estimator, see top of page 12
    if irep<2
        A_t = (1/irep)*((e_t(:,irep)).^2 - x_t(irep,:)*squeeze(R_t(:,:,irep))*x_t(irep,:)');
        if A_t>0
            V_t(:,irep) = A_t;
        else
            V_t(:,irep) = V_0;
        end
    else
        A_t = (e_t(:,irep-1)).^2;
        V_t(:,irep) = kappa*V_t(:,irep-1) + (1-kappa)*A_t;
        if V_t(:,irep)<=0
            V_t(:,irep) = V_t{k,1}(:,irep-1);
        end
    end
    
    %update theta[T]
    theta_update(:,irep) = theta_pred(:,irep) + squeeze(R_t(:,:,irep))*x_t(irep,:)'*inv(V_t(:,irep) + x_t(irep,:)*squeeze(R_t(:,:,irep))*x_t(irep,:)')*e_t(:,irep);      % this is eq.(7)
    S_t(:,:,irep) = squeeze(R_t(:,:,irep)) - squeeze(R_t(:,:,irep))*x_t(irep,:)'*inv(V_t(:,irep) + x_t(irep,:)*squeeze(R_t(:,:,irep))*x_t(irep,:)')*x_t(irep,:)*squeeze(R_t(:,:,irep));   % this is eq.(8)  
end

toc;



