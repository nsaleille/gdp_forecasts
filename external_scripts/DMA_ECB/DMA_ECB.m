% Online prediction using Dynamic Model Averaging. 
% Euro-area price index using monetary measures.
%
% Based on Koop and Korobilis, 2009, Forecasting Inflation using Dynamic Model Averaging)
% http://www.rcfea.org/RePEc/pdf/wp34_09.pdf
%==========================================================================
% THIS IS A PC MEMORY SAVING VERSION, WHICH STORES LESS MATRICES. I AM NOT
% STORING ALL POSSIBLE MODEL COMBINATIONS (MATIX x_t), AS WELL AS THE TIME
% VARYING PARAMETERS FOR ALL K=2^N MODELS. HOWEVER YOU CAN GET ALL THE DMA
% PREDICTIONS AND TIME-VARYING MODEL PROBABILITIES.
%==========================================================================
% TVP-AR with forgetting and recurssive moment estimation of the
% measurement variance.
%
%        y[t] = theta[t] x z[t] + e1,        e1 ~ N(0,V_t)
%    theta[t] = theta[t-1]      + e2,        e2 ~ N(0,S_t)  
%
% where z[t] is the matrix of predictor variables and theta[t] the
% time-varying regression coefficient.
%
% Here first define matrix z[t] in line 116 with the maximum number of
% regressors. Then BMA is done over all possible models defined by the
% columns of z_t (i.e. if N regressors in z_t, then #_of_models = 2.^N)
% I use 2 lags (see variable 'plag') which gives an intercept, the AR(1)
% coefficient and the AR(2) coefficient. All possible models are 8 = 2^3.
%
%==========================================================================
% Written on 02/12/2010
% Dimitris Korobilis,
% Universite Catholique de Louvain
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
addpath('competing')

% =============================| MODEL SPECIFICATION |========================= 
% Estimate intercept?
intercept = 1;            % 0: no
                          % 1: yes
% Define lags
plag = 0;                 % Lags of dependent variables
hlag = 0;                 % Lags of exogenous variables
% Select stationarity transformations (ONLY for EXOGENOUS variables!)
stationarity = 2;         % 1: Use fully stationary variables
                          % 2: Use approximately stationary variables
                          % 3: Do not transform to stationarity
% Select set of monetary predictors to use
use_x = 3;                % 1: Use all monetary predictors
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
% Where do I apply DMA?
apply_dma  = 1;           % 1: Only on the exogenous variables
                          % 2: On the exogenous and the lags of the dependent
                                                          % (requires plag>0)
                          % 3: On the exogenous, the lags of the dependent and the
                                             % intercept (requires 'intercept = 1') 
% Forgetting factors
lambda = 0.99;            % For the time-varying parameters theta
alpha = 0.90;             % For the model switching
kappa = 0.95;             % For the error covariance matrix
% Forgetting method on model switching probabilities
forgetting_method = 2;    % 1: Linear Forgetting
                          % 2: Exponential Forgetting
% Initial values on time-varying parameters
% theta[0] ~ N(PRM,PRV x I)
prior_theta = 2;          % 1: Diffuse N(0,4)
                          % 2: Data-based prior
% Initialize measurement error covariance V[t]
initial_V_0 = 2;          % 1: a small positive value (but NOT exactly zero)
                          % 2: a quarter of the variance of your initial data
% Initialize DMA weights
initial_DMA_weights = 1;  % 1: equal weights
                          % Sorry, no other option yet available
% Define expert opinion (prior) on model weight
expert_opinion = 2;       % 1: Equal weights on all models
                          % 2: No prior expert opinion
% ---------FORECASTING
% Define forecast horizon (applied to direct forecasts)
h_fore = 6;

% Define the last observation of the first sample used to start producing forecasts
% recursively
first_sample_ends = cellstr('1990.Q4');

% Do a last check of the model specification inputs before you run the
% model
checkinput;
% =============================| end model specification |=========================


%=================================| PRELIMINARIES |================================
%============| DATA HANDLING:
% Now load data, transform them accordingly and create left-hand side
% variable y_t (inflation), and R.H.S variables Z_t (unrestricted
% variables) and z_t (restricted variables)
data_in;

% From all the R.H.S. variables you loaded (intercept, lags of inflation and exogenous), 
% create a vector of the names of the variables you will actually use to forecast.
% Call this vector of variable names "Xnames".
effective_variable_names;

%============| DEFINE MODELS:
% Now get all possible model combination and create a variable indexing all
% those 2^N models (wher N is the number of variables in z_t to be
% restricted). Call this index variable "index".
model_index;

%============| PRIORS:
% For data-based priors, get the first sample in the recursive forecasting
% exercise
t0 = find(strcmp(yearlab,first_sample_ends)==1);

prior_hyper;

% Initialize matrices in PC memory
theta_pred = cell(K,1);
R_t = cell(K,1);
prob_pred = zeros(T,K);
y_t_pred = cell(K,1);
y_t_pred_h = cell(K,1);
e_t = cell(K,1);
A_t = cell(K,1);
V_t = cell(K,1);
theta_update = cell(K,1);
S_t = cell(K,1);
variance = cell(K,1);
w_t = cell(K,1);
log_PL = zeros(K,1);
prob_update = zeros(T,K);
y_t_DMA = zeros(T,1);
var_DMA = zeros(T,1);
y_t_BEST = zeros(T,1);
var_BEST = zeros(T,1);
log_PL_DMA = zeros(T,1);
log_PL_BEST = zeros(T,1);
xRx = cell(K,1);
offset = 1e-20; % This offset constant is used in some cases for numerical stability   
% =============================| end of preliminaries |=========================
tic;
disp('You are running DMA_ECB.m')
disp('       ')

% =============================Start now the Kalman filter loop   
for irep = 1:T % for 1 to T time periods
    if mod(irep,ceil(T./20)) == 0
        disp([num2str(100*(irep/T)) '% completed'])
        toc;
    end
    
    % Here get the sum of all K model probabilities, quantity you
    % are going to use to update the individual model probabilities
    if irep>1
        if forgetting_method == 1
            % Linear Forgetting
            sum_prob = sum( (alpha*prob_update(irep-1,:) + (1-alpha)*expert_weight),2); % this is the sum of the K model probabilities (all in multiplied by the forgetting factor 'a')
        elseif forgetting_method == 2
            % Exponential Forgetting
            sum_prob_a = sum((prob_update(irep-1,:).^alpha).*(expert_weight^(1-alpha)),2);  % this is the sum of the K model probabilities (all in the power of the forgetting factor 'a')
        end
    end

    % reset log_PL, A_t and R_t, to zero at each iteration to save memory
    log_PL = zeros(K,1);    
    A_t = cell(K,1);
    R_t = cell(K,1);
    for k = 1:K % for 1 to K competing models
        x_t = cell(1,1);
        x_t{1,1} = [Z_t z_t(:,index_z_t{k,1}')]; %#ok<*USENS>
        % -----------------------Predict
        if irep==1
            theta_pred{k,1} = theta_0_prmean{k,1};  % predict theta[t], this is Eq. (5)
            R_t{k,1} = inv_lambda*theta_0_prvar{k,1};   % predict R[t], this is Eq. (6)
            temp1 = ((prob_0_prmean).^alpha);  
            prob_pred(irep,k) = temp1./(K*temp1);     % predict model probability, this is Eq. (15)
        else
            theta_pred{k,1} = theta_update{k,1};    % predict theta[t], this is Eq. (5)
            R_t{k,1} = inv_lambda.*S_t{k,1};   % predict R[t], this is Eq. (6)
            if forgetting_method == 1
                %Linear Forgetting
                prob_pred(irep,k) = (alpha*prob_update(irep-1,k) + (1-alpha)*expert_weight)./sum_prob;
            elseif forgetting_method == 2
                % Exponential Forgetting           
                prob_pred(irep,k) = ((prob_update(irep-1,k).^alpha)*(expert_weight^(1-alpha)) + offset)...
                    ./(sum_prob_a + offset);   % predict model probability, this is Eq. (15)
            end
        end

        % Now implememnt individual-model predictions of the variable of interest
        y_t_pred{k,1}(irep,:) = x_t{1,1}(irep,:)*theta_pred{k,1};   %one step ahead prediction
        
        % Now do h_fore-step ahead prediction
        y_t_pred_h{k,1}(irep,:) = x_t{1,1}(irep+h_fore,:)*theta_pred{k,1}; % predict t+h given t
        
        % -------------------------Update
        e_t{k,1}(:,irep) = y_t(irep,:) - y_t_pred{k,1}(irep,:); % one-step ahead prediction error
        
        % We will need some products of matrices several times, which is better to define them
        % once here for computational efficiency
        R_mat = R_t{k,1};
        xRx2 = x_t{1,1}(irep,:)*R_mat*x_t{1,1}(irep,:)';
        
        % Update V_t - measurement error covariance matrix using rolling
        % moments estimator, see top of page 12
        if irep==1
            V_t{k,1}(:,irep) = V_0;
        else
            A_t{k,1} = (e_t{k,1}(:,irep-1)).^2;
            V_t{k,1}(:,irep) = kappa*V_t{k,1}(:,irep-1) + (1-kappa)*A_t{k,1};
        end
        
        % Update theta[t] (regression coefficient) and its covariance
        % matrix S[t]
        Rx = R_mat*x_t{1,1}(irep,:)';
        KV = V_t{k,1}(:,irep) + xRx2;
        KG = Rx/KV;
        theta_update{k,1} = theta_pred{k,1} + KG*e_t{k,1}(:,irep);
        S_t{k,1} = R_mat - KG*(x_t{1,1}(irep,:)*R_mat); %#ok<*MINV>
        
        % Update model probability. Feed in the forecast mean and forecast
        % variance and evaluate at the future inflation value a Normal density.
        % This density is called the predictive likelihood (or posterior
        % marginal likelihood). Call this f_l, and use that to update model
        % weight/probability called w_t
        variance{k,1}(irep,:) = V_t{k,1}(:,irep) + xRx2;   % This is the forecast variance of each model
        if variance{k,1}(irep,:)<=0  % Sometimes, the x[t]*R[t]*x[t]' quantity might be negative
            variance{k,1}(irep,:) = abs(variance{k,1}(irep,:));
        end
        mean = x_t{1,1}(irep,:)*theta_pred{k,1};  % This is the forecast mean
        f_l = (1/sqrt(2*pi*variance{k,1}(irep,:)))*exp(-.5*(((y_t(irep,:) - mean)^2)/variance{k,1}(irep,:))); %normpdf(y_t(irep,:),mean,sqrt(variance));
        w_t{k,1}(:,irep) = prob_pred(irep,k)*f_l;
        
        % Calculate log predictive likelihood for each model
        log_PL(k,1) = log(f_l + offset);
    end % end cycling through all possible K models
    
    % First calculate the denominator of Equation (16) (the sum of the w's)
    sum_w_t = 0;
    for k_2=1:K %#ok<*BDSCI>
        sum_w_t = sum_w_t + w_t{k_2,1}(:,irep);
    end
    
    % Then calculate the updated model probabilities
    for k_3 = 1:K
        prob_update(irep,k_3) = (w_t{k_3,1}(:,irep) + offset)./(sum_w_t + offset);  % this is Equation (16)
    end
    
    % Now we have the predictions for each model & the associated model
    % probabilities: Do DMA forecasting
    for k_4 = 1:K
        model_i_weight = prob_pred(irep,k_4);
        % The next temp_XXX calculate individual model quantities, weighted
        % by their model probabilities. Then take the sum of all these.
        temp_pred = y_t_pred_h{k_4,1}(irep,:)*model_i_weight;
        temp_var = variance{k_4,1}(irep,:)*model_i_weight;
        temp_logPL = log_PL(k_4,1)*model_i_weight;
        y_t_DMA(irep,:) = y_t_DMA(irep,:) + temp_pred;  % This is the mean DMA forecast
        var_DMA(irep,:) = var_DMA(irep,:) + temp_var;   % This is the variance of the DMA forecast
        log_PL_DMA(irep,:) = log_PL_DMA(irep,:) + temp_logPL;  % This is the DMA Predictive Likelihood
    end
    
    % Get log_PL_BEST here (cannot get it after the main loop is finished, like with y_t_BEST)
    [temp_max_prob temp_best_model] = max(prob_update(irep,:));
    log_PL_BEST(irep,:) = log_PL(temp_best_model,:);
end
%***********************************************************
% Find now the best models 
max_prob = zeros(T,1);
best_model = zeros(T,1);
for ii=1:T
    [max_prob(ii,1) best_model(ii,1)]=max(prob_pred(ii,:));
end

% ...and make the prediction based on the best model
for ii=1:T
    y_t_BEST(ii,1) = y_t_pred_h{best_model(ii,1),1}(ii,:);
    var_BEST(ii,1) = variance{best_model(ii,1),1}(ii,:);
end

 
% Print some directions for the user to know which variable is which
clc;
disp('End of estimation')
toc;
disp('  ')
disp('If you want to plot the probs, use the command: plot(yearlab,prob_update(:,K)')
disp('If you want to plot the regression coefficients, use the command: CHECK END OF THE CODE....')
disp('where "K" is the model number (1 to 2047, if you are using 11 predictors!)')
disp('Please check which variables are used in each model K using the command: Xnames(index{K,1})')
disp('For a specific choice of K, this gives the names of the variables of z_t included in the exogenous regressors.')
disp('  ')
disp('DMA predictions are in the vector "y_t_DMA"')
disp('Best model predictions (i.e. best at each time "t", not overall) are in the vector "y_t_BEST"')
disp('These are directly comparable with the actual observations vector "y_t"') 
disp('Hence: abs(y_t - y_t_DMA) , gives the mean absolute deviation')
disp('  ')
disp('Maximum probability of a single model at each point in time is in the vector "max_prob"')
disp('The index of the best single model at each point in time is in the vector "best_model"')
disp('Hence you may want to plot the "probabilities" and "theta" parameters for the "Ks" in the vector "best_model"')
disp('instead of using an arbitrary value for K. Also the command plot(yearlab,best_model) will give you an idea of how the')
disp('probabilities move at each point in time and which is the best model for each "t"')
disp('  ')
disp('Have fun')


%--------------------------------------------| PLOTS |----------------------------------------------------
% Make probability plots of restricted variables
if isempty(max(index{K,1}))==1
    firstX = 1;
    lastX = size(z_t,2);
else
    firstX = max(index{K,1}) + 1;
    lastX = max(index{K,1}) + size(z_t,2);
end
% 1. If you want to plot all probabilties in one big plot use this code
figure2 = figure('PaperSize',[20 30],'Color',[1 1 0]);
for index_variable=firstX:lastX
    g_index=[];
    for ii=1:K
        ddd = sum(index{ii,1}==index_variable);
        if ddd==1
            g_index = [g_index ; ii]; %#ok<*AGROW>
        end
    end
    subplot(5,3,index_variable)
    ticks=0:20:size(yearlab,1)-1;
    ticklabels=yearlab(ticks+1);
    prob_variable = sum(squeeze(prob_update(:,g_index))'); %#ok<*UDIM>
    plot([1:size(yearlab,1)],prob_variable,'LineWidth',2,'DisplayName','\pi_{t}') %#ok<NBRAK>
    set(gca,'XTick',ticks)
    set(gca,'XTickLabel',ticklabels,'FontSize',8)
    title({['Time-varying probability of inclusion of variable ' cell2mat(Xnames(index_variable))]},'FontSize',8);
end

% 2. If you want to plot the probability of each variable in a different 
%                                        graph every time, use this code: 

for ii = firstX:lastX
    prob_plot(ii)
end

%save(sprintf('%s_%g_%g_%g_%g.mat','CPIAUCSL',plag,lambda,alpha,d),'-mat');
%pause(20);

%======================FORECAST STATISTICS=================================
MAFE_DMA=abs(y_t(h_fore+1:T)-y_t_DMA(1:T-h_fore));
MSFE_DMA=(y_t(h_fore+1:T)-y_t_DMA(1:T-h_fore)).^2;
BIAS_DMA=(y_t(h_fore+1:T)-y_t_DMA(1:T-h_fore));
disp('MAFE DMA    MSFE DMA       BIAS DMA' )
disp([sum(MAFE_DMA(t0+1:end)) sqrt(sum(MSFE_DMA(t0+1:end))) sum(BIAS_DMA(t0+1:end))])

MAFE_DMS=abs(y_t(h_fore+1:T)-y_t_BEST(1:T-h_fore));
MSFE_DMS=(y_t(h_fore+1:T)-y_t_BEST(1:T-h_fore)).^2;
BIAS_DMS=(y_t(h_fore+1:T)-y_t_BEST(1:T-h_fore));
disp('MAFE DMS    MSFE DMS       BIAS DMS' )
disp([sum(MAFE_DMS(t0+1:end)) sqrt(sum(MSFE_DMS(t0+1:end))) sum(BIAS_DMS(t0+1:end))])



