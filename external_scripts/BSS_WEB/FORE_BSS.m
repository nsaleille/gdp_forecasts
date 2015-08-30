% FORE_BSS.m: Bayesian Semiparametric Selection for Highly Correlated Data
%**************************************************************************
% The model is:
%                 y = a + Z*b + e
% The prior on b is:
%         b~G
%         G~DP(al0 Go)
%         G0~p(0)+(1-p)*N(miu,s2)
%
%         al0~Gamma
%         p~Beta
%         s2~inverseGamma
%         miu~Normal
%
%**************************************************************************
% Written by Dimitris Korobilis
% University of Glasgow
% First version: 02 April, 2012
%
% Notes: Based on Richard MacLehose's code for Epidemiology paper.
%        I want to thank Richard for sharing his code!
%**************************************************************************

% Reset everything, clear memory and screen
clear all;
clc;
% Reset the random seed
RandStream.setDefaultStream(RandStream('mt19937ar','seed',sum(100*clock)));
% Start clock
tic;
% Add path of random number generators
addpath('functions')
addpath('data')

% ===========================| USER INPUT |================================
% Gibbs related preliminaries
nrep = 150000;           % Gibbs draws to keep
nburn = 50000;           % Burn in phase
ntot = nrep + nburn;     % Total draws
it_print = 500;          % Print every "it_print"-th iteration

lags = 2;                % Number of AR lags

% Select which is the dependent variable
select_variable = 76;    %  76: CPI Inflation
                         % 124: GDP Growth
% Select forecast horizon
nfore = 1;
fore_type = 0;           % 0: Recursive, 1: Rolling
% ===========================| LOAD DATA |=================================
load data.dat;
load tcode.dat;
load varnames.mat;

Yraw_full = yfcsta(data(:,select_variable),tcode(select_variable),nfore);
Yraw_full_lags = data(:,select_variable);
Xraw = data;
Xraw(:,select_variable) = []; 

% Now correct sizes
Yraw_full = Yraw_full(1:end-nfore,:);
Yraw_full_lags = Yraw_full_lags(1:end-nfore,:);
Xraw = Xraw(1:end-nfore,:);

% Create RHS variables, x
ylag = mlag2(Yraw_full_lags,lags-1);  % This routine creates the lagged dep variables   
if lags>0
    Zraw = [ones(size(ylag(lags:end,:),1),1) Yraw_full_lags(lags:end,:) ylag(lags:end,:) Xraw(lags:end,:)];
    Yraw = Yraw_full(lags:end,:);
elseif lags==0
    Zraw = [ones(size(ylag(1:end,:),1),1) Xraw(1:end,:)];
    Yraw = Yraw_full(1:end,:);
end

% Start with  first_per% of observations for first period forecast
first_per = 0.50;
T_full = size(Yraw,1)-1;
T_thres = round(first_per*T_full);
anumber = T_full-T_thres-nfore+1;

ej = zeros(anumber,nrep);
e0 = zeros(anumber,nrep);
temp_MAE = zeros(anumber,1);
temp_MSEj = zeros(anumber,1);
temp_MSE0 = zeros(anumber,1);
temp_PL = zeros(anumber,1); 

for sample = T_thres:T_full-nfore
    disp(['Now you are running sample ' num2str(sample) ' of ' num2str(T_full-nfore)] )
    disp(['Variable you are using is ' char(varnames(select_variable)) ' which is number ' num2str(select_variable)])

    if fore_type == 0 % Recursive forecasts
        roll = 1;
    elseif fore_type == 1 % Rolling forecasts   
        roll = sample-59; 
    end
    
    %In-sample observations
    y = Yraw(roll:sample,:);
    Z = Zraw(roll:sample,2:2+lags-1);
    x = Zraw(roll:sample,2+lags:end);
    
%     % Standardize data
%     std_y = std(y);
%     y = y./repmat(std_y,size(y,1),1);
    
    % Full right hand side matrix of predictors
    X_fore = Zraw(roll:sample+nfore,:);
    X_fore2 = X_fore; X_fore2(:,2+lags:end) = standardize(X_fore2(:,2+lags:end));
    X = Zraw(roll:sample,:);
    X(:,2+lags:end) = standardize(X(:,2+lags:end));
    
    T = size(y,1);
    lx=size(x,2);
    lX=size(X,2);
    lz=size(Z,2);
    zint=lz+1;
    
    %Out-of-sample observations
    Yraw_fore = Yraw(sample+nfore,:);

    % ========| PRIOR:
    % Choose hyperparameters -> 
    k0 = 0;              %Prior mean for intercept
    Pk = 1/10;           %Prior Precision for Constant
    mu0 = 0;             %Prior mean for Betas
    P0 = 1/1;            %Prior Precision for Betas
    %Prior for lambda ~ Gamma(a1,a2)
    a1 = 1; 
    a2 = 2;
    al0 = 1;
    %Prior for pi ~ Beta(c1,c2)
    c1 = 1;
    c2 = 1;
    pi0 = 0.5;           % probability of null cluster membership for gamma
    %Prior for phi ~ iGamma(b1,b2)
    b1 = .1;
    b2 = .1;
    % Prior on variance sigma2 ~ iGamma(a, b)
    nu1 = 1;
    nu2 = .01;
    % Prior on all unrestricted coefficients
    pmk = 0;              %Prior mean for variables in Z
    ppk = 1/4;            %Prior precision for variables in Z
    kappapm = [k0,pmk*ones(1,lz)]';
    kappapp = [Pk,ppk*ones(1,lz)]';
    kappapv = 1./kappapp;

    % ========| INITIALIZE COEFFICIENTS AND STORAGE SPACE
    % initialize unknowns related to clustering 
    kG = 1;                        % number of clusters in gamma
    Gam = 0;                       % vector of unique values of gamma
    Tht = [log(mean(y)/(1-mean(y))),Gam(2:kG)']'; %#ok<NASGU> % regression parameters in current configuration
    Sg = ones(lx,1);
    gam = Gam(Sg);
    Sgsum=(repmat(Sg(:,1),[1 kG])-repmat(1:kG,[lx 1])==0);
    pg = ones(1,lx)*Sgsum;   
    Xt = zeros(T,zint);                    % define modified design matrix
    Xt(:,1:zint) = X(:,1:zint);                      % intercept
    for m = 1:kG-1
        Xt(:,zint+m) = X(:,zint+1:lX)*(Sg==m);  
    end
    zint=lz+1;                     %length of conf and intercepts
    kappa=zeros(1,zint);         %inits intercept and Z regressors
    Tht = [kappa, Gam(2:kG)]';

    %===========================| STORAGE SPACE FOR POSTERIORS & STUFF
    y_AR2draws = zeros(nrep,1);
    y_foredraws = zeros(nrep,1);
    PLdraws = zeros(nrep,1);
    
    aoutdp2=zeros(nrep,1); 
    pioutdp2=zeros(nrep,1);
    sgoutdp2=zeros(nrep,lx);
    kGoutdp2=zeros(nrep,1);
    goutdp2=zeros(nrep,lX);  
    poutdp2=zeros(nrep,1);
    sigmaoutdp2=zeros(nrep,1);
    
    %==========================================================================
    %==========================| GIBBS SAMPLER |===============================
    for irep = 1:ntot    % GIBBS iterations start here
        % Print iterations      
        if mod(irep,it_print) == 0
            disp(irep);toc;
        end
        
        % ----STEP 1: Sample variance sigma2
        err = y - Xt*Tht;
        S_1 = (nu1 + T)/2;
        S_2 = (nu2 + err'*err)/2;
        isigma2 = gamrnd(S_1,1./S_2);
        sigma2 = 1./isigma2;

        % ----STEP 2: Sample parameters conditional on configuration into clusters       
        Tht0 = [kappapm' mu0*ones(1,kG-1)]';
        PTht0 = diag([kappapp', P0*ones(1,kG-1)]);
        VTht = inv(PTht0 + Xt'*Xt*isigma2);
        ETht = VTht*(PTht0*Tht0 + (Xt'*y*isigma2));  %#ok<MINV>
        Tht = mvnrnd(ETht,VTht,1)';
        Gam = [0  Tht(zint+1:zint+kG-1)']';
        gam = Gam(Sg); 
  
        % ----STEP 3: Sample configuration of coefficients into clusters
        % a) Update weights of each component
        A = zeros(lx,kG+1);  % first element is new cluster, 2nd is zero cluster
        A(:,1) = (1-pi0)*(al0./(al0+lx-pg(1)+(Sg==1)-1)); 
        A(:,2) = pi0*ones(lx,1); 
        for h = 3:kG+1
            A(:,h) = (1-pi0)*(pg(h-1)-(Sg==h-1))./(al0+lx-pg(1)+(Sg==1)-1); 
        end
        % b) Update mean and variance for new components and obtain conditional probs
        Vg = zeros(lx,1);
        Eg = zeros(lx,1); 
        for h = 1:lx
            ysh = y - Xt*Tht + X(:,h+1)*gam(h); %y - X*[Tht(1);gam] + X(:,h+1)*gam(h);
            Vg(h) = 1/(P0 + X(:,h+1)'*X(:,h+1)*isigma2); 
            Eg(h) = Vg(h)*(P0*mu0 + X(:,h+1)'*ysh*isigma2); 
            A(h,1) = log(A(h,1)) + 0.5*(log(Vg(h)) + log(P0) + Eg(h)^2/Vg(h) - P0*mu0^2);  
            for l = 1:kG
                A(h,l+1) = log(A(h,l+1)) + isigma2*( (X(:,h+1)*Gam(l))'*ysh - ((X(:,h+1)*Gam(l))'*(X(:,h+1)*Gam(l)))/2 ); 
            end
            A(h,:) = exp(A(h,:) - max(A(h,:)));    % To avoid computational problems
        end
        A = A./repmat(A*ones(kG+1,1),[1 kG+1]);   % normalize each row to add to one

        % c) Update configuration for Gamma
        uni = unifrnd(0,1,[lx 1]); 
        i1 = uni<A(:,1);
        if sum(i1)>0
            gam(i1) = normrnd(Eg(i1),Vg(i1).^(0.5)); 
        end
        % if one of existing clusters assign appropriate value
        for l = 1:kG
            gam(uni>A(:,1:l)*ones(l,1) & uni<=A(:,1:l+1)*ones(l+1,1)) = Gam(l); 
        end
        % d) Redefine Gam, Sg, kG, pg appropriately
        Gam = [0 unique(gam(gam~=0))']';
        kG = length(Gam); 
        for l = 1:kG
            Sg(gam==Gam(l))=l;
        end
        Sgsum=(repmat(Sg(:,1),[1 kG])-repmat(1:kG,[lx 1])==0);
        pg = ones(1,lx)*Sgsum;
        Xt = zeros(T,kG);                    % define modified design matrix
        Xt(:,1:zint) = X(:,1:zint);                      % intercept
        for m = 1:kG-1       
            Xt(:,m+zint) = X(:,zint+1:lX)*(Sg==m+1);
        end
        Tht = [Tht(1:zint)' Gam(2:kG)']';
    
        % ----STEP 4: Sample point mass probability
        pi0 = betarnd(c1 + sum(Sg==1),c2 + sum(Sg~=1)) - 1e-04;

        % ----STEP 5: Sample DP precision
        if pg(1)<lx       
            xi = betarnd(al0+1,lx-pg(1));
            pxi = (a1 + kG - (pg(1)>0) - 1)/(a1 + kG - (pg(1)>0) - 1 + (lx-pg(1))*(a2 - log(xi))); 
            al0 = pxi*gamrnd(a1 + kG - (pg(1)>0),1/(a2-log(xi))) + ... 
                (1-pxi)*gamrnd(a1 + kG - (pg(1)>0) - 1,1/(a2-log(xi)));
        else            
            al0 = gamrnd(a1,1/a2);
        end
                
        % ----STEP 6: Sample precision in base distribution
        if kG>1
            ga=b1+(kG-1)/2;
            gb=((Gam(2:kG)-mu0*ones(1,kG-1)')'*(Gam(2:kG)-mu0*ones(1,kG-1)'))/2+1/b2;
            P0=gamrnd(ga,1/gb);
        end
        
        %     % ----STEP 7: Sample mean in base distribution
        %     if kG>1
        %         var_mu0 = inv(0.1 + 1./P0);
        %         mean_mu0 = var_mu0*(sum(Gam(2:kG))./P0); 
        %         mu0 = mean_mu0 + sqrt(var_mu0)*randn;
        %     end
    
        
        if irep > nburn
            % Save post burn-in draws
            goutdp2(irep-nburn,:) = [Tht(1:1+lags);gam]';
            aoutdp2(irep-nburn,:) = al0;
            pioutdp2(irep-nburn,:) = pi0;
            sgoutdp2(irep-nburn,:) = Sg;
            kGoutdp2(irep-nburn,:) = kG;      
            poutdp2(irep-nburn,:) = P0;
            sigmaoutdp2(irep-nburn,:) = sigma2;
            
            
            % ----------------| Forecasting             
            % Benchmark AR2 model
            XX = X(:,1:1+lags);
            beta_OLS = inv(XX'*XX)*(XX'*y);
            sigma_OLS = (y-XX*beta_OLS)'*(y-XX*beta_OLS)./(T-3);
            y_fore_ARp_OLS = X_fore2(end,1:1+lags)*beta_OLS + sqrt(sigma_OLS)*randn;
   
            % Bayesian selection model
            y_fore = X_fore2(end,:)*[Tht(1:1+lags); gam] + sqrt(sigma2)*randn;
            PL = normpdf(Yraw_fore,y_fore,sigma2);
            
            y_AR2draws(irep-nburn,:) = y_fore_ARp_OLS;
            y_foredraws(irep-nburn,:) = y_fore;
            PLdraws(irep-nburn,:) = PL;            
        end
    end    
    %===========================| END SAMPLER |================================
    %==========================================================================
%     s1 = median(sgoutdp2);
%     s2 = min(s1);
%     s1(s1==s2)=0;
%     s1(s1~=0)=1;
%     s3 = [1 ones(1,lags) s1];
%     beta_post_m = mean(goutdp2).*s3;
%     median_forecast = X_fore2(end,:)*beta_post_m';
   
    % Get posterior predictive median
    med_forecast = median(y_foredraws,1);
    med_AR2 = median(y_AR2draws,1);
    
    % Get forecast error for BSS and benchmark model
    FEj = med_forecast - Yraw_fore;    
    FE0 = med_AR2 - Yraw_fore;
    
    % Now calculate the statistics of interest
    ej(sample-T_thres+1,:) = y_foredraws - Yraw_fore;
    e0(sample-T_thres+1,:) = y_AR2draws - Yraw_fore;
    temp_MAE(sample-T_thres+1,:) = abs(FE0) - abs(FEj);
    temp_MSEj(sample-T_thres+1,:) = (FEj).^2;
    temp_MSE0(sample-T_thres+1,:) = (FE0).^2;
    temp_PL(sample-T_thres+1,:)   = mean(PLdraws,1);
end

MSEj = mean(temp_MSEj);
MSE0 = mean(temp_MSE0);
di = e0 - ej;
d_bar_full = mean(di,1);
d_bar_mean = mean(d_bar_full);
d_bar_se = std(d_bar_full);
N = anumber;

% Calculate forecast statistics
R_sq = 1 - MSEj/MSE0;
DMAE = mean(temp_MAE);
DRMSE = sqrt(MSE0) - sqrt(MSEj);
MSE_T = sqrt((N-1)/N)*(d_bar_mean/d_bar_se);
MSE_F = N*((MSE0 - MSEj)/MSEj);
ENC = sum(mean(e0,2).^2 - mean(e0,2)'*mean(ej,2))./MSEj;


model='BSS';
save(sprintf('%s_%g_%g_%g.mat',model,nfore,fore_type,lags),'-mat');