% Bayesian model averaging using g-priors
%**************************************************************************
% The model is:
%                 y = a + Z*b + e,
% where e~N(0,s2).
% The prior on b is:
%         b[j] ~ N( 0,s2 * g * (Z[j]'Z[j])^-1 ) 
%
%**************************************************************************
% Written by Dimitris Korobilis
% University of Glasgow
% First version: 26 March, 2012
% 
% Notes: Code to replicate FLS (2001), JAE, initially written by Justin
% Tobias for Koop, Poirier and Tobias (2007), Bayesian Econometric
% Exercises
%**************************************************************************

% Reset everything, clear memory and screen
clear all;
clc;
% Reset the random seed
RandStream.setDefaultStream(RandStream('mt19937ar','seed',sum(100*clock)));
% Start clock
tic;
% Add path of random number generators
%addpath('generators')

% ===========================| USER INPUT |================================
% Gibbs related preliminaries
nrep = 1500;           % Gibbs draws to keep
nburn = 500;           % Burn in phase
ntot = nrep + nburn;   % Total draws
it_print = 5000;       % Print every "it_print"-th iteration

prior = 2;             % 1. g = 1/n
                       % 2. g = 1/k^2
% Select which is the dependent variable
select_variable = 1;   % CPI Inflation

% Select forecast horizon
nfore = 24;           
% ===========================| LOAD DATA |=================================
load data.dat;
varnames = {'CPIAUCSL';'FEDFUNDS';'BOGNONBR';'M2SL';'INDPRO';'UTL11';'UNRATE';'HOUST';...
    'PPIFCG';'AHEMAN';'M1SL';'PMCP';'SP500';'GS10';'EXUSUK';'PAYEMS';'NAPMNOI';'TB3MS';...
    'BUSLOANS';'TOTALSL';'AAA';'INVEST';'SAVINGSL';'LOANS';'PSCCOM';'NAPM';'BORROW'};

% Define dependent variable and predictor variables
y = data(1+nfore:end,select_variable);
data(:,select_variable) = [];
xraw = data(1:end-nfore,:);
k = size(xraw,2);
T = size(y,1);

% Standardize predictors and dependent variable
std_y = std(y);
%y = y./repmat(std_y,size(y,1),1);
xraw = standardize(xraw);

% molddraw is initialized to contain all explanatory variables
% with t-stats greater than .5
[beta_OLS,sigma2_OLS,bcov_OLS,t_stats] = ols_quantities(y,xraw,1);
molddraw = zeros(k,1);
molddraw(abs(t_stats)>0.5)=1;

% Make up the matrix of explanatory variables for model
xold = [ones(T,1) xraw(:,molddraw>0)];
kold = sum(molddraw)+1;

% Specify g0 for the g-prior
if prior == 1 % 1. g = 1/T
    g0 = 1/(k^2);
elseif prior == 2 % 2. g = 1/k^2
    g0 = 1/T;
end

yty = (y-mean(y))'*(y-mean(y));
xtxinv=inv(xold'*xold);
ymy = y'*y - y'*xold*xtxinv*xold'*y;
g1=g0/(g0+1);
g2=1/(g0+1);
lprobold = .5*kold*log(g1) -.5*(T-1)*log(g2*ymy + g1*yty);
mstart=molddraw;
lprobstart=lprobold;
inccount=zeros(k,1);
msize=0;

%I am keeping records for top 10 drawn models
%Here initialize this to initial model
top10mod = [molddraw molddraw molddraw molddraw molddraw ...
       molddraw molddraw molddraw molddraw molddraw];
lprobtop10=lprobold*ones(10,1);
top10count=zeros(10,1);

%calculate first and second moment of all coefficients
%Initialize them here
b1mo=zeros(k,1);
b2mo=zeros(k,1);

%==========================================================================
%==========================| GIBBS SAMPLER |===============================
for irep=1:ntot
    % Print iterations   
    if mod(irep,it_print) == 0
        disp(irep);toc;
    end
    
    %choose at random one of the k potential explanatory variables  
    %if it is already in the model, delete it else add it
    %Based on this, make up candidate model
    indch=round(k*rand);
    xnew=xold;
    mnewdraw=molddraw;
    if indch>0
        if molddraw(indch,1)==1
            isum=0; 
            for i=1:indch
                isum=isum+molddraw(i,1);
            end
            xnew = [xold(:,1:isum) xold(:,isum+2:kold)];
            mnewdraw(indch,1)=0;    
        else
            isum=0;
            for i=1:indch
                isum=isum+molddraw(i,1);
            end
            xnew = [xold(:,1:isum+1) xraw(:,indch) xold(:,isum+2:kold)];
            mnewdraw(indch,1)=1;
        end
    end
    
    knew=sum(mnewdraw)+1;
    xtxinv=inv(xnew'*xnew);
    ymy = y'*y - y'*xnew*xtxinv*xnew'*y;
    lprobnew = .5*knew*log(g1) -.5*(T-1)*log(g2*ymy + g1*yty); 
    
    %Now decide whether to accept candidate draw
    if log(rand) < (lprobnew - lprobold)
        xold=xnew;
        lprobold=lprobnew;
        molddraw=mnewdraw;
        kold=knew;
    end
    
    if irep>nburn
        %If new drawn model better than current top 10, add it to list  
        for i=1:10
            if lprobold>=lprobtop10(i,1)
                if sum(abs(molddraw - top10mod(:,i)))<.09
                    break
                end
                if i<10
                    lprobtop10(i+1:10,1)=lprobtop10(i:9,1);
                    top10mod(:,i+1:10) = top10mod(:,i:9);
                    top10count(i+1:10,1)=top10count(i:9,1);
                end
                lprobtop10(i,1)=lprobold;
                top10mod(:,i)=molddraw;
                top10count(i,1)=0;
                break
            end
        end
        
        for i=1:10
            temp1=sum(abs(molddraw-top10mod(:,i)));       
            if temp1<.01
                top10count(i,1)=top10count(i,1)+1;
                break
            end
        end        
        inccount = inccount + molddraw;
        msize=msize + kold;
        %calculating posterior properties of coefficients means
        %we have to write out full posterior
        Q1inv = (1+g0)*xold'*xold;
        Q0inv=g0*xold'*xold;       
        Q1=inv(Q1inv);
        b1= Q1*xold'*y;
        vs2 = (y-xold*b1)'*(y-xold*b1) + b1'*Q0inv*b1;
        bcov = (vs2/(T-2))*Q1;
        %the next bit of this is awkward, needed to find out if variable is 
        %included in the model and, if so, to find out where it is
        summer1=1;
        for i=1:k
            bc=zeros(1,kold);        
            if molddraw(i,1)==1
                summer1=summer1+1;
                bc(1,summer1)=1;
                bmean=bc*b1;
                bvar =bc*bcov*bc';          
                b1mo(i,1)=b1mo(i,1) + bmean;
                b2mo(i,1)=b2mo(i,1) + (bvar+bmean^2);
            end
        end
    end
end
%===========================| END SAMPLER |================================
%==========================================================================
inccount=inccount./nrep;
disp('proportion of models visited containing each variable')
varcount=cumsum(ones(k,1));
disp([varcount inccount])
disp('mean number of regressors in models')
disp(msize/nrep)

disp('Prob of top 10 models, analytical (after deleting rest of models)')
lt1=lprobtop10 - max(lprobtop10);
disp(exp(lt1)/sum(exp(lt1)))

disp('Prob of top 10 models, numerical (after deleting rest of models)')
disp(top10count./sum(top10count))

disp('Prob of top 10 models out of total number of models, numerical')
disp(sum(top10count)/nrep)

b1mo=b1mo./nrep;
b2mo=b2mo./nrep;
bsd=sqrt(b2mo-b1mo.^2);
disp('Posterior mean and stand dev of each coefficient')
disp([b1mo bsd])

