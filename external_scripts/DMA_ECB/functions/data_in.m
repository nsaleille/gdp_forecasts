
%first load the dependent variable (inflation)
load HICP_SA.dat;
%and then the explanatory variables, names, year and transforation indexes
load xdata.dat;
load namesX.mat;
load tcode1.dat;
load tcode2.dat;
load sample_begin.dat;

load otherdata.dat;
load namesOTHER.mat;
load tcode_other.dat;
load sample_begin_other.dat;
load yearlab.mat;

% ------------Transform data to be approximately stationary
% Choose which stationarity transformation you want to apply
if stationarity == 1
    tcode = tcode1;
    correct = 2; % we lose 2 observations using these transformations
elseif stationarity == 2
    tcode = tcode2;
    correct = 1;   % we lose 1 observation using these transformations
elseif stationarity == 3
    tcode = round(tcode2./tcode2);
    correct = 0;
    if use_other == 1
        correct = 1;
    end
end

% Transform monetary indicators
xtempraw = zeros(size(xdata,1),size(xdata,2));
for i_x = 1:size(xdata,2)
    if tcode(i_x)==5 || tcode(i_x)==6
        xtempraw(sample_begin(i_x):end,i_x) = 4*transx(xdata(sample_begin(i_x):end,i_x),tcode(i_x)); %#ok<*SAGROW>
    else
        xtempraw(sample_begin(i_x):end,i_x) = transx(xdata(sample_begin(i_x):end,i_x),tcode(i_x));
    end
end

% Transfomr non-monetary indicators of inflation (unemployment,forex rate etc)
othertempraw = zeros(size(otherdata,1),size(otherdata,2));
tcode_other = round(tcode_other./tcode_other);
for i_other = 1:size(otherdata,2)
    if tcode_other(i_other)==5 || tcode_other(i_other)==6
        othertempraw(sample_begin_other(i_other):end,i_other) = 4*transx(otherdata(sample_begin_other(i_other):end,i_other),tcode_other(i_other)); %#ok<*SAGROW>
    else
        othertempraw(sample_begin_other(i_other):end,i_other) = transx(otherdata(sample_begin_other(i_other):end,i_other),tcode_other(i_other));
    end
end

% Transform Inflation
% 1. Make the transformation that will be used to obtain lags pi[t],pi[t-1],...,pi[t-plag+1]
HICP_SA_4lags = 4*transx(HICP_SA,5);
% 2. Make the transformation that will be used to create the dependent
% variable pi[t+h]
HICP_SA_4dependent = (4./h_fore)*yfcst(HICP_SA,5,h_fore);

% Finish by correcting the time series observations after stationarity
% transformations
xtempraw = xtempraw(correct+1:end,:);
othertempraw = othertempraw(correct+1:end,:);
HICP_SA_4lags = HICP_SA_4lags(correct+1:end,:);
HICP_SA_4dependent = HICP_SA_4dependent(correct+1:end,:);
yearlab = yearlab(correct+1:end);


% % Correct for the fact HICP_SA_4depdendent is forwarded h_fore periods ahead (i.e.
% % we lose h_fore observations)
% xtempraw = xtempraw(1:end-h_fore,:);
% othertempraw = othertempraw(1:end-h_fore,:);
% HICP_SA_4lags = HICP_SA_4lags(1:end-h_fore,:);
% HICP_SA_4dependent = HICP_SA_4dependent(1:end-h_fore,:);
% yearlab = yearlabe(1:end-h_fore);
%----------------------------

% ------------Now select which series and which observations to use
% Define here which of the many monetary variables in xdata will be used inforecasting inflation
if use_x == 1
    Xindex = (1:size(xdata,2)); % use all monetary indicators
elseif use_x == 2
    Xindex = [1 2 3 4 5 6 7];   % use only "standard measures"
elseif use_x == 3
    Xindex = [8 9 10 11 12];    % use only "measures of underlying monetary expansion"
elseif use_x == 4
    Xindex = [];    % do not use any monetary indicator
end

% Deal with missing values
if miss_treatment == 1      % Fill missing values with zeros (already done)
elseif miss_treatment == 2  % Begin at the earliest common observation among the selected variables 
    tt = max(sample_begin(Xindex));
    xtempraw = xtempraw(tt:end,:);
    othertempraw = othertempraw(tt:end,:);
    HICP_SA_4lags = HICP_SA_4lags(tt:end,:);
    HICP_SA_4dependent = HICP_SA_4dependent(tt:end,:);
    yearlab = yearlab(tt:end);
end

% Y is GDP deflator. X names are in matrix 'namesX'
% Note that we loose 1 obs because of stationarity transformations
Y = HICP_SA_4dependent(:,1);
if use_other == 0
    X = xtempraw(:,Xindex);
    namesX = namesX(Xindex);
elseif use_other == 1
    OTHERindex = [1 3 4 6];
    X = [othertempraw(:,OTHERindex) xtempraw(:,Xindex)];
    namesX = [namesOTHER(OTHERindex) ; namesX(Xindex)];
end
%----------------------------

% ------------Now create lags, set of un-/restrited variables and finalize the model specification
% Initial time-series observations (we will lose some later when we take lags)
T = size(Y,1);

% Number of exogenous predictors
h = size(X,2);
 
% Number of lags:
LAGS = max(plag,hlag);

% Generate lagged Y matrix
% ylag = zeros(size(HICP_SA_4lags,1),plag);
% for ii=0:plag-1
%     ylag(plag:end,ii+1) = HICP_SA_4lags(plag-ii:end-ii,:);
% end
if plag>0
    ylag = HICP_SA_4lags;
    if plag>1   
        ylag = [ylag mlag2(HICP_SA_4lags,plag-1)];
    end
    ylag = ylag(LAGS+1:end,:);
else
    ylag = [];
end
% Generate lagged X matrix.
xlag = mlag2(X,hlag);
xlag = xlag(LAGS+1:end,:);

% m is the number of R.H.S. variables (intercept, lags and exogenous variables)
m = 1*intercept + plag +  h*(hlag+1);
% Create matrix of RHS variables. Note that z_t has the variables to be
% restricted, and Z_t the variables which are unrestricted
if apply_dma == 1   % restrict only the exogenous variables
    z_t = [X(LAGS+1:T,:) xlag];
    if intercept == 1 
        Z_t = [ones(T-LAGS,1) ylag];
    elseif intercept == 0
        Z_t = ylag;
    end
elseif apply_dma == 2  % restrict the exogenous variables, plus the lags of the dependent
    z_t = [ylag X(LAGS+1:T,:) xlag];
    if intercept == 1 
        Z_t = ones(T-LAGS,1);
    elseif intercept == 0
        Z_t = [];
    end
elseif apply_dma == 3 % restrict all R.H.S. variables (exogenous-lags-intercept)
    z_t = [ones(T-LAGS,1) ylag X(LAGS+1:T,:) xlag];
    Z_t = [];
end

% Redefine variables y, yearlab and T (correct observations since we are taking lags)
y_t = Y(LAGS+1:end,:);
yearlab = yearlab(LAGS+1:end,:);
T=size(y_t,1);

% Correct for h_fore forecast horizon
T = T-h_fore;
yearlab = yearlab(1:end-h_fore);
