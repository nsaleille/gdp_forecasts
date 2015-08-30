
% A simple function to evaluate whether user input is correct
% For example, we do not want a lag length which is not a positive integer,
% or a forgetting factor which is not in the range [0,1].

if intercept~=0 && intercept~=1
    errordlg('Wrong specification of the intercept.Please choose 0/1.','Bad input','modal')
    error('Wrong input, I have to stop')
end

if plag < 0 || mod(plag,1)~=0
    errordlg('Lag-length of the dependent variable (plag) must be a positive integer.','Bad input','modal')
    error('Wrong input, I have to stop')
end

if hlag < 0 || mod(hlag,1)~=0
    errordlg('Lag-length of the exogenous variables (hlag) must be a positive integer.','Bad input','modal')
    error('Wrong input, I have to stop')
end

if stationarity~=1 && stationarity~=2 && stationarity~=3 && stationarity~=4
    errordlg('Wrong specification of stationarity. Please choose 1, 2, 3 or 4','Bad input','modal')
    error('Wrong input, I have to stop')
end

if use_x~=1 && use_x~=2 && use_x~=3 && use_x~=4
    errordlg('Wrong specification of use_x. Please choose 1, 2, 3 or 4','Bad input','modal')
    error('Wrong input, I have to stop')
end

if use_other~=0 && use_other~=1
    errordlg('Wrong specification of use_other. Please choose 0 or 1','Bad input','modal')
    error('Wrong input, I have to stop')
end

if miss_treatment~=1 && miss_treatment~=2 && miss_treatment~=3
    errordlg('Wrong specification of miss_treatment. Please choose 1, 2 or 3','Bad input','modal')
    error('Wrong input, I have to stop')
end

if apply_dma~=1 && apply_dma~=2 && apply_dma~=3
    errordlg('Wrong specification of apply_dma. Please choose 1, 2 or 3','Bad input','modal')
    error('Wrong input, I have to stop')
end

if apply_dma == 3
    if intercept ~= 1
        errordlg('Cannot set apply_dma = 3, when no intercept is defined in the model!','Bad input','modal')
        error('Wrong input, I have to stop')
    end
end

if apply_dma == 2
    if plag == 0
        errordlg('Cannot set apply_dma = 2, when no lags are defined in the model!','Bad input','modal')
        error('Wrong input, I have to stop')
    end
end

if lambda < 0 || lambda > 1
    errordlg('The forgetting factors must take values between 0 and 1','Bad input','modal')
    error('Wrong input, I have to stop')
end

if alpha < 0 || alpha > 1
    errordlg('The forgetting factors must take values between 0 and 1','Bad input','modal')
    error('Wrong input, I have to stop')
end

if kappa < 0 || kappa > 1
    errordlg('The forgetting factors must take values between 0 and 1','Bad input','modal')
    error('Wrong input, I have to stop')
end

if forgetting_method~=1 && forgetting_method~=2
    errordlg('The forgetting method must be either 1 or 2','Bad input','modal')
    error('Wrong input, I have to stop')
end

if prior_theta~=1 && prior_theta~=2
    errordlg('Wrong specification of prior_theta. Please choose 1 or 2','Bad input','modal')
    error('Wrong input, I have to stop')
end

if initial_V_0 ~=1 && initial_V_0 ~=2
    errordlg('Wrong initialization of V_{0}','Bad input','modal')
    error('Wrong input, I have to stop')
end

if initial_DMA_weights ~= 1
    errordlg('Wrong initialization DMA weights','Bad input','modal')
    error('Wrong input, I have to stop')
end

if expert_opinion ~= 1 && expert_opinion ~= 2
    errordlg('Wrong initialization of expert opinion','Bad input','modal')
    error('Wrong input, I have to stop')
end
 
if h_fore<=0
    errordlg('Wrong selection of forecast horizon','Bad input','modal')
    error('Wrong input, I have to stop')
end
