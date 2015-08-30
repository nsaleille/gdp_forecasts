
%-------- Now set prior means and variances and initial conditions
% theta_0 ~ N(theta_0_prmean,theta_0_prvar)
theta_0_prmean = cell(K,1);
theta_0_prvar = cell(K,1);

if prior_theta == 1 % diffuse
    for ll = 1:K
        theta_0_prmean{ll,1} = 0*ones(size(index{ll,1},2),1);
        theta_0_prvar{ll,1} =  4*eye(size(index{ll,1},2));
    end
elseif prior_theta == 2 % data-based 1
    for ll = 1:K
        x_t = cell(1,1);
        if isempty(Z_t)
            x_t{1,1} = z_t(1:t0,index_z_t{ll,1}'); %#ok<*USENS>
        else
            x_t{1,1} = [Z_t(1:t0,:) z_t(1:t0,index_z_t{ll,1}')]; %#ok<*USENS>
        end
        varx = var(x_t{1,1});
        varx(varx==0)=0.01;
        vary = var(y_t(1:t0,:));
        theta_0_prmean{ll,1} = 0*ones(size(index{ll,1},2),1);
        theta_0_prvar{ll,1} =  2*diag((vary./varx)');
    end
end

% Initial value of measurement error covariance matrix V_t
if initial_V_0 == 1
    V_0 = 1e-3;
elseif initial_V_0 == 2;
    V_0 = var(y_t(1:t0,:))./4;
end

% initial model probability for each individual model
if initial_DMA_weights == 1
    prob_0_prmean = 1./K;
end

% Define forgetting factor(s):                 
inv_lambda = 1./lambda;

% Define expert opinion (prior) model weight
if expert_opinion == 1;
    expert_weight = 1./K;
elseif expert_opinion == 2;
    expert_weight = 0;
end

if expert_weight == 0
    if forgetting_method == 2;
        expert_weight = 1;
    end
end
