
% ======================| Form all possible model combinations |======================
% if z_t (the full model matrix of regressors) has N elements, all possible combinations 
% are (2^N - 1), i.e. 2^N minus the model with all predictors/constant excluded (y_t = error)
N = size(z_t,2);
comb = cell(N,1);
for nn = 1:N
    % 'comb' has N cells with all possible combinations for each N
    comb{nn,1} = combntns(1:N,nn);
end

K = round(2.^N);     %Total number of models

% Now create a variable indexing all possible models using only z_t (the
% restricted variables matrix)
index_z_t = cell(K,1);
dim = zeros(1,N+1);
for nn=1:N
    dim(:,nn+1) = size(comb{nn,1},1);
    for jj=1:size(comb{nn,1},1)
        % Take all possible combinations from variable 'comb' and sort them
        % in each row of 'index'. Index now has a vector in each K row that
        % indexes the variables to be used, i.e. for N==3:
        % index = {[1] ; [2] ; [3] ; [1 2] ; [1 3] ; [2 3] ; [1 2 3]}
        index_z_t{jj + sum(dim(:,1:nn)),1} = comb{nn,1}(jj,:);
    end
end

% x_t = cell(K,1);
% for ll2 = 1:K
%     % Now also add the variables in Z_t that are always included in each
%     % model (i.e. no DMA for the variables defined in Z_t, as opposed to
%     % the variables in z_t)
%     x_t{ll2,1} = [Z_t z_t(:,index_z_t{ll2,1}')];
% end

% Now create a big index variable for all possible model combinations using x_t
% which contains both the unrestricted and restricted variables 
index = cell(K,1);
if apply_dma == 1
    if intercept == 1   
        for iii=1:K
            index{iii,1} = [ 1:plag+1 (index_z_t{iii,1} + plag + 1) ];   
        end
    elseif intercept == 0
        for iii=1:K
            index{iii,1} = [ 1:plag (index_z_t{iii,1} + plag) ];   
        end
    end
elseif apply_dma == 2
    if intercept == 1 
        for iii=1:K
            index{iii,1} = [ 1 (index_z_t{iii,1} + 1) ];            
        end
    elseif intercept == 0
        for iii=1:K
            index{iii,1} = (index_z_t{iii,1});            
        end
    end
elseif apply_dma == 3
    index = index_z_t; 
end