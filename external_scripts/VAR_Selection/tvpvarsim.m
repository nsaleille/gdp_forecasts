function [x,b] = tvpvarsim()
%TVPVARSIM Summary of this function goes here
%   Detailed explanation goes here
if nargin==0
    T=100;
    L=1;
    N=4;
    m=L*N*N;
    %Qchol = 0.01*eye(m);
%     sigma_sd = [1.0 0.2 0.6;
%                 0.2 1.25 0.50;
%                 0.6 0.50 1.50;];
            
            
   sigma_sd = [1.0000   -0.5000   -0.2500   -0.1250;
              -0.5000    1.2500   -0.3750   -0.1875;
              -0.2500   -0.3750    1.3125   -0.3437;
              -0.1250   -0.1875   -0.3437    1.3281];
end

b1 = [0.7 0   0.35 0   0.75 ;
      0   0.7 0    0   0    ;
      0   0.65 0.7 0   0.75 ;
      0.4 0    0    0.7 0    ;
      0   0    0    0   0.8  ;];
b1 = b1(1:N,1:N);
b0 = b1(:);
b=zeros(m,T);
Qchol = diag(0.01*round(b0));
for i = 1:T+L      
    if i==1            
        b(:,i) = b0 + Qchol*randn(m,1);
    else
        b(:,i) = b(:,i-1) + Qchol*randn(m,1);
    end
end

y = [rand(L,N); zeros(T,N)];
for i = L+1:T+L
    y(i,:) = y(i-1,:)*reshape(b(1:N*N,i),N,N) + randn(1,N)*chol(sigma_sd);
end

x=y(L+1:T,:);

end