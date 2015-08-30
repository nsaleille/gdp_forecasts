
% create vector with names of variables
namesARY = cell(plag,1);
for ii = 1:plag
    ff = 'ARY_{t-1}';
    fg = strrep(ff, '1', num2str(h_fore+ii-1));
    namesARY{ii,1} =  fg;
end

namesARX = cell(h*(hlag+1),1);
for ii = 1:hlag+1
    for j = 1:h   
        namesARX{(ii-1)*h + j,1} = [namesX{j,1} '_{t-'  num2str(h_fore+ii-1) '}'];
    end
end
if intercept == 1
    Xnames = ['intercept'; namesARY ; namesARX ]; %This vector contains the names of the variables in x_tt
elseif intercept == 0
    Xnames = [namesARY ; namesARX ]; %This vector contains the names of the variables in x_tt
end