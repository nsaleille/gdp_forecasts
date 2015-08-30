function prob_plot(index_variable)

global K index yearlab prob_update Xnames

% Make plots of the time-varying probability of inclusion of each variable
% First choose the number of the variable which you want to use
g_index=[];
for ii=1:K
    ddd = sum(index{ii,1}==index_variable);
    if ddd==1
        g_index = [g_index ; ii]; %#ok<*AGROW>
    end
end
figure2 = figure('PaperSize',[20 30],'Color',[1 1 0]);
ticks=0:20:size(yearlab,1);
ticklabels=yearlab(ticks+1);
axes2 = axes('Parent',figure2,'YGrid','on','XGrid','on');
prob_variable = sum(squeeze(prob_update(:,g_index))'); %#ok<*UDIM>
plot([1:size(yearlab,1)],prob_variable,'Parent',axes2,'LineWidth',2,'DisplayName','\pi_{t}') %#ok<NBRAK>
set(gca,'XTick',ticks)
set(gca,'XTickLabel',ticklabels)
title({['Time-varying probability of inclusion of variable ' cell2mat(Xnames(index_variable))]},'FontSize',14);
legend2 = legend(axes2,'show');
set(legend2,'Orientation','horizontal','FontSize',14);