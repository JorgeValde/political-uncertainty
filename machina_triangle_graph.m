b0 = [0:.01:1];
bp5 = [0:.01:1];
[b0,bp5]=meshgrid(b0,bp5);

t = 1;
bnp5 = t - b0 - bp5;

al = .5;

X = -.5 - al + bnp5;
Y = - al + b0;
Z = .5 - al + bp5;

WB = 3;

obj = WB.*((1./(1+exp(-X))).*(1./(1+exp(-Y))).*(1-1./(1+exp(-Z))) + (1./(1+exp(-X))).*(1./(1+exp(-Z))).*(1-1./(1+exp(-Y))) + (1./(1+exp(-Z))).*(1./(1+exp(-Y))).*(1-1./(1+exp(-X))) + (1./(1+exp(-X))).*(1./(1+exp(-Y))).*(1./(1+exp(-Z)))) - b0 - bp5 - bnp5;

s0 = size(b0,2);
s5 = size(bp5,2);

obj2 = zeros(101);

for j=1:101
    j=50
    i = 25
     for i=1:101
        if bnp5(i,j) ge 0
            disp('test2')
        end
     obj2(i,j) = obj(i,j)
end


figure1 = figure('Color',[1 1 1],...
  'FileName','C:\Users\Kristy\Documents\My Dropbox\Research\Separation of Powers\Write ups\lobby_br.jpg',...
  'PaperPosition',[1 3.313 5.833 4.375]);
 
%% Create axes
axes1 = axes('Parent',figure1);
axis([0 .12 0 .0035]);
%title(axes1,'Lobby''s Optimal Expenditure');
xlabel(axes1,'Trade Agreement Tariff');
ylabel(axes1,'Lobbying Expenditure');
hold(axes1,'all');
 



%% Create plot
plot2 = plot(TA,opt_C_13,'DisplayName','\theta \sim U[-.14,14]','Color',[0 0 1],'LineStyle','--','LineWidth',1,'Parent',axes1);
legend1 = legend(axes1,{'\theta \sim U[-.01,01]','\theta \sim U[-.14,14]'},'Position',[0.1794 0.7725 0.1714 0.119]);


saveas(gcf,'C:\Users\Kristy\Documents\Dropbox\Research\Separation_of_Powers\Work\lobby_br.jpg');