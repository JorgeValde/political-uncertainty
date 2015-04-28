t = 2
b0 = [0:.01*t:t];
bp5 = [0:.01*t:t];
[b0,bp5]=meshgrid(b0,bp5);

bnp5 = t - b0 - bp5;

al = .5;

X = -.5 - al + bnp5;
Y = - al + b0;
Z = .5 - al + bp5;

WB = 5;

obj = WB.*((1./(1+exp(-X))).*(1./(1+exp(-Y))).*(1-1./(1+exp(-Z))) + (1./(1+exp(-X))).*(1./(1+exp(-Z))).*(1-1./(1+exp(-Y))) + (1./(1+exp(-Z))).*(1./(1+exp(-Y))).*(1-1./(1+exp(-X))) + (1./(1+exp(-X))).*(1./(1+exp(-Y))).*(1./(1+exp(-Z)))) - b0 - bp5 - bnp5;

obj2 = obj.*(bnp5>=0);

[value, location] = max(obj2(:));
prob = value/5
[R,C] = ind2sub(size(obj2),location);

%%Outer loop
v = value - b0(R,C) - bp5(R,C) - bnp5(R,C)

mesh(obj2);
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