library("pracma", lib.loc="~/R/win-library/3.1")

%%parameters (beta = 1)
WB = 10
al = .5

v = seq(0.01,WB,0.01)
out = matrix(0,100,5)


for (t in v)
  b0 = seq(0,t,.01*t)
  bp5 = seq(0,t,.01*t)

  tmp=meshgrid(b0,bp5)
  b0=tmp$b0
  bp5=tmp$bp5

  

  bnp5 = t - b0 - bp5;

  Z = -.5 - al + bnp5;
  X = - al + b0;
  Y = .5 - al + bp5;

  obj = WB.*((1./(1+exp(-X))).*(1./(1+exp(-Y))).*(1-1./(1+exp(-Z))) + (1./(1+exp(-X))).*(1./(1+exp(-Z))).*(1-1./(1+exp(-Y))) + (1./(1+exp(-Z))).*(1./(1+exp(-Y))).*(1-1./(1+exp(-X))) + (1./(1+exp(-X))).*(1./(1+exp(-Y))).*(1./(1+exp(-Z)))) - b0 - bp5 - bnp5;
  obj2 = obj.*(bnp5>=0);

  [value, location] = max(obj2(:));
  [R,C] = ind2sub(size(obj2),location);
  prob = (value+b0(R,C) + bp5(R,C) + bnp5(R,C))/WB;
  t2 = round(t*100);
  out(t2,:) = [t,prob,value,R,C];
end

[v, l] = max(out(:,3));
bribe0 = (out(l,5)-1)*l/10000
bribep5 = (out(l,4)-1)*l/10000
bribenp5 = l/100 - bribe0 -bribep5

z = -.5 - al + bribenp5
x = - al + bribe0
y = .5 - al + bribep5

lhs_y = (exp(-x)+exp(-z))/((1+exp(-x))*(1+exp(-z)))*(exp(-y)/((1+exp(-y))^2))
lhs_x = (exp(-y)+exp(-z))/((1+exp(-y))*(1+exp(-z)))*(exp(-x)/((1+exp(-x))^2))
lhs_z = (exp(-y)+exp(-x))/((1+exp(-y))*(1+exp(-x)))*(exp(-z)/((1+exp(-z))^2))