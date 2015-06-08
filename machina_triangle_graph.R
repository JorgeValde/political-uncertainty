library("pracma")

#parameters (beta = 1)
WB = 10
al = .5

v = seq(0.01,WB,0.01) 
out = matrix(0,100,5)


for (t in v) {
  b0 = seq(0,t,.01*t)
  bp5 = seq(0,t,.01*t)

  B0=meshgrid(b0,bp5)$X
  Bp5=meshgrid(b0,bp5)$Y
  Bnp5 = t - B0 - BP5

  Z = -.5 - al + Bnp5
  X = - al + B0
  Y = .5 - al + Bp5
  
  obj = WB*((1/(1+exp(-X)))*(1/(1+exp(-Y)))*(1-1/(1+exp(-Z))) + (1/(1+exp(-X)))*(1/(1+exp(-Z)))*(1-1/(1+exp(-Y))) + (1/(1+exp(-Z)))*(1/(1+exp(-Y)))*(1-1/(1+exp(-X))) + (1/(1+exp(-X)))*(1/(1+exp(-Y)))*(1/(1+exp(-Z)))) - B0 - Bp5 - Bnp5
  obj2 = obj*(Bnp5>=0)
  
  value[t] = max(obj2)
  ind[t] = which.max(obj2)
  RC = arrayInd(ind,c(dim(obj2),dim(obj2)))
  
  out[t,1] = t
  out[t,2] = (value[t] + B0[RC[t,1],RC[t,2]] + Bp5[RC[t,1],RC[t,2]] + Bnp5[RC[t,1],RC[t,2]])/WB
  out[t,3] = value[t]
  out[t,4] = RC[t,1]
  out[t,5] = RC[t,2]
}


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