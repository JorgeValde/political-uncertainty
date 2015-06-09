#library required for meshgrid command
library("pracma")

#parameters (beta = 1)
WB = 10 #willingness to pay for vote buyer B
al = .5 #alpha

v = seq(0.01,WB,0.01) #this will be counter variable in loop
out = matrix(0,length(v),5) #pre-reserve space in output matrix

#this loop goes through each value of expenditure up to the willingness-to-pay
#creating matrix "out" with the probability of winning, expected value, and location
#in meshgrid space of the optimum (later code will translate into $ values)
for (t in v) {
  b0 = seq(0,t,.01*t) #possible bribe values for legislator located at 0
  bp5 = seq(0,t,.01*t) #possible bribe values for legislator located at 0.5

  B0=meshgrid(b0,bp5)$X
  Bp5=meshgrid(b0,bp5)$Y
  Bnp5 = t - B0 - Bp5 #possible bribe values for legislator located at -0.5

  Z = -.5 - al + Bnp5 #these are shorthand variables for the exponents
  X = - al + B0       #in the logistic CDFs
  Y = .5 - al + Bp5
  
  #obj is the objective function for the vote buyer; obj2 imposes a non-negativity
  #constraint for leg. -0.5 (the residual). This is what makes it a "triangle."
  obj = WB*((1/(1+exp(-X)))*(1/(1+exp(-Y)))*(1-1/(1+exp(-Z))) + (1/(1+exp(-X)))*(1/(1+exp(-Z)))*(1-1/(1+exp(-Y))) + (1/(1+exp(-Z)))*(1/(1+exp(-Y)))*(1-1/(1+exp(-X))) + (1/(1+exp(-X)))*(1/(1+exp(-Y)))*(1/(1+exp(-Z)))) - B0 - Bp5 - Bnp5
  obj2 = obj*(Bnp5>=0)
  
  value = max(obj2) #the value at which the objective function is maximized (over non-negative values)
  ind = which.max(obj2) #the location at which the objective fcn is maximized
  RC = arrayInd(ind,c(dim(obj2),dim(obj2))) #row/column version of maximand location
  
  out[t,1] = t #I needed a loop counter in Matlab; may not need it in R
  out[t,2] = (value + B0[RC[1],RC[2]] + Bp5[RC[1],RC[2]] + Bnp5[RC[1],RC[2]])/WB #backs out probability of winning
  out[t,3] = value #expected value
  out[t,4] = RC[1] #row location of maximand: gives bribe to 0.5 legislator
  out[t,5] = RC[2] #column location of maximand: gives bribe to 0.0 legislator
}

#rest of program in Matlab code: won't make any sense if "out" matrix
#isn't properly populated through above loop
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