#library required for meshgrid command
library("pracma")

#parameters (beta = 1)
WB = 10 #willingness to pay for vote buyer B
al = .5 #alpha

v = seq(0.01,WB,0.01) #this will be counter variable in loop
out = matrix(NA,length(v),5) #pre-reserve space in output matrix

#this loop goes through each value of expenditure up to the willingness-to-pay
#creating matrix "out" with the probability of winning, expected value, and location
#in meshgrid space of the optimum (later code will translate into $ values)
for (j in 1:length(v)) {
  t = v[j]
  
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
  
  out[j,1] = t #I needed a loop counter in Matlab; may not need it in R
  out[j,2] = (value + B0[RC[1],RC[2]] + Bp5[RC[1],RC[2]] + Bnp5[RC[1],RC[2]])/WB #backs out probability of winning
  out[j,3] = value #expected value
  out[j,4] = RC[1] #row location of maximand: gives bribe to 0.5 legislator
  out[j,5] = RC[2] #column location of maximand: gives bribe to 0.0 legislator
}

#calculate bribes for maximal value
l = which.max(out[,3])             #maximal value
bribe0 = (out[l,5]-1)*l/10000      #scale column location of max value for total spent (l)
bribep5 = (out[l,4]-1)*l/10000     #likewise scale row column for 0.5 legislator
bribenp5 = l/100 - bribe0 -bribep5 # -0.5 legislator is calculated as residual

z = -.5 - al + bribenp5            #some theoretical checkpoints in terms of
x = - al + bribe0                  # x,y and z; also makes calculation below cleaner
y = .5 - al + bribep5

#Right hand side should equal 1 for legislators with non-zero bribes;
#Right hand side should be less than 1 for those with zero bribes b/c lambda > 0
lhs_y = WB*(exp(-x)+exp(-z))/((1+exp(-x))*(1+exp(-z)))*(exp(-y)/((1+exp(-y))^2))
lhs_x = WB*(exp(-y)+exp(-z))/((1+exp(-y))*(1+exp(-z)))*(exp(-x)/((1+exp(-x))^2))
lhs_z = WB*(exp(-y)+exp(-x))/((1+exp(-y))*(1+exp(-x)))*(exp(-z)/((1+exp(-z))^2))