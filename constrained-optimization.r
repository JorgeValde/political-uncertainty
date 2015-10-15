WB = 60 #willingness to pay parameter
al = .5 #alpha

#the objective function with a negative sign since optim in a minimizer
obj <- function(B) {
  B0 <- B[1]
  Bp5 <- B[2]
  Bnp5 <- B[3]
  -WB*((1/(1+exp(.5 - B0)))*(1/(1+exp(.5 - .5 - Bp5)))*(1-1/(1+exp(.5 + .5 - Bnp5))) + (1/(1+exp(.5 - B0)))*(1/(1+exp(.5 + .5 - Bnp5)))*(1-1/(1+exp(.5 - .5 - Bp5))) + (1/(1+exp(.5 + .5 - Bnp5)))*(1/(1+exp(.5 - .5 - Bp5)))*(1-1/(1+exp(.5 - B0))) + (1/(1+exp(.5 - B0)))*(1/(1+exp(.5 - .5 - Bp5)))*(1/(1+exp(.5 + .5 - Bnp5)))) + B0 + Bp5 + Bnp5
}

#optim function minimizes 'obj'; c vector are starting values; lower is the
#constraint; I choose a null gradient because it was easiest to get up and running
opt <- optim(c(0.01,0.01,0.01),obj,gr=NULL,method = "L-BFGS-B", lower = c(0,0,0))

X = - al + 1.905965        #these are shorthand variables for the exponents
Y = .5 - al + 1.405956   #in the logistic CDFs; I don't use them in the function
Z = -.5 - al + 2.406003         #but I've pasted in values here to check

((exp(-Z)+exp(-Y))*exp(-X))/((1+exp(-Z))*((1+exp(-X))^2)*(1+exp(-Y))) #FOC for X

# -X = al - B0           These were just reminder lines for when I substituted
# -Y = al - .5 - Bp5     back to the fundamental expressions for B0, Bp5 and Bnp5
# -Z = al + .5 - Bnp5    in the objective function "obj" below