WB = 10
al = .5 #alpha

Z = -.5 - al + Bnp5 #these are shorthand variables for the exponents
X = - al + B0       #in the logistic CDFs
Y = .5 - al + Bp5

-X = al - B0
-Y = al - .5 - Bp5
-Z = al + .5 - Bnp5

fr <- function(x) {   ## Rosenbrock Banana function
  x1 <- x[1]
  x2 <- x[2]
  100 * (x2 - x1 * x1)^2 + (1 - x1)^2
}

obj <- function(B) {
  B0 <- B[1]
  Bp5 <- B[2]
  Bnp5 <- B[3]
  -10*((1/(1+exp(.5 - B0)))*(1/(1+exp(.5 - .5 - Bp5)))*(1-1/(1+exp(.5 + .5 - Bnp5))) + (1/(1+exp(.5 - B0)))*(1/(1+exp(.5 + .5 - Bnp5)))*(1-1/(1+exp(.5 - .5 - Bp5))) + (1/(1+exp(.5 + .5 - Bnp5)))*(1/(1+exp(.5 - .5 - Bp5)))*(1-1/(1+exp(.5 - B0))) + (1/(1+exp(.5 - B0)))*(1/(1+exp(.5 - .5 - Bp5)))*(1/(1+exp(.5 + .5 - Bnp5)))) + B0 + Bp5 + Bnp5
}

obj <- function(B0,Bp5,Bnp5) -10*((1/(1+exp(.5 - B0)))*(1/(1+exp(.5 - .5 - Bp5)))*(1-1/(1+exp(.5 + .5 - Bnp5))) + (1/(1+exp(.5 - B0)))*(1/(1+exp(.5 + .5 - Bnp5)))*(1-1/(1+exp(.5 - .5 - Bp5))) + (1/(1+exp(.5 + .5 - Bnp5)))*(1/(1+exp(.5 - .5 - Bp5)))*(1-1/(1+exp(.5 - B0))) + (1/(1+exp(.5 - B0)))*(1/(1+exp(.5 - .5 - Bp5)))*(1/(1+exp(.5 + .5 - Bnp5)))) + B0 + Bp5 + Bnp5

nlm(obj,c(0,0,0))

optim(c(0.01,0.01,0.01),obj,gr=NULL,method = "L-BFGS-B", lower = c(0,0,0))