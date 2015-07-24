WB = 10
al = .5 #alpha

Z = -.5 - al + Bnp5 #these are shorthand variables for the exponents
X = - al + B0       #in the logistic CDFs
Y = .5 - al + Bp5

-X = al - B0
-Y = al - .5 - Bp5
-Z = al + .5 - Bnp5

obj <- function(B0,Bp5,Bnp5) -10*((1/(1+exp(.5 - B0)))*(1/(1+exp(.5 - .5 - Bp5)))*(1-1/(1+exp(.5 + .5 - Bnp5))) + (1/(1+exp(.5 - B0)))*(1/(1+exp(.5 + .5 - Bnp5)))*(1-1/(1+exp(.5 - .5 - Bp5))) + (1/(1+exp(.5 + .5 - Bnp5)))*(1/(1+exp(.5 - .5 - Bp5)))*(1-1/(1+exp(.5 - B0))) + (1/(1+exp(.5 - B0)))*(1/(1+exp(.5 - .5 - Bp5)))*(1/(1+exp(.5 + .5 - Bnp5)))) + B0 + Bp5 + Bnp5

nlm(obj,c(0,0,0))