#optim function minimizes 'f'; c vector are starting values; lower is the
#constraint; I choose a null gradient because it was easiest to get up and running

h <- function(WB,al) {
  
f <- function(B,WB,al) {
  B0 <- B[1]
  Bp5 <- B[2]
  Bnp5 <- B[3]
  -WB*((1/(1+exp(.5 - B0)))*(1/(1+exp(.5 - .5 - Bp5)))*(1-1/(1+exp(.5 + .5 - Bnp5))) + (1/(1+exp(.5 - B0)))*(1/(1+exp(.5 + .5 - Bnp5)))*(1-1/(1+exp(.5 - .5 - Bp5))) + (1/(1+exp(.5 + .5 - Bnp5)))*(1/(1+exp(.5 - .5 - Bp5)))*(1-1/(1+exp(.5 - B0))) + (1/(1+exp(.5 - B0)))*(1/(1+exp(.5 - .5 - Bp5)))*(1/(1+exp(.5 + .5 - Bnp5)))) + B0 + Bp5 + Bnp5
}   

o <- optim(c(0.01,0.01,0.01,2,.05),function(B) f(B,WB,al),gr=NULL,method = "L-BFGS-B", lower = c(0,0,0))

X = - al + o$par[1]       #B0   these are shorthand variables for the exponents
Y = .5 - al + o$par[2]    #Bp5  in the logistic CDFs; I don't use them in the function
Z = -.5 - al + o$par[3]   #Bnp5 but I've pasted in values here to check

pos <- c(X,Y,Z)
bribes <- o$par
out <- list(pos,bribes,WB,al)
}

test = h(10,.5)