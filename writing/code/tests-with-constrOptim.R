#this code unpacks the "h" function of constrained-optimization.r in order to run
#constrOptim to use alternative optimization methods

  al = -0.77
  WB = 10
  
  f <- function(B) {
    Bnp5 <- B[1]
    B0 <- B[2]
    Bp5 <- B[3]
    -WB*((1/(1+exp(al - B0)))*(1/(1+exp(al - .5 - Bp5)))*(1-1/(1+exp(al + .5 - Bnp5))) + (1/(1+exp(al - B0)))*(1/(1+exp(al + .5 - Bnp5)))*(1-1/(1+exp(al - .5 - Bp5))) + (1/(1+exp(al + .5 - Bnp5)))*(1/(1+exp(al - .5 - Bp5)))*(1-1/(1+exp(al - B0))) + (1/(1+exp(al - B0)))*(1/(1+exp(al - .5 - Bp5)))*(1/(1+exp(al + .5 - Bnp5)))) + B0 + Bp5 + Bnp5
  }   
  
  o <- constrOptim(c(0.01,0.01,0.01),f,gr=NULL,method = "Nelder-Mead", ui = rbind(c(1,0,0),c(0,1,0),c(0,0,1)),ci=c(0,0,0))