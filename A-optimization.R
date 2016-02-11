#optim function minimizes 'f'; c vector are starting values; lower is the
#constraint; I choose a null gradient because it was easiest to get up and running
#note that here the beta parameter is hard coded in as equal to 1

h <- function(WB,al,y0,z0,x0) {
  
f <- function(B,WB,al,y0,z0,x0) {
  Bp5 <- B[1]
  B0 <- B[2]
  Bnp5 <- B[3]
  -WB*((1/(1+exp(al +x0- B0)))*(1/(1+exp(al + y0 - Bnp5))) + (1/(1+exp(al +x0- B0)))*(1/(1+exp(al + z0 - Bp5))) + (1/(1+exp(al + z0 - Bp5)))*(1/(1+exp(al + y0 - Bnp5))) - 2*(1/(1+exp(al +x0 - B0)))*(1/(1+exp(al + y0 - Bnp5)))*(1/(1+exp(al + z0 - Bp5)))) + B0 + Bp5 + Bnp5
}   

WB=8
al = 0
y0=-.5
o <- optim(c(1,1,1,WB,al,x0,y0,z0),function(B) f(B,WB,al,x0,y0,z0),gr=NULL,method = "L-BFGS-B", lower = c(0,0,0), control = list(maxit=100000))
#o <- constrOptim(c(0.01,0.01,0.01,WB,al),function(B) f(B,WB,al),gr=NULL,method = "Nelder-Mead", ui = rbind(c(1,0,0),c(0,1,0),c(0,0,1)),ci=c(0,0,0))

X = round(-x0 - al + o$par[2], digits = 2)       #B0   these are shorthand variables for the exponents
Y = round(-y0 - al + o$par[3], digits = 2)    #Bnp5  in the logistic CDFs; I don't use them in the function
Z = round(-z0 - al + o$par[1], digits=2)   #Bp5 but I've pasted in values here to check

pos <- c(Z,X,Y)
bribes <- c(o$par,-o$value)
win <- c(((1/(1+exp(-X)))*(1/(1+exp(-Y)))*(1-1/(1+exp(-Z))) + (1/(1+exp(-X)))*(1/(1+exp(-Z)))*(1-1/(1+exp(-Y))) + (1/(1+exp(-Z)))*(1/(1+exp(-Y)))*(1-1/(1+exp(-X))) + (1/(1+exp(-X)))*(1/(1+exp(-Y)))*(1/(1+exp(-Z)))))
out <- list("solns" = o$par[1:3], "pos" = pos, "objMax" = -o$value, "a" = al, "wb" = WB, "yInit" = y0, "zInit" = z0, "xInit" = x0, "winProb" = win)
return(out)
}

# Create a dataframe of parameter values
x_vector <- seq(0, 1.0, 0.1) 
#y_vector <- seq(-1.0, 0.0, 0.1)
z_vector <- seq(.5, 1.0, 0.1)
#params <- expand.grid("wb" = wb_vector, "y" = y_vector)
params <- expand.grid("x" = x_vector, "z" = z_vector)

# Use "Map" to evaluate the "h" function at each pair of parameter values
#results <- Map(h, y = params$y, WB = params$wb)
results <- Map(h, z = params$z, x = params$x)

# Extract positions and bind to the parameter values
# WOULD LIKE TO HAVE BOTH SOLUTIONS AND POSITIONS IN OUTPUT VECTOR BUT DON'T KNOW HOW
solns <- lapply(seq_along(results), function(x) results[[x]]$solns)
netpos <- lapply(seq_along(results), function(x) results[[x]]$pos)
val <- lapply(seq_along(results), function(x) results[[x]]$objMax)
winProb <- lapply(seq_along(results), function(x) results[[x]]$winProb)
solns <- do.call("rbind", solns)
netpos <- do.call("rbind", netpos)
val <- do.call("rbind", val)
winProb <- do.call("rbind", winProb)
colnames(solns) <- c("foe", "middle", "friend")
colnames(netpos) <- c("Z", "X", "Y")
colnames(val) <- c("value")
colnames(winProb) <- c("winProb")
solns <- cbind(params, solns,netpos,val,winProb)
