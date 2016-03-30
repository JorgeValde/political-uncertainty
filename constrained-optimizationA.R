#optim function minimizes 'f'; c vector are starting values; lower is the
#constraint; I choose a null gradient because it was easiest to get up and running
#note that here the beta parameter is hard coded in as equal to 1
rm(list = ls())
#bp5 = 1.5
#b0 = bp5 + .5
#bnp5 = bp5 + 1

bp5 = 2.36
b0 = 0
bnp5 = 1.36

h <- function(WB,al) {

sy=1
sz=1
  
f <- function(B,WB,al) {
  Ap5 <- B[1]
  A0 <- B[2]
  Anp5 <- B[3]
  -WB*((1/(1+exp(-al - A0 + b0)))*(1/(1+exp((-al + .5 - Anp5 + bnp5)/sy))) + (1/(1+exp(-al - A0 + b0)))*(1/(1+exp((-al - .5 - Ap5 + bp5)/sz))) + (1/(1+exp((-al - .5 - Ap5 + bp5)/sz)))*(1/(1+exp((-al + .5 - Anp5 + bnp5)/sy))) - 2*(1/(1+exp(-al - A0 + b0)))*(1/(1+exp((-al + .5 - Anp5 + bnp5)/sy)))*(1/(1+exp((-al - .5 - Ap5 + bp5)/sz)))) + A0 + Ap5 + Anp5
}   

o <- optim(c(1,1,1,WB,al),function(B) f(B,WB,al),gr=NULL,method = "L-BFGS-B", lower = c(0,0,0), control = list(maxit=100000))
#o <- constrOptim(c(0.01,0.01,0.01,WB,al),function(B) f(B,WB,al),gr=NULL,method = "Nelder-Mead", ui = rbind(c(1,0,0),c(0,1,0),c(0,0,1)),ci=c(0,0,0))

X = round(al -b0 + o$par[2], digits = 2)       #A0   these are shorthand variables for the exponents
Y = round(-.5 + al -bnp5 + o$par[3], digits = 2)    #Anp5  in the logistic CDFs; I don't use them in the function
Z = round(.5 + al - bp5 + o$par[1], digits=2)   #Ap5 but I've pasted in values here to check

pos <- c(Z,X,Y)
bribes <- c(o$par,-o$value)
#note I haven't fixed the win probability to include a's bribes
win <- c(((1/(1+exp(-X)))*(1/(1+exp(-Y/sy))) + (1/(1+exp(-X)))*(1/(1+exp(-Z/sz))) + (1/(1+exp(-Z/sz)))*(1/(1+exp(-Y/sy))) - 2*(1/(1+exp(-X)))*(1/(1+exp(-Y/sy)))*(1/(1+exp(-Z/sz)))))
out <- list("solns" = o$par[1:3], "pos" = pos, "objMax" = -o$value, "a" = al, "wb" = WB, "winProb" = win)
return(out)
}

# Create a dataframe of parameter values
wb_vector <- 9:10 
a_vector <- seq(0.0, 0.0, 0.0)
params <- expand.grid("wb" = wb_vector, "a" = a_vector)

# Use "Map" to evaluate the "h" function at each pair of parameter values
results <- Map(h, al = params$a, WB = params$wb)

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
View(solns)