#optim function minimizes 'f'; c vector are starting values; lower is the
#constraint; I choose a null gradient because it was easiest to get up and running
#note that here the beta parameter is hard coded in as equal to 1
rm(list = ls())
al = 0
WB=8

v = seq(6,8,.4)
o <- matrix(0,length(v),20)
colnames(o) <- c("al","sy","sz","WA","WB","x", "y", "z","foe", "middle", "friend","Z", "X", "Y","value","wProbB","X_a", "Y_a", "Z_a","ValA")

for (j in 1:length(v)) {

  WA = v[j]
  sy=1.1
  sz=1
  
h <- function(x0,y0,z0) {
  
  f <- function(B,x0,y0,z0) {
    Bp5 <- B[1]
    B0 <- B[2]
    Bnp5 <- B[3]
    -WB*((1/(1+exp(al + x0- B0)))*(1/(1+exp((al + y0 - Bnp5)/sy))) + (1/(1+exp(al +x0- B0)))*(1/(1+exp((al + z0 - Bp5)/sz))) + (1/(1+exp((al + z0 - Bp5)/sz)))*(1/(1+exp((al + y0 - Bnp5)/sy))) - 2*(1/(1+exp(al +x0 - B0)))*(1/(1+exp((al + y0 - Bnp5)/sy)))*(1/(1+exp((al + z0 - Bp5)/sz)))) + B0 + Bp5 + Bnp5
  }   
  
  o <- optim(c(1,1,1,x0,y0,z0),function(B) f(B,x0,y0,z0),gr=NULL,method = "L-BFGS-B", lower = c(0,0,0), control = list(maxit=100000))
  
  X = round(-x0 - al + o$par[2], digits = 3)   #B0   these are shorthand variables for the exponents
  Y = round(-y0 - al + o$par[3], digits = 3)   #Bnp5  in the logistic CDFs; I don't use them in the function
  Z = round(-z0 - al + o$par[1], digits = 3)   #Bp5 but I've pasted in values here to check
  
  A0 = x0 
  Ap5 = z0 - .5
  Anp5 = y0 + .5
  
  pos <- c(Z,X,Y)
  bribes <- c(o$par,-o$value)
  bribesA <- c(A0,Anp5,Ap5)
  win <- c(((exp(-X)/(1+exp(-X)))*(exp(-Y/sy)/(1+exp(-Y/sy))) + (exp(-X)/(1+exp(-X)))*(exp(-Z/sz)/(1+exp(-Z/sz))) + (exp(-Z/sz)/(1+exp(-Z/sz)))*(exp(-Y/sy)/(1+exp(-Y/sy))) - 2*(exp(-X)/(1+exp(-X)))*(exp(-Y/sy)/(1+exp(-Y/sy)))*(exp(-Z/sz)/(1+exp(-Z/sz)))))
  ValA = WA*win - A0 - Ap5 - Anp5
  out <- list("solns" = round(o$par[1:3],digits=3), "pos" = pos, "objMax" = -o$value, "a" = al, "wb" = WB, "yInit" = y0, "zInit" = z0, "xInit" = x0, "winProb" = win,"bribesA"=c(A0,Anp5,Ap5,ValA))
  return(out)
}

# Create a dataframe of parameter values
x_vector <- seq(0, 2, 0.2) 
y_vector <- seq(-.5, 2, 0.2)
z_vector <- seq(.5, 2, 0.2)
params <- expand.grid("x" = x_vector, "y" = y_vector, "z" = z_vector)

# Use "Map" to evaluate the "h" function at each pair of parameter values
results <- Map(h, x = params$x, y = params$y, z = params$z)
bribesA <- lapply(seq_along(results), function(x) results[[x]]$bribesA)
bribesA <- do.call("rbind", bribesA)
index_max = which.max( bribesA[,4] )

# Extract values at A's max and bind to the parameter values
solns <- c(results[[index_max]]$solns)
bA <- c(results[[index_max]]$bribesA)
netpos <- c(results[[index_max]]$pos)
val <- c(results[[index_max]]$objMax)
winProb <- c(results[[index_max]]$winProb)
params <- c(params[index_max,])

solns <- rbind(solns)
bA <- rbind(bA)
netpos <- rbind(netpos)
val <- rbind(val)
winProb <- rbind(winProb)
params <- do.call("cbind",params)

o[j,] <- cbind(al,sy,sz,WA,WB,params,solns,netpos,val,winProb,bA)
}
View(o)