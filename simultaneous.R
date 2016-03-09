#what this gives me is a best response function for vote buyer B (columns foe, middle
#friend) as a function of the strategy of vote buyer A (columns X_a,Y_a,Z_a). Middle
#corresponds to X_a and friend corresponds to Y_a and foe corresponds to Z_a since
#this terminology is about vote buyer B and Z is the guy furthest to the right (0.5)).
rm(list = ls())
al = 0
WB=8

  WA = 8
  sy=1
  sz=1
  
h <- function(x0,y0,z0) {
  
  f <- function(B,x0,y0,z0) {
    Bp5 <- B[1]
    B0 <- B[2]
    Bnp5 <- B[3]
    -WB*((1/(1+exp(al + x0- B0)))*(1/(1+exp((al + y0 - Bnp5)/sy))) + (1/(1+exp(al +x0- B0)))*(1/(1+exp((al + z0 - Bp5)/sz))) + (1/(1+exp((al + z0 - Bp5)/sz)))*(1/(1+exp((al + y0 - Bnp5)/sy))) - 2*(1/(1+exp(al +x0 - B0)))*(1/(1+exp((al + y0 - Bnp5)/sy)))*(1/(1+exp((al + z0 - Bp5)/sz)))) + B0 + Bp5 + Bnp5
  }   
  
  o <- optim(c(1,1,1,x0,y0,z0),function(B) f(B,x0,y0,z0),gr=NULL,method = "L-BFGS-B", lower = c(0,0,0), control = list(maxit=100000))
  
  X = round(-x0 - al + o$par[2], digits = 3)       #B0   these are shorthand variables for the exponents
  Y = round(-y0 - al + o$par[3], digits = 3)    #Bnp5  in the logistic CDFs; I don't use them in the function
  Z = round(-z0 - al + o$par[1], digits=3)   #Bp5 but I've pasted in values here to check
  
  A0 = x0 
  Ap5 = z0 - .5
  Anp5 = y0 + .5
  
  pos <- c(Z,X,Y)
  bribes <- c(o$par,-o$value)
  bribesA <- c(A0,Anp5,Ap5)
  win <- c(((1/(1+exp(-X)))*(1/(1+exp(-Y/sy))) + (1/(1+exp(-X)))*(1/(1+exp(-Z/sz))) + (1/(1+exp(-Z/sz)))*(1/(1+exp(-Y/sy))) - 2*(1/(1+exp(-X)))*(1/(1+exp(-Y/sy)))*(1/(1+exp(-Z/sz)))))
  ValA = WA*(1-win) - A0 - Ap5 - Anp5
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

# Extract values at A's max and bind to the parameter values
solns <- lapply(seq_along(results), function(x) results[[x]]$solns)
bA <- lapply(seq_along(results), function(x) results[[x]]$bribesA)
netpos <- lapply(seq_along(results), function(x) results[[x]]$pos)
val <- lapply(seq_along(results), function(x) results[[x]]$objMax)
winProb <- lapply(seq_along(results), function(x) results[[x]]$winProb)

solns <- do.call("rbind", solns)
bA <- do.call("rbind",bA)
netpos <- do.call("rbind", netpos)
val <- do.call("rbind", val)
winProb <- do.call("rbind", winProb)
params <- do.call("rbind", params)

o <- cbind(t(params),solns,netpos,val,winProb,bA)
colnames(o) <- c("x", "y", "z","foe", "middle", "friend","Z", "X", "Y","value","wProbB","X_a", "Y_a", "Z_a","ValA")
View(o)

#to use this, need to figure out how to pad out parameters to be same length as other vectors
#o <- cbind(al,sy,sz,WA,WB,params,solns,netpos,val,winProb,bA)
#colnames(o) <- c("al","sy","sz","WA","WB","x", "y", "z","foe", "middle", "friend","Z", "X", "Y","value","wProbB","X_a", "Y_a", "Z_a","ValA")


#Code for when all six bribes are non-negative, just to solve for b0 and a0.
a=.2
WA=8
WB=8

fx <- function(B,a,WA,WB) {
  a0 <- B[1]
  b0 <- B[2]
  exp(-a+b0-a0)+exp(a-b0+a0) - sqrt(2)*((8*8)^.25)+2
}   

x <- optim(c(2,.2),function(B) fx(B,a,WA,WB),gr=NULL,method = "L-BFGS-B", lower = c(0,0,0), control = list(maxit=100000))