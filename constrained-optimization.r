#optim function minimizes 'f'; c vector are starting values; lower is the
#constraint; I choose a null gradient because it was easiest to get up and running

h <- function(WB,al) {
  
f <- function(B,WB,al) {
  Bnp5 <- B[1]
  B0 <- B[2]
  Bp5 <- B[3]
  -WB*((1/(1+exp(al - B0)))*(1/(1+exp(al - .5 - Bp5)))*(1-1/(1+exp(al + .5 - Bnp5))) + (1/(1+exp(al - B0)))*(1/(1+exp(al + .5 - Bnp5)))*(1-1/(1+exp(al - .5 - Bp5))) + (1/(1+exp(al + .5 - Bnp5)))*(1/(1+exp(al - .5 - Bp5)))*(1-1/(1+exp(al - B0))) + (1/(1+exp(al - B0)))*(1/(1+exp(al - .5 - Bp5)))*(1/(1+exp(al + .5 - Bnp5)))) + B0 + Bp5 + Bnp5
}   

o <- optim(c(0.01,0.01,0.01,WB,al),function(B) f(B,WB,al),gr=NULL,method = "L-BFGS-B", lower = c(0,0,0))

X = round(- al + o$par[1], digits = 1)       #B0   these are shorthand variables for the exponents
Y = round(.5 - al + o$par[2], digits = 1)    #Bp5  in the logistic CDFs; I don't use them in the function
Z = round(-.5 - al + o$par[3], digits=1)   #Bnp5 but I've pasted in values here to check

pos <- c(Z,X,Y)
bribes <- c(o$par,-o$value)
out <- list("solns" = o$par[1:3], "pos" = pos, "objMax" = -o$value, "a" = al, "wb" = WB)
return(out)
}

# Create a dataframe of parameter values
wb_vector <- 1:20
a_vector <- seq(0.1, 0.9, 0.2)
params <- expand.grid("wb" = wb_vector, "a" = a_vector)

# Use "Map" to evaluate the "h" function at each pair of parameter values
results <- Map(h, al = params$a, WB = params$wb)

# Extract positions and bind to the parameter values
# WOULD LIKE TO HAVE BOTH SOLUTIONS AND POSITIONS IN OUTPUT VECTOR BUT DON'T KNOW HOW
solns <- lapply(seq_along(results), function(x) results[[x]]$pos)
solns <- do.call("rbind", solns)
colnames(solns) <- c("person1", "person2", "person3")
solns <- cbind(params, solns)