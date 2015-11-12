# This function solves the model for a fixed pair of parameter values 
solveModel <- function(a, wb){
  solns <- runif(3) * wb 
  objMax <- rnorm(1, 100, 10) + a
  out <- list("solns" = solns, "objMax" = objMax, "a" = a, "wb" = wb)
  return(out)
}

# Create a dataframe of parameter values
wb_vector <- 1:20
a_vector <- seq(0.1, 0.9, 0.1)
params <- expand.grid("wb" = wb_vector, "a" = a_vector)

# Use "Map" to evaluate "solveModel" at each pair of parameter values
results <- Map(solveModel, a = params$a, wb = params$wb)

# Suppose you wanted to extract the solutions *only*
solns <- lapply(seq_along(results), function(x) results[[x]]$solns)
solns <- do.call("rbind", solns)
colnames(solns) <- c("person1", "person2", "person3")
solns <- cbind(params, solns)