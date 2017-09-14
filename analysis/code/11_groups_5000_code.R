#*****************************************************************************************
#	PURPOSE OF THIS FILE: (description comes from original "Order of the files.docx" )
#		This is the R script we run. Differently from the previous model, 
#		we ask R to keep only every tenth of the iteration due to the size concerns.
#*****************************************************************************************

#*****************************************************************************************
# Load a few libraries 
#*****************************************************************************************
library(rstan)
library(coda)
library(ggmcmc)

# setting for rstan
# automatically save a compiled Stan program so not need to be recompiled 
rstan_options(auto_write = TRUE)  
# set core number in case of parallel computing needed
options(mc.cores = parallel::detectCores())

                                        #*****************************************************************************************
# Set working dirctory to the folder source file in
                                        #*****************************************************************************************
# get the dirctory of this source file in order to use relative paths
source.dir <- dirname(sys.frame(1)$ofile)  # returns the path of the current script file
setwd(paste(source.dir))  # set current dir as the working dir 

#*****************************************************************************************
# Prepare data and code for stan
#*****************************************************************************************
# read data files (use relative path for general use in any computer)
votes<- read.csv("../../build/output/data_for_rstan.csv", header=TRUE)
attach(votes)
groups<- read.csv("../../build/output/group_dummy_for_rstan.csv", header=TRUE)

# MODEL WITH GROUP IND
votes_code<- "
## Stan code for multidimensional hierarchical IRT model
data {
  int<lower=1> J;              // number of legislators
  int<lower=1> K;              // number of votes
  int<lower=1> N;              // number of observations
  int<lower=1> G;              // number of groups
  int<lower=1,upper=J> jj[N];  // legislator for observation n
  int<lower=1,upper=K> kk[N];  // vote for observation n
  matrix[N,G] x;			   // group indicators
  int<lower=0,upper=1> y[N];   // position for observation n
//  vector[N] y;               // position for observation n
}

parameters {    
  matrix[J,G] alpha;               //  
  matrix[K,G] beta;                // 
  real mu_beta;
  real<lower=0> sigma_beta;
  matrix<lower=0>[K,G] gamma;
  real mu_gamma;
  real<lower=0> sigma_gamma;
}

transformed parameters { 
//  real total[N]; 
  vector[N] total; 

  { 
    matrix[N,G] summands; 
    for (g in 1:G) 
      for (n in 1:N) 
        summands[n,g]<- (x[n,g]*gamma[kk[n],g])*(alpha[jj[n],g] - beta[kk[n],g]); 

    for (n in 1:N) 
 //     for (g in 1:G) 
        total[n] <- sum(summands[n]); 
  } 
} 

model {
 to_vector(alpha) ~ normal(0,1);         
 
 to_vector(beta) ~ normal(mu_beta, sigma_beta);
 mu_beta~ normal(0, 3);
 sigma_beta ~ cauchy(0,3);
 
 to_vector(gamma) ~ lognormal(mu_gamma, sigma_gamma);
 mu_gamma ~ normal(0, 3);
 sigma_gamma ~ cauchy(0,3);
 
 y ~ bernoulli_logit(total);
}"

# assemble a list of data to fit the model, get all the values from the data
votes_dat <- list(
    N = nrow(votes),                        # number of observations
    J = max(votes$politician_id_numeric),   # number of legislators
    K = max(votes$action_id_numeric),       # number of votes
    G = ncol(groups),                       # number of groups
    jj = politician_id_numeric,             # legislator vector
    kk = action_id_numeric,                 # vote vector
    y = vote,                               # position vector
    x = as.matrix(groups)                   # group indicators
)

# call the stan function to draw posterior samples
fit6 <- stan(
    model_code = votes_code,                # Stan model
    data = votes_dat,                       # named list of data
    iter = 2000,                            # total number of iterations per chain
    warmup = 1000,                          # number of warmup iterations per chain
    chains = 4,                             # number of Markov chains
    seed = 1234,                            # set seed for replication
    verbose = TRUE                          # print intemediate output from stan
)

                                        #*****************************************************************************************
# GRAPHING & EXPORTING                                        #*****************************************************************************************
# ggmcmc requires Tidyr, which has a naming overlap with extract, so call rstan::extract explicitly
#s <- rstan::extract(fit6, inc_warmup = FALSE)  
#s <- mcmc.list(lapply(1:ncol(fit6), function(x) mcmc(as.array(fit6)[,x,])))
#S <- ggs(s)
#library(foreign)
#write.dta(S, file = "../output/mcmc_output.dta")

# save workplace for later analysis
save.image("../temp/32groups_2chains_2000iters.RData")

# exporting ideal points and match them with legislators
# extract out ideal points(alpha)
alpha_sum <- rstan::summary(fit6, pars = c("alpha"), probs = c(0.05, 0.95))$summary

# factor out politician names with id from votes
politician <- subset(votes, select = c("politician_id_numeric", "fullname"))
# get all the values of politician names and id only once
politician_unique <- unique(politician[,1:2])
# sort this data by politician_id_numeric
politician_sorted <- politician_unique[order(politician_unique$politician_id_numeric),]

# add one column called groups to use expandRows() function
politician_sorted$groups <- ncol(groups)
# expand this data "number_of_groups" times to match alpha_sum
library(splitstackshape)
politician_expanded <- expandRows(politician_sorted, count = "groups")

# combine alpha_sum with politician names and ids
alpha_sum_names <- cbind(alpha_sum, politician_expanded[,1:2])
# export it to a csv file
write.csv(alpha_sum_names, file = "../output/alpha_with_names.csv", row.names = TRUE)

