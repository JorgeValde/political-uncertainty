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
votes<- read.csv("../../build/output/data_7_19.csv", header=TRUE)
attach(votes)
groups<- read.csv("../../build/output/11_groups_dummy.csv", header=TRUE)
attach(groups)

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
 for (g in 1:G)
	alpha[g]~ normal(0,1);         
 
 for (g in 1:G)
	beta[g] ~ normal(mu_beta, sigma_beta);
	mu_beta~ normal(0, 3);
	sigma_beta ~ cauchy(0,3);
 
 for (g in 1:G)
	gamma[g] ~ lognormal(mu_gamma, sigma_gamma);
	mu_gamma ~ normal(0, 3);
	sigma_gamma ~ cauchy(0,3);
 
 for (n in 1:N)
	y[n] ~ bernoulli_logit(total[n]); 
}"

# assemble a list of data to fit the model, get all the values from the data
votes_dat <- list(
 N = nrow(votes),                        # number of observations
 J = max(votes$politician_id_numeric),   # number of legislators
 K = max(votes$action_id_numeric),       # number of votes
 G = 11,                                 # number of groups ???
 jj = politician_id_numeric,             # legislator vector
 kk = action_id_numeric,                 # vote vector
 y = vote_1,                             # position vector
 x = matrix(group, nrow = nrow(votes))   # group indicators
)

# call the stan function to draw posterior samples
fit6 <- stan(
 model_code = votes_code,                # Stan model
 data = votes_dat,                       # named list of data
 iter = 5000,                            # total number of iterations per chain
 warmup = 1000,                          # number of warmup iterations per chain
 chains = 1,                             # number of Markov chains
 verbose = TRUE                          # print intemediate output from stan
)

#*****************************************************************************************
# GRAPHING & EXPORTING
#*****************************************************************************************
# ggmcmc requires Tidyr, which has a naming overlap with extract, so call rstan::extract explicitly
s <- rstan::extract(fit6, inc_warmup = FALSE)  
s <- mcmc.list(lapply(1:ncol(fit6), function(x) mcmc(as.array(fit6)[,x,])))
#S <- ggs(s)
#library(foreign)
#write.dta(S, file = "../output/output.dta")
