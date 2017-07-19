library(rstan)
library(coda)
library(ggmcmc)
votes<- read.csv("//stu05-fsrv.ad.syr.edu/ykbagir$/Desktop/kristy/data_7_19.csv", header=TRUE)
attach(votes)
parameters<- read.csv("//stu05-fsrv.ad.syr.edu/ykbagir$/Desktop/kristy/parameters.csv", header=TRUE)
attach(parameters)


votes_code<- '
data {
int<lower=1> J;              // number of legislators
int<lower=1> K;              // number of votes
int<lower=1> N;              // number of observations
int<lower=1,upper=J> jj[N];  // legislator for observation 
int<lower=1,upper=K> kk[N];  // vote for observation 
int<lower=0,upper=1> y[N];   // position for observation 
}
parameters {    
real alpha[K];               //  
real beta[K];                // 
real xx[J];                // 
}
model {
for (k in 1:K)
{ alpha[k] ~ normal(0,0.16);         // informative true prior
}
for (k in 1:K)
{beta[k] ~ normal(0,0.16);          // informative true prior
}
for (j in 1:J)
{xx[j] ~ normal(0,1);          // informative true prior
}
for (n in 1:N)
y[n] ~ bernoulli_logit(xx[jj[n]]*alpha[kk[n]] - beta[kk[n]]);
}
'

votes_dat<-list(N=66281, J=445, K=159, jj=politician_id_numeric, kk=action_id_numeric, y=vote_1)
fit2<-stan(model_code=votes_code, data=votes_dat, iter=5000, warmup=1000, chains=2)
print(fit2)