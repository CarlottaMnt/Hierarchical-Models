#NO POOLING
#In this second specification of the model the groups within the population are considered as complete independent among each other. 
#A set of regression parameters is estimated for each group and the correlation among them is set to zero.
#No pooling of information.

#Import the required packages

library(pacman)
p_load(dplyr, tidyr, readxl, writexl, ggplot2, forcats, sjlabelled,
       here, hablar, naniar, devtools, stringr, splitstackshape,
       validate,ggrepel,DataExplorer, psycho, tidybayes,shinystan,bayesplot,rstan,arm,loo,devtools,matrixStats)

#Select the response variable from the dataset
y <-data$duration_day_log

#Rescale the explanatory variables within each group independently

x_nopool <- data %>% group_by(gen_6_municip) %>% 
  mutate_at(
    c("se1_6_educ","se3_6_nrdwelrooms","se1_8_timemkt","yearselec","duration_day"), .funs = c(scale = ~as.numeric(arm::rescale(., )))) %>% ungroup()

x_rescale_nopool <- x_nopool %>% dplyr::select(se1_6_educ_scale,se3_6_nrdwelrooms_scale,
                                        grid_conn,se1_8_timemkt_scale,yearselec_scale) 
                                        
#Stan model:No-pooling: each municipality has its own regression.
# the variance is estimated hierarchically.

stan_data <- list(
  K=6,
  N=dim(x_rescale_nopool)[1],
  J=15,
  jj=as.numeric(data$gen_6_municip),
  x=cbind(1,x_rescale_nopool),
  y=data$duration_day_log
)

norm_code <-"data {
int<lower=0> N;// Number of observation
int<lower=0> K;
int<lower=1>J;
matrix[N , K] x; //individual predictor
vector[N] y;//outcomes
int<lower=1,upper=J>jj[N];
}
parameters {
real<lower=0>sigma[J];
vector[K]beta[J];
real tau;
real<lower=0>nu;
}
transformed parameters{
real lognu;
lognu =log(nu);
}
model { 
for (j in 1:J){
sigma[j] ~ cauchy(tau, nu);//hierarchical variances
}
for (j in 1:J){ 
beta[j] ~ normal(0,10);//indipendent prior for each coefficient
}
for (n in 1:N)
y[n] ~ normal(x[n]*beta[jj[n]],sigma[jj[n]]);//likelihood
}
generated quantities {
vector[N]y_pred;
vector[N]log_lik;
for(n in 1:N){
y_pred[n]=normal_rng(x[n]*beta[jj[n]],sigma[jj[n]]);//replications
}
for (n in 1:N){
log_lik[n]= normal_lpdf(y[n]|x[n]*beta[jj[n]],sigma[jj[n]]);//log-likelihood
}
}
"
