#COMPLETE POOLING MODEL WITH NORMAL LIKELIHOOD
# In this case the model will NOT consider the presence of several groups within the population and estimate only one set of regression
#coefficients for the entire sample. The dimension of the set of coefficients depends on K, the number of regressors.

#Install the required packages
library(pacman)
p_load(dplyr, tidyr, readxl, writexl, ggplot2, forcats, sjlabelled,
       here, hablar, naniar, devtools, stringr, splitstackshape,
       validate,ggrepel,DataExplorer, psycho, tidybayes,shinystan,bayesplot,rstan,arm,loo,devtools,matrixStats)

#Remove NA from the dataset: Stan not allowed the presence of missing data
data <- data[complete.cases(data),]
#
#Select the response variable and the explanatory variables

y <-data$duration_day_log #response variable

x <-data %>% mutate_at(
    c("se1_6_educ","se3_6_nrdwelrooms","se1_8_timemkt","yearselec"), .funs = c(scale = ~as.numeric(arm::rescale(., )))) # Rescale the explanatory variables

x_rescale <- x %>% dplyr::select(se1_6_educ_scale,se3_6_nrdwelrooms_scale,
                                        grid_conn,se1_8_timemkt_scale,yearselec_scale) #select the rescaled explanatory variables
                                        
#Stan model----------------------------------------------------------------------
#Non hierarchical model(complete pooling of every municipality and independent regressor)
stan_data <- list(
  K=6, #number of regressor
  N=dim(x_rescale)[1],#Number of observations
  x=cbind(1,x_rescale),#Insert a constant for estimating the intercept
  y=data$duration_day_log
)
norm_code <-"data {
int<lower=0> N;// Number of observation
int<lower=0> K;// Regressor
matrix[N , K] x;//Individual predictor
vector[N] y;// Response variable
}
parameters {
real<lower=0>sigma; //Standard deviation of the observations
vector[K]beta; //Vector of regression coefficients (Including the intercept)
}
model {  
sigma ~ cauchy(0 ,2.5); //weakly informative prior distribution for the standard deviation
beta ~ normal(0,10);//weakly informative prior distribution for the regression paramenters
for (n in 1:N)
y[n] ~ normal(x[n]*beta,sigma);// likelihood
}
generated quantities {
vector[N]y_pred;//replications
vector[N]log_lik;//log-likelihood 
for(n in 1:N){
y_pred[n]=normal_rng(x[n]*beta,sigma);
}
for (n in 1:N){
log_lik[n]= normal_lpdf(y[n]|x[n]*beta,sigma);
}
}
"
