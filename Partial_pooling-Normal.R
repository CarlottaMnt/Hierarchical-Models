#PARTIAL POOLING
#The model now estimates a set of regression coefficients for each group (15 municipalities) by partial pooling the information among them.
#Moreover I have included beyond the explanatory variable at an individual level, also explanatory variables at group level that should improve the precsion of the regression coefficients estimates.
#The hyper prior distribution are all weakly informative.


#Import the required packages

library(pacman)
p_load(dplyr, tidyr, readxl, writexl, ggplot2, forcats, sjlabelled,
       here, hablar, naniar, devtools, stringr, splitstackshape,
       validate,ggrepel,DataExplorer, psycho, tidybayes,shinystan,bayesplot,rstan,arm,loo,devtools,matrixStats)

#Remove missing values
data <- data[complete.cases(data),]

#Select response variable

y <-data$duration_day_log

#Rescale and select the explanatory variables at individual level
x <-data %>% mutate_at(
    c("se1_6_educ","se3_6_nrdwelrooms","se1_8_timemkt","yearselec"), .funs = c(scale = ~as.numeric(arm::rescale(., )))) 

x_rescale <- x %>% dplyr::select(se1_6_educ_scale,se3_6_nrdwelrooms_scale,
                                        grid_conn,se1_8_timemkt_scale,yearselec_scale) 

#Select and rescale the group level explanatory variables
z <-mun_data[,c("municip_alt","municip_gdp","municip_capstock","municip_popdens")]
z_rescale <-apply(z,2,arm::rescale)

#Hierarchical model with group predictors.

stan_data <- list(
  K=6, #Number of individual level regressors 
  L=5, #Number of group level regressors
  N=dim(x_rescale)[1], #Number of observations
  jj=as.numeric(data$gen_6_municip), #Grouping variable
  J=15, #Number of groups
  x=cbind(1,x_rescale), #Individual regressor matrix
  z=cbind(1,z_rescale), # Group-level regressor matrix
  y=data$duration_day_log #Response variable
)

hiera_code <-"data {
int<lower=0> N;    // Number of observation
int<lower=0> K;    // Number of individual predictor
int<lower=0> L;    //NUmber of group level predictor
matrix[N , K] x;   //individual predictors
int<lower=1>J;     //number of groups 
row_vector[L] z[J];//group-level predictor
vector[N] y;       //outcomes
int<lower=1,upper=J> jj[N];
}
parameters {
real<lower=0>sigma[J]; //hierarchial variances
corr_matrix[K] Omega; //Covariance matrix
vector <lower=0>[K]tau; //scale components of the covariance matrix
matrix[L , K] gamma;  //Group-level coefficients
vector[K]beta[J]; //Individual level coefficients
real eta;
real<lower=0>nu;
}
transformed parameters{
real lognu;
lognu =log(nu);
}
model {
for (j in 1:J){
sigma[j] ~ cauchy(eta, nu);//hierarchical variances
}
tau ~ cauchy(0, 2.5);
Omega ~ lkj_corr(2);
to_vector(gamma) ~ normal(0,5);
{
  row_vector[K] u_gamma[J];
  for (j in 1:J)
  u_gamma[j] = z[j]*gamma;
  beta ~ multi_normal(u_gamma,quad_form_diag(Omega,tau));
}
for (n in 1:N)
y[n] ~ normal(x[n] * beta[jj[n]], sigma[jj[n]]);
}

generated quantities{
vector[N]y_rep;
vector[N]log_lik;
for(n in 1:N){
y_rep[n]= normal_rng(x[n]*beta[jj[n]],sigma[jj[n]]);
}
for (n in 1:N){
log_lik[n]= normal_lpdf(y[n]|x[n]*beta[jj[n]],sigma[jj[n]]);
}
}
"
