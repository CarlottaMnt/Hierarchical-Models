#PARTIAL POOLING MODEL
#The code now allows to estimate the regression parameters of a hierarchical Poisson generalized linear model.
#In order to partially pool information across the 15 municipalities here considered, it is requested to define an hyper-prior distribution 
#for the parameters that define the regression coefficients prior distribution. 
#The model assumes weakly informative hyper-prior distribution.
#The response variable is the number of different electric appliances household own in the different municipalities.
#Addtionaly to the individual level regressor (at a household level), I have added municipal level regressors that should increase the precision of the regression estimates.

#Import the required packages

library(pacman)
p_load(dplyr,summarytools, tidyr, readxl, writexl, ggplot2, forcats, sjlabelled,
       here, hablar, naniar, devtools, stringr, splitstackshape,
       validate,ggrepel,DataExplorer,rethinking, psycho, tidybayes,shinystan,bayesplot,matrixStats)

#Remove missing values

data <- data[complete.cases(data),]

#Select the response variable
y<-data$num_app

#Select and rescale explanatory variables at household level

data_rescale <- data %>%
  mutate_at(
    c("se1_6_educ","se3_6_nrdwelrooms","se1_8_timemkt","yearselec","duration_day"), .funs = c(scale = ~as.numeric(arm::rescale(., )))) 

x_rescale <-data_rescale %>% dplyr::select(se3_6_nrdwelrooms_scale,se1_6_educ_scale,duration_day_scale,
                                    yearselec_scale,se1_8_timemkt_scale)

#Group-level predictor
z <-mun_data[,c("municip_alt","municip_gdp","municip_gridperc","municip_popdens")]
z_rescale <-apply(z,2,arm::rescale)


#Stan model:Hierarchical model with group predictor----------------------------
poiss_data <- list(
  K=6,
  L=5,
  N=dim(x_rescale)[1],
  jj=as.numeric(data$gen_6_municip),
  J=15,
  x=cbind(1,x_rescale),
  z=cbind(1,z_rescale),
  y=data$num_app
)

poiss_code2 <- "data {
  int<lower=0> N;
  int<lower=0>y[N];//observation vector
  int<lower=0> K;
  matrix[N , K]x;
  int<lower=0> L; //number of group level regressor
  int<lower=1>J;//number of groups 
  row_vector[L] z[J];
  int<lower=1,upper=J> jj[N];
}
parameters {
corr_matrix[K] Omega;
vector <lower=0>[K]tau;
vector[K]beta[J];
matrix[L , K] gamma;
}

model {
tau ~ cauchy(0, 2.5);//hyper-prior distribution for scale component
Omega ~ lkj_corr(2);//hyper-prior distribution for covariance matrix
to_vector(gamma) ~ normal(0,5);
{
  row_vector[K] u_gamma[J];
  for (j in 1:J)
  u_gamma[j] = z[j]*gamma;
  beta ~ multi_normal(u_gamma,quad_form_diag(Omega,tau));//prior distribution
}
for (n in 1:N){
  y[n] ~ poisson_log(x[n] * beta[jj[n]]);//likelihood
}
}
generated quantities {
vector[N]y_rep;
vector[N]log_lik;
for(n in 1:N){
 if (x[n]*beta[jj[n]]< 20.7)
y_rep[n]=poisson_log_rng(x[n]*beta[jj[n]]);// replications for fit assessment.
else
y_rep[n]=9999;
 }
 for(n in 1:N){
 log_lik[n]=poisson_log_lpmf(y[n]|x[n]*beta[jj[n]]); //log-likelihood
}
}
"





