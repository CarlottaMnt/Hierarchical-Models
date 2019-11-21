#NO POOLING
#The code allow to estimate the regression parameters of a Poisson generalized linear model with log link function.
#The main characteristic of this model is the independence assumption across the several groups (15 municipalities) within the population.


#Import packages

library(pacman)
p_load(dplyr,summarytools, tidyr, readxl, writexl, ggplot2, forcats, sjlabelled,
       here, hablar, naniar, devtools, stringr, splitstackshape,
       validate,ggrepel,DataExplorer,rethinking, psycho, tidybayes,shinystan,bayesplot,matrixStats)

#Remove missing values

data <- data[complete.cases(data),]

#Select response variable
y<-data$num_app
#Explanatory variable matrix for the nopooled model: selecet and rescale.
x_nopool <- data %>% group_by(gen_6_municip) %>% 
  mutate_at(
    c("se1_6_educ","se3_6_nrdwelrooms","se1_8_timemkt","yearselec","duration_day"), .funs = c(scale = ~as.numeric(arm::rescale(., )))) %>% ungroup()
x_rescale_nopool <- x_nopool %>% dplyr::select(se1_6_educ_scale,se3_6_nrdwelrooms_scale,
                                        duration_day_scale,se1_8_timemkt_scale,yearselec_scale) 

#Stan model:nopooling model---------------------------
poiss_data <- list(
  N=dim(x_rescale_nopool)[1],
  y=data$num_app,
  x=cbind(1,x_rescale_nopool),
  K=6,
  jj=as.numeric(data$gen_6_municip),
  J=15
)


poiss_code1 <- "data {
  int<lower=0> N; 
  int<lower=0>y[N];
  int<lower=0> K;
  matrix[N , K]x;
  int<lower=1>J;//number of groups 
  int<lower=1,upper=J> jj[N];
}
parameters {
vector[K]beta[J];
}
model {
beta[1]~ cauchy(0,10); //weakly informative prior for the regression intercept
for (j in 2:K)
beta[j] ~ cauchy(0,2.5); //weakly informative prior for the regression slope, independent groups.
for (n in 1:N)
  y[n] ~ poisson_log(x[n] * beta[jj[n]]);//likelihood
}
generated quantities {
vector[N]y_rep;
vector[N]log_lik;
for(n in 1:N){
 if (x[n]*beta[jj[n]] < 20.7)
y_rep[n]=poisson_log_rng(x[n]*beta[jj[n]]);
else
y_rep[n]=9999;
 }
 for(n in 1:N){
 log_lik[n]=poisson_log_lpmf(y[n]|x[n]*beta[jj[n]]);
 }
}
"

