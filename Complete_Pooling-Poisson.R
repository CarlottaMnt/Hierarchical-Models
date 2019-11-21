#COMPLETE POOLING POISSON MODEL
#The code is made for estimating the regression paramenters of a generalized linear model.
#This kind of models are characterize for having a likelihood distribution which is not the traditional Normal.
#In this example, since the response variable is a counted variable ( household numbers of different electric appliances) the Poisson distribution is the more suitable.
#The log link function is assumed that relates the unique parameter of the Poisson distribution to the regression equation.
#The complete pooling of information provides a single set of regression estimates.

#Import the required packages
library(pacman)
p_load(dplyr,summarytools, tidyr, readxl, writexl, ggplot2, forcats, sjlabelled,
       here, hablar, naniar, devtools, stringr, splitstackshape,
       validate,ggrepel,DataExplorer,rethinking, psycho, tidybayes,shinystan,bayesplot,matrixStats)

# Remove missing variables   
data <- data[complete.cases(data),]
#Select response variable
y<-data$num_app

# Rescale the regressors 
data_rescale <- data %>%
  mutate_at(
    c("se1_6_educ","se3_6_nrdwelrooms","se1_8_timemkt","yearselec","duration_day"), .funs = c(scale = ~as.numeric(arm::rescale(., )))) 
# Create matrix for estimation
x_rescale <-data_rescale %>% dplyr::select(se3_6_nrdwelrooms_scale,se1_6_educ_scale,duration_day_scale,
                                    yearselec_scale,se1_8_timemkt_scale)

#Poisson model for appliances (Non-hierarc:complete pooling)-------------------------------------
poiss_data <- list(
  N=dim(x_rescale)[1],
  y=data$num_app,
  x=cbind(1,x_rescale),
  K=6
)

poiss_code<- "data {
  int<lower=0> N;
  int<lower=0>y[N];
  int<lower=0> K;
  matrix[N , K]x;
}

parameters {
  vector[K]beta;
}
model {
  beta[1]~ cauchy(0,10);
for (i in 2:K)
 beta[i] ~ cauchy(0,2.5);
for (i in 1:N)
   y[i]~ poisson_log(x[i] * beta); 
}

generated quantities {
vector[N]y_pred;
vector[N]log_lik;
for(i in 1:N){
 if (x[i]*beta < 20.7)
 	y_pred[i]= poisson_log_rng(x[i]*beta);
 else
     y_pred[i]=-1;
}
for(i in 1:N){
 log_lik[i]=poisson_log_lpmf(y[i]|x[i]*beta);
 }
}
"

