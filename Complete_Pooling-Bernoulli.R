#COMPLETE POOLING 
#The code above implement a logistic regression model with Stan
#The 15 municipalities are pooled togheter and a unique set of regression parameters is estimated.
#The main characteristic is the assignment of a Bernoulli distribution as likelihood of the response variable.
#In this case the response variable is the ownership of a specific electric appliances, so it takes only 0/1 values.
#The link function that relates the unique paramenter of the bernoulli distribution to the regression equation is the logit function.

#Import the required packages
library(pacman)
p_load(dplyr,summarytools, tidyr, readxl, writexl, ggplot2, forcats, sjlabelled,
       here, hablar, naniar, devtools, stringr, splitstackshape,
       validate,ggrepel,DataExplorer,rethinking, psycho, tidybayes,shinystan,bayesplot,matrixStats)

#Remove the missing values

data <- data[complete.cases(data),]
#Select the response variable

y <-data$app_yn

#Select and rescale the explanatory variables 
x <-data %>% mutate_at(
    c("se1_6_educ","se3_6_nrdwelrooms","se1_8_timemkt","yearselec","duration_day"), .funs = c(scale = ~as.numeric(arm::rescale(., )))) 

x_rescale <- x %>% dplyr::select(se3_6_nrdwelrooms_scale,se1_6_educ_scale,
                                        duration_day_scale,yearselec_scale,se1_8_timemkt_scale) 

#Stan model:Complete pooling(Input rescaled)------------------------------------
data_bern <- list(
  y=data$tv,
  x = cbind(1,x_rescale),
  N=dim(x_rescale)[1],
  K=ncol(x_rescale)+1
)


bin_code <- "data{
  int<lower=1> K;//number of predictors
  int<lower=1> N;
  int<lower=0,upper=1>y[N];
  matrix[N , K] x;
}
parameters {
  vector[K]beta;//regression parameters (including the intercept)
}
model {
 beta[1]~ cauchy(0,10);
for (i in 2:K)
 beta[i] ~ cauchy(0,2.5);
 for (n in 1:N)
  y[n] ~ bernoulli_logit(x[n] * beta);//likelihood
}
generated quantities{
int<lower=0,upper=1>y_pred[N];
vector[N]log_lik;
for (n in 1:N)
y_pred[n]=bernoulli_rng(inv_logit(x[n]*beta));//fitted values
for(n in 1:N){
log_lik[n]=bernoulli_logit_lpmf(y[n]|x[n]*beta);
}
}
"
                                        
                                       
