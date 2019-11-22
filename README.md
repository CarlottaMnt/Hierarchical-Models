# Stan-Hierarchical-Models
Stan codes for hierarchical regression models written in R.

For the final reasearch of my Master degree in Economics and Social Sciences I have built three hierarchical regression models with Normal, Poisson and Bernoulli likelihood distributions, for analysing the demand and supply of electricity in some rural Nepal municipalities.

The estimates of interest are the regression intercept and slopes.

The hierarchical structure allows to infer the regression parameters for several municipalities at one time, as well as it highlights the variability between and within the same municipalities. 

Through the different model specifications, the parameters are estimated with a complete pooling, no-pooling and finally a partial pooling of information across the groups. The partial pooling of information is especially achieved insering a hyper-prior distribution among all the regression parameters, hence it is the only tha has a proper hierarchical specification. 
For this last model, weakly informative hyper-prior distributions are used.

The main reference for the project are:

1- https://mc-stan.org/users/documentation/.

2- A.Gelman et al. "Bayesian Data Analysis"(2013).

3- A.Gelman, J.Hill "Data Analysis using regression and Multilevel/Hierarchical Models "(2007).
