# Stan-Hierarchical-Models
Stan codes for hierarchical regression models written in R.
Three hierarchical regression models with Normal, Poisson and Bernoulli likelihood distributions are implemented for estimating the regression intercept and slopes. 

The hierarchical structure allows to infer the regression parameters for several groups at one time.
Through the different model specifications, the parameters are estimated with a complete pooling, no-pooling and finally a partial pooling of information across the groups. The partial pooling of information is especially achieved insering a hyper-prior distribution among all the regression paramenters. 
For this last model, weakly informative hyper-prior distributions are used.

The reference for the project are:

1- https://mc-stan.org/users/documentation/.

2- A.Gelman et al. "Bayesian Data Analysis"(2013).

3- A.Gelman, J.Hill "Data Analysis using regression and Multilevel/Hierarchical Models (2007).
