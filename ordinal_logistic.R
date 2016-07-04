rm(list=ls())

library(ordinal)
library(rstan)

set.seed(382992)


fm <- clm(rating ~ temp * contact, data = wine)
fm2 <- clm(rating ~ temp + contact, data = wine)
fm3 <- clm(rating ~ contact, data = wine[wine$temp=="cold",])
#fm2 <- clm(rating ~ temp * contact - temp, data = wine)
summary(fm)
summary(fm2)
summary(fm3)

model_code <- "
data {
    int<lower=2> K; //Number of categories
    int<lower=0> N; //Number of observations
    int<lower=1> P; //Number of covariates
    int<lower=1, upper=K> y[N];
    row_vector[P] x[N];
}
parameters {
    vector[P] beta;
    ordered[K-1] c;
}
model {
    for(n in 1:N) y[n] ~ ordered_logistic(x[n] * beta, c);
}
"
x <- model.matrix(fm)$X[,-1]
dat <- list(x=x, K=5, N=nrow(wine), y=c(wine$rating), P=ncol(x))
bm <- stan(model_code=model_code, seed=10, data=dat, iter=2000)
