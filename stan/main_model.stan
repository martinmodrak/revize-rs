//Heavily based on code from brms
data {
  int<lower=1> N;
  int<lower=2> ncat;  // number of categories
  int<lower=1, upper = ncat> Y[N]; //response
  vector[N] X;

  //prior data
  real<lower=0> intercept_sigma;
}

transformed data {
  //TODO: maybe check that predictors are centered
}

parameters {
  real b;
  ordered[ncat - 1] intercept;
}

model {
  vector[N] mu = X * b;
  // priors
  //target += student_t_lpdf(intercept | 3, 0, 10);
  intercept ~ student_t(3, 0, intercept_sigma);
  b ~ normal(0, 1);

  // likelihood
  //target += ordered_logistic_lpmf(Y[n] | mu[n], intercept);
  Y ~ ordered_logistic_lpmf(mu, intercept);
}
