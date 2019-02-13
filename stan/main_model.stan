functions {
  vector cumsum(vector x) {
    real s = 0;
    vector[rows(x)] res;
    for(n in 1:rows(x)) {
      s = s + x[n];
      res[n] = s;
    }
    return res;
  }
}

//Heavily based on code from brms
data {
  int<lower=1> N;
  int<lower=2> ncat;  // number of categories
  int<lower=1, upper = ncat> Y[N]; //response

  //Fixed effects
  vector[N] X;
  real<lower=0> intercept_sigma;

  //Monotonic  effects
  int<lower=0> N_monotonic;
  int<lower=2> N_monotonic_cat;
  int<lower=1, upper = N_monotonic_cat> X_monotonic [N, N_monotonic];
}

transformed data {
  //TODO: maybe check that predictors are centered
}

parameters {
  //Fixed effects
  real b;
  ordered[ncat - 1] intercept;

  //Monotonic effects
  vector[N_monotonic] b_monotonic;
  simplex[N_monotonic_cat] zeta_monotonic[N_monotonic];
}

transformed parameters {
  matrix[N_monotonic_cat, N_monotonic] b_monotonic_trans;
  for(n in 1:N_monotonic) {
    b_monotonic_trans[,n] = b_monotonic[n] * cumsum(zeta_monotonic[n]);
  }
}

model {
  vector[N] mu;
  for(n in 1:N) {
    mu[n] = X[n] * b + sum(diagonal(b_monotonic_trans[X_monotonic[n,]]));
  }
  // priors
  //target += student_t_lpdf(intercept | 3, 0, 10);
  intercept ~ student_t(3, 0, intercept_sigma);
  b ~ normal(0, 1);
  b_monotonic ~ normal(0, 1);

  // likelihood
  //target += ordered_logistic_lpmf(Y[n] | mu[n], intercept);
  Y ~ ordered_logistic_lpmf(mu, intercept);
}
