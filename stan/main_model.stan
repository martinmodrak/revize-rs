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

  //Monotonic fixed effects
  int<lower=0> N_monotonic;
  int<lower=2> N_monotonic_cat;
  int<lower=1, upper = N_monotonic_cat> X_monotonic [N, N_monotonic];

  //Random effects
  int<lower=0> N_random;
  int<lower=0> N_random_groups[N_random];
  int<lower=0, upper = max(N_random_groups)> X_random_groups[N, N_random];
  matrix[N_random, N] X_random;
  vector[N_random] random_hyper_sigma;
}

transformed data {
  //TODO: maybe check that predictors are centered
  int<lower=0> b_random_group_start[N_random];
  int<lower=0> b_random_to_group[sum(N_random_groups)];
  int<lower=0> b_random_index[N, N_random];

  b_random_group_start[1] = 1;
  for(i in 2:N_random) {
    b_random_group_start[i] = b_random_group_start[i - 1] + N_random_groups[i - 1];
  }
  for(i in 1:N_random) {
    for(k in b_random_group_start[i]:(b_random_group_start[i] + N_random_groups[i] - 1)) {
      b_random_to_group[k] = i;
    }

    for(n in 1:N) {
      b_random_index[n, i] = b_random_group_start[i] + X_random_groups[n, i] - 1;
    }

  }
}

parameters {
  //Fixed effects
  real b;
  ordered[ncat - 1] intercept;

  //Monotonic effects
  vector[N_monotonic] b_monotonic;
  simplex[N_monotonic_cat - 1] zeta_monotonic[N_monotonic];

  //Random effects
  vector[sum(N_random_groups)] b_random_raw;
  vector[N_random] sigma_random;
}

transformed parameters {
  matrix[N_monotonic_cat, N_monotonic] b_monotonic_trans;

  vector[sum(N_random_groups)] b_random;

  for(n in 1:N_monotonic) {
    b_monotonic_trans[1, n] = 0;
    b_monotonic_trans[2:N_monotonic_cat, n] = b_monotonic[n] * cumsum(zeta_monotonic[n]);
  }

  b_random = b_random_raw .* sigma_random[b_random_to_group];
}

model {
  vector[N] mu;
  for(n in 1:N) {
    vector[N_monotonic] mon;
    for(n_m in 1:N_monotonic) {
      mon[n_m] = b_monotonic_trans[X_monotonic[n, n_m], n_m];
    }
    mu[n] = X[n] * b + sum(mon) +//sum(diagonal(b_monotonic_trans[X_monotonic[n,]])) +
      sum(X_random[,n] .* b_random[b_random_index[n,]]);
  }
  // priors
  //target += student_t_lpdf(intercept | 3, 0, 10);
  intercept ~ student_t(3, 0, intercept_sigma);
  b ~ normal(0, 1);
  b_monotonic ~ normal(0, 1);

  b_random_raw ~ normal(0, 1);
  sigma_random ~ normal(0, random_hyper_sigma);

  // likelihood
  //target += ordered_logistic_lpmf(Y[n] | mu[n], intercept);
  Y ~ ordered_logistic_lpmf(mu, intercept);
}
