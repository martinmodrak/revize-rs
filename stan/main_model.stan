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

  matrix b_raw_to_b(matrix b_raw, vector b_sd, matrix q_corr_chol, int questions_correlation) {
    matrix[rows(b_raw), cols(b_raw)] b;
    if(rows(b_raw) != 0 && cols(b_raw) != 0) {
      if(questions_correlation) {
        b = diag_post_multiply(q_corr_chol * b_raw, b_sd);
      } else {
        for(f in 1:cols(b_raw)) {
          b[,f] = b_raw[,f] * b_sd[f];
        }
      }
    }
    return b;
  }
}

//Heavily based on code from brms
data {
  int<lower=1> N;
  int<lower=1> N_questions;
  int<lower=2> ncat;  // number of categories
  int<lower=1, upper = ncat> Y[N]; //response

  int<lower=1, upper=N_questions> questions[N];

  int<lower=0> N_fixed;
  //Fixed effects
  matrix[N, N_fixed] X;
  real<lower=0> effect_hyper_sigma;
  real<lower=0> intercept_sigma;

  //Monotonic fixed effects
  int<lower=0> N_monotonic;
  int<lower=2> N_monotonic_cat;
  int<lower=1, upper = N_monotonic_cat> X_monotonic [N, N_monotonic];

  //Random effects
  int<lower=0> N_random;
  int<lower=0> N_random_groups[N_random];
  int<lower=0, upper = max(N_random_groups)> X_random_groups[N, N_random];
  matrix[N, N_random] X_random;
  vector[N_random] random_hyper_sigma;

  //If true, coefficients for questions are correlated
  int<lower=0, upper=1> questions_correlation;
}

transformed data {
  //TODO: maybe check that predictors are centered
  int<lower=0> b_random_group_start[N_random];
  int<lower=0> b_random_to_group[sum(N_random_groups)];
  int<lower=0> b_random_index[N, N_random];

  if(N_random > 0) {
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
}

parameters {
  //Correlation across questions
  cholesky_factor_corr[questions_correlation ? N_questions : 1] q_corr_chol;

  //Fixed effects
  matrix[N_questions, N_fixed] b_raw;
  vector<lower=0>[N_fixed] b_sd;
  ordered[ncat - 1] intercept;

  //Monotonic effects
  matrix[N_questions, N_monotonic] b_monotonic_raw;
  vector<lower=0>[N_monotonic] b_monotonic_sd;
  simplex[N_monotonic_cat - 1] zeta_monotonic[N_monotonic];

  //Random effects
  matrix[N_questions, sum(N_random_groups)] b_random_raw;
  matrix<lower=0>[N_questions, N_random] b_random_sd;
}

transformed parameters {
  matrix[N_questions, N_fixed] b;
  matrix[N_questions, N_monotonic] b_monotonic;
  matrix[N_questions, sum(N_random_groups)] b_random;
  matrix[N_monotonic_cat, N_monotonic] b_monotonic_trans[N_questions];

  b = b_raw_to_b(b_raw, b_sd, q_corr_chol, questions_correlation);
  b_monotonic = b_raw_to_b(b_monotonic_raw, b_monotonic_sd, q_corr_chol, questions_correlation);

  {
    matrix[N_questions, sum(N_random_groups)] b_random_correlated_unscaled;
    for(r in 1:N_random) {
      int start = b_random_group_start[r];
      int end = start + N_random_groups[r] - 1;
      b_random_correlated_unscaled[, start:end] = b_raw_to_b(b_random_raw[, start:end], rep_vector(1, N_random_groups[r]), q_corr_chol, questions_correlation);
    }
    b_random = b_random_correlated_unscaled .* b_random_sd[,b_random_to_group];
  }

  {
    matrix[N_monotonic_cat, N_monotonic] b_monotonic_trans_base;
    for(n in 1:N_monotonic) {
      b_monotonic_trans_base[1, n] = 0;
      b_monotonic_trans_base[2:N_monotonic_cat, n] = cumsum(zeta_monotonic[n]);
    }

    for(q in 1:N_questions) {
      for(n in 1:N_monotonic) {
        b_monotonic_trans[q,,n] = b_monotonic_trans_base[,n] * b_monotonic[q,n];
      }
    }
  }


}

model {
  vector[N] mu;
  for(n in 1:N) {
    vector[N_monotonic] mon;
    real random = 0;
    real fixed = 0;
    for(n_m in 1:N_monotonic) {
      mon[n_m] = b_monotonic_trans[questions[n], X_monotonic[n, n_m], n_m];
    }
    if(N_random > 0) {
      random = X_random[n,] * to_vector(b_random[questions[n], b_random_index[n,]]);
    }
    if(N_fixed > 0) {
      fixed = X[n] * to_vector(b[questions[n],]);
    }
    mu[n] = fixed + sum(mon) +
      random;
  }


  // priors
  intercept ~ student_t(3, 0, intercept_sigma);
  q_corr_chol ~ lkj_corr_cholesky(1);
  b_sd ~ normal(0, effect_hyper_sigma);
  to_vector(b_raw) ~ normal(0, 1);

  b_monotonic_sd ~ normal(0, effect_hyper_sigma);
  to_vector(b_monotonic_raw) ~ normal(0, 1);

  to_vector(b_random_raw) ~ normal(0, 1);
  for(q in 1:N_questions) {
    b_random_sd[q,] ~ normal(0, random_hyper_sigma);
  }

  for(n_m in 1:N_monotonic) {
    zeta_monotonic[n_m] ~ dirichlet(rep_vector(1, N_monotonic_cat - 1));
  }

  // likelihood
  //target += ordered_logistic_lpmf(Y[n] | mu[n], intercept);
  Y ~ ordered_logistic_lpmf(mu, intercept);
}

generated quantities {
  vector[questions_correlation ? (N_questions * (N_questions-1))/2 : 0] q_corr_vec;
  if(questions_correlation) {
    matrix[N_questions, N_questions] q_corr = q_corr_chol * q_corr_chol';
    int i = 1;
    for(n1 in 1:(N_questions - 1)) {
      for(n2 in (n1+1):N_questions) {
        q_corr_vec[i] = q_corr[n1,n2];
        i = i + 1;
      }
    }
  }
}
