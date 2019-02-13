library(rstan)

functions_to_expose <-
  '
functions {
int ordered_logit_rng(real eta, vector c) {
  return ordered_logistic_rng(eta, c);
}
}
'
functions_to_expose <- stanc(model_code = functions_to_expose, model_name = "functions_to_expose")
# NOT RUN {
expose_stan_functions(functions_to_expose)

# }


simulate_data <- function(N, ncat, N_monotonic, N_monotonic_cat = 4) {
  intercept_sigma = 1

  b <- rnorm(1, 0, 1)
  intercept <- sort(rt(ncat - 1, 3) * intercept_sigma)

  X <- rnorm(N, 0, 1)

  X_monotonic = array(sample(1:N_monotonic_cat, N * N_monotonic, replace = TRUE), c(N, N_monotonic))

  b_monotonic = rnorm(N_monotonic, 0, 1)
  zeta_monotonic = MCMCpack::rdirichlet(N_monotonic, rep(1, length.out = N_monotonic_cat))

  mu <-  X * b;
  for(n in 1:N) {
    for(N_m in 1:N_monotonic) {
      mu[n] <- mu[n] + b_monotonic[N_m] * sum(zeta_monotonic[N_m, 1:X_monotonic[n,N_m]])
    }
  }

  Y <- purrr::map_dbl(mu, ordered_logit_rng, intercept)

  data = list(
    observed = list(
      N = N,
      ncat = ncat,
      X = X,
      N_monotonic = N_monotonic,
      N_monotonic_cat = N_monotonic_cat,
      X_monotonic = X_monotonic,
      Y = Y,
      intercept_sigma = 1
    ),
    true = list(
      b = b,
      b_monotonic = b_monotonic,
      zeta_monotonic = zeta_monotonic,
      intercept = intercept
    )
  )
}
