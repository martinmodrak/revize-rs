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
expose_stan_functions(functions_to_expose)


simulate_data <- function(N, ncat, N_random_groups, N_monotonic, N_monotonic_cat = 4)  {
  intercept_sigma <- 1

  b <- rnorm(1, 0, 1)
  intercept <- sort(rt(ncat - 1, 3) * intercept_sigma)

  X <- rnorm(N, 0, 1)

  X_monotonic <- array(sample(1:N_monotonic_cat, N * N_monotonic, replace = TRUE), c(N, N_monotonic))

  b_monotonic <- rnorm(N_monotonic, 0, 1)
  zeta_monotonic <- MCMCpack::rdirichlet(N_monotonic, rep(1, length.out = N_monotonic_cat - 1))

  N_random <- length(N_random_groups)
  random_hyper_sigma <- rep(1, N_random)
  sigma_random <- abs(rnorm(N_random, 0, random_hyper_sigma))

  b_random_group_start <- cumsum(N_random_groups) - N_random_groups + 1
  b_random_to_group <- integer(sum(N_random_groups))
  for(i in 1:N_random) {
    for(k in 1:N_random_groups[i]) {
      b_random_to_group[b_random_group_start[i] + k - 1] <- i
    }
  }

  b_random <- rnorm(sum(N_random_groups), 0, sigma_random[b_random_to_group])

  X_random_groups <- array(NA_real_, c(N, N_random))
  for(i in 1:N_random) {
    X_random_groups[,i] <- sample(1:N_random_groups[i], N, replace = TRUE)
  }
  X_random <- array(rbinom(N_random * N, 1, prob = 0.3), c(N_random, N))

  mu <- X * b;
  for(n in 1:N) {
    for(N_m in 1:N_monotonic) {
      if(X_monotonic[n, N_m] > 1) {
        mu[n] <- mu[n] + b_monotonic[N_m] * sum(zeta_monotonic[N_m, 1:(X_monotonic[n,N_m] -1)])
      }
    }
    mu[n] <- mu[n] + sum(X_random[,n] * b_random[X_random_groups[n,]])
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
      N_random = N_random,
      N_random_groups = N_random_groups,
      X_random = X_random,
      X_random_groups = X_random_groups,
      random_hyper_sigma = random_hyper_sigma,
      Y = Y,
      intercept_sigma = 1
    ),
    true = list(
      b = b,
      b_monotonic = b_monotonic,
      zeta_monotonic = zeta_monotonic,
      b_random = b_random,
      sigma_random = sigma_random,
      intercept = intercept
    )
  )
}
